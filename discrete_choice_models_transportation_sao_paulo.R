rm(list=ls())

library(tidyverse)
library(sf)
library(mlogit)
library(rgdal)
library(haven)
library(geosphere)
library(broom)
library(stargazer)
library(janitor)
library(xtable)
library(lmtest)
library(knitr)
library(kableExtra)
library(bbplot)
options(scipen=999)
##########################################################################################################################
# OD DATA TREATMENT 
##########################################################################################################################

dataset <- read_sav("/OD 2017/OD_2017_v1.sav")


### CREATING IMPORTANT VARIABLES
dataset <- dataset %>%
  filter(modoprin != 0, renda_fa != 0, idade > 18) %>% # some filters 
  mutate(choice = case_when(modoprin %in% 1:3 ~ 2,
                            modoprin %in% 4:8 ~ 3,
                            modoprin %in% 9 ~ 4,
                            modoprin %in% 11:12 ~ 5, 
                            modoprin %in% 13 ~ 6,
                            TRUE ~ 1),
         distance = distancia/1000,
         cost = case_when(modoprin %in% 1:3 ~ 4, # metro
                          modoprin == 4 ~ 4, # sp trans
                          modoprin %in% 5:6 ~ 5.2, # emtu
                          modoprin == 7 ~ (2/10)*distance, # fretado
                          modoprin == 8 ~ 4, # onibus escolar
                          modoprin == 9 ~ 4*distance/8, # carro
                          modoprin == 10 ~ 0, # carona
                          modoprin %in% 11:12 ~ 4.50 + 2.5*distance, # taxi
                          modoprin == 13 ~ 4*distance/40, # moto
                          TRUE ~ 0), # caronas, bike, a pé, outros
         mode = case_when(choice == 1 ~ "walk",
                          choice == 2 ~ "metro",
                          choice == 3 ~ "bus",
                          choice == 4 ~ "car",
                          choice == 5 ~ "taxi",
                          choice == 6 ~ "moto"),
         hours = duracao/60,
         speed = distance/hours,
         sp_fora = ifelse(zona_o %in% 1:342 & zona_d %in% 343:517,1,0),
         fora_sp = ifelse(zona_o %in% 343:517 & zona_d %in% 1:342,1,0),
         ce = ifelse(zona_o %in% c(1:108) | zona_d %in% c(1:108),1,0),
         peak = ifelse(h_saida %in% c(6,7,8,12,17,18),1,0),
         educ = ifelse(estuda == 1, 0, 1 ),
         trabalho_casa = ifelse(zona_o == zonatra1,1,0))



### SOME ADJUSTMENTS

# adjusting bus and subway costs when transshipment happens
dataset <- dataset %>%
  mutate(cost = case_when((modo1 %in% 1:3 & modo2 %in% 4:8) | (modo1 %in% 4:8 & modo2 %in% 1:3) |
                            (modo1 %in% 1:3 & modo3 %in% 4:8) | (modo1 %in% 4:8 & modo3 %in% 1:3) |
                            (modo2 %in% 1:3 & modo3 %in% 4:8) | (modo2 %in% 4:8 & modo3 %in% 1:3) ~ 7.2,
                          TRUE ~ cost))

# cost is zero when employer pays for it
dataset <- dataset %>% mutate(cost = ifelse(pag_viag == 2 & choice %in% c(2,3), 0, cost))

### DESCRIPTIVE ANALYSIS
dataset %>% group_by(modoprin) %>% summarise(n = sum(fe_via)) %>% 
  mutate(nome = case_when(modoprin == 1 ~ "Subway",
                          modoprin == 2 ~ "Rail",
                          modoprin == 4 ~ "Bus inside SP",
                          modoprin == 5 ~ "Bus from another city",
                          modoprin == 6 ~ "Metropolitan Bus",
                          modoprin == 7 ~ "Hired Bus",
                          modoprin == 8 ~ "School Bus",
                          modoprin == 9 ~ "Car",
                          modoprin == 10 ~ "Car Lift",
                          modoprin == 11 ~ "Taxi",
                          modoprin == 12 ~ "Ride Hailing",
                          modoprin == 13 ~ "Motorcycle",
                          modoprin == 14 ~ "Motorcycle Lift",
                          modoprin == 15 ~ "Bicycle",
                          modoprin == 16 ~ "Walk",
                          modoprin == 17 ~ "Others")) %>% 
  mutate(perc = round(n/sum(n), 2)) %>% 
  ggplot(aes(x = nome, y = perc, 
             label= scales::percent(perc)))+
  geom_bar(stat="identity", position="identity", fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() + ylab("") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   hjust=1, size = 12),
        axis.text.y=element_blank()) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 4)


# TABLE 1

dataset %>% group_by(mode) %>% summarise(ce = sum(ce), n = n()) %>% 
  mutate(prop = ce/n) %>% ungroup()

dataset %>% group_by(mode) %>%
  summarise(Time = mean(hours, na.rm = T),
            Distance = mean(distance, na.rm = T),
            Income = mean(renda_fa, na.rm = T),
            Education = mean(grau_ins, na.rm = T),
            Age = mean(idade, na.rm = T),
            n = sum(fe_via)) %>% mutate(Trips = n/ sum(n)) %>% select(-n) %>% kable()

# TABLE 2

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}


dataset %>% 
  mutate(quantile = ntile(renda_fa,10),
         in_ec = ifelse(zona %in% c(1:108), 1, 0)) %>% 
  group_by(quantile) %>% 
  summarise(`Mean Income` = mean(renda_fa),
            n = n(),
            `% in EC` = sum(in_ec)/n,
            `Trip Time` = mean(hours),
            `Most Frequent Mode` = Mode(mode)) %>% # return the most frequent mode
  select(-n, -quantile)  %>% xtable()


##########################################################################################################################
# COUNTERFACTUAL REGRESSIONS V2
##########################################################################################################################

# Loop for regression
counterfactual_ols_regression <- function(df, dependent_var, choice_mode) {
  formula_str <- paste(dependent_var, "~ distance + factor(h_saida) + factor(h_cheg) + trabalho_casa")
  lm(as.formula(formula_str), data = df %>% filter(choice == choice_mode))
}


# Cost regression
car_cost <- counterfactual_ols_regression(dataset, "cost", 4)
taxi_cost <- counterfactual_ols_regression(dataset, "cost", 5)
moto_cost <- counterfactual_ols_regression(dataset, "cost", 6)

# Time regression
metro_time <- counterfactual_ols_regression(dataset, "hours", 2)
bus_time <- counterfactual_ols_regression(dataset, "hours", 3)
car_time <- counterfactual_ols_regression(dataset, "hours", 4)
taxi_time <- counterfactual_ols_regression(dataset, "hours", 5)
moto_time <- counterfactual_ols_regression(dataset, "hours", 6)


##### COST BETA 0
car_beta0 <- tidy(car_cost) %>% select(2) %>% slice(1) %>%
  mutate(mode = "car") %>% rename(cost_beta0 = estimate)

moto_beta0 <- tidy(moto_cost) %>% select(2) %>% slice(1) %>%
  mutate(mode = "moto") %>% rename(cost_beta0 = estimate)

taxi_beta0 <- tidy(taxi_cost) %>% select(2) %>% slice(1) %>%
  mutate(mode = "taxi") %>% rename(cost_beta0 = estimate)

walk_beta0 <- car_beta0 %>% transmute(cost_beta0 = 0, mode = "walk")

bus_beta0 <- car_beta0 %>% transmute(cost_beta0 = 4, mode = "bus")

metro_beta0 <- car_beta0 %>% transmute(cost_beta0 = 4, mode = "metro")

cost_beta0 <- bind_rows(car_beta0, moto_beta0, walk_beta0, bus_beta0, metro_beta0, taxi_beta0)
rm(car_beta0, moto_beta0, walk_beta0, bus_beta0, metro_beta0, taxi_beta0)

##### COST BETA 1

car_beta1 <- tidy(car_cost) %>% select(2) %>% slice(2) %>%
  mutate(mode = "car") %>% rename(cost_beta1 = estimate)

moto_beta1 <- tidy(moto_cost) %>% select(2) %>% slice(2) %>%
  mutate(mode = "moto") %>% rename(cost_beta1 = estimate)

taxi_beta1 <- tidy(taxi_cost) %>% select(2) %>% slice(2) %>%
  mutate(mode = "taxi") %>% rename(cost_beta1 = estimate)

cost_beta1 <- bind_rows(car_beta1, moto_beta1, taxi_beta1)
rm(car_beta1, moto_beta1, taxi_beta1)


##### COST BETA 2
a <-tidy(car_cost)
car_beta2 <- tidy(car_cost) %>% select(1:2) %>% slice(3:25) %>%
  mutate(mode = "car", h_saida = seq(1:23)) %>% select(-term) %>%
  rename(cost_beta2 = estimate)

moto_beta2 <- tidy(moto_cost) %>% select(1:2) %>% slice(3:25) %>%
  mutate(mode = "moto", h_saida = seq(1:23)) %>% select(-term) %>%
  rename(cost_beta2 = estimate)

taxi_beta2 <- tidy(taxi_cost) %>% select(1:2) %>% slice(3:25) %>%
  mutate(mode = "taxi", h_saida = seq(1:23)) %>% select(-term) %>%
  rename(cost_beta2 = estimate)

cost_beta2 <- bind_rows(car_beta2, moto_beta2, taxi_beta2)
rm(car_beta2, moto_beta2, taxi_beta2)

##### COST BETA 3

car_beta3 <- tidy(car_cost) %>% select(2) %>% slice(26:48) %>%
  mutate(mode = "car", h_cheg = seq(1:23)) %>%
  rename(cost_beta3 = estimate)

moto_beta3 <- tidy(moto_cost) %>% select(2) %>% slice(26:48) %>%
  mutate(mode = "moto", h_cheg = seq(1:23)) %>%
  rename(cost_beta3 = estimate)

taxi_beta3 <- tidy(taxi_cost) %>% select(2) %>% slice(26:48) %>%
  mutate(mode = "taxi", h_cheg = seq(1:23)) %>%
  rename(cost_beta3 = estimate)

cost_beta3 <- bind_rows(car_beta3, moto_beta3, taxi_beta3)
rm(car_beta3, moto_beta3, taxi_beta3)

##### COST BETA 4
car_beta4 <- tidy(car_cost) %>% select(2) %>% slice(49) %>%
  mutate(mode = "car",  trabalho_casa = 1) %>%
  rename(cost_beta4 = estimate)

moto_beta4 <- tidy(moto_cost) %>% select(2) %>% slice(49) %>%
  mutate(mode = "moto",  trabalho_casa = 1) %>%
  rename(cost_beta4 = estimate)


taxi_beta4 <- tidy(taxi_cost)  %>% select(2) %>% slice(49) %>%
  mutate(mode = "taxi", trabalho_casa = 1) %>%
  rename(cost_beta4 = estimate)


cost_beta4 <- bind_rows(car_beta4, moto_beta4, taxi_beta4)
rm(car_beta4, moto_beta4, taxi_beta4)

# TIME COSTS

# BETA 0
#timewalk_beta0 <- tidy(metro_time) %>% 
#  transmute(time_beta0 = 0, mode = "walk") %>% slice(1)

timemetro_beta0 <- tidy(metro_time) %>% select(2) %>%  slice(1) %>%
  mutate(mode = "metro") %>% rename(time_beta0 = estimate)

timebus_beta0 <- tidy(bus_time) %>% select(2) %>%  slice(1) %>%
  mutate(mode = "bus") %>% rename(time_beta0 = estimate)

timecar_beta0 <- tidy(car_time) %>% select(2) %>%  slice(1) %>%
  mutate(mode = "car") %>% rename(time_beta0 = estimate)

timemoto_beta0 <- tidy(moto_time) %>% select(2) %>%  slice(1) %>%
  mutate(mode = "moto") %>% rename(time_beta0 = estimate)

timetaxi_beta0 <- tidy(taxi_time) %>% select(2) %>%  slice(1) %>%
  mutate(mode = "taxi") %>% rename(time_beta0 = estimate)

time_beta0 <- bind_rows(timemetro_beta0, timebus_beta0, timetaxi_beta0, timecar_beta0, timemoto_beta0)
rm(timemetro_beta0, timebus_beta0, timetaxi_beta0, timecar_beta0, timemoto_beta0)

# BETA 1

timewalk_beta1 <- tidy(metro_time) %>% 
  transmute(time_beta1 = 1/3, mode = "walk") %>% slice(1) # pessoas andam a 3km/h

timemetro_beta1 <- tidy(metro_time) %>% select(2) %>%  slice(2) %>%
  mutate(mode = "metro") %>% rename(time_beta1 = estimate)

timebus_beta1 <- tidy(bus_time) %>% select(2) %>%  slice(2) %>%
  mutate(mode = "bus") %>% rename(time_beta1 = estimate)

timecar_beta1 <- tidy(car_time) %>% select(2) %>%  slice(2) %>%
  mutate(mode = "car") %>% rename(time_beta1 = estimate)

timemoto_beta1 <- tidy(moto_time) %>% select(2) %>%  slice(2) %>%
  mutate(mode = "moto") %>% rename(time_beta1 = estimate)

timetaxi_beta1 <- tidy(taxi_time) %>% select(2) %>%  slice(2) %>%
  mutate(mode = "taxi") %>% rename(time_beta1 = estimate)

time_beta1 <- bind_rows(timewalk_beta1, timemetro_beta1, timetaxi_beta1, timebus_beta1, timecar_beta1, timemoto_beta1)
rm(timewalk_beta1, timemetro_beta1, timebus_beta1,timetaxi_beta1, timecar_beta1, timemoto_beta1)

#BETA 2

#timewalk_beta2 <- tidy(walk_time) %>% select(2) %>%  slice(3:25) %>%
#  mutate(mode = "walk", h_saida = seq(1,23)) %>% rename(time_beta2 = estimate)

timemetro_beta2 <- tidy(metro_time) %>% select(2) %>%  slice(3:24) %>%
  mutate(mode = "metro", h_saida = seq(2,23)) %>% rename(time_beta2 = estimate)

timebus_beta2 <- tidy(bus_time) %>% select(2) %>%  slice(3:25) %>%
  mutate(mode = "bus", h_saida = seq(1,23)) %>% rename(time_beta2 = estimate)

timecar_beta2 <- tidy(car_time) %>% select(2) %>%  slice(3:25) %>%
  mutate(mode = "car", h_saida = seq(1,23)) %>% rename(time_beta2 = estimate)

timemoto_beta2 <- tidy(moto_time) %>% select(2) %>%  slice(3:25) %>%
  mutate(mode = "moto", h_saida = seq(1,23)) %>% rename(time_beta2 = estimate)

timetaxi_beta2 <- tidy(taxi_time) %>% select(2) %>%  slice(3:25) %>%
  mutate(mode = "taxi", h_saida = seq(1,23)) %>% rename(time_beta2 = estimate)

time_beta2 <- bind_rows(timemetro_beta2, timetaxi_beta2, timebus_beta2, timecar_beta2, timemoto_beta2)
rm(timewalk_beta2, timemetro_beta2, timebus_beta2, timetaxi_beta2, timecar_beta2, timemoto_beta2)

# BETA 3
#timewalk_beta3 <- tidy(walk_time) %>% select(2) %>%  slice(26:48) %>%
#  mutate(mode = "walk", h_cheg = seq(1,23)) %>% rename(time_beta3 = estimate)

timemetro_beta3 <- tidy(metro_time) %>% select(2) %>%  slice(25:45) %>%
  mutate(mode = "metro", h_cheg = c(1,seq(4,23))) %>% rename(time_beta3 = estimate)

timebus_beta3 <- tidy(bus_time) %>% select(2) %>%  slice(26:48) %>%
  mutate(mode = "bus", h_cheg = seq(1,23)) %>% rename(time_beta3 = estimate)

timecar_beta3 <- tidy(car_time) %>% select(2) %>%  slice(26:48) %>%
  mutate(mode = "car", h_cheg = seq(1,23)) %>% rename(time_beta3 = estimate)

timemoto_beta3 <- tidy(moto_time) %>% select(2) %>%  slice(26:48) %>%
  mutate(mode = "moto", h_cheg = seq(1,23)) %>% rename(time_beta3 = estimate)

timetaxi_beta3 <- tidy(taxi_time) %>% select(2) %>%  slice(26:48) %>%
  mutate(mode = "taxi", h_cheg = seq(1,23)) %>% rename(time_beta3 = estimate)

time_beta3 <- bind_rows(timemetro_beta3, timetaxi_beta3, timebus_beta3, timecar_beta3, timemoto_beta3)
rm(timemetro_beta3, timebus_beta3, timetaxi_beta3, timecar_beta3, timemoto_beta3)


# BETA 4
#timewalk_beta4 <- tidy(walk_time) %>% select(2) %>%  slice(49) %>%
#  mutate(mode = "walk", trabalho_casa = 1) %>% rename(time_beta4 = estimate)

timemetro_beta4 <- tidy(metro_time) %>% select(2) %>%  slice(46) %>%
  mutate(mode = "metro", trabalho_casa = 1) %>% rename(time_beta4 = estimate)

timebus_beta4 <- tidy(bus_time) %>% select(2) %>%  slice(49) %>%
  mutate(mode = "bus", trabalho_casa = 1) %>% rename(time_beta4 = estimate)

timecar_beta4 <- tidy(car_time) %>% select(2) %>%  slice(49) %>%
  mutate(mode = "car", trabalho_casa = 1) %>% rename(time_beta4 = estimate)

timemoto_beta4 <- tidy(moto_time) %>% select(2) %>%  slice(49) %>%
  mutate(mode = "moto", trabalho_casa = 1) %>% rename(time_beta4 = estimate)

timetaxi_beta4 <- tidy(taxi_time) %>% select(2) %>%  slice(49) %>%
  mutate(mode = "taxi", trabalho_casa = 1) %>% rename(time_beta4 = estimate)

time_beta4 <- bind_rows(timemetro_beta4, timetaxi_beta4, timebus_beta4, timecar_beta4, timemoto_beta4)
rm(timemetro_beta4, timebus_beta4, timetaxi_beta4, timecar_beta4, timemoto_beta4)


rm(car_cost, car_time, moto_cost, moto_time, metro_time, bus_time, walk_time, taxi_cost, taxi_time)


########### transforming data to fit discrete choice model format (long format)

data_reg <- dataset %>% mutate(count = 6) %>% group_by(id_ordem) %>%
  expand(count = seq(1:count))


data_reg <- dataset %>% select(id_ordem, choice, zona_o, zona_d, f_pess, idade, sexo, vl_ren_i,
                         cd_ativi, cost, grau_ins, renda_fa, hours, distance, motivo_o, motivo_d,
                         educ, h_saida, h_cheg, sp_fora, fora_sp, ce, peak, trabalho_casa) %>%
  right_join(data_reg, dataset, by = "id_ordem")

data_reg <- data_reg %>%
  mutate(mode = case_when(count == 1 ~ "walk",
                          count == 2 ~ "metro",
                          count == 3 ~ "bus",
                          count == 4 ~ "car",
                          count == 5 ~ "taxi",
                          count == 6 ~ "moto"),
         mode_choice = case_when(count == choice ~ 1,
                                 TRUE ~ 0)) %>% select(-count, -choice)


# joining costs
data_reg <- data_reg %>% left_join(cost_beta0, by = "mode")
data_reg <- data_reg %>% left_join(cost_beta1, by = "mode")
data_reg <- data_reg %>% left_join(cost_beta2, by = c("mode","h_saida"))
data_reg <- data_reg %>% left_join(cost_beta3, by = c("mode","h_cheg"))
data_reg <- data_reg %>% left_join(cost_beta4, by = c("mode","trabalho_casa"))



# joining time
data_reg <- data_reg %>% left_join(time_beta0, by = "mode")
data_reg <- data_reg %>% left_join(time_beta1, by = "mode")
data_reg <- data_reg %>% left_join(time_beta2, by = c("mode","h_saida"))
data_reg <- data_reg %>% left_join(time_beta3, by = c("mode","h_cheg"))
data_reg <- data_reg %>% left_join(time_beta4, by = c("mode","trabalho_casa"))


# removing NA
data_reg <- data_reg %>%
  mutate(cost_beta1 = ifelse(is.na(cost_beta1), 0, cost_beta1),
         cost_beta2 = ifelse(is.na(cost_beta2), 0, cost_beta2),
         cost_beta3 = ifelse(is.na(cost_beta3), 0, cost_beta3),
         cost_beta4 = ifelse(is.na(cost_beta4), 0, cost_beta4),
         time_beta0 = ifelse(is.na(time_beta0), 0, time_beta0),
         time_beta1 = ifelse(is.na(time_beta1), 0, time_beta1),
         time_beta2 = ifelse(is.na(time_beta2), 0, time_beta2),
         time_beta3 = ifelse(is.na(time_beta3), 0, time_beta3),
         time_beta4 = ifelse(is.na(time_beta4), 0, time_beta4))


# time and cost variables for counterfactual
data_reg <- data_reg %>%
  mutate(cost = ifelse(mode_choice == 0, cost_beta0 + cost_beta1*distance + cost_beta2 +
                          cost_beta3 + cost_beta4, cost),
         time = ifelse(mode_choice == 0, time_beta0 + time_beta1*distance + time_beta2 +
                          time_beta3 + time_beta4, hours))
                                          

rm(cost_beta0,cost_beta1,cost_beta2,cost_beta3,cost_beta4)
rm(time_beta0,time_beta1,time_beta2,time_beta3,time_beta3,time_beta4)


##########################################################################################################################
# DISCRETE CHOICE MODEL  
##########################################################################################################################

data_reg <- data_reg %>% mutate(income_1000 = renda_fa/1000,
                                cost = ifelse(mode == "walk", 0, cost),
                                cost_per_inc = cost/(income_1000),
                                time_inc = time/income_1000,
                                mode = as.factor(mode),
                                nest = ifelse(mode %in% c("car", "moto", "taxi"), "private","public"),
                                genero = ifelse(sexo == 1,1,0)) # dummy para homem

# who does not have a car or moto has no car/moto options
id.no.cars <- dataset %>% filter(qt_auto == 0) %>% select(id_ordem) %>%
  mutate(key = paste(id_ordem, "car", 0, sep = "_")) %>% select(-id_ordem) %>% as_vector()
#38k obs sem carro

id.no.motos <- dataset %>% filter(qt_moto == 0) %>% select(id_ordem) %>%
  mutate(id_ordem = paste(id_ordem, "moto", 0, sep ="_")) %>% as_vector()
#100k obs sem moto

df <- data_reg %>% mutate(key = paste(id_ordem, mode, mode_choice, sep = "_")) %>%
  filter(!(key %in% id.no.cars)) %>% filter(!(key %in% id.no.motos))
# Tirando essas op??es para esses individuos

# if nobody answered metro in their zone, not a option

zonas.metro <- dataset %>% filter(mode == "metro") %>% select(zona_o) %>% distinct() 
zonas.no.metro <- data.frame(zona_o = c(seq(1,517))) %>% anti_join(zonas.metro, by = "zona_o") %>%
  mutate(zona_o = paste(zona_o, "metro", 0, sep ="_")) %>% as_vector()
# 21 zonas sem ninguem escolhendo metro

df <- df %>% mutate(key3 = paste(zona_o, mode, mode_choice, sep = "_")) %>%
  filter(!(key3 %in% zonas.no.metro))

# limiting outliers
df <- df %>% mutate(cost = ifelse(cost > 300, 300, cost),
                    time = ifelse(time > 8, 8, time),
                    time = ifelse(time < 0.02, 0.02, time),
                    time_inc = time/income_1000,
                    cost_per_inc = cost/income_1000)
  
  
# Separating df for regression
df <- df %>% select(id_ordem, mode, mode_choice, time, cost, cost_per_inc, #fe_via,
                    time_inc, income_1000, genero, idade, distance,
                    ce, motivo_o, motivo_d, peak)


# CONTRAFACTUAL TABLE
df %>% group_by(mode, mode_choice) %>% summarise(mean_cost = mean(cost,na.rm = T),
                                                 sd_cost = sd(cost, na.rm = T),
                                                 mean_time = mean(time, na.rm = T),
                                                 sd_time = sd(time, na.rm = T)) %>% 
  arrange(mode_choice) %>% 
  pivot_wider(id_cols = c(mode),
              names_from = mode_choice, values_from = c(mean_cost, sd_cost)) %>% 
  relocate(mode, mean_cost_1, sd_cost_1, mean_cost_0, sd_cost_0) %>%
  kable("latex", digits = 2, booktabs = T, linesep = '')

df %>% group_by(mode, mode_choice) %>% summarise(mean_cost = mean(cost,na.rm = T),
                                                 sd_cost = sd(cost, na.rm = T),
                                                 mean_time = mean(time, na.rm = T),
                                                 sd_time = sd(time, na.rm = T)) %>% 
  arrange(mode_choice) %>% 
  pivot_wider(id_cols = c(mode),
              names_from = mode_choice, values_from = c(mean_time, sd_time)) %>% 
  relocate(mode, mean_time_1, sd_time_1, mean_time_0, sd_time_0) %>%
  kable("latex", digits = 2, booktabs = T, linesep = '')



nested_controles <- mlogit(mode_choice ~  time  + cost + cost_per_inc + time_inc|
                             income_1000 + genero + distance + idade + ce | 0 , data = df,
                           idx = c("id_ordem", "mode"), reflevel = "walk",
                           nests = list(public = c("metro", "bus", "walk"), private = c("car", "moto", "taxi")))

multi <- mlogit(mode_choice ~  time  + cost + cost_per_inc + time_inc|
                  income_1000 + genero + distance + idade + ce | 0 , data = df,
                idx = list("id_ordem", c("mode")), reflevel = "walk")

summary(multi)
summary(nested_controles)



df_work <- df %>% filter(motivo_o %in% c(1,2,3) | motivo_d %in% c(1,2,3))
df_educ <- df %>% filter(motivo_o == 4| motivo_d == 4)


multi_work <- mlogit(mode_choice ~  time  + cost + cost_per_inc + time_inc |
                  income_1000 + genero + distance + idade + ce| 0 , data = df_work,
                idx = list("id_ordem", c("mode")), reflevel = "walk")

nested_work <- mlogit(mode_choice ~  time  + cost + cost_per_inc + time_inc |
                        income_1000 + genero + distance + idade + ce| 0 , data = df_work,
                             idx = c("id_ordem", "mode"), reflevel = "walk",
                             nests = list(public = c("metro", "bus", "walk"), private = c("car", "moto", "taxi")))

coeftest(nested_work)

stargazer(multi, multi_work, nested_controles, nested_work,
          column.labels =  c("Multinomial", "Multinomial (work only)",
                             "Nested", "Nested (work only)"),
          omit = "Intercept",
          title = "Estimativas do modelo de escolha discreta",
          order = c("time", "time_inc", "cost", "cost_per_inc",
                    "income_1000:bus", "genero:bus", "distance:bus", "idade:bus", "ce:bus",
                    "income_1000:metro", "genero:metro", "distance:metro", "idade:metro", "ce:metro",
                    "income_1000:car", "genero:car", "distance:car", "idade:car", "ce:car",
                    "income_1000:moto", "genero:moto", "distance:moto", "idade:moto", "ce:moto",
                    "income_1000:taxi", "genero:taxi", "distance:taxi", "idade:taxi", "ce:taxi"),
          covariate.labels = c("Time", "Time/Income", "Cost", "Cost/Income",
                               "Income", "Male", "Distance", "Age", "Expanded Center",
                               "Income", "Male", "Distance", "Age", "Expanded Center",
                               "Income", "Male", "Distance", "Age", "Expanded Center",
                               "Income", "Male", "Distance", "Age", "Expanded Center",
                               "Income", "Male", "Distance", "Age", "Expanded Center",
                              "IV Public", "IV Private"),
          add.lines = list(c("Alternative specific")),
          table.layout = "=#c-a-ts-n")


##########################################################################################################################
# POLICY EVALUATION
##########################################################################################################################

# PROBABILITIES OF CHOICE
# GOODNESS OF FIT
# frequency on data
summary(nested_controles)

# model prediction
apply(fitted(nested_controles, outcome = FALSE), 2, mean)

# Vou calcular na m?o pra ver se bate

# FIRST GROUP (ALL) 

X_reg <- model.matrix(nested_controles)
beta <- nested_controles$coefficients[1:34]

iv_public <- nested_controles$coefficients[35]
iv_private <- nested_controles$coefficients[36]

options <- c("bus", "car", "metro", "moto", "taxi", "walk")
nests <- c("public", "private", "public", "private", "private", "public")
ids_reg <- df %>% select(id_ordem) %>% distinct() %>% as_vector

# aqui eu monto a primeira parte do numerador (exp(vnj/iv))
Vnj <- tibble(vnj = X_reg %*% beta) %>% 
  mutate(id_ordem = rep(ids_reg, each = 6),
         choice = rep(options, 124667),
         nest = rep(nests, 124667),
         iv = ifelse(nest == "public", iv_public, iv_private),
         vnj_iv = ifelse(is.na(vnj), 0, exp(vnj/iv)))

# aqui eu monto a segunda parte do numerador (somatorio dos exp(vnj/iv))
nests_sum <- Vnj %>% group_by(id_ordem, nest) %>% summarise(sum_vnj = sum(vnj_iv)) %>% ungroup()

Vnj <- left_join(Vnj, nests_sum, by=c("id_ordem","nest"))%>% 
  mutate(numerador = vnj_iv*(sum_vnj^(iv-1)))

# aqui vou montar o denominador (somar os exp(vnj/iv)^iv para os dois nests)
all_nests_sum <- Vnj %>% mutate(vnj_iv_iv = sum_vnj^iv)%>% group_by(id_ordem, nest) %>%
  summarise(denominador = mean(vnj_iv_iv)) %>% ungroup() %>%  group_by(id_ordem) %>%
  summarise(denominador = sum(denominador)) %>% ungroup()
 
Vnj <- left_join(Vnj, all_nests_sum, by = "id_ordem") %>%
  mutate(prob = numerador/denominador)

probabilities <- Vnj %>% select(id_ordem, choice, prob) %>% 
  pivot_wider(id_cols = "id_ordem", names_from = "choice", values_from = "prob") %>%
  relocate(c(id_ordem, walk, bus, car, metro, moto, taxi)) 

mean_prob <- function(x){mean(x, na.rm = T)}

apply(probabilities, 2, mean_prob)
apply(fitted(nested_controles, outcome = F),2, mean)

# MUDANÇA DE PROBABILIDADE COM O PED?GIO URBANO 

X_reg_pedagio <- as_tibble(X_reg) %>%
  mutate(cost = ifelse(`(Intercept):car` == 1 & `ce:car` == 1 , cost + `distance:car`*0.47, cost),
         cost_per_inc = ifelse(`(Intercept):car` == 1 & `ce:car` == 1, cost/`income_1000:car`, cost_per_inc)) %>% as.matrix()


# aqui eu monto a primeira parte do numerador (exp(vnj/iv))
Vnj_2 <- tibble(vnj = X_reg_pedagio %*% beta) %>% 
  mutate(id_ordem = rep(ids_reg, each = 6),
         choice = rep(options, 124667),
         nest = rep(nests, 124667),
         iv = ifelse(nest == "public", iv_public, iv_private),
         vnj_iv = ifelse(is.na(vnj), 0, exp(vnj/iv)))


# aqui eu monto a segunda parte do numerador (somatorio dos exp(vnj/iv))
nests_sum_2 <- Vnj_2 %>% group_by(id_ordem, nest) %>% summarise(sum_vnj = sum(vnj_iv)) %>% ungroup()

Vnj_2 <- left_join(Vnj_2, nests_sum_2, by=c("id_ordem","nest"))%>% 
  mutate(numerador = vnj_iv*(sum_vnj^(iv-1)))

# aqui vou montar o denominador (somar os exp(vnj/iv)^iv para os dois nests)
all_nests_sum_2 <- Vnj_2 %>% mutate(vnj_iv_iv = sum_vnj^iv)%>% group_by(id_ordem, nest) %>%
  summarise(denominador = mean(vnj_iv_iv)) %>% ungroup() %>%  group_by(id_ordem) %>%
  summarise(denominador = sum(denominador)) %>% ungroup()


Vnj_2 <- left_join(Vnj_2, all_nests_sum_2, by = "id_ordem") %>%
  mutate(prob = numerador/denominador)

#PROBABILITIES
probabilities_pedagio <- Vnj_2 %>% select(id_ordem, choice, prob) %>% 
  pivot_wider(id_cols = "id_ordem", names_from = "choice", values_from = "prob") %>%
  relocate(c(id_ordem, walk, bus, car, metro, moto, taxi)) 

apply(probabilities_pedagio, 2, mean_prob)
apply(probabilities, 2, mean_prob)


# SECOND GROUP (ONLY WORK) 
##################################

X_reg_w <- model.matrix(nested_work)
beta_w <- nested_work$coefficients[1:34]

iv_public_w <- nested_work$coefficients[35]
iv_private_w <- nested_work$coefficients[36]

options <- c("bus", "car", "metro", "moto", "taxi", "walk")
nests <- c("public", "private", "public", "private", "private", "public")
ids_reg_w <- df_work %>% select(id_ordem) %>% distinct() %>% as_vector

# aqui eu monto a primeira parte do numerador (exp(vnj/iv))
Vnj_w <- tibble(vnj = X_reg_w %*% beta_w) %>% 
  mutate(id_ordem = rep(ids_reg_w, each = 6),
         choice = rep(options, length(ids_reg_w)),
         nest = rep(nests, length(ids_reg_w)),
         iv = ifelse(nest == "public", iv_public_w, iv_private_w),
         vnj_iv = ifelse(is.na(vnj), 0, exp(vnj/iv)))

# aqui eu monto a segunda parte do numerador (somatorio dos exp(vnj/iv))
nests_sum_w <- Vnj_w %>% group_by(id_ordem, nest) %>% summarise(sum_vnj = sum(vnj_iv)) %>% ungroup()

Vnj_w <- left_join(Vnj_w, nests_sum_w, by=c("id_ordem","nest"))%>% 
  mutate(numerador = vnj_iv*(sum_vnj^(iv-1)))

# aqui vou montar o denominador (somar os exp(vnj/iv)^iv para os dois nests)
all_nests_sum_w <- Vnj_w %>% mutate(vnj_iv_iv = sum_vnj^iv)%>% group_by(id_ordem, nest) %>%
  summarise(denominador = mean(vnj_iv_iv)) %>% ungroup() %>%  group_by(id_ordem) %>%
  summarise(denominador = sum(denominador)) %>% ungroup()

Vnj_w <- left_join(Vnj_w, all_nests_sum_w, by = "id_ordem") %>%
  mutate(prob = numerador/denominador)

probabilities_w <- Vnj_w %>% select(id_ordem, choice, prob) %>% 
  pivot_wider(id_cols = "id_ordem", names_from = "choice", values_from = "prob") %>%
  relocate(c(id_ordem, walk, bus, car, metro, moto, taxi)) 

mean_prob <- function(x){mean(x, na.rm = T)}

apply(probabilities_w, 2, mean_prob)
apply(fitted(nested_work, outcome = F),2, mean)

# MUDAN?A DE PROBABILIDADE COM O PED?GIO URBANO 

X_reg_w_pedagio <- as_tibble(X_reg_w) %>%
  mutate(cost = ifelse(`(Intercept):car` == 1 & `ce:car` == 1 , cost + `distance:car`*0.47, cost),
         cost_per_inc = ifelse(`(Intercept):car` == 1 & `ce:car` == 1, cost/`income_1000:car`, cost_per_inc)) %>% as.matrix()


# aqui eu monto a primeira parte do numerador (exp(vnj/iv))
Vnj_w_2 <- tibble(vnj = X_reg_w_pedagio %*% beta_w) %>% 
  mutate(id_ordem = rep(ids_reg_w, each = 6),
         choice = rep(options, length(ids_reg_w)),
         nest = rep(nests, length(ids_reg_w)),
         iv = ifelse(nest == "public", iv_public, iv_private),
         vnj_iv = ifelse(is.na(vnj), 0, exp(vnj/iv)))


# aqui eu monto a segunda parte do numerador (somatorio dos exp(vnj/iv))
nests_sum_w_2 <- Vnj_w_2 %>% group_by(id_ordem, nest) %>% summarise(sum_vnj = sum(vnj_iv)) %>% ungroup()

Vnj_w_2 <- left_join(Vnj_w_2, nests_sum_w_2, by=c("id_ordem","nest"))%>% 
  mutate(numerador = vnj_iv*(sum_vnj^(iv-1)))

# aqui vou montar o denominador (somar os exp(vnj/iv)^iv para os dois nests)
all_nests_sum_w_2 <- Vnj_w_2 %>% mutate(vnj_iv_iv = sum_vnj^iv)%>% group_by(id_ordem, nest) %>%
  summarise(denominador = mean(vnj_iv_iv)) %>% ungroup() %>%  group_by(id_ordem) %>%
  summarise(denominador = sum(denominador)) %>% ungroup()


Vnj_w_2 <- left_join(Vnj_w_2, all_nests_sum_w_2, by = "id_ordem") %>%
  mutate(prob = numerador/denominador)

#PROBABILITIES

probabilities_w_pedagio <- Vnj_w_2 %>% select(id_ordem, choice, prob) %>% 
  pivot_wider(id_cols = "id_ordem", names_from = "choice", values_from = "prob") %>%
  relocate(c(id_ordem, walk, bus, car, metro, moto, taxi)) 

apply(probabilities_w_pedagio, 2, mean_prob)
apply(probabilities_w, 2, mean_prob)



#################################################################
########################## CONSUMER SURPLUS

# ALL GROUPS
logsum_before <- logsum(nested_controles, X = X_reg) 
logsum_after <- logsum(nested_controles, X = X_reg_pedagio) 

#log_sum_teste <- exp(X_reg_pedagio %*% beta)

ids_logsum_before <- dimnames(logsum_before) %>% unlist() %>% as.numeric() %>%
  as_tibble() %>%
  rename(id_ordem = value)

ids_logsum_after <- dimnames(logsum_after) %>% unlist() %>% as.numeric() %>%
  as_tibble() %>%
  rename(id_ordem = value)

logsum_bf <- logsum_before %>% as_tibble() %>% bind_cols(ids_logsum_before) %>% 
  rename(logsum_bf = value) 
logsum_af <- logsum_after %>% as_tibble() %>% bind_cols(ids_logsum_after) %>% 
  rename(logsum_af = value) 

welfare_all <- dataset %>%  
select(id_ordem, mode, ce, renda_fa, distance, fe_via) %>% 
  mutate(income_1000 = renda_fa/1000) %>% 
  left_join(logsum_bf, by = "id_ordem") %>% 
  left_join(logsum_af, by = "id_ordem")

# tarifa média 
welfare_all %>% filter(mode == "car" & ce == 1) %>% 
  summarise(mean_toll = mean(distance*0.47))

# esse final de menos vem da ideia de que a utilidade marginal da renda é a igual a
# utilidade marginal do custo, só que com sinal trocado, pag 66 do livro do train

welfare_all <- welfare_all %>% 
  mutate(alpha = -(nested_controles$coefficients[7] +
           nested_controles$coefficients[8]/income_1000),
         surplus_bf = (1/alpha)*logsum_bf,
         surplus_af = (1/alpha)*logsum_af,
         quantile = ntile(income_1000,10),
         diff_sample = surplus_af - surplus_bf,
         diff_population = (surplus_af - surplus_bf)*fe_via) 
  

# WELFARE LOSS 
welfare_lost <- welfare_all %>% filter(ce == 1 & mode == "car") %>% 
  summarise(sum = sum(diff_population))


# WELFARE DECILES (only the ones that lose, as in Moita and Lucinda)

welfare_all %>% filter(ce == 1 & mode == "car") %>% 
  group_by(quantile) %>% 
  summarise(diff_mean = mean(diff_sample),
            diff_sum = sum(diff_population)) %>%
  select(diff_mean, diff_sum) %>%
  xtable(digits = c(2,2,0), display = c("f", "f","f"))

############################################
# CHOICES CHANGE ALL
choices_pre <- Vnj %>% select(id_ordem, choice, prob) %>% arrange(id_ordem, -prob) %>%
  group_by(id_ordem) %>% filter(row_number() == 1) %>% select(-prob)

choices_pos <- Vnj_2 %>% select(id_ordem, choice, prob) %>% arrange(id_ordem, -prob) %>%
  group_by(id_ordem) %>% filter(row_number() == 1) %>% select(-prob)

choices_change <- left_join(choices_pre, choices_pos, by = "id_ordem")
colnames(choices_change) <- c("id_ordem", "choices_pre", "choices_pos")


aggregate_change <- choices_change %>%
  mutate(change = ifelse(choices_pre == choices_pos, choices_pre,
                         paste0(choices_pre,"_", choices_pos))) %>%
  group_by(change) %>% tally()



choices_pre_work <- Vnj_w %>% select(id_ordem, choice, prob) %>% arrange(id_ordem, -prob) %>%
  group_by(id_ordem) %>% filter(row_number() == 1) %>% select(-prob)

choices_pos_work <- Vnj_w_2 %>% select(id_ordem, choice, prob) %>% arrange(id_ordem, -prob) %>%
  group_by(id_ordem) %>% filter(row_number() == 1) %>% select(-prob)

choices_change_work <- left_join(choices_pre_work, choices_pos_work, by = "id_ordem")
colnames(choices_change_work) <- c("id_ordem", "choices_pre", "choices_pos")


aggregate_change_work <- choices_change_work %>%
  mutate(change = ifelse(choices_pre == choices_pos, choices_pre,
                         paste0(choices_pre,"_", choices_pos))) %>%
  group_by(change) %>% tally()


# VOT 

vot <- df %>% group_by(id_ordem) %>% summarise(income_1000 = income_1000) %>% 
  filter(row_number() == 1) %>% ungroup() %>%  
  mutate(Nested = (nested_controles$coefficients[6] + nested_controles$coefficients[9]/income_1000)/
           (nested_controles$coefficients[7] + nested_controles$coefficients[8]/income_1000),
         Multinomial = (multi$coefficients[6] + multi$coefficients[9]/income_1000)/
           (multi$coefficients[7] + multi$coefficients[8]/income_1000)) %>%
  pivot_longer(cols = c("Multinomial", "Nested")) %>% 
  rename(Modelo = name) %>% ungroup()


vot %>% group_by(Modelo) %>%
  summarise(mean_vot = mean(value, na.rm = T), median_vot = median(value, na.rm = T)) %>% 
  kable("latex",digits = 2, booktabs = T, linesep = '')

vot %>% filter(Modelo == "Nested") %>% 
  ggplot() + geom_density(aes(x = value), size = 1) + 
  labs(x = "Value of Time", y = "Density") + theme(legend.position="bottom") + 
  scale_color_brewer(palette = "Set1") + xlim(4,6.5) + theme_classic() +
  theme(legend.key.size = unit(0.3, 'cm'),
        legend.title = element_blank(),
        rect = element_blank(),
        legend.text = element_text(size = 10))

dataset %>% summarise(mean_individual_income = mean(vl_ren_i, na.rm = T),
                      mean_family_income = mean(renda_fa, na.rm = T))
  
  
  
  
# density
vot %>% filter(Modelo == "Nested") %>% arrange(value) %>%
  mutate(new_ids = seq_along(id_ordem))  %>% 
  ggplot + geom_line(aes(x=new_ids, y = income_1000), size = 1, colour = "darkgreen") +
  theme_classic() +  scale_color_brewer(palette = "Set1") +
  labs(x = "", y = "Income (in thousands)") 


###################################################################################
# POLLUTION EXTERNALITIES
ids_car_migration <- choices_change %>%
  mutate(migration_car = ifelse(choices_pre == "car" & choices_pos != "car",1,0)) %>% 
  filter(migration_car == 1)%>% 
  select(id_ordem) %>% as_vector()

dataset %>% filter(id_ordem %in% ids_car_migration) %>% mutate(kms = distance*fe_via) %>% 
  summarise(sum_km = sum(kms))
# 1 liter of gasoline produces 2.3kg of CO2
# (https://www.nrcan.gc.ca/sites/www.nrcan.gc.ca/files/oee/pdf/transportation/fuel-efficient-technologies/autosmart_factsheet_6_e.pdf)

# Segundo IPCC 2017 (link Paula), 1 litro de gasolina produz 2.27kg de CO2 

# Externalidade do carbono: 24 dolares por tonelada 
# (https://www.downtoearth.org.in/dte-infographics/social_cost_corbon/index.html#:~:text=For%20US%2C%20the%20cost%20is,of%20carbon%20is%20too%20low.)

# Livro "Getting energy prices right" do Parry estima 35 dolares por tonelada de carbono
#co2_emission <- 2.27
#liters_per_km <- 0.125
#custo_tonelada_carbono <- 35


# EMISSIONS FACTORES PER KM (in kg) (CETESB)
nox_ef <- 0.056/1000 
pm2.5_ef <- 0.001/1000 
co2_ef <- 190.67/1000

# POLLUTION COSTS PER KG (in dollars as in Parry) 
nox_cost <- 1.021
pm2.5_cost <- 130.726
co2_cost <- 0.035

# ACCIDENT COSTS (as in Parry, in dollars per km)
accident_cost <- 0.025

# DOLAR
dolar <- 3.3
euro <- 3.8587

# POLLUTION AVERTED

pollution_averted <-dataset %>% filter(id_ordem %in% ids_car_migration) %>% 
mutate(co2 = distance*fe_via*co2_ef*round(co2_cost*dolar,3),
         nox = distance*fe_via*nox_ef*round(nox_cost*dolar,3),
         pm2.5 = distance*fe_via*pm2.5_ef*round(pm2.5_cost*dolar,3),
         accident = distance*fe_via*round(accident_cost*dolar,3)) %>%
  summarise(co2_pollution = sum(co2),
            nox_pollution = sum(nox),
            pm2.5_pollution = sum(pm2.5),
            accident_ext = sum(accident))
  dataset %>% filter(id_ordem %in% ids_car_migration) %>% 
  mutate(daily_km = distance*fe_via) %>% summarise(sum_km = sum(daily_km), sum_trips = sum(fe_via))

  # EFEITO LIQUIDO
(sum(pollution_averted) + welfare_lost)*365 


