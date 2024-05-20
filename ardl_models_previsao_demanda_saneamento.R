# ARDL CO

rm(list = ls())
library(tidyverse)
library(TSA)
library(seasonal)
library(fpp3)
library(forecast)
library(lmtest)

setwd("C:/Users/KG961DH/OneDrive - EY/Documents")
data <- readxl::read_xlsx("data previsao MS.xlsx", sheet = 1)

#################################################################################################################
# CRIAÇÃO DE VARIAVEIS 
#################################################################################################################

economias_agua.ts <- ts(data[,2], start = c(2019,1),end=c(2024,3), freq = 12)
populacao.ts <- ts(log(data[,3]), start = c(2019,1), end = c(2051,12), freq = 12)

#################################################################################################################
# CRIAÇÃO DE LAGS
#################################################################################################################

# Lags
populacao_lag.ts <- stats::lag(populacao.ts, k = -1) %>% window(end = c(2051,12))
populacao_lag2.ts <- stats::lag(populacao.ts, k = -2) %>% window(end = c(2051,12))
populacao_lag3.ts <- stats::lag(populacao.ts, k = -3) %>% window(end = c(2051,12))


##########################################################################################
# DESCRIPTIVE ANALYSIS
##########################################################################################

data <- data %>% as_tsibble(index = "month", regular = F) %>% 
  rename(populacao = População,         
         economias_ativas_agua = `Economias de água - histórico mensal`)%>% 
  mutate(log_pop = log(populacao),
         log_economias_agua = log(economias_ativas_agua))


autoplot(economias_agua.ts)
autoplot(populacao.ts)

##########################################################################################
# MODELO AUXILIAR: Indice de Atividade Economica Regional - IBCR Centro Oeste
##########################################################################################

ibcr_co<- read.delim("ibcr_sazonal_co.csv", sep = ";")

ibcr_co <- ibcr_co %>% mutate(ibcr = as.numeric(gsub(",", ".", ibcr))) %>% head(-1)
ibcr.ts <- ts(ibcr_co[,2], start = c(2003,1),end=c(2024,2), freq = 12)

autoplot(ibcr.ts)

seas_ibcr <- seas(ibcr.ts)

# DUMMIES ESTRUTURAIS
LS2020APR <- ts(0, start = c(2003,1), end = c(2051,12), freq = 12)
window(LS2020APR, start = c(2020, 4), end = c(2051,12)) <- 1

AO2022JAN <- ts(0, start = c(2003,1), end = c(2051,12), freq = 12)
window(AO2022JAN, start = c(2022, 1), end = c(2022,1)) <- 1

LS2022MAR <- ts(0, start = c(2003,1), end = c(2051,12), freq = 12)
window(LS2022MAR, start = c(2022, 3), end = c(2051,12)) <- 1

aut

training_date <- c(2024,2)
test_date <-  c(2024,3)

ardl_ibcr <- auto.arima(window(ibcr.ts, end = training_date),
                               xreg = window(ts.union(LS2020APR,
                                                      AO2022JAN,
                                                      LS2022MAR),end = training_date),
                               lambda = 0, trace = T)

coeftest(ardl_ibcr)

f_ibcr <- forecast::forecast(ardl_ibcr, level = c(50,75,95),
                             xreg = window(ts.union(LS2020APR,
                                                    AO2022JAN,
                                                    LS2022MAR),start = test_date)) %>% 
  as_tibble() %>% 
  rename(fteste_mean = "Point Forecast", fteste_50_l = "Lo 50", fteste_50_h = "Hi 50",
         fteste_75_l = "Lo 75", fteste_75_h = "Hi 75", fteste_95_l = "Lo 95", fteste_95_h = "Hi 95") %>% 
  select(fteste_mean) %>% 
  mutate(year = seq(as.Date("2024-03-01"), as.Date("2051-12-01"), by = "month"))

f_ibcr.ts <- ts(f_ibcr[,1], start = c(2024,3), end = c(2051,12), freq = 12)

autoplot(ts.union(ibcr.ts, f_ibcr.ts))

ibcr_forecast.ts <- ts(c(ibcr.ts, f_ibcr.ts), start = c(2003,1), end = c(2051,12), freq = 12) %>% 
  window(start = c(2019,1))



##########################################################################################
# ARDL ECONOMIAS ATIVAS AGUA
##########################################################################################

ardl_auto_agua <- auto.arima(window(economias_agua.ts, end = training_date),
                               xreg = window(ts.union(ibcr_forecast.ts,
                                                      populacao.ts),end = training_date),
                               lambda = 0, trace = T)

coeftest(ardl_auto_agua)


f_agua <- forecast::forecast(ardl_auto_agua, level = c(50,75,95),
                             xreg = window(ts.union(ibcr_forecast.ts,
                                                    populacao.ts),start = test_date)) %>%
  as_tibble() %>% 
  rename(fteste_mean = "Point Forecast", fteste_50_l = "Lo 50", fteste_50_h = "Hi 50",
         fteste_75_l = "Lo 75", fteste_75_h = "Hi 75", fteste_95_l = "Lo 95", fteste_95_h = "Hi 95")


##########################################################################################
# PLOT
##########################################################################################

actual_agua <- window(economias_agua.ts, start = c(2019,1), end = c(2024,2)) %>% as_tibble() %>%
  transmute(fteste_mean = x,
            fteste_50_l = x,
            fteste_50_h = x,
            fteste_75_l = x,
            fteste_75_h = x,
            fteste_95_l = x,
            fteste_95_h = x)


all_data_agua <- bind_rows(actual_agua, f_agua) %>%
  mutate(year = seq(as.Date("2019-01-01"), as.Date("2051-12-01"), by = "month"))

# Criar gráfico
all_data_agua %>% filter(year < "2030-12-01") %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = fteste_mean), color = "blue", size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = fteste_50_l, ymax = fteste_50_h, fill = "50% CI"), alpha = 0.5) +
  geom_ribbon(aes(ymin = fteste_75_l, ymax = fteste_75_h, fill = "75% CI"), alpha = 0.4) +
  geom_ribbon(aes(ymin = fteste_95_l, ymax = fteste_95_h, fill = "95% CI"), alpha = 0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2024-2-01")), linetype = "dashed", color = "royalblue", alpha = 0.5, size = 1) +
  labs(title = "Previsão de Demanda para Economias Ativas de Água",
       x = "Ano",
       y = "Estimativa",
       subtitle = "Para os anos de 2023 a 2028",
       fill = "Intervalo de Confiança") +
  theme_minimal() +
  scale_fill_manual(values = c("95% CI" = "lightblue", "75% CI" = "royalblue", "50% CI" = "darkblue"))

