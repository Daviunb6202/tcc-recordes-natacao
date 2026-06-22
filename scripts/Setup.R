library(dplyr)
library(lubridate)
library(ggplot2) 
library(tidyverse)
library(nortest)
library(broom)
library(goftest)
library(purrr)
library(tseries) 
require(extRemes)
library(MASS) 
library(forecast) # Para projetar as tendências de Mu e Sigma

data_m <- read.csv("dado_tratado_homens.csv", header = TRUE, sep = ",")
data_w <- read.csv("dado_tratado_mulheres.csv", header = TRUE, sep = ",")
data_rm <- read.csv("recordes_tratados_homens.csv", header = TRUE, sep = ",")
data_rw <- read.csv("recordes_tratados_mulheres.csv", header = TRUE, sep = ",")

#É necessário escolher usar data_m ou data_w antes de rodar os outros arquivos
df_filtrado <- data_m %>%
  group_by(Year) %>%
  filter(n() >= 8) %>%
  slice_min(order_by = Results, n = 8) %>%
  ungroup() %>%
  arrange(Year) 

df_recordes <- data_rm %>%
  filter(Year >= 1972) %>%
  group_by(Year) %>%
  slice_min(order_by = Results, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  arrange(Year)

