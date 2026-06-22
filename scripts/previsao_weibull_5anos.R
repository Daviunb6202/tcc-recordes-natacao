source("setup.R")         
source("AdequacaoExpo.R") 
source("serie_weibull.R")
# --- MODELAGEM DA TENDÊNCIA ---
modelo_treino <- ajustar_exponencial(df_dados = df_filtrado, ano_limite = 2019)
coeffs <- modelo_treino$coeffs
sigma_fit <- modelo_treino$sigma_fit
ano_base_t <- modelo_treino$min_year_treino
df_treino_full <- df_filtrado %>% filter(Year <= 2019)
coeffs
# --- CÁLCULO DAS ESTATÍSTICAS ANUAIS ---
df_stats_treino <- df_treino_full %>%
  group_by(Year) %>%
  summarise(mu_local = mean(Results, na.rm = TRUE), .groups = 'drop')

# --- PROJEÇÃO DE TENDÊNCIA --- 
t_hist <- df_stats_treino$Year - min(df_stats_treino$Year)
df_tunel_treino <- df_stats_treino %>%
  mutate(
    Tunel_Centro = coeffs["a"] * exp(coeffs["b"] * t_hist) + coeffs["c"]
  )

# --- AJUSTE WEIBULL (SOBRE RESÍDUOS DE TREINO) ---
df_res_melhores <- df_treino_full %>%
    filter(Year <= 2019) %>%
    group_by(Year) %>%
    slice_min(order_by = Results, n = 1) %>% 
    ungroup() %>%
    left_join(df_calc %>% dplyr::select(Year, Tendencia_Expo), by = "Year") %>%
    mutate(
      Abs_Residuals = abs(Results - Tendencia_Expo)
    ) %>%
    filter(Abs_Residuals > 0.0001)

fit_w <- fitdistr(df_res_melhores$Abs_Residuals, "weibull")
forma_5 <- fit_w$estimate["shape"]
forma_5
escala_5 <- fit_w$estimate["scale"]
escala_5
largura_weibull_05 <- qweibull(0.05, shape = forma_5, scale = escala_5)
largura_weibull_95 <- qweibull(0.95, shape = forma_5, scale = escala_5)

# --- ENVELOPE E ESCADA HISTÓRICA --- 
df_theory_treino <- df_tunel_treino %>%
  mutate(
    Tunel_Inferior = Tunel_Centro - largura_weibull_95,
    Tunel_Superior = Tunel_Centro - largura_weibull_05,
    Step_Theoretical = cummin(Tunel_Inferior[order(Year)])
)
# --- Escada Teórica Ancorada no Recorde Inicial --- 
df_theory_treino$Step_Theoretical[1] = cummax(pmax(df_theory_treino$Step_Theoretical[1], primeiro_recorde$Results))

# --- SIMULAÇÃO DE MONTE CARLO ---
ultimo_rec_2019 <- df_theory_treino %>% filter(Year <= 2019) %>% pull(Step_Theoretical) %>% tail(1)
anos_teste <- 2020:2025
t_test <- anos_teste - min(df_filtrado$Year)
n_simulacoes <- 5000
set.seed(123)
mc_valida <- list()
for(i in 1:n_simulacoes) {
  sim_trend <- (coeffs["a"] * exp(coeffs["b"] * t_test) + coeffs["c"]) + 
    rnorm(length(anos_teste), 0, sd = sigma_fit)

  sim_res <- rweibull(length(anos_teste), shape = forma_5, scale = escala_5)
  raw_lim <- sim_trend - sim_res 
  
  mc_valida[[i]] <- tibble(
    Year = anos_teste,
    Value = raw_lim,
    Simulation_ID = i
  ) %>%
    mutate(Step_Sim = cummin(pmin(Value, ultimo_rec_2019))) 
}

# --- SUMARIZAÇÃO ---
df_mc_5years <- bind_rows(mc_valida) %>%
  group_by(Year) %>%
  summarise(
    Mean_Step = mean(Step_Sim),
    Lower_Bound =  quantile(Step_Sim, 0.025),
    Upper_Bound = quantile(Step_Sim, 0.975)
  )


# --- Escada Teórica Da Previsão ancorada no Recorde Inicial --- 
df_mc_5years$Step_Theoretical <- tail(df_theory_treino$Step_Theoretical, 1)
for(i in 2:nrow(df_mc_5years)) {
  df_mc_5years$Step_Theoretical[i] <- min(df_mc_5years$Step_Theoretical[i-1], df_mc_5years$Mean_Step[i])
}

# --- TENDÊNCIA EXPONENCIAL COMPLETA ---
years_all <- seq(min(df_theory_treino$Year), 2026, by = 1)
df_expo_full <- tibble(
  Year = years_all,
  Trend_Value = coeffs["a"] * exp(coeffs["b"] * (years_all - min(df_theory_treino$Year))) + coeffs["c"]
)

ultimo_ponto_historico <- df_theory_treino %>% 
  filter(Year == max(Year, na.rm = TRUE)) %>% 
  dplyr::select(
    Year, 
    Mean_Step = Tunel_Inferior,
    Median_Lower_Bound = Tunel_Inferior, 
    Median_Upper_Bound = Tunel_Superior
  ) %>% 
  tail(1)

df_mc_5years <- bind_rows(ultimo_ponto_historico, df_mc_5years)
ultimo_valor_teorico <- tail(df_mc_5years$Mean_Step, 1)
val_teorico_label <- round(ultimo_valor_teorico, 2)
primeiro_recorde <- head(df_recordes, 1)
ultimo_recorde <- tail(df_recordes, 1)

# --- AVALIAÇÃO DE MÉTRICAS ----- 
anos_teste_novo <- 2022:2025 
df_real_teste <- tibble(Year = anos_teste_novo) %>%
  rowwise() %>%
  mutate(
    minimos = min(df_filtrado$Results[df_filtrado$Year == Year], na.rm = TRUE)
  ) %>%
  ungroup() 
df_aval_weibull <- df_mc_5years %>%
  filter(Year %in% anos_teste_novo) %>% 
  left_join(df_real_teste, by = "Year")

df_aval_weibull
print(df_aval_weibull %>% dplyr::select(Year, minimos, Mean_Step, Lower_Bound, Upper_Bound))
MAE_weibull <- mean(abs(df_aval_weibull$minimos - df_aval_weibull$Mean_Step), na.rm = TRUE)
cat(sprintf("MAE (Weibull): %.4f segundos\n", MAE_weibull))

largura_weibull <- mean(df_aval_weibull$Upper_Bound - df_aval_weibull$Lower_Bound)
cat(sprintf("Largura Weibull: %.4f segundos\n", largura_weibull))

# --- VISUALIZAÇÃO ---
ggplot() +
  
  geom_point(data = df_filtrado, aes(x = Year, y = Results), alpha = 0.5) +
  
  geom_segment(
    data = primeiro_recorde,
    aes(x = Year, y = Results, xend = -Inf, yend = Results),
    linetype = "dotted",
    color = "black"
  ) +
  
  geom_segment(
    data = ultimo_recorde,
    aes(x = Year, y = Results, xend = Inf, yend = Results),
    linetype = "dotted",
    color = "black"
  ) +
  
  geom_segment(
    data = ultimo_recorde,
    aes(x = Year, y = Results, xend = 2025, yend = Results),
    linetype = "solid",
    color = "black"
  ) +
  
  geom_point(data = df_recordes, aes(x = Year, y = Results), color = "black", size = 1.5) +
  
  geom_ribbon(data = df_mc_5years, aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound), 
              fill = "red", alpha = 0.15) +
  
  geom_step(data = df_theory_treino, aes(x = Year, y = Step_Theoretical), 
            color = "red", linewidth = 1, alpha = 0.6, direction = "hv") +
  
  geom_line(data = df_expo_full, aes(x = Year, y = Trend_Value), 
            color = "#C71585", linewidth = 1) +
  
  geom_step(data = df_mc_5years, aes(x = Year, y = Step_Theoretical), 
            color = "red", linewidth = 1.2, direction = "hv") +
  
  geom_segment(
    aes(x = 2019, y = ultimo_rec_2019, xend = 2020, yend = ultimo_rec_2019),
    color = "red",  linewidth = 1, alpha = 0.6
  ) +
  
  geom_step(data = df_recordes, 
            aes(x = Year, y = Results), color = "black", linewidth = 0.7, direction = "hv") + 

  # OPCAO PLOT MODALIDADE MASCULINA #
  #scale_y_continuous(
  #  breaks = sort(c(seq(45, 54, 2), ultimo_valor_teorico)),
  #  minor_breaks = NULL,
  #  labels = function(x) sprintf("%.2f", x) 
  # ) + 
  
  # OPCAO PLOT MODALIDADE FEMININA #
  scale_y_continuous(
    breaks = sort(c(seq(46, 60, 8),49, 59, ultimo_valor_real, ultimo_valor_teorico)),
    minor_breaks = NULL,
    labels = function(x) sprintf("%.2f", x) 
  ) + 
  
  scale_x_continuous(
    breaks = c(seq(1970, 2020, 10), 2025),
    minor_breaks = NULL,
    labels = function(x) sprintf("%.0f", x) 
  ) +
  
  labs(
    x = "Ano", 
    y = "Tempo (s)"
  ) +
  
  theme_gray(base_family ="Arial") +
  
  theme(
    panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold", size = 20),
        axis.text.y = element_text(color = "black", size = 15),
        axis.ticks.y = element_line(color = "black", linewidth = 0.8),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.text.x = element_text(color = "black", size = 15),
        axis.ticks.x = element_line(color = "black", linewidth = 0.6)
)
