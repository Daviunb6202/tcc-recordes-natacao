source("setup.R")         
source("AdequacaoExpo.R") 
ultimo_valor_real <- df_filtrado %>% arrange(abs(Results)) %>% pull(Results) %>% head(1)

# --- MODELAGEM DA TENDÊNCIA ---
modelo_treino <- ajustar_exponencial(df_dados = df_filtrado, ano_limite = 2019)
coeffs <- modelo_treino$coeffs
sigma_fit <- modelo_treino$sigma_fit
ano_base_t <- modelo_treino$min_year_treino

df_theory_backtest <- df_filtrado %>%
  filter(Year <= 2019) %>%
  group_by(Year) %>%
  summarise(
    n_anual = n(),
    mu_local = mean(Results, na.rm = TRUE),
    sigma_local = sd(Results, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Year) %>%
  mutate(
    n_cum = cumsum(n_anual),
    alpha = case_when(
      n_cum == 2   ~ 0.330, 
      n_cum <= 25  ~ 0.377,
      n_cum <= 50  ~ 0.384,
      n_cum <= 100 ~ 0.391,
      n_cum <= 200 ~ 0.396,
      TRUE         ~ 0.401  # Para n_cum >= 400 ou valores maiores extrapolados
    ),
    p_n = (1 - alpha) / (n_cum - 2 * alpha + 1),
    Ez = qnorm(p_n),
    Expected_Min_Raw = mu_local + sigma_local * Ez,
    prob_low = 0.025,
    z_low = qnorm(1 - (1 - prob_low)^(1/n_cum)),
    
    prob_high = 0.975,
    z_high = qnorm(1 - (1 - prob_high)^(1/n_cum)),
    
    Raw_Lower_Bound = mu_local + sigma_local * z_low,
    Raw_Upper_Bound = mu_local + sigma_local * z_high,
    Step_Theoretical = cummin(Expected_Min_Raw),
    Step_Lower = cummin(Raw_Lower_Bound),
    Step_Upper = cummin(Raw_Upper_Bound) 
  )

ultimo_rec_2019 <- tail(df_theory_backtest$Step_Theoretical, 1)
anos_todos <- seq(ano_base_t, 2026, by = 1)
t_full <- anos_todos - ano_base_t
anos_teste <- 2020:2025
t_testprojection <- anos_teste - ano_base_t
n_ultimo_2019 <- tail(df_theory_backtest$n_cum, 1)
n_est_anual <- 8 
n_cum_futuro <- n_ultimo_2019 + (seq_along(t_testprojection-1) * n_est_anual)

alpha_futuro <- case_when(
  n_cum_futuro == 2   ~ 0.330, 
  n_cum_futuro <= 25  ~ 0.377,
  n_cum_futuro <= 50  ~ 0.384,
  n_cum_futuro <= 100 ~ 0.391,
  n_cum_futuro <= 200 ~ 0.396,
  TRUE                ~ 0.401  
)

ultimo_aprox_blom_2019 <- tail(df_theory_backtest$Step_Theoretical, 1)
p_n_sim <- (1 - alpha_futuro) / (n_cum_futuro - 2 * alpha_futuro + 1)
Ez_sim <- qnorm(p_n_sim)
sigma_individual <- mean(df_theory_backtest$sigma_local, na.rm = TRUE)

# --- SIMULAÇÃO DE MONTE CARLO ---
n_simulacoes <- 5000
set.seed(123)
mc_backtest <- list()
for(i in 1:n_simulacoes) {
  sim_noises <- rnorm(length(anos_teste), 0, sd = sigma_fit)
  
  sim_raw_values <- (coeffs["a"] * exp(coeffs["b"] * t_testprojection) + coeffs["c"]) + 
    sim_noises + 
      (sigma_individual * Ez_sim)
  
  sim_path <- cummin(sim_raw_values)

  mc_backtest[[i]] <- tibble(
    Year = anos_teste,
    Step_Blom_Predict = sim_path,
    Simulation_ID = i
  ) %>%
    mutate(Step_Sim = cummin(pmin(Step_Blom_Predict, ultimo_aprox_blom_2019)))
}

# --- SUMARIZAÇÃO ---
df_mc_valida <- bind_rows(mc_backtest) %>%
  group_by(Year) %>%
  summarise(
    Median_Blom = median(Step_Sim),
    Lower_Bound = quantile(Step_Sim, 0.025),
    Upper_Bound = quantile(Step_Sim, 0.975)
  )

ultimo_ponto_historico <- df_theory_backtest %>% 
  filter(Year == max(Year, na.rm = TRUE)) %>% 
  dplyr::select(
    Year, 
    Median_Blom = Step_Theoretical,
    Lower_Bound = Step_Theoretical, 
    Upper_Bound = Step_Theoretical
  ) %>% 
  tail(1)

df_mc_valida <- bind_rows(ultimo_ponto_historico, df_mc_valida)
primeiro_recorde <- head(df_recordes, 1)
ultimo_recorde <- tail(df_recordes, 1)
ultimo_valor_teorico <- tail(df_mc_valida$Median_Blom, 1)
val_teorico_label <- round(ultimo_valor_teorico, 2)

# --- PROJEÇÃO DE TENDÊNCIA --- 
df_expo_completa <- tibble(
  Year = anos_todos,
  Trend_Value = coeffs["a"] * exp(coeffs["b"] * t_full) + coeffs["c"]
)

# --- AVALIAÇÃO DE MÉTRICAS -----
anos_teste_novo <- 2022:2025 
df_real_teste <- tibble(Year = anos_teste_novo) %>%
  rowwise() %>%
  mutate(
    minimos = min(df_filtrado$Results[df_filtrado$Year == Year], na.rm = TRUE)) %>% 
  ungroup() 
df_aval <- df_mc_valida %>%
  filter(Year %in% anos_teste_novo) %>% 
  left_join(df_real_teste, by = "Year")

# MÉTRICAS 
MAE_blom <- mean(abs(df_aval$minimos - df_aval$Median_Blom), na.rm = TRUE)
cat(sprintf("MAE: %.4f segundos\n", MAE_blom))
largura_blom <- mean(df_aval$Upper_Bound - df_aval$Lower_Bound)
cat(sprintf("Largura: %.4f segundos\n", largura_blom))

df_theory_backtest$Step_Theoretical[1] = primeiro_recorde$Results

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
  
  geom_step(data = df_recordes, 
            aes(x = Year, y = Results), 
            color = "black", linewidth = 0.7, direction = "hv") + 
  
  geom_step(data = df_theory_backtest, aes(x = Year, y = Step_Theoretical), 
            color = "blue", linewidth = 1) +
  
  geom_ribbon(data = df_mc_valida, aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound), 
              fill = "blue", alpha = 0.15) +
  
  geom_line(data = df_expo_completa, aes(x = Year, y = Trend_Value), 
            color = "#C71585", linewidth = 1) +
  
  geom_step(data = df_mc_valida, aes(x = Year, y = Median_Blom), 
            color = "blue", linewidth = 1, direction = "hv") +
  
  geom_segment(
    aes(x = 2019, y = ultimo_rec_2019, xend = 2020, yend = ultimo_rec_2019),
    color = "blue",  linewidth = 1, alpha = 0.6
  ) +
  
  # OPCAO PLOT MODALIDADE MASCULINA #
  #scale_y_continuous(
  #  breaks = sort(c(seq(45, 54, 2), ultimo_valor_real, ultimo_valor_teorico)),
  #  minor_breaks = NULL, 
  #  labels = function(x) sprintf("%.2f", x) 
  #) + 
  
  # OPCAO PLOT MODALIDADE FEMININA #
  scale_y_continuous(
    breaks = sort(c(seq(45, 60, 8), 59, ultimo_valor_real, ultimo_valor_teorico)),
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
