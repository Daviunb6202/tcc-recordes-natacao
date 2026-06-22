source("AdequacaoExpo.R")
source("serie_normal.R")
# --- MODELAGEM DA TENDÊNCIA ---
modelo_treino <- ajustar_exponencial(df_dados = df_filtrado, ano_limite = 2025)
coeffs <- modelo_treino$coeffs
sigma_fit <- modelo_treino$sigma_fit
ano_base_t <- modelo_treino$min_year_treino

sim_horizon <- 12 
ultimo_ano <- max(df_filtrado$Year)
anos_projetados <- seq(ultimo_ano+1, ultimo_ano + sim_horizon, by = 3)
t_projection <- anos_projetados - ano_base_t
ultimo_ano_cum <- tail(df_theory$n_cum, 1)
n_cum_futuro <- ultimo_ano_cum + ((anos_projetados - min(anos_projetados-1)) - 1) * 8

alpha_futuro <- case_when(
  n_cum_futuro == 2   ~ 0.330, 
  n_cum_futuro <= 25  ~ 0.377,
  n_cum_futuro <= 50  ~ 0.384,
  n_cum_futuro <= 100 ~ 0.391,
  n_cum_futuro <= 200 ~ 0.396,
  TRUE                ~ 0.401  # Para n_cum >= 400 ou valores maiores extrapolados
)

p_n_futuro <- (1 - alpha_futuro) / (n_cum_futuro - 2 * alpha_futuro + 1)
Ez_futuro <- qnorm(p_n_futuro)
ultimo_minimo <- df_theory %>% filter(Year == max(Year, na.rm = TRUE)) %>% pull(Step_Theoretical) %>% tail(1)
sigma_individual <- mean(df_theory_backtest$sigma_local, na.rm = TRUE)

# --- SIMULAÇÃO DE MONTE CARLO ---
n_simulacoes <- 5000
mc_results <- list()
set.seed(123)
for(i in 1:n_simulacoes) {
  
  sim_noise <- rnorm(length(anos_projetados), 0, sd = sigma_fit)
  
  sim_raw_values <- (coeffs["a"] * exp(coeffs["b"] * t_projection) + coeffs["c"]) + 
    sim_noise + 
    (sigma_individual * Ez_futuro)
  
  sim_path <- cummin(sim_raw_values)
  
  mc_results[[i]] <- tibble(
    Year = anos_projetados,
    Step_Blom_Predict = sim_path,
    Simulation_ID = i
  ) %>%
    mutate(Step_Sim = cummin(pmin(Step_Blom_Predict, ultimo_minimo)))
}

# --- SUMARIZAÇÃO ---
df_mc_summary <- bind_rows(mc_results) %>%
  group_by(Year) %>%
  summarise(
    Median_Blom = median(Step_Sim),
    Lower_Bound = quantile(Step_Sim, 0.025),
    Upper_Bound = quantile(Step_Sim, 0.975)
  )


ultimo_ponto_historico <- df_theory %>% 
  filter(Year == max(Year, na.rm = TRUE)) %>% 
  dplyr::select(
    Year, 
    Median_Blom = Step_Theoretical,
    Lower_Bound = Step_Theoretical, 
    Upper_Bound = Step_Theoretical
  ) %>% 
  tail(1)

df_mc_summary <- bind_rows(ultimo_ponto_historico, df_mc_summary)
ultimo_valor_teorico <- tail(df_mc_summary$Median_Blom, 1)
val_teorico_label <- round(ultimo_valor_teorico, 2)

# --- PROJEÇÃO DE TENDÊNCIA --- 
years_full <- seq(ano_base_t, 2037, by = 1)
df_expo_extrapolacao <- tibble(
  Year = years_full,
  Trend_Value = coeffs["a"] * exp(coeffs["b"] * (years_full - ano_base_t)) + coeffs["c"]
)

# --- VISUALIZAÇÃO ---
ggplot() +
  geom_point(data = df_filtrado, aes(x = Year, y = Results), alpha = 0.5, color = "black") +

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
            aes(x = Year, y = Results), color = "black", linewidth = 0.7, direction = "hv") + 
  
  geom_step(data = df_theory, aes(x = Year, y = Step_Theoretical), 
            color = "blue", linewidth = 1, direction = "hv") +
  
  geom_ribbon(data = df_mc_summary, aes(x = Year, ymin = Lower_Bound, ymax = Upper_Bound), 
              fill = "blue", alpha = 0.15) +
  
  geom_step(data = df_mc_summary, aes(x = Year, y = Median_Blom), 
            color = "blue", linewidth = 1.2, direction = "hv") +
  
  geom_line(data = df_expo_extrapolacao, aes(x = Year, y = Trend_Value), 
            color = "#C71585", linewidth = 1) +
  
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
    limits = c(1970, 2037), 
    breaks = c(seq(1970, 2020, by = 10), 2026, 2029, 2032, 2035)
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
        axis.text.x = element_text(color = "black", size = 13),
        axis.ticks.x = element_line(color = "black", linewidth = 0.6)
)
