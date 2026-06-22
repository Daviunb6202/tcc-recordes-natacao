source("setup.R") 
source("AdequacaoExpo.R")
source("serie_weibull.R")
# --- MODELAGEM DA TENDÊNCIA E VALORES EXTREMOS (WEIBULL SOBRE EXPONENCIAL) ---
modelo_treino <- ajustar_exponencial(df_dados = df_filtrado, ano_limite = 2025)
coeffs <- modelo_treino$coeffs
sigma_fit <- modelo_treino$sigma_fit
ano_base_t <- modelo_treino$min_year_treino
coeffs
# --- CONFIGURAÇÃO DA PROJEÇÃO ---
sim_horizon <- 12
ultimo_ano <- max(df_filtrado$Year)
ano_final <- ultimo_ano + sim_horizon 
anos_futuros <- seq(ultimo_ano+1, ano_final, by = 3)
t_futuro <- anos_futuros - ano_base_t

ultimo_minimo <- df_sumario %>% 
  filter(Year == max(Year)) %>% 
  pull(Step_Theoretical) %>% 
  tail(1)

# --- SIMULAÇÃO DE MONTE CARLO ---
n_simulacoes <- 5000
mc_resultados <- list()
set.seed(123)
for(i in 1:n_simulacoes) {

  sim_trend <- (coeffs["a"] * exp(coeffs["b"] * t_futuro) + coeffs["c"]) + 
    rnorm(length(anos_futuros), 0, sd = sigma_fit)
  
  sim_res <- rweibull(length(anos_futuros), shape = forma_w, scale = escala_w)
  raw_limit <- sim_trend - sim_res
 
  mc_resultados[[i]] <- tibble(
    Year = anos_futuros,
    Raw_Limit = raw_limit,
    Simulation_ID = i
  ) %>%
    mutate(
      Step_Limit = cummin(pmin(ultimo_minimo, Raw_Limit)))
    
}

# --- SUMARIZAÇÃO ---
df_mc_futuro <- bind_rows(mc_resultados) %>%
  group_by(Year) %>%
  summarise(
    Mean_Step = mean(Step_Limit),
    Median_Lower_Bound = quantile(Step_Limit, 0.025),
    Median_Upper_Bound = quantile(Step_Limit, 0.975)
  )

# --- PROJEÇÃO DE TENDÊNCIA --- 
anos_todos <- seq(ano_base_t, 2037, by = 1)
df_expo_extrapolacao <- tibble(
  Year = anos_todos,
  Trend_Value = coeffs["a"] * exp(coeffs["b"] * (anos_todos - ano_base_t)) + coeffs["c"]
)

# --- Escada Teórica Ancorada no Recorde Inicial --- 
ultimo_minimo <- tail(df_sumario$Step_Theoretical, 1)
df_mc_futuro$Step_Theoretical <- ultimo_minimo
for(i in 2:nrow(df_mc_futuro)) {
  df_mc_futuro$Step_Theoretical[i] <- min(df_mc_futuro$Step_Theoretical[i-1], df_mc_futuro$Mean_Step[i])
}

ultimo_ponto_historico <- df_sumario %>% 
  filter(Year == max(Year, na.rm = TRUE)) %>% 
  dplyr::select(
    Year, 
    Mean_Step = Tunel_Centro,
    Median_Lower_Bound = Tunel_Inferior, 
    Median_Upper_Bound = Tunel_Inferior
  ) %>% 
  tail(1)

df_mc_futuro <- bind_rows(ultimo_ponto_historico, df_mc_futuro)
ultimo_valor_teorico <- tail(df_mc_futuro$Step_Theoretical, 1)
val_teorico_label <- round(ultimo_valor_teorico, 2)

# --- VISUALIZAÇÃO ---
ggplot() +
  geom_point(data = df_filtrado, aes(x = Year, y = Results), alpha = 0.5) +
  
  geom_ribbon(data = df_mc_futuro, 
              aes(x = Year, ymin = Median_Lower_Bound, ymax = Median_Upper_Bound), 
              fill = "#DC143C", alpha = 0.15) +
  
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
  
  geom_line(data = df_expo_extrapolacao, aes(x = Year, y = Trend_Value), 
            color = "#C71585", linewidth = 1) +
  
  geom_step(data = df_mc_futuro, aes(x = Year, y = Step_Theoretical), 
            color = "red", linewidth = 1.2, direction = "hv") +
  
  geom_step(data = df_recordes, aes(x = Year, y = Results), 
            color = "black", linewidth = 0.7, direction = "hv") + 
  
  geom_step(data = df_sumario, aes(x = Year, y = Step_Theoretical), 
            color = "red", linewidth = 1, alpha=0.6, direction = "hv") +
  
  geom_segment(
    aes(x = 2025, y = ultimo_minimo, xend = 2026, yend = ultimo_minimo),
    color = "red",  linewidth = 1, alpha = 0.6
  ) +

  # OPCAO PLOT MODALIDADE MASCULINA #
  #scale_y_continuous(
  #  breaks = sort(c(seq(45, 54, 2), ultimo_valor_real, ultimo_valor_teorico)),
  #  minor_breaks = NULL,
  #  labels = function(x) sprintf("%.2f", x) 
  #) + 
  
  # OPCAO PLOT MODALIDADE FEMININA #
  scale_y_continuous(
    breaks = sort(c(seq(46, 60, 8), 49, 59, ultimo_valor_real, ultimo_valor_teorico)),
    minor_breaks = NULL,
    labels = function(x) sprintf("%.2f", x) 
  ) + 
  
  scale_x_continuous(
    limits = c(1970, 2037), 
    breaks = c(seq(1970, 2020, by = 10), 2026, 2029, 2032, 2035)
  )+
  
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
