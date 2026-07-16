source("setup.R") 
source("AdequacaoExpo.R")
primeiro_recorde <- head(df_recordes, 1)
ultimo_recorde <- tail(df_recordes, 1)

# --- CÁLCULO DAS ESTATÍSTICAS ANUAIS ---
df_stats <- df_filtrado %>%
  group_by(Year) %>%
  summarise(
    mu_local = mean(Results, na.rm = TRUE),
    sigma_local = sd(Results, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Year)

# --- MODELAGEM DA TENDÊNCIA E VALORES EXTREMOS (WEIBULL SOBRE EXPONENCIAL) ---
modelo_treino <- ajustar_exponencial(df_dados = df_filtrado, ano_limite = 2025)
coeffs <- modelo_treino$coeffs
sigma_fit <- modelo_treino$sigma_fit
ano_base_t <- modelo_treino$min_year_treino
coeffs
# --- PROJEÇÃO DE TENDÊNCIA --- 
t_hist <- df_stats$Year - min(df_stats$Year)
df_calc <- df_stats %>%
  mutate(
    Tendencia_Expo = coeffs["a"] * exp(coeffs["b"] * t_hist) + coeffs["c"]
  )

# --- AJUSTE WEIBULL (SOBRE RESÍDUOS DE TREINO) ---
df_melhores_residuos <- df_filtrado %>%
  group_by(Year) %>%
  slice_min(order_by = Results, n = 1) %>% 
  ungroup() %>%
  left_join(df_calc %>% dplyr::select(Year, Tendencia_Expo), by = "Year") %>%
  mutate(
    Abs_Residuals = abs(Results - Tendencia_Expo)
  ) %>%
  filter(Abs_Residuals > 0.0001)

fit_w <- fitdistr(df_melhores_residuos$Abs_Residuals, "weibull")
forma_w <- fit_w$estimate["shape"]
escala_w <- fit_w$estimate["scale"]
largura_weibull_05 <- qweibull(0.05, shape = forma_w, scale = escala_w)
largura_weibull_95 <- qweibull(0.95, shape = forma_w, scale = escala_w)

# --- VISUALIZAÇÃO DENSIDADE WEIBULL ---
ggplot(df_melhores_residuos, aes(x = Abs_Residuals)) +
  
  geom_line(data = df_curva_weibull, aes(x = Abs_Residuals, y = Densidade), 
            color = "red", 
            linewidth = 1.) +

  scale_x_continuous(limits = c(0, limite_maximo_eixo_x), 
                     breaks = seq(0, limite_maximo_eixo_x, by = 0.5)) +
  
  theme_bw(base_family = "Arial") +
  
  labs(
    x = "Resíduos Absolutos (Resultados vs Tendência)",
    y = "Densidade"
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 20),
    axis.text.y = element_text(color = "black", size = 15),
    axis.ticks.y = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length.y = unit(0.2, "cm"),
    axis.text.x = element_text(color = "black", size = 15),
    axis.ticks.x = element_line(color = "black", linewidth = 0.6)
  )

# --- ENVELOPE E ESCADA HISTÓRICA --- 
df_sumario <- df_calc %>%
  mutate(
    Tunel_Centro = Tendencia_Expo,
    Tunel_Inferior = Tendencia_Expo - largura_weibull_95,
    Tunel_Superior = Tendencia_Expo - largura_weibull_05
  )

# --- Escada Teórica Ancorada no Recorde Inicial --- 
df_sumario$Step_Theoretical <- primeiro_recorde$Results
for(i in 2:nrow(df_sumario)) {
  df_sumario$Step_Theoretical[i] <- min(df_sumario$Step_Theoretical[i-1], df_sumario$Tunel_Inferior[i])
}

ultimo_valor_real <- df_filtrado %>% arrange(abs(Results)) %>% pull(Results) %>% head(1)
ultimo_valor_teorico <- tail(df_sumario$Tunel_Inferior, 1)
val_real_label <- round(ultimo_valor_real, 2)
val_teorico_label <- round(ultimo_valor_teorico, 2)


# --- VISUALIZAÇÃO ---
ggplot(df_filtrado, aes(x = Year, y = Results)) +
  geom_point(color = "black", size = 1, alpha = 0.5) +
  
  geom_ribbon(data = df_sumario, 
              aes(x = Year, ymin = Tunel_Inferior, ymax = Tunel_Superior, y = NULL), 
              fill = "red", alpha = 0.2) + 
  
  geom_line(data = df_sumario, aes(x = Year, y = Tunel_Centro), 
            color = "#C71585", linetype = "dashed", linewidth = 0.8) +
  
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
  
  geom_step(data = df_recordes, aes(x = Year, y = Results), 
            color = "black", linewidth = 0.7, direction = "hv") + 
  
  geom_step(data = df_sumario, aes(x = Year, y = Step_Theoretical), 
            color = "red", linewidth = 1, direction = "hv") +
  
  geom_segment(data = ultimo_recorde, aes(x = Year, y = Results, xend = 2026, yend = Results),
               linetype = "solid", color = "black") +
  
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
  
  labs(
    x = "Ano", 
    y = "Tempo (s)"
  ) +
  
  theme_gray(base_family ="Arial") +
  
  theme(
    panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold", size = 20),
        axis.text.y = element_text(color = "black", size = 14),
        axis.ticks.y = element_line(color = "black", linewidth = 0.8),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.text.x = element_text(color = "black", size = 15),
        axis.ticks.x = element_line(color = "black", linewidth = 0.6)
)
