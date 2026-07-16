source('Setup.R')
source('AdequacaoExpo.R')
primeiro_recorde <- head(df_recordes, 1)
ultimo_recorde <- tail(df_recordes, 1)
ultimo_valor_real <- df_filtrado %>% arrange(abs(Results)) %>% pull(Results) %>% head(1)
val_real_label <- round(ultimo_valor_real, 2)

# --- VISUALIZAÇÃO ---
ggplot(df_filtrado, aes(x = Year, y = Results)) +
  
  geom_point(color = "black", size = 1, alpha = 0.5) +

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
  
  geom_step(data = df_recordes, 
            aes(x = Year, y = Results), 
            color = "black", 
            linewidth = 0.7,
            direction = "hv") + 
  
  geom_point(data = df_recordes, aes(x = Year, y = Results), color = "black", size = 1.5) +

  # OPCAO PLOT MODALIDADE MASCULINA #
  scale_y_continuous(
    breaks = sort(c(seq(45, 54, 2), ultimo_valor_real)),
    minor_breaks = NULL,
    labels = function(x) sprintf("%.2f", x) 
  ) + 
  
  # OPCAO PLOT MODALIDADE FEMININA #
  #  scale_y_continuous(
  #  breaks = sort(c(seq(45, 60, 8), 59, ultimo_valor_real)),
  #  minor_breaks = NULL,
  #  labels = function(x) sprintf("%.2f", x) 
  #   ) + 
  
  labs(x = "Ano",
       y = "Tempo (s)") +
  
  theme_gray(base_family = "Arial") + 
  
  theme(
    axis.title = element_text(face = "bold", size = 20),
    axis.text.y = element_text(color = "black", size = 15),
    axis.ticks.y = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length.y = unit(0.2, "cm"),
    axis.text.x = element_text(color = "black", size = 15),
    axis.ticks.x = element_line(color = "black", linewidth = 0.6)
  )

# --- VISUALIZAÇÃO COM CURVA DE REGRESSÃO 
modelo_treino <- ajustar_exponencial(df_dados = df_filtrado, ano_limite = 2019)
coeffs <- modelo_treino$coeffs
sigma_fit <- modelo_treino$sigma_fit
ano_base_t <- modelo_treino$min_year_treino

anos_todos <- seq(ano_base_t, 2026, by = 1)
t_full <- anos_todos - ano_base_t
df_expo_completa <- tibble(
  Year = anos_todos,
  Trend_Value = coeffs["a"] * exp(coeffs["b"] * t_full) + coeffs["c"]
)
# --- VISUALIZAÇÃO ---
ggplot(df_filtrado, aes(x = Year, y = Results)) +
  
  geom_point(color = "black", size = 1, alpha = 0.5) +
  
  geom_line(data = df_expo_completa, aes(x = Year, y = Trend_Value), 
            color = "red", linewidth = 1) +
  
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
  
  geom_step(data = df_recordes, 
            aes(x = Year, y = Results), 
            color = "black", 
            linewidth = 0.7,
            direction = "hv") + 
  
  geom_point(data = df_recordes, aes(x = Year, y = Results), color = "black", size = 1.5) +
  
  # OPCAO PLOT MODALIDADE MASCULINA #
  scale_y_continuous(
    breaks = sort(c(seq(45, 54, 2), ultimo_valor_real)),
    minor_breaks = NULL,
    labels = function(x) sprintf("%.2f", x) 
  ) + 
  
  # OPCAO PLOT MODALIDADE FEMININA #
  #  scale_y_continuous(
  #  breaks = sort(c(seq(45, 60, 8), 59, ultimo_valor_real)),
  #  minor_breaks = NULL,
  #  labels = function(x) sprintf("%.2f", x) 
  #   ) + 
  
  labs(x = "Ano",
       y = "Tempo (s)") +
  
  theme_gray(base_family = "Arial") + 
  
  theme(
    axis.title = element_text(face = "bold", size = 20),
    axis.text.y = element_text(color = "black", size = 15),
    axis.ticks.y = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length.y = unit(0.2, "cm"),
    axis.text.x = element_text(color = "black", size = 15),
    axis.ticks.x = element_line(color = "black", linewidth = 0.6)
  )


