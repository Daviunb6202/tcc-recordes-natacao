source('Setup.R')
source('AdequacaoExpo.R')
primeiro_recorde <- head(df_recordes, 1)
ultimo_recorde <- tail(df_recordes, 1)
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


