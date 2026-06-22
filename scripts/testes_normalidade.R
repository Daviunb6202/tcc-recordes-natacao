source("setup.R")
primeiro_recorde <- head(df_recordes, 1)
ultimo_recorde <- tail(df_recordes, 1)

#Histograma Simples
hist(df_filtrado$Results,
     breaks = 30,
     main = "Distribuicao de Tempo de Natacao 1973-2025",
     xlab = "Tempo de Natacao (segundos)",
     ylab = "Frequencia",
     col = "skyblue", border = "white")

#1. Shapiro-Wilk Test
shapiro.test(df_filtrado$Padronizado)
# W = 0.98224, p-value = 0.0194

# 2. Anderson-Darling Test (from nortest package)
nortest::ad.test(df_filtrado$Padronizado)# A = 0.88054, p-value = 0.02376

# 3. Lilliefors (Kolmogorov-Smirnov) Test (from nortest package)
nortest::lillie.test(df_filtrado$Padronizado)
#D = 0.069121, p-value = 0.03217

nortest::cvm.test(df_filtrado$Padronizado)
#W = 0.1477, p-value = 0.02525

dim(df_filtrado)

df_minimos <- df_filtrado %>%
  group_by(Year) %>%
  slice_min(order_by = Results, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    Padronizado_Minimos = (Results - mean(Results, na.rm = TRUE)) / sd(Results, na.rm = TRUE)
  ) %>%
  arrange(Padronizado_Minimos) %>%
  mutate(
    n_points = n(),
    Theoretical_Quantiles = qnorm((row_number() - 0.5) / n_points)
  )

ggplot(df_minimos, aes(x = Theoretical_Quantiles, y = Padronizado_Minimos)) +

  geom_point(alpha = 0.5, color = "black", size = 2.5) +
  
  geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed", linewidth = 1) +
  
  labs(
    x = "Quantis Teóricos (Distribuição Normal)",
    y = "Mínimo dos Blocos Padronizados (escore Z)"
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
