source("setup.R")
primeiro_recorde <- head(df_recordes, 1)
ultimo_recorde <- tail(df_recordes, 1)

# --- CÁLCULO LOCAL COM INTERVALO DE CONFIANÇA (ENVELOPE) ---
df_theory <- df_filtrado %>%
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
    
    # --- Cálculo do Envelope (IC 95% para o Mínimo) ---
    # Dado que P(Min <= z) = 1 - (1 - Phi(z))^n
    
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
ultimo_valor_real <- df_filtrado %>% arrange(abs(Results)) %>% pull(Results) %>% head(1)
ultimo_valor_teorico <- tail(df_theory$Step_Theoretical, 1)

val_real_label <- round(ultimo_valor_real, 2)
val_teorico_label <- round(ultimo_valor_teorico, 2)

df_theory$Step_Theoretical[1] = primeiro_recorde$Results

# --- VISUALIZAÇÃO ---
ggplot(df_filtrado, aes(x = Year, y = Results)) +
  
  geom_point(color = "black", size = 1, alpha = 0.5) +
  
  geom_ribbon(data = df_theory, 
              aes(x = Year, ymin = Step_Lower, ymax = Step_Upper, y = NULL), 
              fill = "blue", 
              alpha = 0.15) + 
  
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
  
  geom_step(data = df_theory, 
            aes(x = Year, y = Step_Theoretical), 
            color = "blue", linewidth = 1, direction = "hv") +
  
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

