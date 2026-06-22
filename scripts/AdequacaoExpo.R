ajustar_exponencial <- function(df_dados, ano_limite) {
  
  df_treino <- df_dados %>%
    filter(Year <= ano_limite)
  
  df_history_clean <- df_treino %>%
    group_by(Year) %>%
    summarise(
      mu_local = mean(Results, na.rm = TRUE), 
      .groups = 'drop'
    ) %>%
    arrange(Year) %>%
    mutate(t = Year - min(Year))
  
  # --- AJUSTE DA EXPONENCIAL NA MÉDIA DOS BLOCOS ---
  start_c_mu <- min(df_history_clean$mu_local) * 0.95
  start_a_mu <- max(df_history_clean$mu_local) - start_c_mu
  start_b_mu <- -0.05
  
  fit_exp <- nls(mu_local ~ a * exp(b * t) + c, 
                 data = df_history_clean,
                 start = list(a = start_a_mu, b = start_b_mu, c = start_c_mu),
                 control = nls.control(maxiter = 1000, warnOnly = TRUE))
  
  coeffs <- coef(fit_exp)
  sigma_fit <- summary(fit_exp)$sigma 
  
  return(list(
    coeffs = coeffs,
    sigma_fit = sigma_fit,
    min_year_treino = min(df_history_clean$Year)
  ))
}