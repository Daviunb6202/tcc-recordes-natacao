source("setup.R")
source("AdequacaoExpo.R")

# --- AJUSTE DO MODELO EXPONENCIAL DE REFERÊNCIA ---
modelo_estudado <- ajustar_exponencial(df_dados = df_filtrado, ano_limite = 2025)
coeffs <- modelo_estudado$coeffs
sigma_fit <- modelo_estudado$sigma_fit
ano_base_t <- modelo_estudado$min_year_treino
anos_dados <- seq(ano_base_t, 2025, by = 1)

expo_referencia <- tibble(
  Year = anos_dados,
  Trend_Value = coeffs["a"] * exp(coeffs["b"] * (anos_dados - ano_base_t)) + coeffs["c"]
)

# --- CÁLCULO DOS MAIORES RESÍDUOS EM RELAÇÃO A MÉDIA ---
df_melhores_residuos <- df_filtrado %>%
  group_by(Year) %>%
  slice_min(order_by = Results, n = 1) %>% 
  ungroup() %>%
  left_join(expo_referencia %>% dplyr::select(Year, Trend_Value), by = "Year") %>%
  mutate(
    Abs_Residuals = abs(Results - Trend_Value)
  ) %>%
  filter(Abs_Residuals > 0.0001)

# --- AJUSTE DA DISTRIBUIÇÃO WEIBULL NOS EXTREMOS ---
fit_weibull <- MASS::fitdistr(df_melhores_residuos$Abs_Residuals, "weibull")
shape_est   <- fit_weibull$estimate["shape"]
scale_est   <- fit_weibull$estimate["scale"]
print(fit_weibull$estimate)

# --- TESTES DE ADERÊNCIA ESTATÍSTICA ---
ks_result <- LcKS(df_melhores_residuos$Abs_Residuals, "pweibull")
ad_result <- goftest::ad.test(df_melhores_residuos$Abs_Residuals, 
                              null = "pweibull", 
                              shape = shape_est, 
                              scale = scale_est)


cat(sprintf("1. Teste de Kolmogorov-Smirnov (KS):\n   p-valor = %.4f\n", ks_result$p.value))
cat(sprintf("2. Teste de Anderson-Darling (AD):\n   p-valor = %.4f\n", ad_result$p.value))

# --- QUANTIL-QUANTIL PLOT (QQ-PLOT) PARA O TCC ---
n_points <- nrow(df_melhores_residuos)
df_qq <- df_melhores_residuos %>%
  arrange(Abs_Residuals) %>%
  mutate(
    Theoretical_Quantiles = qweibull((1:n_points - 0.5)/n_points, shape = shape_est, scale = scale_est)
  )

ggplot(df_qq, aes(x = Theoretical_Quantiles, y = Abs_Residuals)) +
  
  geom_point(alpha = 0.5, color = "black", size = 2.5) +
  
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    x = "Quantis Teóricos (Distribuição Weibull)",
    y = "Magnitude dos Resíduos dos Mínimos Observados"
  )+
  
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
