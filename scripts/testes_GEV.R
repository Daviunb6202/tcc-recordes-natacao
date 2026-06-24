source("Setup.R")
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

# --- AJUSTE GEV (SOBRE RESÍDUOS DA TENDÊNCIA) ---
df_melhores_residuos <- df_filtrado %>%
  group_by(Year) %>%
  slice_min(order_by = Results, n = 1) %>% 
  ungroup() %>%
  left_join(expo_referencia %>% dplyr::select(Year, Trend_Value), by = "Year") %>%
  mutate(
    Abs_Residuals = abs(Results - Trend_Value)
  ) %>%
  filter(Abs_Residuals > 0.0001)

fit_gev <- fevd(df_melhores_residuos$Abs_Residuals, type = "GEV")


loc_g <- fit_gev$results$par["location"]
scale_g <- fit_gev$results$par["scale"]
shape_g <- fit_gev$results$par["shape"]


tolerancia <- 0.01

familia_gev <- case_when(
  shape_g < -tolerancia ~ "Weibull",
  shape_g > tolerancia ~ "Fréchet",
  TRUE ~ "Gumbel"
)

ci_parametros <- ci(fit_gev, type = "parameter")

loc_inf <- ci_parametros["location", 1]
loc_sup <- ci_parametros["location", 3]

scale_inf <- ci_parametros["scale", 1]
scale_sup <- ci_parametros["scale", 3]

shape_inf <- ci_parametros["shape", 1]
shape_sup <- ci_parametros["shape", 3]

# --- RESULTADOS DAS ESTIMAÇÕES DE PARÂMETROS ---
cat("--- RESULTADOS DO AJUSTE GEV ---\n")
cat("Parâmetro de Localização (μ):", round(loc_g, 4), 
    "  [IC 95%:", round(loc_inf, 4), " a ", round(loc_sup, 4), "]\n")
cat("Parâmetro de Escala (σ):", round(scale_g, 4), 
    "  [IC 95%:", round(scale_inf, 4), " a ", round(scale_sup, 4), "]\n")
cat("Parâmetro de Forma (ξ):", round(shape_g, 4), 
    "  [IC 95%:", round(shape_inf, 4), " a ", round(shape_sup, 4), "]\n")
cat("Família Extrema Identificada:", familia_gev, "\n\n")