library(testthat)
library(ggplot2) # Para expect_s3_class de ggplot (aunque no se usa aquí directamente, es común)
library(dplyr) # Para %>%

# Crear datos de ejemplo (para que los tests sean autocontenidos y no dependan de df_ejemplo global)
df_ejemplo_test <- data.frame(
  genotipo = rep(c("G1", "G2", "G3", "G4", "Control"), each = 20),
  tratamiento = factor(rep(c("T1", "T2"), times = 50)), # Añadir tratamiento para la interacción
  rendimiento = c(rnorm(20, 50, 5), rnorm(20, 55, 5), rnorm(20, 45, 5), rnorm(20, 70, 5), rnorm(20, 52, 5)),
  rendimiento_log = c(log(rnorm(20, 50, 5)), log(rnorm(20, 55, 5)), log(rnorm(20, 45, 5)), log(rnorm(20, 70, 5)), log(rnorm(20, 52, 5))),
  sabor_score = sample(1:5, 100, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.2))
)
df_ejemplo_test$rendimiento[c(5, 25, 45, 85)] <- c(10, 100, NA, 15)
df_ejemplo_test$sabor_score[c(10, 30, 90)] <- NA


test_that("run_analysis funciona para tipo cuantitativo", {
  # Establecer temporalmente los contrastes para la prueba, como lo hace la función
  old_contrasts <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(old_contrasts)) # Asegurarse de restaurar al salir
  
  # Ejecutar la función (desactivar verbose para tests)
  results <- run_analysis(
    var_name = "rendimiento",
    data = df_ejemplo_test, # Usar los datos de test
    formula = ~ genotipo * tratamiento, # Formula con interacción
    response_type = "quantitative"
  )
  
  # Validaciones básicas
  expect_type(results, "list")
  expect_equal(results$type, "quantitative")
  expect_equal(results$variable, "rendimiento")
  
  # Validar la herencia de clase más robusta para ANOVA y Levene
  expect_s3_class(results$quantitative_results$anova_table, "data.frame") # O más general si falla
  expect_true(inherits(results$quantitative_results$anova_table, "Anova.res")) # Clase específica
  
  expect_true("normal_ok" %in% names(results$quantitative_results))
  expect_true("homoscedastic_ok" %in% names(results$quantitative_results))
  
  expect_s3_class(results$quantitative_results$homoscedasticity_test, "list") # O más general si falla
  expect_true(inherits(results$quantitative_results$homoscedasticity_test, "htest")) # Clase específica

  expect_true(nrow(results$quantitative_results$anova_table) > 0)
})

test_that("run_analysis funciona para tipo ordinal", {
  # Establecer temporalmente los contrastes para la prueba, como lo hace la función
  old_contrasts <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(old_contrasts)) # Asegurarse de restaurar al salir
  
  # Ejecutar la función (desactivar verbose para tests)
  results <- run_analysis(
    var_name = "sabor_score", # Usar sabor_score
    data = df_ejemplo_test, # Usar los datos de test
    formula = ~ genotipo + tratamiento, # Formula con efectos principales
    response_type = "ordinal"
  )
  
  # Validaciones básicas
  expect_type(results, "list")
  expect_equal(results$type, "ordinal")
  expect_equal(results$variable, "sabor_score")
  
  expect_true("kruskal_wallis_table" %in% names(results$ordinal_results))
  expect_true(is.matrix(results$ordinal_results$kruskal_wallis_table))
  expect_true(nrow(results$ordinal_results$kruskal_wallis_table) > 0)
})