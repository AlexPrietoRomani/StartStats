# StartStats\StartStats\tests\testthat.R
library(testthat)
library(ggplot2)
library(dplyr)

# Datos de ejemplo
df_ejemplo_test <- data.frame(
  genotipo = rep(c("G1", "G2", "G3", "G4", "Control"), each = 20),
  tratamiento = factor(rep(c("T1", "T2"), times = 50)),
  rendimiento = c(rnorm(20, 50, 5), rnorm(20, 55, 5), rnorm(20, 45, 5), rnorm(20, 70, 5), rnorm(20, 52, 5)),
  rendimiento_log = c(log(rnorm(20, 50, 5)), log(rnorm(20, 55, 5)), log(rnorm(20, 45, 5)), log(rnorm(20, 70, 5)), log(rnorm(20, 52, 5))),
  sabor_score = sample(1:5, 100, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.2))
)
df_ejemplo_test$rendimiento[c(5, 25, 45, 85)] <- c(10, 100, NA, 15)
df_ejemplo_test$sabor_score[c(10, 30, 90)] <- NA


test_that("run_analysis funciona para tipo cuantitativo", {
  # Establecer temporalmente los contrastes para la prueba, como lo hace la función
  old_contrasts <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(old_contrasts)) 
  
  results <- run_analysis(
    var_name = "rendimiento",
    data = df_ejemplo_test,
    formula = ~ genotipo * tratamiento,
    response_type = "quantitative"
  )
  
  # Validaciones
  expect_type(results, "list")
  expect_equal(results$type, "quantitative")
  expect_equal(results$variable, "rendimiento")
  
  # Validación de clase más robusta para anova_table (car::Anova)
  expect_s3_class(results$quantitative_results$anova_table, "data.frame")
  expect_true(inherits(results$quantitative_results$anova_table, "Anova.res")) 
  
  expect_true("normal_ok" %in% names(results$quantitative_results))
  expect_true("homoscedastic_ok" %in% names(results$quantitative_results))
  
  # Validación de clase más robusta para homoscedasticity_test (car::leveneTest)
  expect_s3_class(results$quantitative_results$homoscedasticity_test, "list") # htest hereda de list
  expect_true(inherits(results$quantitative_results$homoscedasticity_test, "htest")) 

  expect_true(nrow(results$quantitative_results$anova_table) > 0)
})

test_that("run_analysis funciona para tipo ordinal", {
  old_contrasts <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(old_contrasts)) 
  
  results <- run_analysis(
    var_name = "sabor_score",
    data = df_ejemplo_test,
    formula = ~ genotipo + tratamiento,
    response_type = "ordinal"
  )
  
  expect_type(results, "list")
  expect_equal(results$type, "ordinal")
  expect_equal(results$variable, "sabor_score")
  
  expect_true("kruskal_wallis_table" %in% names(results$ordinal_results))
  expect_true(is.matrix(results$ordinal_results$kruskal_wallis_table))
  expect_true(nrow(results$ordinal_results$kruskal_wallis_table) > 0)
})