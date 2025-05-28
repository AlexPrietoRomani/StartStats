test_that("generate_groups funciona correctamente con Tukey", {
  # Crea datos de prueba mínimos (distintos de los de los ejemplos si quieres)
  df_test <- data.frame(
    genotipo = factor(c("A", "A", "B", "B")),
    valor_original = c(10, 12, 20, 22),
    valor_transformado = c(log(10), log(12), log(20), log(22))
  )
  
  # Ejecuta la función
  result <- generate_groups(
    data = df_test,
    var_original = "valor_original",
    var_transformada = "valor_transformado",
    formula_interaccion = "genotipo",
    method = "tukey",
    verbose = FALSE # Para que no imprima mensajes durante el test
  )
  
  # Verifica las expectativas
  expect_s3_class(result, "tbl_df") # Es un tibble
  expect_equal(nrow(result), 2)    # Tiene 2 filas (para A y B)
  expect_true(all(c("interaccion", "means", "groups") %in% colnames(result)))
  # Puedes agregar más expectativas según tus datos de prueba y resultados esperados
  expect_equal(result$interaccion, c("A", "B"))
  # expect_equal(result$means, c(11, 21)) # Asegúrate de que las medias sean correctas
  # expect_true(result$groups[1] != result$groups[2]) # Si esperas diferencia
})

test_that("generate_groups maneja var_transformada = NULL", {
  df_test <- data.frame(
    genotipo = factor(c("A", "A", "B", "B")),
    valor_original = c(10, 12, 20, 22)
  )
  result <- generate_groups(
    data = df_test,
    var_original = "valor_original",
    formula_interaccion = "genotipo",
    method = "tukey",
    verbose = FALSE
  )
  expect_s3_class(result, "tbl_df")
})