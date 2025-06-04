# tests/testthat/test-analisis_diseno.R

context("Pruebas para la función analisis_diseno")

# --- Datos de prueba ---
datos_bal <- data.frame(
  Genotipo = factor(rep(c("G1", "G2"), each = 6)),
  Tratamiento = factor(rep(rep(c("T1", "T2", "T3"), each = 2), 2)),
  Respuesta = rnorm(12)
)

datos_desbal_sin_vacias <- data.frame(
  Genotipo = factor(c(rep("G1", 5), rep("G2", 7))), # G1:5, G2:7
  Tratamiento = factor(c(rep("T1", 3), rep("T2", 2), rep("T1", 4), rep("T2", 3))), # G1T1:3, G1T2:2, G2T1:4, G2T2:3
  Respuesta = rnorm(12)
)

datos_desbal_con_vacias <- datos_bal %>%
  dplyr::filter(!(Genotipo == "G1" & Tratamiento == "T1")) # G1T1 tendrá 0 obs.

datos_un_factor <- data.frame(
  FactorA = factor(rep(c("A1", "A2", "A3"), times = c(2, 3, 2))),
  Respuesta = rnorm(7)
)

datos_error_factor_inexistente <- data.frame(A = 1:3)


# --- Pruebas ---

test_that("Diseño balanceado se identifica correctamente", {
  res_bal <- analisis_diseno(datos_bal, factores = c("Genotipo", "Tratamiento"))
  
  expect_true(res_bal$estadisticas_balance$es_balanceado)
  expect_equal(res_bal$estadisticas_balance$n_min, 2)
  expect_equal(res_bal$estadisticas_balance$n_max, 2)
  expect_false(res_bal$estadisticas_balance$celdas_vacias)
  expect_true(grepl("perfectamente balanceado", res_bal$diagnostico_general, ignore.case = TRUE))
  expect_equal(nrow(res_bal$tabla_frecuencias), 2 * 3) # G*T niveles
  expect_true(all(res_bal$tabla_frecuencias$n == 2))
})

test_that("Diseño desbalanceado sin celdas vacías se identifica correctamente", {
  res_desbal <- analisis_diseno(datos_desbal_sin_vacias, factores = c("Genotipo", "Tratamiento"))
  
  expect_false(res_desbal$estadisticas_balance$es_balanceado)
  expect_equal(res_desbal$estadisticas_balance$n_min, 2)
  expect_equal(res_desbal$estadisticas_balance$n_max, 4)
  expect_false(res_desbal$estadisticas_balance$celdas_vacias)
  expect_true(grepl("desbalanceado", res_desbal$diagnostico_general, ignore.case = TRUE))
  expect_true(grepl("Tipo III SS", res_desbal$implicaciones_recomendaciones))
  expect_equal(nrow(res_desbal$tabla_frecuencias), 2 * 2) # G*T niveles
  expect_equal(sum(res_desbal$tabla_frecuencias$n), nrow(datos_desbal_sin_vacias))
})

test_that("Diseño desbalanceado con celdas vacías se identifica correctamente", {
  res_vacias <- analisis_diseno(datos_desbal_con_vacias, factores = c("Genotipo", "Tratamiento"))
  
  expect_false(res_vacias$estadisticas_balance$es_balanceado)
  expect_true(res_vacias$estadisticas_balance$celdas_vacias)
  expect_equal(res_vacias$estadisticas_balance$n_min, 0) # Una celda tiene 0
  expect_equal(res_vacias$estadisticas_balance$n_max, 2) 
  expect_true(grepl("celdas vacías", res_vacias$diagnostico_general, ignore.case = TRUE))
  expect_true(grepl("celdas vacías", res_vacias$implicaciones_recomendaciones, ignore.case = TRUE))
  expect_equal(nrow(res_vacias$tabla_frecuencias), 2 * 3) # G*T niveles, tidyr::complete rellena
  expect_true(any(res_vacias$tabla_frecuencias$n == 0))
})

test_that("Funciona con un solo factor", {
  res_un_factor <- analisis_diseno(datos_un_factor, factores = "FactorA")
  
  expect_false(res_un_factor$estadisticas_balance$es_balanceado) # 2,3,2 no es balanceado
  expect_equal(res_un_factor$estadisticas_balance$n_min, 2)
  expect_equal(res_un_factor$estadisticas_balance$n_max, 3)
  expect_equal(nrow(res_un_factor$tabla_frecuencias), 3) # 3 niveles
})

test_that("Retorna los componentes esperados en la lista de resultados", {
  res <- analisis_diseno(datos_bal, factores = c("Genotipo", "Tratamiento"))
  
  expect_named(res, c("tabla_frecuencias", "estadisticas_balance", 
                      "descripcion_factores", "diagnostico_general", 
                      "implicaciones_recomendaciones"))
  expect_s3_class(res$tabla_frecuencias, "data.frame")
  expect_type(res$estadisticas_balance, "list")
  expect_s3_class(res$descripcion_factores, "data.frame")
  expect_type(res$diagnostico_general, "character")
  expect_type(res$implicaciones_recomendaciones, "character")
})

test_that("Maneja errores de entrada correctamente", {
  expect_error(analisis_diseno(datos_bal, factores = c("Factor_Inexistente")), 
               "No todos los factores especificados")
  expect_error(analisis_diseno("no es un dataframe", factores = "Genotipo"))
  expect_error(analisis_diseno(datos_bal, factores = character(0))) # Vector de factores vacío
})

test_that("La tabla de frecuencias suma el total de observaciones correctas", {
    res_bal <- analisis_diseno(datos_bal, factores = c("Genotipo", "Tratamiento"))
    expect_equal(sum(res_bal$tabla_frecuencias$n), nrow(datos_bal))

    res_desbal <- analisis_diseno(datos_desbal_sin_vacias, factores = c("Genotipo", "Tratamiento"))
    expect_equal(sum(res_desbal$tabla_frecuencias$n), nrow(datos_desbal_sin_vacias))

    # Para el caso con celdas vacías, la suma de 'n' debe ser el número de filas *originales* del df
    # (antes de que tidyr::complete pudiera añadir filas con n=0 si alguna combinación no estaba)
    # En este caso, datos_desbal_con_vacias ya tiene las filas originales correctas.
    res_vacias <- analisis_diseno(datos_desbal_con_vacias, factores = c("Genotipo", "Tratamiento"))
    expect_equal(sum(res_vacias$tabla_frecuencias$n), nrow(datos_desbal_con_vacias))
})

test_that("Descripción de factores es correcta", {
    res_bal <- analisis_diseno(datos_bal, factores = c("Genotipo", "Tratamiento"))
    df_desc_factores <- res_bal$descripcion_factores
    expect_equal(df_desc_factores$Factor, c("Genotipo", "Tratamiento"))
    expect_equal(df_desc_factores$Niveles, c(2, 3)) # 2 niveles de Genotipo, 3 de Tratamiento
})