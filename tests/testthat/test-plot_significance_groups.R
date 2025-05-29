# tests/testthat/test-plot_significance_groups.R
library(testthat)
library(ggplot2) # Para expect_s3_class de ggplot
library(dplyr) # Para %>%

# Es crucial que generate_groups funcione para que estos tests puedan ejecutarse.
# Si generate_groups aún no está 100% test_that-compatible, podrías crear un df_ejemplo_grupos
# manual para testear solo plot_significance_groups.
# Por ahora, asumo que generate_groups sí funciona correctamente.
# df_ejemplo_test viene de la definición global en test-run_analysis.R
# O puedes pegarla aquí también para que este test sea autocontenido.
df_ejemplo_test <- data.frame(
  genotipo = rep(c("G1", "G2", "G3", "G4", "Control"), each = 20),
  tratamiento = factor(rep(c("T1", "T2"), times = 50)),
  rendimiento = c(rnorm(20, 50, 5), rnorm(20, 55, 5), rnorm(20, 45, 5), rnorm(20, 70, 5), rnorm(20, 52, 5)),
  rendimiento_log = c(log(rnorm(20, 50, 5)), log(rnorm(20, 55, 5)), log(rnorm(20, 45, 5)), log(rnorm(20, 70, 5)), log(rnorm(20, 52, 5))),
  sabor_score = sample(1:5, 100, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.2))
)
df_ejemplo_test$rendimiento[c(5, 25, 45, 85)] <- c(10, 100, NA, 15)
df_ejemplo_test$sabor_score[c(10, 30, 90)] <- NA

# Generar un resultado de generate_groups para usar en el test
# Asegúrate de que generate_groups funcione correctamente para esto
grupos_ejemplo <- generate_groups(
  data = df_ejemplo_test,
  var_original = "rendimiento",
  formula_interaccion = "genotipo",
  method = "tukey",
  verbose = FALSE
)

test_that("plot_significance_groups crea un gráfico ggplot2 sin faceta", {
  p <- plot_significance_groups(
    data = grupos_ejemplo,
    title = "Test Plot",
    y_label = "Valor"
  )
  
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Test Plot")
  expect_equal(p$labels$y, "Valor")
  expect_true(any(sapply(p$layers, function(x) inherits(x$geom, "GeomColPattern"))))
  expect_true(any(sapply(p$layers, function(x) inherits(x$geom, "GeomText"))))
})

test_that("plot_significance_groups reordena correctamente", {
  p_reordered <- plot_significance_groups(
    data = grupos_ejemplo,
    reorder_x = TRUE,
    desc = TRUE
  )
  
  expect_true(is.factor(p_reordered$data$interaccion))
  # Podemos testear el orden real si conocemos las medias de los datos de prueba
  # (asumiendo que genotipo en df_ejemplo_test da medias distintas)
  expected_order_levels <- (grupos_ejemplo %>% dplyr::arrange(desc(means)))$interaccion
  expect_equal(levels(p_reordered$data$interaccion), expected_order_levels)
})

test_that("plot_significance_groups crea gráfico con facetas", {
    # Primero, preparar los datos para la faceta (ejemplo simple)
    # Aquí simulamos un escenario donde 'grupos_interaccion' tendría una columna para facetar
    # Para este test, la 'interaccion' real no se puede "desagrupar" con facet_var directamente
    # a menos que crees la columna de faceta explícitamente en el dataframe.
    # Usaremos el mismo df_ejemplo_test con una columna extra
    grupos_con_facet_dummy <- grupos_ejemplo %>%
        dplyr::mutate(campana_facet = rep(c("C1", "C2"), length.out = nrow(.))) 
    
    p_facet <- plot_significance_groups(
        data = grupos_con_facet_dummy,
        facet_var = "campana_facet",
        title = "Test Plot with Facets"
    )
    expect_s3_class(p_facet, "ggplot")
    # Verificar que el objeto de faceta está presente y es del tipo correcto
    expect_s3_class(p_facet$facet, "FacetWrap")
})