# tests/testthat/test-plot_significance_groups.R
library(testthat)
library(ggplot2)
library(dplyr)

# Definir un dataframe de ejemplo que simule la salida de generate_groups
# Esto hace el test más aislado y rápido, ya que no depende de generate_groups
grupos_ejemplo_dummy <- tibble::tibble(
  interaccion = factor(c("G1", "G2", "G3", "G4", "Control"), levels = c("G1", "G2", "G3", "G4", "Control")),
  means = c(50.2, 55.5, 45.1, 70.8, 52.3),
  groups = c("b", "ab", "b", "a", "ab"),
  campana_facet = rep(c("C1", "C2"), length.out = 5) # Para probar facetas
)

test_that("plot_significance_groups crea un gráfico ggplot2 sin faceta", {
  p <- plot_significance_groups(
    data = grupos_ejemplo_dummy,
    title = "Test Plot Sin Faceta",
    y_label = "Valor Promedio",
    plot_width = 6, plot_height = 4 # Probar los nuevos argumentos
  )
  
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Test Plot Sin Faceta")
  expect_equal(p$labels$y, "Valor Promedio")
  expect_true(any(sapply(p$layers, function(x) inherits(x$geom, "GeomColPattern"))))
  expect_true(any(sapply(p$layers, function(x) inherits(x$geom, "GeomText"))))
  # No podemos probar directamente el tamaño de salida del archivo, ya que ggsave no se llama dentro.
  # Pero los argumentos están definidos.
})

test_that("plot_significance_groups reordena correctamente", {
  # Crea una copia para que el reordenamiento no afecte el dummy original
  data_for_reorder <- dplyr::tibble(grupos_ejemplo_dummy) 
  
  p_reordered <- plot_significance_groups(
    data = data_for_reorder,
    reorder_x = TRUE,
    desc = TRUE
  )
  
  expect_s3_class(p_reordered, "ggplot")
  expect_true(is.factor(p_reordered$data$interaccion))
  
  # Validar el orden esperado: G4 (70.8), G2 (55.5), Control (52.3), G1 (50.2), G3 (45.1)
  expected_order_levels <- c("G4", "G2", "Control", "G1", "G3")
  expect_equal(levels(p_reordered$data$interaccion), expected_order_levels)
})

test_that("plot_significance_groups crea gráfico con facetas", {
    p_facet <- plot_significance_groups(
        data = grupos_ejemplo_dummy,
        facet_var = "campana_facet",
        title = "Test Plot con Facetas"
    )
    expect_s3_class(p_facet, "ggplot")
    expect_s3_class(p_facet$facet, "FacetWrap")
})