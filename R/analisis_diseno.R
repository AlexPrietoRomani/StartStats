# StartStats\R\analisis_diseno.R

#' @title Análisis Preliminar de la Estructura de un Diseño Experimental
#'
#' @description
#' Evalúa la estructura de un diseño experimental basado en un conjunto de datos
#' y los factores especificados. Proporciona un resumen del balance, la
#' cantidad de niveles por factor, y ofrece interpretaciones y recomendaciones
#' según si el diseño es balanceado o desbalanceado.
#'
#' @param data Un data.frame. El conjunto de datos que contiene los factores y,
#'   potencialmente, las unidades experimentales.
#' @param factores Un vector de strings. Los nombres de las columnas en `data`
#'   que representan los factores del diseño.
#' @param verbose Booleano. Si es `TRUE` (por defecto), imprime un resumen del
#'   análisis en la consola. Si es `FALSE`, la función opera silenciosamente
#'   y solo devuelve los resultados.
#'
#' @details
#' La función realiza los siguientes pasos:
#' 1.  **Validación de Entradas:** Verifica que los factores especificados existan en el `data.frame`.
#' 2.  **Tabla de Frecuencias:** Construye una tabla de contingencia con el número de observaciones
#'     (réplicas) para cada combinación única de niveles de los factores, incluyendo celdas vacías.
#' 3.  **Estadísticas de Balance:** Calcula el número mínimo, máximo y mediano de observaciones
#'     por celda experimental, el rango y un porcentaje de desbalance relativo.
#'     Detecta si existen celdas con cero observaciones.
#' 4.  **Diagnóstico del Diseño:** Determina si el diseño es balanceado (todas las celdas
#'     tienen el mismo número de observaciones y ninguna está vacía) o desbalanceado.
#' 5.  **Descripción de Factores:** Lista cada factor con su respectivo número de niveles.
#' 6.  **Interpretación y Recomendaciones:** Basado en el balance del diseño, ofrece
#'     implicaciones para el análisis (ej. elección del tipo de ANOVA, manejo de
#'     confounding) y sugerencias para pruebas post-hoc.
#'
#' @return Una lista invisible que contiene:
#'   \describe{
#'     \item{`tabla_frecuencias`}{Un data.frame con el conteo de observaciones por
#'       combinación de niveles de los factores.}
#'     \item{`estadisticas_balance`}{Una lista con: `n_min`, `n_max`, `n_mediana`,
#'       `rango`, `pct_desbalance_relativo`, `celdas_vacias` (booleano),
#'       `es_balanceado` (booleano).}
#'     \item{`descripcion_factores`}{Un data.frame con los factores y su número de niveles.}
#'     \item{`diagnostico_general_txt`}{Un string formateado describiendo si el diseño es balanceado o no
#'       y sus características principales.}
#'     \item{`descripcion_factores_txt`}{Un string formateado con la descripción de los factores.}
#'     \item{`implicaciones_recomendaciones_txt`}{Un string formateado con las implicaciones y recomendaciones
#'       detalladas según el balance del diseño.}
#'     \item{`mensajes_consola_txt`}{ (Solo si `verbose = TRUE`) Un string concatenado con todo lo que se habría impreso en consola.}
#'   }
#'   Si `verbose = TRUE`, la función también imprime un resumen de estos hallazgos en la consola.
#'
#' @examples
#' # Crear datos de ejemplo
#' set.seed(123)
#' datos_ejemplo_bal <- data.frame(
#'   Genotipo = factor(rep(c("G1", "G2"), each = 12)),
#'   Tratamiento = factor(rep(rep(c("T1", "T2", "T3"), each = 4), 2)),
#'   Bloque = factor(rep(1:4, 6)),
#'   Respuesta = rnorm(24)
#' )
#'
#' # (Asegúrate de tener dplyr, glue, rlang y tidyr cargados)
#' # library(dplyr); library(glue); library(rlang); library(tidyr)
#'
#' # Ejecutar con salida en consola (verbose = TRUE por defecto)
#' # resultados_bal_verbose <- analisis_diseno(datos_ejemplo_bal,
#' #                                   factores = c("Genotipo", "Tratamiento", "Bloque"))
#'
#' # Ejecutar silenciosamente y acceder a los resultados
#' # resultados_bal_silent <- analisis_diseno(datos_ejemplo_bal,
#' #                                          factores = c("Genotipo", "Tratamiento", "Bloque"),
#' #                                          verbose = FALSE)
#' # print(resultados_bal_silent$estadisticas_balance)
#' # cat(resultados_bal_silent$diagnostico_general_txt) # Imprimir una parte específica
#'
#' @importFrom dplyr count n_distinct syms group_by summarise across all_of
#' @importFrom glue glue
#' @importFrom rlang !!!
#' @importFrom tidyr complete
#'
analisis_diseno <- function(data, factores, verbose = TRUE) {
  # --- 1. Validación de entrada ---
  stopifnot(is.data.frame(data))
  stopifnot(is.character(factores) && length(factores) > 0)
  if (!all(factores %in% names(data))) {
    stop(glue::glue("No todos los factores especificados ('{paste(factores[!factores %in% names(data)], collapse=', ')}') existen en el data.frame."))
  }
  stopifnot(is.logical(verbose) && length(verbose) == 1)

  df_analisis <- data

  # --- 2. Construcción de la tabla de frecuencias ---
  complete_spec_list <- lapply(factores, function(f) unique(df_analisis[[f]]))
  names(complete_spec_list) <- factores
  
  tabla_frecuencias <- df_analisis %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(factores))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(!!!complete_spec_list, fill = list(n = 0)) %>%
    as.data.frame()

  # --- 3. Estadísticas de Balance ---
  conteo_celdas <- tabla_frecuencias$n
  n_min  <- min(conteo_celdas, na.rm = TRUE)
  n_max  <- max(conteo_celdas, na.rm = TRUE)
  n_mediana  <- median(conteo_celdas, na.rm = TRUE)
  rango_conteo  <- n_max - n_min
  
  pct_desbalance_relativo <- if (n_mediana > 0) {
    round(100 * rango_conteo / n_mediana, 1)
  } else {
    NA_real_
  }
  
  celdas_vacias <- any(conteo_celdas == 0)
  es_balanceado <- (n_min == n_max) && (n_min > 0)

  estadisticas_balance_list <- list(
    n_min = n_min,
    n_max = n_max,
    n_mediana = n_mediana,
    rango = rango_conteo,
    pct_desbalance_relativo = pct_desbalance_relativo,
    celdas_vacias = celdas_vacias,
    es_balanceado = es_balanceado
  )

  # --- 4. Preparación del reporte textual (para retorno y consola) ---
  
  # -- 4.1 Diagnóstico General --
  diagnostico_general_txt <- if (es_balanceado) {
    glue::glue("Diseño perfectamente balanceado: cada combinación de niveles de factores (celda experimental) ",
               "tiene exactamente {n_min} observación(es).\n")
  } else {
    base_msg <- glue::glue("Diseño desbalanceado: el número de observaciones por celda varía entre {n_min} y {n_max} ",
                 "(rango = {rango_conteo}). ")
    if (!is.na(pct_desbalance_relativo)) {
      base_msg <- glue::glue("{base_msg}Esto representa aproximadamente un {pct_desbalance_relativo}% de variación relativa respecto a la mediana de observaciones por celda ({n_mediana}).\n")
    } else {
      base_msg <- glue::glue("{base_msg}\n")
    }
    if (celdas_vacias) {
      base_msg <- glue::glue("{base_msg}* Atención: Se detectaron celdas con 0 observaciones (celdas vacías).\n")
    }
    base_msg
  }

  # -- 4.2 Descripción de Factores --
  descripcion_factores_list_internal <- list()
  desc_factores_vec_txt <- sapply(factores, function(f) {
    n_niveles <- dplyr::n_distinct(df_analisis[[f]])
    descripcion_factores_list_internal[[f]] <<- n_niveles
    glue::glue("- Factor '{f}': {n_niveles} nivel(es).")
  })
  descripcion_factores_df_ret <- data.frame(
      Factor = names(descripcion_factores_list_internal),
      Niveles = unlist(descripcion_factores_list_internal),
      row.names = NULL
  )
  descripcion_factores_txt <- paste("Estructura de los factores analizados:\n", paste(desc_factores_vec_txt, collapse = "\n"), "\n")

  # -- 4.3 Implicaciones y Recomendaciones --
  implicaciones_recomendaciones_txt <- if (es_balanceado) {
    paste(
      "Implicaciones de un diseño factorial balanceado:",
      "  • Ortogonalidad: Los efectos de los factores (principales e interacciones) son independientes, permitiendo una partición clara de la varianza.",
      "  • Tipos de ANOVA: Los resultados de ANOVA Tipo I, II y III suelen coincidir, simplificando la elección del método.",
      "  • Potencia Estadística: Generalmente se maximiza la potencia para detectar efectos.",
      "\nRecomendaciones para el análisis:",
      "  • ANOVA: Un ANOVA estándar es apropiado. Considerar el modelo completo (incluyendo interacciones relevantes).",
      "  • Post-hoc: Pruebas como Tukey HSD son adecuadas para comparaciones múltiples, controlando la tasa de error familiar.",
      "  • Supuestos: Aunque el diseño sea balanceado, siempre verificar los supuestos del ANOVA (normalidad, homocedasticidad de residuos). Si no se cumplen, considerar transformaciones o métodos robustos.",
      sep = "\n"
    )
  } else {
    rec_base <- paste(
      "Implicaciones de un diseño factorial desbalanceado:",
      "  • No Ortogonalidad (Confounding): Los efectos de los factores pueden estar correlacionados (confundidos), lo que dificulta la atribución de la varianza a un factor específico.",
      "  • Tipos de ANOVA: Es crucial elegir el tipo de Suma de Cuadrados (SS) adecuado. Tipo III SS es generalmente recomendado para estimar efectos ajustados por otros factores, asumiendo un modelo completo. Tipo II SS puede ser útil si no hay interacciones significativas.",
      "  • Celdas Vacías: Si existen celdas con cero observaciones, algunas interacciones no podrán ser estimadas o las estimaciones pueden ser inestables.",
      "\nRecomendaciones para el análisis:",
      "  • Elección de ANOVA: Priorizar ANOVA con Sumas de Cuadrados Tipo III si se sospechan interacciones o si se desea evaluar cada efecto en presencia de los demás. Si se sabe que no hay interacciones, Tipo II podría ser más potente.",
      "  • Modelos Mixtos: Si el desbalance se debe a datos faltantes aleatorios o si hay estructura de anidamiento/bloques, los modelos mixtos (ej. `lme4::lmer`) pueden ser más apropiados y robustos.",
      "  • Celdas Vacías/Bajas: Investigar la causa. Si son estructurales (combinaciones imposibles), el modelo debe reflejarlo. Si son accidentales, considerar si afectan la interpretabilidad de ciertas interacciones.",
      "  • Post-hoc: Pruebas como Tukey HSD pueden no ser exactas. Considerar comparaciones basadas en mínimos cuadrados medios (LS-means o emmeans) del paquete `emmeans`, que manejan bien el desbalance.",
      "  • Robustez: Si además hay heterocedasticidad, métodos como Games-Howell o ANOVA robustos (ej. `WRS2::t2way`) pueden ser necesarios.",
      sep = "\n"
    )
    if (celdas_vacias) {
        rec_base <- paste(rec_base, 
                          "\n  • ¡Atención con Celdas Vacías!: Esto puede impedir la estimación de ciertas interacciones. El modelo factorial completo podría no ser estimable. Considere simplificar el modelo o investigar por qué estas celdas están vacías.",
                          sep="\n")
    }
    rec_base
  }
  
  # --- 5. Construcción del Mensaje para Consola
  mensajes_consola_txt_list <- NULL
  
  if (verbose) {
    # Usamos capture.output para capturar lo que print() enviaría a consola
    # y así poder retornarlo también si es necesario.
    
    header_consola <- "--- Análisis Preliminar del Diseño Experimental ---\n\n"
    diag_gen_consola <- paste0("1. Diagnóstico General del Diseño:\n", diagnostico_general_txt, "\n")
    desc_fact_consola <- paste0("2. Descripción de Factores:\n", descripcion_factores_txt, "\n")
    
    # Para la tabla de frecuencias, la capturamos
    tabla_frec_consola_capture <- capture.output(print(tabla_frecuencias))
    tabla_frec_consola <- paste0("3. Tabla de Frecuencias (Observaciones por Combinación de Factores):\n",
                                 paste(tabla_frec_consola_capture, collapse = "\n"), "\n\n")
                                 
    impl_recom_consola <- paste0("4. Implicaciones y Recomendaciones para el Análisis Estadístico:\n",
                                 implicaciones_recomendaciones_txt, "\n")
    footer_consola <- "\n--- Fin del Análisis Preliminar ---\n"
    
    # Imprimir en consola
    cat(header_consola)
    cat(diag_gen_consola)
    cat(desc_fact_consola)
    cat(tabla_frec_consola)
    cat(impl_recom_consola)
    cat(footer_consola)
    
    # Guardar la salida de consola completa para el retorno
    mensajes_consola_txt_list <- paste0(
      header_consola, diag_gen_consola, desc_fact_consola, 
      tabla_frec_consola, impl_recom_consola, footer_consola
    )
  }

  # --- 6. Retorno de la lista de resultados ---
  resultados_completos <- list(
    tabla_frecuencias = tabla_frecuencias,
    estadisticas_balance = estadisticas_balance_list,
    descripcion_factores = descripcion_factores_df_ret,
    diagnostico_general_txt = diagnostico_general_txt,
    descripcion_factores_txt = descripcion_factores_txt,
    implicaciones_recomendaciones_txt = implicaciones_recomendaciones_txt,
    mensajes_consola_txt = mensajes_consola_txt_list
  )
  
  invisible(resultados_completos)
}