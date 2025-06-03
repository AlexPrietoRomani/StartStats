# StartStats\R\run_analysis.R

# Se establecen contrastes sum-to-zero para asegurar sumas de cuadrados tipo III
# (esencial para car::Anova en modelos desbalanceados o con interacciones).
options(contrasts = c("contr.sum", "contr.poly"))

# No es necesario cargar las librerías aquí, roxygen2 las manejará vía @importFrom y DESCRIPTION


#' @title Análisis Estadístico Integral para Datos Agronómicos
#'
#' @description
#' Realiza un análisis estadístico completo para una variable respuesta, incluyendo
#' ANOVA tipo III para variables cuantitativas o pruebas no paramétricas para ordinales.
#' Incorpora validación de supuestos (normalidad y homocedasticidad) y sugiere
#' pruebas post-hoc. Si los supuestos fallan para variables cuantitativas,
#' ejecuta un Análisis de Rangos Alineados (ART ANOVA).
#'
#' @param var_name Un string. El nombre de la columna que contiene la variable respuesta a analizar.
#' @param data Un data.frame. El conjunto de datos que contiene `var_name` y los predictores.
#' @param formula Una fórmula. La fórmula del modelo que define los factores predictores
#'   (ej: `~ Factor1 * Factor2 * Factor3`), sin incluir la variable respuesta.
#' @param response_type Un string. El tipo de variable respuesta. Puede ser `"quantitative"`
#'   (por defecto) o `"ordinal"`. Determina el flujo de análisis.
#'
#' @details
#' Esta función maneja dos tipos principales de variables respuesta:
#'
#' **Para `response_type = "quantitative"` (cuantitativa):**
#' 1.  **Ajuste del Modelo Lineal:** Se ajusta un modelo lineal (`lm()`) con contrastes
#'     sum-to-zero para la variable respuesta y los factores definidos en `formula`.
#' 2.  **ANOVA Tipo III:** Se realiza un análisis de varianza tipo III usando `car::Anova()`,
#'     adecuado para diseños desbalanceados o con interacciones.
#' 3.  **Verificación de Normalidad:**
#'     *   Para `n < 50` observaciones, se aplica la prueba de Shapiro-Wilk.
#'     *   Para `n ≥ 50` observaciones, se aplican las pruebas de Anderson-Darling y Lilliefors.
#'     Se evalúa la normalidad de los residuos del modelo lineal.
#' 4.  **Verificación de Homocedasticidad:** Se usa la prueba de Levene (`car::leveneTest()`)
#'     para evaluar la homogeneidad de varianzas entre los grupos formados por la interacción
#'     de todos los predictores.
#' 5.  **Sugerencia de Post-Hoc:** Se recomienda Tukey HSD si se cumple la homocedasticidad,
#'     o Games-Howell si se detecta heterocedasticidad.
#' 6.  **Análisis ART (Aligned Rank Transform):** Si los supuestos de normalidad Y
#'     homocedasticidad fallan, se ejecuta un ART ANOVA (`ARTool::art()` y `anova()`)
#'     como alternativa no paramétrica robusta para diseños factoriales.
#'     Se verifica la existencia de datos completos antes de ejecutar ART.
#'
#' **Para `response_type = "ordinal"` (ordinal):**
#' 1.  **Preparación de Datos:** Se seleccionan y filtran los datos, convirtiendo los predictores
#'     a factores.
#' 2.  **Pruebas de Kruskal-Wallis:** Se realiza una prueba de Kruskal-Wallis independiente
#'     para cada efecto principal y para la interacción total definida en la `formula`.
#'     Esta es una aproximación no paramétrica al ANOVA para datos ordinales. Se verifica
#'     la existencia de datos completos antes de ejecutar las pruebas.
#' 3.  **Reporte de Significancia:** Se presenta una tabla con los resultados (Chi-cuadrado,
#'     grados de libertad, p-valor) y códigos de significancia.
#' 4.  **Recomendación Post-Hoc:** Se sugiere un post-hoc adecuado como Dunn con Bonferroni
#'     si algún término resulta significativo.
#'
#' @return Una lista con los resultados de los análisis, incluyendo:
#'   \describe{
#'     \item{`type`}{El tipo de análisis realizado ("quantitative" o "ordinal").}
#'     \item{`variable`}{El nombre de la variable respuesta analizada.}
#'     \item{`quantitative_results`}{Una lista (presente si `type = "quantitative"`) que incluye:
#'       \itemize{
#'         \item `anova_table`: La tabla ANOVA tipo III del modelo lineal (`car::Anova`).
#'         \item `normality_tests`: Una lista con los resultados de las pruebas de normalidad
#'                                (Shapiro-Wilk, Anderson-Darling, Lilliefors).
#'         \item `normal_ok`: Booleano indicando si la normalidad residual es aceptada (TRUE/FALSE).
#'         \item `homoscedasticity_test`: El objeto de resultado de la prueba de Levene (`car::leveneTest`).
#'         \item `homoscedastic_ok`: Booleano indicando si la homocedasticidad es aceptada (TRUE/FALSE).
#'         \item `posthoc_recommendation`: Un string con la sugerencia de prueba post-hoc (Tukey HSD o Games-Howell).
#'         \item `art_anova_table`: (Opcional) La tabla ANOVA ART si los supuestos no se cumplen.
#'       }}
#'     \item{`ordinal_results`}{Una lista (presente si `type = "ordinal"`) que incluye:
#'       \itemize{
#'         \item `kruskal_wallis_table`: La tabla resumen de las pruebas de Kruskal-Wallis para cada término.
#'         \item `posthoc_recommendation`: Un string con la sugerencia de post-hoc para ordinales.
#'       }}
#'   }
#'   Si no se cumplen las condiciones para un análisis específico (ej. no hay datos completos para ART),
#'   los elementos correspondientes en la lista de resultados pueden contener mensajes informativos o `NULL`.
#'   La función también imprime un resumen del progreso y los resultados clave en la consola para feedback inmediato.
#'
#' @examples
#' # Crear datos de ejemplo (el mismo usado en generate_groups)
#' set.seed(123)
#' df_ejemplo <- data.frame(
#'   genotipo = rep(c("G1", "G2", "G3", "G4", "Control"), each = 20),
#'   rendimiento = c(rnorm(20, 50, 5), rnorm(20, 55, 5), rnorm(20, 45, 5), rnorm(20, 70, 5), rnorm(20, 52, 5)),
#'   rendimiento_log = c(log(rnorm(20, 50, 5)), log(rnorm(20, 55, 5)), log(rnorm(20, 45, 5)), log(rnorm(20, 70, 5)), log(rnorm(20, 52, 5))),
#'   sabor_score = sample(1:5, 100, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.2))
#' )
#' # Introducir algunos outliers y NAs para probar robustez
#' df_ejemplo$rendimiento[c(5, 25, 45, 85)] <- c(10, 100, NA, 15)
#' df_ejemplo$sabor_score[c(10, 30, 90)] <- NA
#'
#' # Ejemplo 1: Análisis para una variable cuantitativa (Rendimiento)
#' \dontrun{
#' resultados_rendimiento <- run_analysis(
#'   var_name = "rendimiento",
#'   data = df_ejemplo,
#'   formula = ~ genotipo * tratamiento, # Ejemplo de formula con interacción
#'   response_type = "quantitative"
#' )
#' print(resultados_rendimiento$quantitative_results$anova_table)
#' # Acceder a otros resultados:
#' # print(resultados_rendimiento$quantitative_results$normality_tests)
#' }
#'
#' # Ejemplo 2: Análisis para una variable ordinal (sabor_score)
#' \dontrun{
#' resultados_calidad <- run_analysis(
#'   var_name = "sabor_score",
#'   data = df_ejemplo,
#'   formula = ~ genotipo, # Kruskal-Wallis se aplica a términos individuales
#'   response_type = "ordinal"
#' )
#' print(resultados_calidad$ordinal_results$kruskal_wallis_table)
#' }
#'
#' @export
#' @importFrom car Anova leveneTest
#' @importFrom nortest ad.test lillie.test
#' @importFrom ARTool art
#' @importFrom tidyr drop_na
#' @importFrom dplyr %>% select all_of mutate across
#' @importFrom rlang .data

run_analysis <- function(
    var_name,
    data,
    formula,
    response_type = "quantitative" # Parámetro para especificar el tipo de variable respuesta
) {
  
  # Se establecen contrastes sum-to-zero para asegurar sumas de cuadrados tipo III
  old_contrasts <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(old_contrasts)) # Asegurarse de restaurar los contrastes al salir de la función


  # Inicializamos la lista para almacenar todos los resultados y luego retornarla
  results_list <- list(
    type     = response_type,
    variable = var_name
  )

  # Validamos y restringimos los valores permitidos para 'response_type'
  response_type <- match.arg(response_type,
                             choices = c("quantitative", "ordinal"))

  # Construir la fórmula completa del modelo, incluyendo la variable respuesta
  model_formula <- as.formula(paste(var_name, "~", as.character(formula)[2]))
  # Extraer los nombres de las variables predictoras (factores) de la fórmula
  predictors    <- all.vars(as.formula(formula))

  # HEADER del bloque de resultados para la variable actual
  cat("\n========================\n",
      "Variable:", var_name, "| Tipo:", response_type, "\n",
      "========================\n")

  if (response_type == "quantitative") {
    # --- Flujo de análisis para variables cuantitativas (ANOVA paramétrico y ART) ---
    quantitative_results <- list() # Sub-lista para acumular resultados cuantitativos

    # 1) Ajuste del modelo lineal y ANOVA tipo III
    # ========================================================
    fit <- stats::lm(model_formula, data = data) # Explicitly calling stats::lm

    cat("\n— ANOVA Tipo III (contrastes sum-to-zero) —\n")
    anova_table <- car::Anova(fit, type = 3, singular.ok = TRUE)
    print(anova_table) # Imprime para feedback inmediato en la consola
    quantitative_results$anova_table <- anova_table # Almacena el resultado para el retorno

    # 2) Prueba de normalidad de residuos
    # ========================================================
    residuales <- stats::resid(fit) # Explicitly calling stats::resid
    n <- length(residuales)  # Obtiene el número de residuos (tamaño muestral efectivo)
    cat("\n— Prueba de Normalidad (n =", n, ") —\n")

    # Variable flag para controlar si los residuos son normales, inicialmente asumido como verdadero
    normal_ok <- TRUE
    normality_tests <- list() # Lista para almacenar los objetos de las pruebas de normalidad
    # Lógica de selección de prueba según el tamaño muestral (n)
    if (n < 50) {
      sw <- stats::shapiro.test(residuales) # Explicitly calling stats::shapiro.test
      cat("Shapiro–Wilk p-valor:", round(sw$p.value, 5), "\n")
      if(sw$p.value > 0.05) {
        cat("  → Residuales normales (no se rechaza H₀).\n")
      } else {
        cat("  → Normalidad rechazada. Considera transformaciones adicionales o ART.\n")
        normal_ok <- FALSE # Se actualiza el flag si la normalidad es rechazada
      }
      normality_tests$shapiro_wilk <- sw
    } else {
      # Pruebas para muestras grandes (Anderson–Darling y Lilliefors)
      ad <- nortest::ad.test(residuales)
      lf <- nortest::lillie.test(residuales)

      # Actualiza el flag 'normal_ok' si al menos una prueba rechaza la normalidad
      normal_ok <- ad$p.value > 0.05 && lf$p.value > 0.05
      p_ad <- ad$p.value > 0.05
      p_lf <- lf$p.value > 0.05

      cat("Anderson–Darling p-valor:", round(ad$p.value, 5), "\n")
      cat("Lilliefors p-valor:      ", round(lf$p.value, 5), "\n")

      if (p_ad && p_lf) {
        # Ambas pruebas no rechazan la normalidad
        cat("  → Ambas pruebas no rechazan normalidad, los residuos parecen ajustarse bien a la distribución normal.\n")
      } else if (p_ad || p_lf) {
        # Al menos una prueba indica desviación
        cat("  → Al menos una prueba indica desviación de normalidad, puede haber la presencia de moderados outliers.\n")
        normal_ok <- FALSE # Se actualiza el flag
      } else {
        # Ambas pruebas rechazan la normalidad
        cat("  → Ambas pruebas indican desviación de normalidad, se justifica usar métodos robustos o no paramétricos.\n")
        normal_ok <- FALSE # Se actualiza el flag
      }
      normality_tests$anderson_darling <- ad
      normality_tests$lilliefors       <- lf
    }
    quantitative_results$normal_ok       <- normal_ok # Almacena el estado de normalidad
    quantitative_results$normality_tests <- normality_tests # Almacena los resultados de las pruebas

    # 3) Prueba de homocedasticidad (Levene)
    # ========================================================
    cat("\n— Prueba de Homocedasticidad (Levene) —\n")

    # Crear una variable de agrupamiento combinando todos los predictores para la prueba de Levene
    grouping <- with(data, do.call(interaction, c(data[predictors], drop = TRUE)))

    # Ejecutar la prueba de Levene.
    lev <- car::leveneTest(data[[var_name]] ~ grouping)
    p_lev <- lev[["Pr(>F)"]][1] # Extrae el p-valor

    # Variable flag para controlar si las varianzas son homogéneas
    homocedastic_ok <- p_lev > 0.05

    cat("Levene p-valor:", round(p_lev, 5), "\n")
    if (homocedastic_ok) {
      cat("  → Homocedasticidad aceptada. ANOVA válido.\n")
    } else {
      cat("  → Heterocedasticidad detectada. ANOVA puede ser no confiable, considera Games-Howell o ART.\n")
    }
    quantitative_results$homoscedastic_ok    <- homocedastic_ok # Almacena el estado de homocedasticidad
    quantitative_results$homoscedasticity_test <- lev # Almacena el resultado de la prueba de Levene

    # 4) Sugerencia de post-hoc
    # ========================================================
    cat("\n— Post-hoc recomendado —\n")
    posthoc_rec <- if(homocedastic_ok) { # Si la homocedasticidad es aceptada
      "Tukey HSD (homocedasticidad cumplida)."
    } else { # Si se detecta heterocedasticidad
      "Games–Howell (robusto a heterocedasticidad)."
    }
    cat(paste0("✔️ Usar ", posthoc_rec, "\n"))
    quantitative_results$posthoc_recommendation <- posthoc_rec # Almacena la recomendación

    # 5) Análisis no paramétrico ART (Aligned Rank Transform)
    # ========================================================
    # Este bloque se ejecuta solo si fallan AMBOS supuestos: normalidad Y homocedasticidad.
    if (!normal_ok && !homocedastic_ok) {
      cat("\n— Prueba no paramétrica factorial (Aligned Rank Transform) —\n")

      # La fórmula ART es la misma que la del modelo lineal
      fmla_art <- model_formula 

      # Filtrar filas con datos completos en todas las variables de la fórmula
      vars_in_formula <- all.vars(fmla_art)
      data_complete <- data %>%
        tidyr::drop_na(all_of(vars_in_formula)) %>%
        # Convertir dinámicamente las variables predictoras a factores si no lo son ya
        dplyr::mutate(dplyr::across(dplyr::all_of(predictors), factor))

      # Aviso si se eliminaron filas debido a NAs
      removed <- nrow(data) - nrow(data_complete)
      if (removed > 0) {
        cat("⚠️ Se quitaron", removed, "filas con NAs antes del análisis ART.\n")
      }
      # Verificar si quedan datos para ART.
      if (nrow(data_complete) == 0) {
          cat("❗️ No quedan datos para ejecutar ART después de eliminar NAs. Se omite ART.\n")
          quantitative_results$art_anova_table <- "ART no ejecutado: No hay datos completos después de eliminar NAs."
      } else {
          # Ajustar el modelo ART
          art_mod <- ARTool::art(fmla_art, data = data_complete)

          # Intentar ANOVA Type III con singular.ok = TRUE para el modelo ART
          cat("\n— ANOVA ART Type III (rangos alineados) —\n")
          art_anova <- tryCatch(
            # Se usa la función genérica anova(), que despachará al método anova.art
            stats::anova(art_mod, type = 3, singular.ok = TRUE), 
            error = function(e) {
              # Fallback a Type II si la prueba Type III falla por problemas de aliasing
              cat("⚠️ Type III falló por aliasing en ART, usando ANOVA Type II en su lugar.\n")
              stats::anova(art_mod, type = 2)
            }
          )
          print(art_anova) # Imprime para feedback inmediato
          quantitative_results$art_anova_table <- art_anova # Almacena el resultado ART

          cat("---\n",
              "F = estadístico sobre rangos alineados; p.value indica significación\n")
      }
    } else {
      quantitative_results$art_anova_table <- "ART no ejecutado: Supuestos de normalidad y/o homocedasticidad cumplidos."
    }
    results_list$quantitative_results <- quantitative_results # Asigna todos los resultados cuantitativos a la lista principal

  } else { # Bloque para response_type == "ordinal"
    ordinal_results <- list() # Sub-lista para acumular resultados ordinales

    # 1) Preparar datos para análisis de variables ordinales
    df_ord <- data %>%
      dplyr::select(dplyr::all_of(c(var_name, predictors))) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(predictors),
          ~ factor(.x)
        )
      )

    # Verificar si quedan datos para ejecutar Kruskal-Wallis
    if (nrow(df_ord) == 0) {
        cat("❗️ No quedan datos para ejecutar Kruskal-Wallis después de eliminar NAs. Se omite el análisis ordinal.\n")
        ordinal_results$kruskal_wallis_table <- "Análisis ordinal omitido: No hay datos completos después de eliminar NAs."
        ordinal_results$posthoc_recommendation <- "Análisis ordinal omitido por falta de datos."
    } else {
        # 2) Definir términos para las pruebas (efectos principales + su interacción)
        main_terms <- predictors
        int_term   <- paste(predictors, collapse = ":")
        all_terms  <- c(main_terms, int_term)

        # 3) Ejecutar kruskal.test para cada término y recopilar resultados
        res <- lapply(all_terms, function(term) {
          grp <- if (term %in% main_terms) {
            df_ord[[term]]
          } else {
            do.call(interaction, c(df_ord[predictors], drop = TRUE))
          }
          if (length(unique(grp)) < 2) {
            warning(sprintf("El término '%s' no tiene al menos dos niveles únicos para Kruskal-Wallis. Se omitirá.", term))
            return(data.frame(Term=term, Chi.sq=NA, Df=NA, P.value=NA, stringsAsFactors=FALSE))
          }
          kt <- tryCatch(
            stats::kruskal.test(df_ord[[var_name]] ~ grp), # Explicitly calling stats::kruskal.test
            error = function(e) {
              warning(sprintf("Kruskal–Wallis falló para '%s': %s", term, e$message))
              return(list(statistic=NA, parameter=NA, p.value=NA))
            }
          )
          data.frame(
            Term     = term,
            Chi.sq   = round(as.numeric(kt$statistic), 5),
            Df       = as.integer(kt$parameter),
            P.value  = signif(kt$p.value, 5),
            stringsAsFactors = FALSE
          )
        })
        tab <- do.call(rbind, res)
        tab <- tab[!is.na(tab$Chi.sq), ] # Eliminar filas con NAs en Chi.sq

        if(nrow(tab) == 0) { # Añadir chequeo si no quedan términos para analizar
            cat("❗️ No hay términos válidos para análisis Kruskal-Wallis después de filtrar.\n")
            ordinal_results$kruskal_wallis_table <- "No hay términos válidos para Kruskal-Wallis."
            ordinal_results$posthoc_recommendation <- "No hay términos válidos para post-hoc."
        } else {
            rownames(tab) <- tab$Term
            tab$Term <- NULL

            # 4) Asignar manualmente códigos de significancia basados en el p-valor
            tab$Signif <- as.character(
            cut(
                tab$P.value,
                breaks = c(-Inf,0.001,0.01,0.05,0.1,Inf),
                labels = c("***","**","*","."," "),
                right = TRUE
            )
            )

            # 5) Construir una matriz de caracteres para una presentación limpia
            mat_char <- cbind(
            formatC(tab$Chi.sq,  format = "f", digits = 4),
            formatC(tab$Df,      format = "d", digits = 0),
            formatC(tab$P.value, format = "g", digits = 4),
            Signif = tab$Signif
            )
            colnames(mat_char) <- c("Chi-sq", "Df", "P.value", "Signif")
            rownames(mat_char) <- rownames(tab)
            print(mat_char, quote = FALSE)
            ordinal_results$kruskal_wallis_table <- mat_char

            cat("---\n",
                "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n\n")

            # 6) Recomendación de post-hoc para análisis ordinal
            ordinal_posthoc_rec <- "si algún término es significativo, aplica un post-hoc adecuado (p. ej. Dunn con Bonferroni) solo para ese efecto significativo."
            cat("Recomendación: ", ordinal_posthoc_rec, "\n")
            ordinal_results$posthoc_recommendation <- ordinal_posthoc_rec
        }
    }
    results_list$ordinal_results <- ordinal_results
  }

  cat("-------------------------------------------------\n")

  invisible(results_list) # Retorna la lista de resultados estructurados
}