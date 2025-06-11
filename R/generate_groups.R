#' @title Análisis de Comparaciones Múltiples Post-Hoc para Agrupamiento
#'
#' @description
#' `generate_groups` realiza pruebas post-hoc de comparaciones múltiples
#' (Tukey HSD, Games-Howell, Duncan, Dunn o Dunnett) para agrupar los niveles
#' de un factor (`formula_interaccion`) basado en una variable respuesta.
#' La función permite obtener grupos de letras para una variable transformada
#' y presentar las medias (o medianas) de la variable original, lo cual es
#' común en análisis donde se realizan transformaciones de datos para cumplir
#' supuestos estadísticos.
#'
#' @param data Un dataframe que contiene las variables necesarias para el análisis.
#' @param var_original Un string, el nombre de la columna en `data` que representa
#'   la variable respuesta original (sin transformar). Esta variable se usará
#'   para calcular las medias (o medianas) que se mostrarán en la salida final.
#'   **Obligatorio.**
#' @param var_transformada Un string, el nombre de la columna en `data` que representa
#'   la variable respuesta transformada (o la misma `var_original` si no hay
#'   transformación). Esta variable se usará para realizar las pruebas post-hoc
#'   y obtener los grupos de letras. Si es `NULL` (valor por defecto),
#'   `var_original` se utilizará para este propósito.
#' @param formula_interaccion Un string, el nombre de la columna en `data` que
#'   representa el factor cuyos niveles serán comparados y agrupados.
#'   Por defecto es "GenTrat" (Genotipo x Tratamiento, un nombre común en agronomía).
#' @param method Un string que especifica el método de prueba post-hoc a utilizar.
#'   Puede ser:
#'   \itemize{
#'     \item `"tukey"`: Tukey HSD para ANOVA (paramétrica, asume normalidad y homocedasticidad).
#'       Utiliza `agricolae::HSD.test`. Para más detalles, ver Steel et al. (1997).
#'     \item `"games_howell"`: Games-Howell para ANOVA (paramétrica, robusta ante violación de homocedasticidad).
#'       Utiliza `rstatix::games_howell_test`.
#'     \item `"duncan"`: Duncan's Multiple Range Test (paramétrica, compara todos los pares, a menudo más liberal que Tukey).
#'       Utiliza `agricolae::duncan.test`. Para más detalles, ver Steel et al. (1997).
#'     \item `"dunn"`: Dunn's Test (no paramétrica, para Kruskal-Wallis, útil para datos ordinales o no normales). La medida de centralidad en la salida será la mediana.
#'       Utiliza `rstatix::dunn_test`. Para más detalles, ver Dunn (1964).
#'     \item `"dunnett"`: Dunnett's Test (paramétrica, compara múltiples grupos contra un único grupo control). Requiere especificar `control_group`.
#'       Utiliza `DescTools::DunnettTest`.
#'   }
#'   Por defecto es "tukey".
#' @param dunn_p_adjust_method Un string, el método de ajuste de p-valores a usar
#'   con el método `"dunn"`. Opciones comunes incluyen `"bonferroni"`, `"holm"`,
#'   `"fdr"` (Benjamini-Hochberg). Por defecto es `"bonferroni"`. Ignorado para otros métodos.
#' @param control_group Un string, el nombre del nivel del factor
#'   `formula_interaccion` que se considera el grupo control. **Obligatorio si
#'   `method = "dunnett"`**. Ignorado para otros métodos.
#' @param verbose Un valor lógico (`TRUE`/`FALSE`). Si es `TRUE`, la función imprimirá
#'   mensajes de progreso y advertencias detalladas en la consola. Si es `FALSE`,
#'   operará de forma "silenciosa" (solo errores fatales detendrán la ejecución).
#'   Por defecto es `TRUE`.
#'
#' @return Un tibble con tres columnas:
#'   \describe{
#'     \item{interaccion}{Los niveles únicos del factor `formula_interaccion`.}
#'     \item{means}{Las medias de la `var_original` para cada nivel de `interaccion`.
#'       **Nota**: Si `method = "dunn"`, esta columna contendrá las medianas de la `var_original`,
#'       ya que la mediana es la medida de centralidad más apropiada para datos ordinales/no paramétricos.}
#'     \item{groups}{Las letras de agrupamiento asignadas a cada nivel de `interaccion`
#'       basadas en las comparaciones de la `var_transformada` (o `var_original` si no se transformó)
#'       según el `method` elegido. Para `method = "dunnett"`, las letras reflejan
#'       la significancia estadística solo en comparación con el `control_group`.}
#'   }
#'   En caso de que una prueba no pueda realizarse para una variable (ej. no hay suficientes grupos),
#'   la función intentará manejar el error y devolverá una tabla vacía con advertencia.
#'
#' @details
#' La función maneja automáticamente los valores `NA` filtrando las filas que contienen
#' `NA` en las variables de interés antes de realizar los análisis.
#' También "droplevels" los factores para evitar problemas con niveles vacíos.
#' Los `tryCatch` se usan para aumentar la robustez de la función, permitiendo que
#' continúe la ejecución y emita advertencias en lugar de detenerse completamente
#' si una prueba post-hoc falla por condiciones inusuales de los datos.
#'
#' **Referencias:**
#' \itemize{
#'   \item Dunn, O. J. (1964). Multiple comparisons using rank sums. *Technometrics*, 6(3), 241-252.
#'   \item Steel, R. G. D., Torrie, J. H., & Dickey, D. A. (1997). *Principles and Procedures of Statistics: A Biometrical Approach* (3rd ed.). McGraw-Hill.
#'   \item Piepho, H. P. (2004). An algorithm for a letter-based representation of all-pairwise comparisons. *Journal of Computational and Graphical Statistics*, 13(2), 456-466.
#' }
#'
#' @examples
#' # Crear datos de ejemplo
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
#' # Ejemplo 1: Tukey HSD con variable transformada
#' \dontrun{
#' grupos_tukey <- generate_groups(
#'   data = df_ejemplo,
#'   var_original = "rendimiento",
#'   var_transformada = "rendimiento_log",
#'   formula_interaccion = "genotipo",
#'   method = "tukey",
#'   verbose = TRUE
#' )
#' print(grupos_tukey)
#' }
#'
#' # Ejemplo 2: Games-Howell (misma variable para original y transformada)
#' \dontrun{
#' grupos_gh <- generate_groups(
#'   data = df_ejemplo,
#'   var_original = "rendimiento",
#'   formula_interaccion = "genotipo", # var_transformada se usa como rendimiento
#'   method = "games_howell",
#'   verbose = FALSE # Salida más limpia en consola
#' )
#' print(grupos_gh)
#' }
#'
#' # Ejemplo 3: Dunn Test para variable ordinal (sabor_score) con ajuste Bonferroni
#' \dontrun{
#' grupos_dunn_bonf <- generate_groups(
#'   data = df_ejemplo,
#'   var_original = "sabor_score",
#'   formula_interaccion = "genotipo",
#'   method = "dunn",
#'   dunn_p_adjust_method = "bonferroni", # Explicitamente Bonferroni
#'   verbose = TRUE
#' )
#' print(grupos_dunn_bonf)
#' }
#'
#' # Ejemplo 4: Duncan's Multiple Range Test
#' \dontrun{
#' grupos_duncan <- generate_groups(
#'   data = df_ejemplo,
#'   var_original = "rendimiento",
#'   formula_interaccion = "genotipo",
#'   method = "duncan",
#'   verbose = TRUE
#' )
#' print(grupos_duncan)
#' }
#'
#' # Ejemplo 5: Dunnett's Test (comparando contra el grupo "Control")
#' \dontrun{
#' grupos_dunnett <- generate_groups(
#'   data = df_ejemplo,
#'   var_original = "rendimiento",
#'   formula_interaccion = "genotipo",
#'   method = "dunnett",
#'   control_group = "Control", # Especificar el grupo control
#'   verbose = TRUE
#' )
#' print(grupos_dunnett)
#' }
#' @export
#' @importFrom dplyr %>% filter mutate count select left_join tibble ungroup arrange group_by summarise
#' @importFrom rlang .data sym
#' @importFrom agricolae HSD.test duncan.test LSD.test
#' @importFrom rstatix games_howell_test dunn_test
#' @importFrom multcompView multcompLetters
#' @importFrom DescTools DunnettTest
#' @importFrom stats as.formula aov relevel mean median tapply
generate_groups <- function(
    data,
    var_original,
    var_transformada = NULL,
    formula_interaccion = "GenTrat",
    method = c("tukey", "LSD", "games_howell", "duncan", "dunn", "dunnett"),
    dunn_p_adjust_method = "bonferroni",
    control_group = NULL,
    verbose = TRUE
) {
  # Coerción y validación de argumentos principales
  method <- match.arg(method)
  trt    <- formula_interaccion # Alias para la variable de interacción

  # --- Validaciones iniciales de argumentos ---
  if (missing(var_original) || is.null(var_original)) {
    stop("El argumento 'var_original' es obligatorio y no puede ser NULL.")
  }

  if (is.null(var_transformada)) {
    var_transformada <- var_original
    if (verbose) message("Advertencia: 'var_transformada' no especificada. Usando 'var_original' ('", var_original, "') para los cálculos de agrupamiento.")
  }

  if (!all(c(var_transformada, var_original, trt) %in% names(data))) {
    missing_vars <- setdiff(c(var_transformada, var_original, trt), names(data))
    stop(paste0("Una o más variables (", paste(missing_vars, collapse = ", "), ") no se encontraron en el dataframe proporcionado."))
  }

  if (method == "dunnett") {
    if (is.null(control_group)) {
      stop("Para el método 'dunnett', el argumento 'control_group' es obligatorio.")
    }
    if (!control_group %in% unique(data[[trt]])) {
      stop(paste0("El 'control_group' especificado ('", control_group, "') no se encontró en la variable de interacción ('", trt, "')."))
    }
  }

  data <- dplyr::ungroup(data)
  data[[trt]] <- as.factor(data[[trt]])


  # --- Helper genérico para preparar datos antes de cada test ---
  prepare_data_for_test <- function(current_data, var_name_for_test, test_name) {
    if (verbose) message(paste0("  Preparando datos para ", test_name, " en '", var_name_for_test, "'..."))

    temp_data <- current_data %>%
      dplyr::filter(!is.na(.data[[var_name_for_test]]) & !is.na(.data[[trt]])) %>%
      dplyr::mutate(!!rlang::sym(trt) := droplevels(as.factor(.data[[trt]])))

    niveles_activos <- levels(temp_data[[trt]])
    if (length(niveles_activos) < 2) {
      warning(paste0("Para la variable '", var_name_for_test, "', no hay suficientes grupos únicos para realizar ", test_name, " después de filtrar NAs. Retornando una tabla vacía para esta variable."))
      return(NULL)
    }
    
    if (!is.numeric(temp_data[[var_name_for_test]])) {
      stop(paste0("La variable '", var_name_for_test, "' no es numérica. ", test_name, " requiere una variable respuesta numérica."))
    }

    group_counts <- temp_data %>% dplyr::count(!!rlang::sym(trt))
    problem_groups <- group_counts %>% dplyr::filter(n < 2)
    if (nrow(problem_groups) > 0) {
      warning(paste0("Para la variable '", var_name_for_test, "', los siguientes grupos tienen menos de 2 observaciones válidas (después de filtrar NAs): ", 
                     paste(problem_groups[[trt]], collapse = ", "), 
                     ". Esto puede afectar la prueba ", test_name, " y los resultados de agrupamiento."))
    }

    return(temp_data)
  }

  # --- Función auxiliar para Tukey HSD ---
  get_tukey <- function(var_name_for_test) {
    temp_data <- prepare_data_for_test(data, var_name_for_test, "Tukey HSD")
    if (is.null(temp_data)) return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))

    # Reordenar niveles del factor trt por la media de var_name_for_test (descendente)
    means_order <- temp_data %>%
      dplyr::group_by(!!rlang::sym(trt)) %>%
      dplyr::summarise(mean_val = mean(.data[[var_name_for_test]], na.rm = TRUE)) %>% # <-- mean sin stats::
      dplyr::arrange(dplyr::desc(mean_val))
    temp_data[[trt]] <- factor(temp_data[[trt]], levels = means_order[[trt]])


    # Modelo de ANOVA
    model_aov <- stats::aov(stats::as.formula(paste(var_name_for_test, "~", trt)), data = temp_data)

    # Aplicar HSD.test con tryCatch para manejar posibles errores
    tuk_result <- tryCatch({
      agricolae::HSD.test(
        model_aov,
        trt = trt,
        console = FALSE
      )$groups
    }, error = function(e) {
      warning(paste0("Error en agricolae::HSD.test para '", var_name_for_test, "': ", e$message, ". Retornando tabla vacía para los grupos."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    })

    if ("tbl_df" %in% class(tuk_result)) { return(tuk_result) }

    df <- as.data.frame(tuk_result, stringsAsFactors = FALSE)
    df$interaccion <- rownames(df)

    df_tibble <- tibble::as_tibble(df)

    df_tibble %>%
      dplyr::select(
        interaccion = interaccion,
        means = !!rlang::sym(var_name_for_test),
        groups = groups
      ) %>%
      dplyr::mutate(interaccion = as.character(interaccion))
  }

  # --- Función auxiliar para Multicomparación LSD ---
  get_LSD <- function(var_name_for_test) {
    temp_data <- prepare_data_for_test(data, var_name_for_test, "Tukey HSD")
    if (is.null(temp_data)) return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))

    # Reordenar niveles del factor trt por la media de var_name_for_test (descendente)
    means_order <- temp_data %>%
      dplyr::group_by(!!rlang::sym(trt)) %>%
      dplyr::summarise(mean_val = mean(.data[[var_name_for_test]], na.rm = TRUE)) %>% # <-- mean sin stats::
      dplyr::arrange(dplyr::desc(mean_val))
    temp_data[[trt]] <- factor(temp_data[[trt]], levels = means_order[[trt]])


    # Modelo de ANOVA
    model_aov <- stats::aov(stats::as.formula(paste(var_name_for_test, "~", trt)), data = temp_data)

    # Aplicar HSD.test con tryCatch para manejar posibles errores
    tuk_result <- tryCatch({
      agricolae::LSD.test(
        model_aov,
        trt = trt,
        console = FALSE
      )$groups
    }, error = function(e) {
      warning(paste0("Error en agricolae::HSD.test para '", var_name_for_test, "': ", e$message, ". Retornando tabla vacía para los grupos."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    })

    if ("tbl_df" %in% class(tuk_result)) { return(tuk_result) }

    df <- as.data.frame(tuk_result, stringsAsFactors = FALSE)
    df$interaccion <- rownames(df)

    df_tibble <- tibble::as_tibble(df)

    df_tibble %>%
      dplyr::select(
        interaccion = interaccion,
        means = !!rlang::sym(var_name_for_test),
        groups = groups
      ) %>%
      dplyr::mutate(interaccion = as.character(interaccion))
  }

  # --- Función auxiliar para Games–Howell ---
  get_games_howell <- function(var_name_for_test) {
    temp_data <- prepare_data_for_test(data, var_name_for_test, "Games-Howell")
    if (is.null(temp_data)) return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))

    # Reordenar niveles del factor trt por la media de var_name_for_test (descendente)
    means_order <- temp_data %>%
      dplyr::group_by(!!rlang::sym(trt)) %>%
      dplyr::summarise(mean_val = mean(.data[[var_name_for_test]], na.rm = TRUE)) %>% # <-- mean sin stats::
      dplyr::arrange(dplyr::desc(mean_val))
    temp_data[[trt]] <- factor(temp_data[[trt]], levels = means_order[[trt]])

    niveles_activos <- levels(temp_data[[trt]]) # Obtener los niveles activos después del reordenamiento


    gh_raw <- tryCatch({
      rstatix::games_howell_test(temp_data, stats::as.formula(paste(var_name_for_test, "~", trt)))
    }, error = function(e) {
      warning(paste0("Error en rstatix::games_howell_test para '", var_name_for_test, "': ", e$message, ". Retornando tabla vacía para los grupos."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    })

    if (nrow(gh_raw) == 0 || !all(c("group1", "group2", "p.adj") %in% colnames(gh_raw))) {
      warning(paste0("Resultado de rstatix::games_howell_test para '", var_name_for_test, "' no tiene las columnas esperadas o está vacío. Retornando tabla vacía para los grupos."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    }

    M <- matrix(1, nrow = length(niveles_activos), ncol = length(niveles_activos),
                dimnames = list(niveles_activos, niveles_activos))

    for (i in seq_len(nrow(gh_raw))) {
      g1 <- as.character(gh_raw$group1[i])
      g2 <- as.character(gh_raw$group2[i])
      p <- gh_raw$p.adj[i]

      if (is.na(p)) {
        if (verbose) warning(paste0("P-valor NA encontrado para el par (", g1, ", ", g2, ") en '", var_name_for_test, "'. Saltando este par."))
        next
      }
      if (g1 %in% niveles_activos && g2 %in% niveles_activos) {
        M[g1, g2] <- p
        M[g2, g1] <- p
      } else {
        if (verbose) warning(paste0("Grupo '", g1, "' o '", g2, "' del Games-Howell test no encontrado en los niveles activos para variable '", var_name_for_test, "'. Este par no será considerado en la matriz de p-valores."))
      }
    }

    letras <- tryCatch({
      multcompView::multcompLetters(M, threshold = 0.05)$Letters # <-- ¡CORREGIDO!
    }, error = function(e) {
      warning(paste0("Error en multcompView::multcompLetters para '", var_name_for_test, "': ", e$message, ". No se pudieron asignar letras. Retornando tabla vacía para los grupos."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    })

    means_by_group <- tapply(temp_data[[var_name_for_test]], temp_data[[trt]], mean, na.rm = TRUE) # <-- mean sin stats::

    means_tibble <- tibble::tibble(
      interaccion = names(means_by_group),
      means       = unname(means_by_group)
    )

    letras_tibble <- tibble::tibble(
      interaccion = names(letras),
      groups      = unname(letras)
    ) %>%
      dplyr::mutate(interaccion = as.character(interaccion))

    if (nrow(means_tibble) > 0 && nrow(letras_tibble) > 0) {
      final_tibble <- means_tibble %>%
        dplyr::left_join(letras_tibble, by = "interaccion")
      return(final_tibble)
    } else {
      warning(paste0("No se pudieron combinar medias y letras para '", var_name_for_test, "' debido a datos vacíos. Retornando tabla vacía."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    }
  }

  # --- Función auxiliar para Dunn Test (no paramétrico) ---
  get_dunn <- function(var_name_for_test) {
    if (verbose) message(paste0("  Calculando Dunn test para '", var_name_for_test, "' con ajuste '", dunn_p_adjust_method, "' (Se reportará la mediana)..."))

    temp_data <- prepare_data_for_test(data, var_name_for_test, "Dunn Test")
    if (is.null(temp_data)) return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))

    # Reordenar niveles del factor trt por la MEDIANA de var_name_for_test (descendente)
    medians_order <- temp_data %>%
      dplyr::group_by(!!rlang::sym(trt)) %>%
      dplyr::summarise(median_val = median(.data[[var_name_for_test]], na.rm = TRUE)) %>% # <-- median sin stats::
      dplyr::arrange(dplyr::desc(median_val))
    temp_data[[trt]] <- factor(temp_data[[trt]], levels = medians_order[[trt]])

    niveles_activos <- levels(temp_data[[trt]]) # Obtener los niveles activos después del reordenamiento


    # Realizar Dunn test con tryCatch
    dunn_raw <- tryCatch({
      rstatix::dunn_test(temp_data, stats::as.formula(paste(var_name_for_test, "~", trt)),
                         p.adjust.method = dunn_p_adjust_method)
    }, error = function(e) {
      warning(paste0("Error en rstatix::dunn_test para '", var_name_for_test, "': ", e$message, ". Retornando tabla vacía para los grupos."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    })

    if (nrow(dunn_raw) == 0 || !all(c("group1", "group2", "p.adj") %in% colnames(dunn_raw))) {
      warning(paste0("Resultado de rstatix::dunn_test para '", var_name_for_test, "' no tiene las columnas esperadas o está vacío. Retornando tabla vacía para los grupos."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    }

    M <- matrix(1, nrow = length(niveles_activos), ncol = length(niveles_activos),
                dimnames = list(niveles_activos, niveles_activos))

    for (i in seq_len(nrow(dunn_raw))) {
      g1 <- as.character(dunn_raw$group1[i])
      g2 <- as.character(dunn_raw$group2[i])
      p <- dunn_raw$p.adj[i]

      if (is.na(p)) {
        if (verbose) warning(paste0("P-valor NA encontrado para el par (", g1, ", ", g2, ") en '", var_name_for_test, "'. Saltando este par."))
        next
      }
      if (g1 %in% niveles_activos && g2 %in% niveles_activos) {
        M[g1, g2] <- p
        M[g2, g1] <- p
      } else {
        if (verbose) warning(paste0("Grupo '", g1, "' o '", g2, "' del Dunn test no encontrado en los niveles activos para variable '", var_name_for_test, "'. Este par no será considerado en la matriz de p-valores."))
      }
    }

    letras <- tryCatch({
      multcompView::multcompLetters(M, threshold = 0.05)$Letters # <-- ¡CORREGIDO!
    }, error = function(e) {
      warning(paste0("Error en multcompView::multcompLetters para '", var_name_for_test, "': ", e$message, ". No se pudieron asignar letras. Retornando tabla vacía para los grupos."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    })

    medians_by_group <- tapply(temp_data[[var_name_for_test]], temp_data[[trt]], median, na.rm = TRUE) # <-- tapply y median sin stats::

    means_tibble <- tibble::tibble(
      interaccion = names(medians_by_group),
      means       = unname(medians_by_group)
    )

    letras_tibble <- tibble::tibble(
      interaccion = names(letras),
      groups      = unname(letras)
    ) %>%
      dplyr::mutate(interaccion = as.character(interaccion))

    if (nrow(means_tibble) > 0 && nrow(letras_tibble) > 0) {
      final_tibble <- means_tibble %>%
        dplyr::left_join(letras_tibble, by = "interaccion")
      return(final_tibble)
    } else {
      warning(paste0("No se pudieron combinar medianas y letras para '", var_name_for_test, "' debido a datos vacíos. Retornando tabla vacía."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    }
  }

  # --- Función auxiliar para Duncan's Multiple Range Test ---
  get_duncan <- function(var_name_for_test) {
    if (verbose) message(paste0("  Calculando Duncan's Multiple Range Test para '", var_name_for_test, "'..."))

    temp_data <- prepare_data_for_test(data, var_name_for_test, "Duncan Test")
    if (is.null(temp_data)) return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))

    # Reordenar niveles del factor trt por la media de var_name_for_test (descendente)
    means_order <- temp_data %>%
      dplyr::group_by(!!rlang::sym(trt)) %>%
      dplyr::summarise(mean_val = mean(.data[[var_name_for_test]], na.rm = TRUE)) %>% # <-- mean sin stats::
      dplyr::arrange(dplyr::desc(mean_val))
    temp_data[[trt]] <- factor(temp_data[[trt]], levels = means_order[[trt]])

    # Modelo de ANOVA
    model_aov <- stats::aov(stats::as.formula(paste(var_name_for_test, "~", trt)), data = temp_data)

    # Aplicar duncan.test con tryCatch
    duncan_result <- tryCatch({
      agricolae::duncan.test(
        model_aov,
        trt = trt,
        console = FALSE
      )$groups
    }, error = function(e) {
      warning(paste0("Error en agricolae::duncan.test para '", var_name_for_test, "': ", e$message, ". Retornando tabla vacía para los grupos."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    })

    if ("tbl_df" %in% class(duncan_result)) { return(duncan_result) }

    df <- as.data.frame(duncan_result, stringsAsFactors = FALSE)
    df$interaccion <- rownames(df)

    df_tibble <- tibble::as_tibble(df)

    df_tibble %>%
      dplyr::select(
        interaccion = interaccion,
        means = !!rlang::sym(var_name_for_test),
        groups = groups
      ) %>%
      dplyr::mutate(interaccion = as.character(interaccion))
  }

  # --- Función auxiliar para Dunnett's Test (comparación contra un control) ---
  get_dunnett <- function(var_name_for_test, control_group) {
    if (verbose) message(paste0("  Calculando Dunnett's Test para '", var_name_for_test, "' comparando contra el grupo control '", control_group, "'..."))

    temp_data <- prepare_data_for_test(data, var_name_for_test, "Dunnett Test")
    if (is.null(temp_data)) return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))

    if (!control_group %in% levels(temp_data[[trt]])) {
      stop(paste0("El grupo control '", control_group, "' no existe en los niveles activos de la variable de interacción para '", var_name_for_test, "' después de filtrar NAs."))
    }

    # Reordenar niveles del factor trt por la media de var_name_for_test (descendente)
    # Excluyendo el grupo control del ordenamiento inicial y luego reinsertándolo al principio
    means_order <- temp_data %>%
      dplyr::group_by(!!rlang::sym(trt)) %>%
      dplyr::summarise(mean_val = mean(.data[[var_name_for_test]], na.rm = TRUE)) %>% # <-- mean sin stats::
      dplyr::ungroup() %>%
      dplyr::filter(!!rlang::sym(trt) != control_group) %>%
      dplyr::arrange(dplyr::desc(mean_val))
    
    ordered_levels <- c(control_group, means_order[[trt]])
    temp_data[[trt]] <- factor(temp_data[[trt]], levels = ordered_levels)

    niveles_activos <- levels(temp_data[[trt]]) # Obtener los niveles activos después del reordenamiento


    dunnett_raw <- tryCatch({
      model_aov <- stats::aov(stats::as.formula(paste(var_name_for_test, "~", trt)), data = temp_data)

      DescTools::DunnettTest(model_aov)$p.value
    }, error = function(e) {
      warning(paste0("Error en DescTools::DunnettTest para '", var_name_for_test, "': ", e$message, ". Retornando tabla vacía para los grupos."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    })

    if (!is.matrix(dunnett_raw)) {
      warning(paste0("Resultado de DescTools::DunnettTest para '", var_name_for_test, "' no es una matriz. Retornando tabla vacía para los grupos."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    }

    M <- matrix(1, nrow = length(niveles_activos), ncol = length(niveles_activos),
                dimnames = list(niveles_activos, niveles_activos))

    for (group_name in colnames(dunnett_raw)) {
      p_val <- dunnett_raw[1, group_name]
      if (!is.na(p_val) && control_group %in% niveles_activos && group_name %in% niveles_activos) {
        M[control_group, group_name] <- p_val
        M[group_name, control_group] <- p_val
      }
    }

    letras <- tryCatch({
      multcompView::multcompLetters(M, threshold = 0.05)$Letters # <-- ¡CORREGIDO!
    }, error = function(e) {
      warning(paste0("Error en multcompView::multcompLetters para '", var_name_for_test, "': ", e$message, ". No se pudieron asignar letras. Retornando tabla vacía para los grupos."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    })

    means_by_group <- tapply(temp_data[[var_name_for_test]], temp_data[[trt]], mean, na.rm = TRUE) # <-- tapply y mean sin stats::

    means_tibble <- tibble::tibble(
      interaccion = names(means_by_group),
      means       = unname(means_by_group)
    )

    letras_tibble <- tibble::tibble(
      interaccion = names(letras),
      groups      = unname(letras)
    ) %>%
      dplyr::mutate(interaccion = as.character(interaccion))

    if (nrow(means_tibble) > 0 && nrow(letras_tibble) > 0) {
      final_tibble <- means_tibble %>%
        dplyr::left_join(letras_tibble, by = "interaccion")
      return(final_tibble)
    } else {
      warning(paste0("No se pudieron combinar medias y letras para '", var_name_for_test, "' debido a datos vacíos. Retornando tabla vacía."))
      return(tibble::tibble(interaccion = character(0), means = numeric(0), groups = character(0)))
    }
  }


  # --- Ejecución de las pruebas post-hoc ---
  if (verbose) message(paste0("Realizando análisis post-hoc con ", method, "..."))

  selected_test_helper <- switch(method,
                                 "tukey" = get_tukey,
                                 "LSD" = get_LSD,
                                 "games_howell" = get_games_howell,
                                 "duncan" = get_duncan,
                                 "dunn" = get_dunn,
                                 "dunnett" = get_dunnett)

  if (method == "dunnett") {
    grupos_tr <- selected_test_helper(var_transformada, control_group)
  } else {
    grupos_tr <- selected_test_helper(var_transformada)
  }

  if (method == "dunnett") {
    grupos_orig <- selected_test_helper(var_original, control_group)
  } else {
    grupos_orig <- selected_test_helper(var_original)
  }

  grupos_final <- grupos_orig %>%
    dplyr::select(
      interaccion,
      means = means
    ) %>%
    dplyr::left_join(
      grupos_tr %>%
        dplyr::select(interaccion, groups = groups),
      by = "interaccion"
    ) %>%
    dplyr::select(
      interaccion,
      means,
      groups
    ) %>%
    dplyr::arrange(interaccion) # Se mantiene el orden alfabético para la salida final

  if (verbose) message("Análisis de grupos post-hoc completado.")
  return(grupos_final)
}
