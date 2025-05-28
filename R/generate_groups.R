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
#'     \item `"games_howell"`: Games-Howell para ANOVA (paramétrica, robusta ante violación de homocedasticidad).
#'     \item `"duncan"`: Duncan's Multiple Range Test (paramétrica, compara todos los pares, a menudo más liberal que Tukey).
#'     \item `"dunn"`: Dunn's Test (no paramétrica, para Kruskal-Wallis, útil para datos ordinales o no normales). La medida de centralidad en la salida será la mediana.
#'     \item `"dunnett"`: Dunnett's Test (paramétrica, compara múltiples grupos contra un único grupo control). Requiere especificar `control_group`.
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
#'   la función intentará manejar el error y devolverá una tabla vacía o asignará el grupo 'a'
#'   a todas las interacciones, junto con una advertencia.
#'
#' @details
#' La función maneja automáticamente los valores `NA` filtrando las filas que contienen
#' `NA` en las variables de interés antes de realizar los análisis.
#' También "droplevels" los factores para evitar problemas con niveles vacíos.
#' Los `tryCatch` se usan para aumentar la robustez de la función, permitiendo que
#' continúe la ejecución y emita advertencias en lugar de detenerse completamente
#' si una prueba post-hoc falla por condiciones inusuales de los datos.
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
#' @importFrom dplyr %>% filter mutate count select left_join tibble ungroup arrange
#' @importFrom rlang .data sym
#' @importFrom agricolae HSD.test duncan.test
#' @importFrom rstatix games_howell_test dunn_test
#' @importFrom PMCMRplus multcompLetters
#' @importFrom DescTools DunnettTest
generate_groups <- function(
    data,
    var_original,
    var_transformada = NULL,
    formula_interaccion = "GenTrat",
    method = c("tukey", "games_howell", "duncan", "dunn", "dunnett"),
    dunn_p_adjust_method = "bonferroni",
    control_group = NULL,
    verbose = TRUE
) {
  # Coerción y validación de argumentos principales
  method <- match.arg(method)
  trt    <- formula_interaccion # Alias para la variable de interacción

  # --- Validaciones iniciales de argumentos ---
  # 1. Asegurar que var_original siempre tenga un valor
  if (missing(var_original) || is.null(var_original)) {
    stop("El argumento 'var_original' es obligatorio y no puede ser NULL.")
  }

  # 2. Si var_transformada no se especifica, usar var_original
  if (is.null(var_transformada)) {
    var_transformada <- var_original
    if (verbose) message("Advertencia: 'var_transformada' no especificada. Usando 'var_original' ('", var_original, "') para los cálculos de agrupamiento.")
  }

  # 3. Validar que todas las variables existen en el dataframe
  if (!all(c(var_transformada, var_original, trt) %in% names(data))) {
    missing_vars <- setdiff(c(var_transformada, var_original, trt), names(data))
    stop(paste0("Una o más variables (", paste(missing_vars, collapse = ", "), ") no se encontraron en el dataframe proporcionado."))
  }

  # 4. Validación específica para Dunnett: requiere control_group
  if (method == "dunnett") {
    if (is.null(control_group)) {
      stop("Para el método 'dunnett', el argumento 'control_group' es obligatorio.")
    }
    if (!control_group %in% unique(data[[trt]])) {
      stop(paste0("El 'control_group' especificado ('", control_group, "') no se encontró en la variable de interacción ('", trt, "')."))
    }
  }

  # Desagrupar los datos al inicio para evitar problemas con operaciones dplyr
  data <- ungroup(data)

  # Asegurarse de que la variable de interacción sea un factor
  # Es crucial para tapply y las pruebas post-hoc.
  data[[trt]] <- as.factor(data[[trt]])


  # --- Helper genérico para preparar datos antes de cada test ---
  # Esta función filtra NAs, droplevels y valida tipo numérico para la variable de test
  prepare_data_for_test <- function(current_data, var_name_for_test, test_name) {
    if (verbose) message(paste0("  Preparando datos para ", test_name, " en '", var_name_for_test, "'..."))

    # Filtrar NA de la variable de respuesta y la variable de interacción
    # y droplevels para remover niveles de factor sin observaciones después del filtrado.
    temp_data <- current_data %>%
      filter(!is.na(.data[[var_name_for_test]]) & !is.na(.data[[trt]])) %>%
      mutate(!!sym(trt) := droplevels(as.factor(.data[[trt]])))

    # Verificar si hay suficientes grupos únicos para realizar la prueba
    niveles_activos <- levels(temp_data[[trt]])
    if (length(niveles_activos) < 2) {
      warning(paste0("Para la variable '", var_name_for_test, "', no hay suficientes grupos únicos para realizar ", test_name, " después de filtrar NAs. Retornando una tabla vacía para esta variable."))
      return(NULL) # Retorna NULL si no hay datos suficientes
    }

    # Asegurarse de que la variable de respuesta sea numérica para la mayoría de las pruebas
    if (!is.numeric(temp_data[[var_name_for_test]])) {
      stop(paste0("La variable '", var_name_for_test, "' no es numérica. ", test_name, " requiere una variable respuesta numérica."))
    }

    # Advertencia si hay grupos con muy pocas observaciones (común en Games-Howell, Duncan, Dunn)
    group_counts <- temp_data %>% count(!!sym(trt))
    problem_groups <- group_counts %>% filter(n < 2)
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
    if (is.null(temp_data)) return(tibble(interaccion = character(0), means = numeric(0), groups = character(0)))

    # Modelo de ANOVA
    model_aov <- aov(as.formula(paste(var_name_for_test, "~", trt)), data = temp_data)

    # Aplicar HSD.test con tryCatch para manejar posibles errores
    tuk_result <- tryCatch({
      agricolae::HSD.test( # Explicitly calling agricolae::HSD.test
        model_aov,
        trt = trt,
        console = FALSE
      )$groups
    }, error = function(e) {
      warning(paste0("Error en agricolae::HSD.test para '", var_name_for_test, "': ", e$message, ". Asignando grupo 'a' por defecto."))
      means_by_group <- tapply(temp_data[[var_name_for_test]], temp_data[[trt]], mean, na.rm = TRUE)
      return(tibble(
        interaccion = names(means_by_group),
        means       = unname(means_by_group),
        groups      = rep("a", length(means_by_group))
      ))
    })

    # Si el resultado fue el fallback (un tibble ya creado), retornarlo directamente
    if ("tbl_df" %in% class(tuk_result)) { return(tuk_result) }

    # Procesar el resultado de HSD.test (que es un data.frame base)
    # y convertirlo explícitamente a tibble para consistencia.
    df <- as.data.frame(tuk_result, stringsAsFactors = FALSE)
    df$interaccion <- rownames(df)

    # Convertir a tibble ANTES de los pipes dplyr
    df_tibble <- tibble::as_tibble(df)

    df_tibble %>%
      select(
        interaccion = interaccion,
        means = !!sym(var_name_for_test),
        groups = groups
      ) %>%
      mutate(interaccion = as.character(interaccion))
  }

  # --- Función auxiliar para Games–Howell ---
  get_games_howell <- function(var_name_for_test) {
    temp_data <- prepare_data_for_test(data, var_name_for_test, "Games-Howell")
    if (is.null(temp_data)) return(tibble(interaccion = character(0), means = numeric(0), groups = character(0)))

    niveles_activos <- levels(temp_data[[trt]])

    gh_raw <- tryCatch({
      rstatix::games_howell_test(temp_data, as.formula(paste(var_name_for_test, "~", trt)))
    }, error = function(e) {
      warning(paste0("Error en rstatix::games_howell_test para '", var_name_for_test, "': ", e$message, ". No se pudieron calcular los p-valores. Asignando grupo 'a' por defecto."))
      # Fallback: Si la prueba falla, retornar un tibble con medias y grupo 'a'
      means_by_group <- tapply(temp_data[[var_name_for_test]], temp_data[[trt]], mean, na.rm = TRUE)
      return(tibble(
        interaccion = names(means_by_group),
        means       = unname(means_by_group),
        groups      = rep("a", length(means_by_group))
      ))
    })

    if (!all(c("group1", "group2", "p.adj") %in% colnames(gh_raw))) { return(gh_raw) }

    # Inicializar la matriz M con 1s (no significativo) para todos los pares
    M <- matrix(1, nrow = length(niveles_activos), ncol = length(niveles_activos),
                dimnames = list(niveles_activos, niveles_activos))

    # Poblar la matriz M con los p-valores ajustados obtenidos del test
    for (i in seq_len(nrow(gh_raw))) {
      g1 <- as.character(gh_raw$group1[i])
      g2 <- as.character(gh_raw$group2[i])
      p <- gh_raw$p.adj[i]

      # Asegurar que el p-valor no sea NA y que los grupos existan antes de asignar
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

    # Calcular las letras de agrupamiento usando multcompLetters
    letras <- tryCatch({
      PMCMRplus::multcompLetters(M, threshold = 0.05)$Letters
    }, error = function(e) {
      warning(paste0("Error en PMCMRplus::multcompLetters para '", var_name_for_test, "': ", e$message, ". Posiblemente la matriz de p-valores es inválida o singular. Asignando grupo 'a' por defecto."))
      setNames(rep("a", length(niveles_activos)), niveles_activos)
    })

    # Calcular las medias por grupo
    means_by_group <- tapply(temp_data[[var_name_for_test]], temp_data[[trt]], mean, na.rm = TRUE)

    # Crear el tibble final, alineando por los nombres de interacción
    tibble(
      interaccion = names(letras),
      means       = unname(means_by_group[names(letras)]),
      groups      = unname(letras)
    ) %>%
      mutate(interaccion = as.character(interaccion))
  }

  # --- Función auxiliar para Dunn Test (no paramétrico) ---
  get_dunn <- function(var_name_for_test) {
    if (verbose) message(paste0("  Calculando Dunn test para '", var_name_for_test, "' con ajuste '", dunn_p_adjust_method, "' (Se reportará la mediana)..."))

    temp_data <- prepare_data_for_test(data, var_name_for_test, "Dunn Test")
    if (is.null(temp_data)) return(tibble(interaccion = character(0), means = numeric(0), groups = character(0)))

    niveles_activos <- levels(temp_data[[trt]])

    # Realizar Dunn test con tryCatch
    dunn_raw <- tryCatch({
      rstatix::dunn_test(temp_data, as.formula(paste(var_name_for_test, "~", trt)),
                         p.adjust.method = dunn_p_adjust_method)
    }, error = function(e) {
      warning(paste0("Error en rstatix::dunn_test para '", var_name_for_test, "': ", e$message, ". No se pudieron calcular los p-valores. Asignando grupo 'a' por defecto."))
      # Fallback: Si la prueba falla, retornar un tibble con medianas y grupo 'a'
      medians_by_group <- tapply(temp_data[[var_name_for_test]], temp_data[[trt]], median, na.rm = TRUE)
      return(tibble(
        interaccion = names(medians_by_group),
        means       = unname(medians_by_group),
        groups      = rep("a", length(medians_by_group))
      ))
    })

    # Si dunn_raw no contiene las columnas esperadas (fallback activado), retornar el tibble de fallback.
    if (!all(c("group1", "group2", "p.adj") %in% colnames(dunn_raw))) { return(dunn_raw) }

    # Inicializar la matriz M con 1s (no significativo) para todos los pares
    M <- matrix(1, nrow = length(niveles_activos), ncol = length(niveles_activos),
                dimnames = list(niveles_activos, niveles_activos))

    # Poblar la matriz M con los p-valores ajustados
    for (i in seq_len(nrow(dunn_raw))) {
      g1 <- as.character(dunn_raw$group1[i])
      g2 <- as.character(dunn_raw$group2[i])
      p <- dunn_raw$p.adj[i]

      # Asegurar que el p-valor no sea NA y que los grupos existan
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

    # Calcular las letras de agrupamiento
    letras <- tryCatch({
      PMCMRplus::multcompLetters(M, threshold = 0.05)$Letters
    }, error = function(e) {
      warning(paste0("Error en PMCMRplus::multcompLetters para '", var_name_for_test, "': ", e$message, ". Posiblemente la matriz de p-valores es inválida o singular. Asignando grupo 'a' por defecto."))
      setNames(rep("a", length(niveles_activos)), niveles_activos)
    })

    # Calcular las MEDIANAS por grupo
    medians_by_group <- tapply(temp_data[[var_name_for_test]], temp_data[[trt]], median, na.rm = TRUE)

    # Crear el tibble final
    tibble(
      interaccion = names(letras),
      means       = unname(medians_by_group[names(letras)]),
      groups      = unname(letras)
    ) %>%
      mutate(interaccion = as.character(interaccion))
  }

  # --- Función auxiliar para Duncan's Multiple Range Test ---
  get_duncan <- function(var_name_for_test) {
    if (verbose) message(paste0("  Calculando Duncan's Multiple Range Test para '", var_name_for_test, "'..."))

    temp_data <- prepare_data_for_test(data, var_name_for_test, "Duncan Test")
    if (is.null(temp_data)) return(tibble(interaccion = character(0), means = numeric(0), groups = character(0)))

    # Modelo de ANOVA
    model_aov <- aov(as.formula(paste(var_name_for_test, "~", trt)), data = temp_data)

    # Aplicar duncan.test con tryCatch
    duncan_result <- tryCatch({
      agricolae::duncan.test(
        model_aov,
        trt = trt,
        console = FALSE
      )$groups
    }, error = function(e) {
      warning(paste0("Error en agricolae::duncan.test para '", var_name_for_test, "': ", e$message, ". Asignando grupo 'a' por defecto."))
      means_by_group <- tapply(temp_data[[var_name_for_test]], temp_data[[trt]], mean, na.rm = TRUE)
      return(tibble(
        interaccion = names(means_by_group),
        means       = unname(means_by_group),
        groups      = rep("a", length(means_by_group))
      ))
    })

    # Si el resultado fue el fallback (un tibble ya creado), retornarlo directamente
    if ("tbl_df" %in% class(duncan_result)) { return(duncan_result) }

    df <- as.data.frame(duncan_result, stringsAsFactors = FALSE)
    df$interaccion <- rownames(df)

    # Convertir a tibble ANTES de los pipes dplyr
    df_tibble <- tibble::as_tibble(df)

    df_tibble %>%
      select(
        interaccion = interaccion,
        means = !!sym(var_name_for_test),
        groups = groups
      ) %>%
      mutate(interaccion = as.character(interaccion))
  }

  # --- Función auxiliar para Dunnett's Test (comparación contra un control) ---
  get_dunnett <- function(var_name_for_test, control_group) {
    if (verbose) message(paste0("  Calculando Dunnett's Test para '", var_name_for_test, "' comparando contra el grupo control '", control_group, "'..."))

    temp_data <- prepare_data_for_test(data, var_name_for_test, "Dunnett Test")
    if (is.null(temp_data)) return(tibble(interaccion = character(0), means = numeric(0), groups = character(0)))

    # Asegurarse que el control_group existe en los niveles activos
    if (!control_group %in% levels(temp_data[[trt]])) {
      stop(paste0("El grupo control '", control_group, "' no existe en los niveles activos de la variable de interacción para '", var_name_for_test, "' después de filtrar NAs."))
    }

    niveles_activos <- levels(temp_data[[trt]])

    # Realizar Dunnett test con tryCatch
    dunnett_raw <- tryCatch({
      # DescTools::DunnettTest requiere que el factor tenga el control como primer nivel
      temp_data[[trt]] <- relevel(temp_data[[trt]], ref = control_group)

      # Modelo de ANOVA
      model_aov <- aov(as.formula(paste(var_name_for_test, "~", trt)), data = temp_data)

      DescTools::DunnettTest(model_aov)$p.value
    }, error = function(e) {
      warning(paste0("Error en DescTools::DunnettTest para '", var_name_for_test, "': ", e$message, ". Asignando grupo 'a' por defecto."))
      means_by_group <- tapply(temp_data[[var_name_for_test]], temp_data[[trt]], mean, na.rm = TRUE)
      return(tibble(
        interaccion = names(means_by_group),
        means       = unname(means_by_group),
        groups      = rep("a", length(means_by_group))
      ))
    })

    # Si el resultado del tryCatch no es una matriz (fallback activado), retornar el tibble de fallback.
    if (!is.matrix(dunnett_raw)) { return(dunnett_raw) }

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
      PMCMRplus::multcompLetters(M, threshold = 0.05)$Letters
    }, error = function(e) {
      warning(paste0("Error en PMCMRplus::multcompLetters para '", var_name_for_test, "': ", e$message, ". Posiblemente la matriz de p-valores es inválida o singular. Asignando grupo 'a' por defecto."))
      setNames(rep("a", length(niveles_activos)), niveles_activos)
    })

    # Calcular las medias por grupo
    means_by_group <- tapply(temp_data[[var_name_for_test]], temp_data[[trt]], mean, na.rm = TRUE)

    # Crear el tibble final
    tibble(
      interaccion = names(letras),
      means       = unname(means_by_group[names(letras)]),
      groups      = unname(letras)
    ) %>%
      mutate(interaccion = as.character(interaccion))
  }


  # --- Ejecución de las pruebas post-hoc ---
  if (verbose) message(paste0("Realizando análisis post-hoc con ", method, "..."))

  # Seleccionar el helper de la prueba según el método
  selected_test_helper <- switch(method,
                                 "tukey" = get_tukey,
                                 "games_howell" = get_games_howell,
                                 "duncan" = get_duncan,
                                 "dunn" = get_dunn,
                                 "dunnett" = get_dunnett)

  # Ejecutar la prueba para la variable transformada (para las letras)
  # Si el método es "dunnett", pasamos control_group
  if (method == "dunnett") {
    grupos_tr <- selected_test_helper(var_transformada, control_group)
  } else {
    grupos_tr <- selected_test_helper(var_transformada)
  }

  # Ejecutar la prueba para la variable original (para las medias/medianas)
  if (method == "dunnett") {
    grupos_orig <- selected_test_helper(var_original, control_group)
  } else {
    grupos_orig <- selected_test_helper(var_original)
  }

  # --- Combinar resultados manteniendo el formato deseado ---
  # Se hace un left_join desde las medias/medianas de la original hacia los grupos de la transformada
  grupos_final <- grupos_orig %>%
    select(
      interaccion,
      means = means
    ) %>%
    left_join(
      grupos_tr %>%
        select(interaccion, groups = groups),
      by = "interaccion"
    ) %>%
    select(
      interaccion,
      means,
      groups
    ) %>%
    # Asegurar un orden consistente en la salida para facilitar la prueba y el uso
    arrange(interaccion)

  if (verbose) message("Análisis de grupos post-hoc completado.")
  return(grupos_final)
}
