#' @title Visualización de Grupos de Significancia Post-Hoc
#'
#' @description
#' `plot_significance_groups` crea un gráfico de barras a partir del resultado
#' de la función `generate_groups`, mostrando las medias (o medianas) de los
#' grupos y sus letras de significancia estadística. La función está diseñada
#' para ser rápida y didáctica, abstraiendo la complejidad de `ggplot2` para el usuario.
#' Permite una fácil visualización de los resultados de pruebas post-hoc como Tukey,
#' Games-Howell, Duncan, Dunn o Dunnett.
#'
#' @param data Un tibble o data.frame. **Debe ser la salida directa de la función
#'   `generate_groups`**. Se espera que contenga las columnas `interaccion` (factor/carácter),
#'   `means` (numérica), y `groups` (carácter).
#' @param facet_var Un string opcional. El nombre de una variable en el `data`
#'   que se utilizará para crear facetas (sub-gráficos). Si es `NULL` (por defecto),
#'   no se crearán facetas. Si la columna `interaccion` ya es un factor combinado
#'   (ej. "Genotipo:Tratamiento"), este argumento puede ser útil si deseas
#'   desglosar la visualización por uno de los componentes de la interacción.
#'   **Nota:** Para usar `facet_var`, la columna `interaccion` en `data`
#'   probablemente necesitará ser el resultado de `forcats::fct_reorder` o
#'   similar, con el factor combinado. Si la `interaccion` ya es una cadena
#'   combinada (ej. "G1:T1"), `facet_var` no podrá usarse directamente para desagruparla
#'   a menos que se cree una nueva columna con esa información.
#' @param reorder_x Un valor lógico (`TRUE`/`FALSE`). Si `TRUE` (por defecto),
#'   las categorías en el eje X (`interaccion`) se reordenarán según los valores
#'   de `means`.
#' @param desc Un valor lógico (`TRUE`/`FALSE`). Si `TRUE` (por defecto),
#'   el reordenamiento se hará de mayor a menor. Si `FALSE`, de menor a mayor.
#'   Solo aplica si `reorder_x = TRUE`.
#' @param x_label Un string. Etiqueta para el eje X. Por defecto es una cadena vacía.
#' @param rotation_x_ticks Un número. Grados de rotación para las etiquetas del eje X.
#'   Útil para evitar solapamiento. Por defecto es `90`.
#' @param y_label Un string. Etiqueta para el eje Y. Por defecto es una cadena vacía.
#' @param title Un string. Título principal del gráfico. Por defecto es una cadena vacía.
#' @param text_offset Un valor numérico. Factor de desplazamiento vertical para
#'   las letras de significancia sobre las barras. Un valor de `0.05` (5%) significa
#'   que el texto estará un 5% por encima de la altura de la barra.
#' @param y_lim_percent Un valor numérico. Porcentaje adicional para el límite
#'   superior del eje Y, calculado como `max_y * (1 + y_lim_percent)`. Útil para
#'   asegurar espacio para las etiquetas y evitar que se corten. Un valor de `0.1`
#'   (10% más) es el valor por defecto.
#'
#' @return Un objeto `ggplot2` que representa el gráfico de barras con las
#'   letras de significancia. Este objeto puede ser impreso directamente o
#'   personalizado aún más usando la sintaxis de `ggplot2`.
#'
#' @details
#' La función asume que el `data` de entrada es el resultado de `generate_groups`,
#' lo que significa que ya contiene las columnas `interaccion`, `means`, y `groups`.
#' Utiliza `geom_col_pattern` para un estilo visual atractivo y `viridis` para
#' la escala de colores, lo que mejora la accesibilidad.
#'
#' @examples
#' # Asegúrate de haber ejecutado la función generate_groups primero
#' # y tener los paquetes necesarios cargados:
#' # library(dplyr); library(ggplot2); library(ggpattern); library(viridis); library(forcats)
#'
#' # Crear datos de ejemplo (los mismos usados en generate_groups)
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
#' # Ejemplo de uso:
#' # 1. Generar los grupos de significancia (usando la función generate_groups de tu librería)
#' grupos_rendimiento <- generate_groups(
#'   data = df_ejemplo,
#'   var_original = "rendimiento",
#'   formula_interaccion = "genotipo",
#'   method = "tukey",
#'   verbose = FALSE # Para no llenar la consola con mensajes de generate_groups
#' )
#'
#' # 2. Generar el gráfico con la nueva función
#' \dontrun{
#' plot_significance_groups(
#'   data = grupos_rendimiento,
#'   title = "Rendimiento por Genotipo con Grupos de Tukey",
#'   y_label = "Rendimiento Promedio (kg/ha)",
#'   x_label = "Genotipo"
#' )
#' }
#'
#' # Ejemplo con faceta (si tu interacción tuviera otra variable)
#' # Nota: Este ejemplo asume una estructura de datos más compleja,
#' # donde 'interaccion' en 'grupos_rendimiento_facet' sería de la forma "G1:T1",
#' # y 'facet_var' se extraería de alguna manera. Por la simplicidad de la
#' # salida de generate_groups (que es solo una columna 'interaccion'),
#' # el uso de facet_var para desagrupar interacciones combinadas no es directo
#' # sin un procesamiento adicional en el dataframe de entrada.
#' # Sin embargo, si 'interaccion' es simplemente 'genotipo' y quieres
#' # facetar por otra variable que está en 'df_ejemplo' pero no en 'grupos_rendimiento',
#' # necesitarías hacer un join primero.
#'
#' # Aquí un ejemplo adaptado que usa la salida de generate_groups directamente
#' # y asume que 'interaccion' ya es lo que queremos en el eje X.
#' # Si quisieras facetas, la salida de generate_groups debería estar preparada
#' # para ello (ej. generate_groups genera "genotipo:campana" y luego agregas una
#' # columna "campana_extraida" para el facet).
#'
#' # Considera este ejemplo si quieres facetas por algo YA PRESENTE EN df_ejemplo
#' # Y lo agregas a grupos_rendimiento
#' df_ejemplo_con_campana <- df_ejemplo %>%
#'   mutate(campana = rep(c("C1", "C2"), 50)) # Añadir una variable de campaña
#'
#' # Generar grupos para la interacción genotipo:campana
#' grupos_interaccion <- generate_groups(
#'   data = df_ejemplo_con_campana,
#'   var_original = "rendimiento",
#'   formula_interaccion = "~ genotipo * campana", # Interacción en la fórmula
#'   method = "tukey",
#'   verbose = FALSE
#' )
#'
#' # Para poder usar facet_var, necesitamos que la columna de faceta exista en grupos_interaccion.
#' # Esto requiere un poco de procesamiento en la salida de generate_groups
#' grupos_con_facet <- grupos_interaccion %>%
#'   tidyr::separate(interaccion, into = c("genotipo_facet", "campana_facet"), sep = ":") %>%
#'   mutate(genotipo_facet = as.factor(genotipo_facet), campana_facet = as.factor(campana_facet))
#'
#' \dontrun{
#' plot_significance_groups(
#'   data = grupos_con_facet,
#'   facet_var = "campana_facet", # Ahora podemos facetar
#'   title = "Rendimiento por Genotipo y Campaña con Grupos de Tukey",
#'   y_label = "Rendimiento Promedio (kg/ha)",
#'   x_label = "Genotipo",
#'   rotation_x_ticks = 45 # Menos rotación para facetas si los nombres son cortos
#' )
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes labs theme_minimal theme element_text element_rect element_line coord_cartesian facet_wrap
#' @importFrom ggpattern geom_col_pattern
#' @importFrom viridis scale_fill_viridis_d
#' @importFrom forcats fct_reorder
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#' @importFrom tidytext reorder_within scale_x_reordered # Necesarias para facetas con reordenamiento, aunque el ejemplo base las simplifica
#' # Nota: tidyr::separate también sería útil para el ejemplo de facetas, pero no es una dependencia directa de la función
plot_significance_groups <- function(
    data, 
    facet_var = NULL,
    reorder_x = TRUE,
    desc = TRUE,
    x_label = "",
    rotation_x_ticks = 90,
    y_label = "",
    title = "",
    text_offset = 0.05,
    y_lim_percent = 0.1
) {
    # --- Validaciones iniciales ---
    required_cols <- c("interaccion", "means", "groups")
    if (!all(required_cols %in% colnames(data))) {
        stop(paste0("El dataframe de entrada debe contener las columnas: '", 
                    paste(setdiff(required_cols, colnames(data)), collapse = "', '"), 
                    "'. Asegúrate de que sea la salida de `generate_groups`."))
    }
    if (!is.null(facet_var) && !(facet_var %in% colnames(data))) {
        stop("La variable de faceta '", facet_var, "' no se encontró en el dataframe proporcionado.")
    }
    
    # Asegurarse de que 'groups' es un factor para el orden de la leyenda
    # El orden predeterminado de los niveles de factor para las letras ('a', 'ab', 'b', etc.)
    # puede ser tricky. Viridis por defecto suele usar un buen orden.
    data$groups <- as.factor(data$groups)

    # --- Reordenamiento condicional de categorías del eje X ---
    if (reorder_x) {
        if (!is.null(facet_var)) {
        # Si hay facetas, usar reorder_within para reordenar dentro de cada faceta
        # Nota: Esto asume que 'interaccion' puede necesitar ser parseado si es un factor combinado
        # y que 'facet_var' es una de las partes de esa combinación.
        # Para la salida típica de generate_groups, 'interaccion' es el factor completo.
        # Esta lógica se mantiene por si el usuario prepara los datos para facetas de este modo.
        data <- data %>%
            dplyr::mutate(
            interaccion = tidytext::reorder_within(
                .data$interaccion, 
                .data$means, 
                .data[[facet_var]], 
                # Revertir el orden si desc=TRUE para que fct_reorder lo ordene descendente
                # O pasar el signo negativo a la media directamente para reordenar
                # tidytext::reorder_within usa el valor directo, así que negamos si es descendente
                fun = function(x) if (desc) -mean(x, na.rm = TRUE) else mean(x, na.rm = TRUE)
            )
            )
        } else {
        # Para ordenamiento global: Usamos fct_reorder
        data <- data %>%
            dplyr::mutate(
            interaccion = forcats::fct_reorder(
                .data$interaccion, 
                .data$means, 
                .desc = desc # Controla la dirección del reordenamiento
            )
            )
        }
    }
    
    # --- Cálculo de límites dinámicos para el eje Y ---
    max_y <- max(data$means, na.rm = TRUE)
    # Asegurar que el límite inferior sea 0 o el valor mínimo de las medias si hay negativos
    min_y_val <- min(0, min(data$means, na.rm = TRUE))
    upper_y <- max_y * (1 + y_lim_percent)
    if (max_y < 0) { # Si todas las medias son negativas, ajustar el límite superior
        upper_y <- max_y * (1 - y_lim_percent) # Un poco más abajo del mínimo (más negativo)
        min_y_val <- min(data$means, na.rm = TRUE) * (1 + y_lim_percent) # Un poco más abajo (más negativo)
    }
    
    # Posición del texto para las etiquetas de grupo
    # Asegurar que el texto siempre esté por encima de la barra, incluso si las medias son negativas
    text_position <- ifelse(data$means >= 0, 
                            data$means + max_y * text_offset, 
                            data$means - abs(min_y_val) * text_offset)
    
    # --- Creación del gráfico base con ggplot2 ---
    p <- ggplot2::ggplot(data, ggplot2::aes(
        x = .data$interaccion, 
        y = .data$means, 
        fill = .data$groups
    )) +
        # Barras con patrón para un estilo visual distintivo
        ggpattern::geom_col_pattern(
        pattern = "stripe",
        pattern_angle = 45,
        pattern_density = 0.05,
        pattern_spacing = 0.02,
        pattern_key_scale_factor = 0.6,
        color = "black", # Borde de las barras
        width = 0.7      # Ancho de las barras
        ) +
        # Etiquetas de las letras de significancia
        ggplot2::geom_text(
        # 'y' se calcula dinámicamente para que el texto aparezca por encima de la barra
        ggplot2::aes(y = ifelse(.data$means >= 0, .data$means + max_y * text_offset, .data$means - abs(min_y_val) * text_offset), 
                    label = .data$groups),
        vjust = ifelse(data$means >= 0, 0, 1), # Ajuste vertical: 0 para barras positivas (arriba), 1 para negativas (abajo)
        size = 3, 
        angle = 0, 
        color = "black"
        ) +
        # Escala de colores para el relleno de las barras, usando Viridis para contraste y accesibilidad
        viridis::scale_fill_viridis_d(name = "Grupos") +
        # Etiquetas y título del gráfico
        ggplot2::labs(
        title = title,
        x = x_label,
        y = y_label
        ) +
        # Tema minimalista para un aspecto limpio
        ggplot2::theme_minimal() +
        # Personalización del tema para mejorar la presentación
        ggplot2::theme(
        axis.text.x = ggplot2::element_text(
            angle = rotation_x_ticks, # Rotación de las etiquetas del eje X
            hjust = 0.5, # Ajuste horizontal del texto
            vjust = 0.5  # Ajuste vertical del texto
        ),
        axis.title.x = ggplot2::element_blank(), # Ocultar título del eje X si x_label es vacío
        panel.border = ggplot2::element_rect(
            color = "black", 
            fill = NA, 
            linewidth = 1
        ), # Borde alrededor del panel principal
        panel.grid.major = ggplot2::element_line(
            color = "#E0E0E0", 
            linetype = "solid", 
            linewidth = 0.3
        ), # Líneas de la grilla principal
        legend.position = "right" # Posición de la leyenda
        ) +
        # Ajuste dinámico del rango del eje Y para incluir espacio para las etiquetas
        ggplot2::coord_cartesian(ylim = c(min_y_val, upper_y))
    
    # --- Configuración de facetas (si se especifica facet_var) ---
    if (!is.null(facet_var)) {
        p <- p + ggplot2::facet_wrap(
        as.formula(paste("~", facet_var)), # Crear facetas dinámicamente
        scales = "free_x" # Permitir que cada faceta tenga su propio rango en el eje X
        ) +
        # Restablecer etiquetas originales después de reorder_within()
        # Esto es crucial si reorder_within se usó y genera etiquetas como "valor___facet"
        tidytext::scale_x_reordered()
    }
    
    return(p)
}