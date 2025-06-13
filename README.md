# StartStats

StartStats es un paquete de R pensado para facilitar el análisis estadístico en agronomía. Incluye funciones que simplifican la obtención de grupos de significancia, la creación de gráficas y la ejecución de análisis ANOVA o no paramétricos.

## Funciones principales
- **`generate_groups()`** &mdash; Obtiene grupos de letras para un factor después de pruebas post-hoc (Tukey, Games‑Howell, Duncan, Dunn o Dunnett).
- **`plot_significance_groups()`** &mdash; Visualiza de forma rápida los grupos de significancia en un gráfico de barras con `ggplot2`.
- **`run_analysis()`** &mdash; Ejecuta un análisis integral para una variable respuesta: ANOVA tipo III, verificación de supuestos, pruebas no paramétricas y recomendaciones de post-hoc.

## Instalación
Puedes instalar la versión en desarrollo directamente desde GitHub con `remotes` o `devtools`:

```r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("usuario/StartStats")
```

Si descargaste este repositorio de manera local, instala el paquete con:

```r
remotes::install_local("/ruta/al/directorio/StartStats")
```

## Actualización
Para actualizar a la versión más reciente vuelve a ejecutar `install_github`:

```r
remotes::install_github("usuario/StartStats", force = TRUE)
```

## Desinstalación
Para eliminar el paquete de tu biblioteca de R:

```r
remove.packages("StartStats")
```

## Próximas funcionalidades
- Soporte para diseños con medidas repetidas.
- Nuevas funciones de visualización interactiva.
- Exportación de resultados a formatos amigables (por ejemplo Excel).

## Contacto
Para dudas o sugerencias puedes escribir a [alexprieto1997@gmail.com](mailto:alexprieto1997@gmail.com).
