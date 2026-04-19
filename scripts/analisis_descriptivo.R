# ============================================================
# Script: analisis_descriptivo.R
# Objetivo: Realizar el análisis descriptivo inicial de la tabla
#           integrada tab_total, incluyendo:
#           - estadística descriptiva básica
#           - selección de 6 municipios representativos para
#             gráficos temporales
#           - análisis bivariante exploratorio con diagramas
#             de dispersión sobre todos los datos
#           - cálculo de correlaciones lineales
# ============================================================

# --- Carga de paquetes necesarios ---
pkgs <- c("dplyr", "stringr", "tidyr", "ggplot2", "plotly", "ggpubr")
inst <- setdiff(pkgs, rownames(installed.packages()))
if (length(inst)) install.packages(inst, dependencies = TRUE,
                                   type = ifelse(.Platform$OS.type == "windows", "binary", "both"))
invisible(lapply(pkgs, library, character.only = TRUE))

# ============================================================
# 1) Cálculo de estadística descriptiva básica
# ============================================================

# Tabla de estadísticos descriptivos para el conjunto total
tab_medidas_total <- tab_total %>%
  reframe(
    Metrica = c("Media", "Mediana", "Desviación típica", "Mínimo", "Máximo", "Rango intercuartílico"),
    across(
      -c(1, 2) & where(is.numeric),
      \(x) c(
        mean(x, na.rm = TRUE),
        median(x, na.rm = TRUE),
        sd(x, na.rm = TRUE),
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        IQR(x, na.rm = TRUE)
      )
    )
  )

# Tabla de estadísticos descriptivos por año
tab_medidas_anios <- tab_total %>%
  group_by(TIME_PERIOD_CODE) %>%
  reframe(
    Metrica = c("Media", "Mediana", "Desviación típica", "Mínimo", "Máximo", "Rango intercuartílico"),
    across(
      -c(1, 2) & where(is.numeric),
      \(x) c(
        mean(x, na.rm = TRUE),
        median(x, na.rm = TRUE),
        sd(x, na.rm = TRUE),
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        IQR(x, na.rm = TRUE)
      )
    )
  )

tab_medidas_total
tab_medidas_anios

# ============================================================
# 2) Selección de 6 municipios representativos
# ============================================================

# Función que calcula la distancia absoluta a la mediana
calcular_dist_mediana <- function(x) {
  mediana <- median(x, na.rm = TRUE)
  abs(x - mediana)
}

# Para seleccionar municipios representativos, se promedian las variables
# numéricas por municipio a lo largo del periodo completo 2019-2023.
tab_mostrar <- tab_total %>%
  group_by(TERRITORIO_CODE) %>%
  summarise(
    TIME_PERIOD_CODE = paste0(min(TIME_PERIOD_CODE), "-", max(TIME_PERIOD_CODE)),
    across(where(is.numeric) & !matches("^TIME_PERIOD_CODE$"), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  ungroup()

# Selección de municipios:
# - 2 con valores máximos de VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL
# - 2 con valores mínimos
# - 2 más cercanos a la mediana
var_referencia <- "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL"

m_max <- tab_mostrar %>%
  slice_max(order_by = .data[[var_referencia]], n = 2)

m_min <- tab_mostrar %>%
  filter(!TERRITORIO_CODE %in% m_max$TERRITORIO_CODE) %>%
  slice_min(order_by = .data[[var_referencia]], n = 2)

m_med <- tab_mostrar %>%
  filter(!TERRITORIO_CODE %in% c(m_max$TERRITORIO_CODE, m_min$TERRITORIO_CODE)) %>%
  mutate(distancia = calcular_dist_mediana(.data[[var_referencia]])) %>%
  slice_min(order_by = distancia, n = 2) %>%
  select(-distancia)

tab_mostrar_seleccion <- bind_rows(
  "Máximo" = m_max,
  "Mínimo" = m_min,
  "Cercano_mediana" = m_med,
  .id = "Tipo_municipio"
)

# Tabla con todas las observaciones de los 6 municipios seleccionados
tab_tot_mostrar <- tab_total %>%
  semi_join(tab_mostrar_seleccion, by = "TERRITORIO_CODE") %>%
  arrange(TERRITORIO_CODE, TIME_PERIOD_CODE)

tab_mostrar_seleccion
tab_tot_mostrar

# ============================================================
# 3) Gráficos temporales de los 6 municipios representativos
# ============================================================

# Evolución de viviendas vacacionales disponibles por cada 1000 habitantes
grafico_vv_disp <- ggplot(
  tab_tot_mostrar,
  aes(x = TIME_PERIOD_CODE, y = VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL)
) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  facet_wrap(~ TERRITORIO_CODE, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Evolución de viviendas vacacionales disponibles por cada 1000 habitantes",
    x = "Año",
    y = "Viviendas vacacionales disponibles por 1000 habitantes"
  )

# Evolución del alquiler mediano por metro cuadrado (colectivo)
grafico_alquiler_m2_col <- ggplot(
  tab_tot_mostrar,
  aes(x = TIME_PERIOD_CODE, y = ALQUILER_MED_M_M2_COL)
) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  facet_wrap(~ TERRITORIO_CODE, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Evolución del alquiler mediano por metro cuadrado (vivienda colectiva)",
    x = "Año",
    y = "Alquiler mediano por m²"
  )

# Evolución de la renta neta media por hogar
grafico_renta_neta <- ggplot(
  tab_tot_mostrar,
  aes(x = TIME_PERIOD_CODE, y = RENTA_NETA_MEDIA_HOGAR)
) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  facet_wrap(~ TERRITORIO_CODE, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Evolución de la renta neta media por hogar",
    x = "Año",
    y = "Renta neta media por hogar"
  )

grafico_vv_disp
grafico_alquiler_m2_col
grafico_renta_neta

# ============================================================
# 4) Análisis bivariante exploratorio
# ============================================================

# Los diagramas de dispersión se realizan sobre todos los datos
# del panel, no por municipio, ya que solo hay 5 observaciones
# temporales por municipio.

# ------------------------------------------------------------
# 4.1) Definición de pares de variables a analizar
# ------------------------------------------------------------

pares_bivariantes <- tibble::tribble(
  ~x, ~y, ~titulo, ~xlab, ~ylab,
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_T_INMU_COL",
  "Viviendas vacacionales disponibles por 1000 habitantes y alquiler mediano total (colectivo)",
  "Viviendas vacacionales disponibles por 1000 habitantes", "Alquiler mediano total (colectivo)",
  
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL", "ALQUILER_MED_M_T_INMU_COL",
  "Viviendas vacacionales reservadas por 1000 habitantes y alquiler mediano total (colectivo)",
  "Viviendas vacacionales reservadas por 1000 habitantes", "Alquiler mediano total (colectivo)",
  
  "PLAZAS_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_T_INMU_COL",
  "Plazas disponibles por 1000 habitantes y alquiler mediano total (colectivo)",
  "Plazas disponibles por 1000 habitantes", "Alquiler mediano total (colectivo)",
  
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_T_INMU_U",
  "Viviendas vacacionales disponibles por 1000 habitantes y alquiler mediano total (unifamiliar)",
  "Viviendas vacacionales disponibles por 1000 habitantes", "Alquiler mediano total (unifamiliar)",
  
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL", "ALQUILER_MED_M_T_INMU_U",
  "Viviendas vacacionales reservadas por 1000 habitantes y alquiler mediano total (unifamiliar)",
  "Viviendas vacacionales reservadas por 1000 habitantes", "Alquiler mediano total (unifamiliar)",
  
  "PLAZAS_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_T_INMU_U",
  "Plazas disponibles por 1000 habitantes y alquiler mediano total (unifamiliar)",
  "Plazas disponibles por 1000 habitantes", "Alquiler mediano total (unifamiliar)",
  
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_M2_COL",
  "Viviendas vacacionales disponibles por 1000 habitantes y alquiler mediano por m² (colectivo)",
  "Viviendas vacacionales disponibles por 1000 habitantes", "Alquiler mediano por m² (colectivo)",
  
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL", "ALQUILER_MED_M_M2_COL",
  "Viviendas vacacionales reservadas por 1000 habitantes y alquiler mediano por m² (colectivo)",
  "Viviendas vacacionales reservadas por 1000 habitantes", "Alquiler mediano por m² (colectivo)",
  
  "PLAZAS_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_M2_COL",
  "Plazas disponibles por 1000 habitantes y alquiler mediano por m² (colectivo)",
  "Plazas disponibles por 1000 habitantes", "Alquiler mediano por m² (colectivo)",
  
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_M2_U",
  "Viviendas vacacionales disponibles por 1000 habitantes y alquiler mediano por m² (unifamiliar)",
  "Viviendas vacacionales disponibles por 1000 habitantes", "Alquiler mediano por m² (unifamiliar)",
  
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL", "ALQUILER_MED_M_M2_U",
  "Viviendas vacacionales reservadas por 1000 habitantes y alquiler mediano por m² (unifamiliar)",
  "Viviendas vacacionales reservadas por 1000 habitantes", "Alquiler mediano por m² (unifamiliar)",
  
  "PLAZAS_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_M2_U",
  "Plazas disponibles por 1000 habitantes y alquiler mediano por m² (unifamiliar)",
  "Plazas disponibles por 1000 habitantes", "Alquiler mediano por m² (unifamiliar)",
  
  "RENTA_NETA_MEDIA_HOGAR", "ALQUILER_MED_M_M2_U",
  "Renta neta media por hogar y alquiler mediano por m² (unifamiliar)",
  "Renta neta media por hogar", "Alquiler mediano por m² (unifamiliar)",
  
  "RENTA_NETA_MEDIA_HOGAR", "ALQUILER_MED_M_M2_COL",
  "Renta neta media por hogar y alquiler mediano por m² (colectivo)",
  "Renta neta media por hogar", "Alquiler mediano por m² (colectivo)",
  
  "RENTA_NETA_MEDIA_HOGAR", "ALQUILER_MED_M_T_INMU_U",
  "Renta neta media por hogar y alquiler mediano total (unifamiliar)",
  "Renta neta media por hogar", "Alquiler mediano total (unifamiliar)",
  
  "RENTA_NETA_MEDIA_HOGAR", "ALQUILER_MED_M_T_INMU_COL",
  "Renta neta media por hogar y alquiler mediano total (colectivo)",
  "Renta neta media por hogar", "Alquiler mediano total (colectivo)",
  
  "RENTA_BRUTA_MEDIA_HOGAR", "ALQUILER_MED_M_M2_U",
  "Renta bruta media por hogar y alquiler mediano por m² (unifamiliar)",
  "Renta bruta media por hogar", "Alquiler mediano por m² (unifamiliar)",
  
  "RENTA_BRUTA_MEDIA_HOGAR", "ALQUILER_MED_M_M2_COL",
  "Renta bruta media por hogar y alquiler mediano por m² (colectivo)",
  "Renta bruta media por hogar", "Alquiler mediano por m² (colectivo)",
  
  "RENTA_BRUTA_MEDIA_HOGAR", "ALQUILER_MED_M_T_INMU_U",
  "Renta bruta media por hogar y alquiler mediano total (unifamiliar)",
  "Renta bruta media por hogar", "Alquiler mediano total (unifamiliar)",
  
  "RENTA_BRUTA_MEDIA_HOGAR", "ALQUILER_MED_M_T_INMU_COL",
  "Renta bruta media por hogar y alquiler mediano total (colectivo)",
  "Renta bruta media por hogar", "Alquiler mediano total (colectivo)"
)

# ------------------------------------------------------------
# 4.2) Cálculo de correlaciones de Pearson
# ------------------------------------------------------------

tabla_correlaciones <- pares_bivariantes %>%
  rowwise() %>%
  mutate(
    correlacion_pearson = cor(
      tab_total[[x]],
      tab_total[[y]],
      use = "complete.obs",
      method = "pearson"
    )
  ) %>%
  ungroup()

tabla_correlaciones

# ------------------------------------------------------------
# 4.3) Función para generar gráficos de dispersión
# ------------------------------------------------------------

crear_grafico_dispersion <- function(datos, xvar, yvar, titulo, xlab, ylab) {
  ggplot(datos, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(
      aes(
        text = paste(
          "Territorio:", TERRITORIO_CODE,
          "<br>Año:", TIME_PERIOD_CODE,
          paste0("<br>", xlab, ": ", round(.data[[xvar]], 2)),
          paste0("<br>", ylab, ": ", round(.data[[yvar]], 2))
        )
      ),
      color = "darkgreen",
      alpha = 0.6,
      size = 3
    ) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top") +
    theme_minimal() +
    labs(
      title = titulo,
      x = xlab,
      y = ylab
    )
}

# ------------------------------------------------------------
# 4.4) Crear lista de gráficos bivariantes
# ------------------------------------------------------------

lista_graficos_bivariantes <- lapply(seq_len(nrow(pares_bivariantes)), function(i) {
  crear_grafico_dispersion(
    datos  = tab_total,
    xvar   = pares_bivariantes$x[i],
    yvar   = pares_bivariantes$y[i],
    titulo = pares_bivariantes$titulo[i],
    xlab   = pares_bivariantes$xlab[i],
    ylab   = pares_bivariantes$ylab[i]
  )
})

# Mostrar un gráfico concreto, por ejemplo el primero:
lista_graficos_bivariantes[[1]]

# Si se quiere versión interactiva del primero:
ggplotly(lista_graficos_bivariantes[[1]], tooltip = "text")

# Guardar las imagenes generadas en una carpeta

mi_ruta <- "C:/Users/marco/OneDrive/Documentos/tfg_viviendas_vacacionales/figuras_informe/fotos_corr_simple"

lapply(seq_along(lista_graficos_bivariantes), function(i) {
  # Creamos un nombre de archivo dinámico
  nombre_archivo <- file.path(mi_ruta, paste0("grafico_corr_simple_", i, ".png"))
  
  # Guardamos usando ggsave
  ggplot2::ggsave(
    filename = nombre_archivo,
    plot = lista_graficos_bivariantes[[i]],
    device = "png",
    width = 10,
    height = 7,
    dpi = 300 # Alta resolución
  )
})

# ============================================================
# 5) Selección orientativa de relaciones más destacadas
# ============================================================

# Esta tabla ordena los pares de variables de mayor a menor
# correlación absoluta para facilitar una revisión exploratoria.
tabla_correlaciones_ordenada <- tabla_correlaciones %>%
  mutate(correlacion_abs = abs(correlacion_pearson)) %>%
  arrange(desc(correlacion_abs))

tabla_correlaciones_ordenada