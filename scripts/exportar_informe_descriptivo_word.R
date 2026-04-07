# ============================================================
# Script: exportar_informe_descriptivo_word.R
# Objetivo: Volcar los principales resultados del análisis
#           descriptivo en un documento Word con tablas
#           adaptadas para facilitar su lectura interna
# ============================================================

# --- Carga de paquetes necesarios ---
pkgs <- c("dplyr", "tidyr", "officer", "flextable", "ggplot2")
inst <- setdiff(pkgs, rownames(installed.packages()))
if (length(inst)) install.packages(
  inst,
  dependencies = TRUE,
  type = ifelse(.Platform$OS.type == "windows", "binary", "both")
)
invisible(lapply(pkgs, library, character.only = TRUE))

# ============================================================
# 1) Ruta de salida del documento
# ============================================================
ruta_salida <- "informe_analisis_descriptivo.docx"

# ============================================================
# 2) Funciones auxiliares
# ============================================================

# ------------------------------------------------------------
# 2.1) Convertir tabla descriptiva ancha a formato vertical
#      (una fila por variable y columnas por métricas)
# ------------------------------------------------------------
convertir_descriptiva_vertical <- function(tabla_desc, incluir_anio = FALSE) {
  
  if (incluir_anio) {
    tabla_larga <- tabla_desc %>%
      pivot_longer(
        cols = -c(TIME_PERIOD_CODE, Metrica),
        names_to = "Variable",
        values_to = "Valor"
      ) %>%
      pivot_wider(
        names_from = Metrica,
        values_from = Valor
      ) %>%
      relocate(TIME_PERIOD_CODE, Variable)
  } else {
    tabla_larga <- tabla_desc %>%
      pivot_longer(
        cols = -Metrica,
        names_to = "Variable",
        values_to = "Valor"
      ) %>%
      pivot_wider(
        names_from = Metrica,
        values_from = Valor
      ) %>%
      relocate(Variable)
  }
  
  tabla_larga
}

# ------------------------------------------------------------
# 2.2) Crear flextable legible para Word
# ------------------------------------------------------------
crear_flextable_legible <- function(df, landscape = FALSE, font_size = 8) {
  ft <- flextable(df)
  
  ft <- ft %>%
    theme_booktabs() %>%
    fontsize(size = font_size, part = "all") %>%
    padding(padding = 2, part = "all") %>%
    align(align = "center", part = "header") %>%
    valign(valign = "center", part = "all") %>%
    bold(part = "header") %>%
    autofit() %>%
    width(j = seq_len(ncol(df)), width = if (landscape) 1.2 else 1.0) %>%
    set_table_properties(
      layout = "autofit",
      width = 1,
      align = "center"
    )
  
  ft
}

# ------------------------------------------------------------
# 2.3) Añadir una tabla con salto de sección apaisado
# ------------------------------------------------------------
anadir_tabla_apaisada <- function(doc, titulo, flextable_obj) {
  doc <- doc %>%
    body_end_block_section(
      block_section(prop_section(
        page_size = page_size(orient = "landscape"),
        type = "continuous"
      ))
    ) %>%
    body_add_par(titulo, style = "heading 2") %>%
    body_add_flextable(flextable_obj) %>%
    body_end_block_section(
      block_section(prop_section(
        page_size = page_size(orient = "portrait"),
        type = "continuous"
      ))
    )
  
  doc
}

# ------------------------------------------------------------
# 2.4) Partir una tabla larga en bloques de filas
# ------------------------------------------------------------
partir_tabla_en_bloques <- function(df, tam_bloque = 20) {
  split(df, ceiling(seq_len(nrow(df)) / tam_bloque))
}

# ============================================================
# 3) Preparación de tablas para Word
# ============================================================

# ------------------------------------------------------------
# 3.1) Descriptivos globales en formato vertical
# ------------------------------------------------------------
tabla_desc_total_vertical <- convertir_descriptiva_vertical(
  tab_medidas_total,
  incluir_anio = FALSE
)

# Clasificación por bloques temáticos
vars_vv <- c(
  "VIVIENDAS_VACACIONALES_RESERVADAS_M",
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M",
  "PLAZAS_DISPONIBLES_M",
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL",
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL",
  "PLAZAS_DISPONIBLES_M_PER_MIL"
)

vars_alquiler <- c(
  "ALQUILER_MED_M_T_INMU_U",
  "ALQUILER_MED_M_T_INMU_COL",
  "ALQUILER_MED_M_M2_U",
  "ALQUILER_MED_M_M2_COL"
)

vars_renta_pob <- c(
  "RENTA_BRUTA_MEDIA_HOGAR",
  "RENTA_NETA_MEDIA_HOGAR",
  "POBLACION"
)

tabla_desc_vv <- tabla_desc_total_vertical %>%
  filter(Variable %in% vars_vv)

tabla_desc_alquiler <- tabla_desc_total_vertical %>%
  filter(Variable %in% vars_alquiler)

tabla_desc_renta_pob <- tabla_desc_total_vertical %>%
  filter(Variable %in% vars_renta_pob)

# ------------------------------------------------------------
# 3.2) Descriptivos por año en formato vertical
# ------------------------------------------------------------
tabla_desc_anios_vertical <- convertir_descriptiva_vertical(
  tab_medidas_anios,
  incluir_anio = TRUE
)

# Para mejorar la legibilidad, se puede dividir por variable
bloques_desc_anios <- partir_tabla_en_bloques(tabla_desc_anios_vertical, tam_bloque = 20)

# ------------------------------------------------------------
# 3.3) Correlaciones
# ------------------------------------------------------------
tabla_corr_completa <- tabla_correlaciones_ordenada %>%
  transmute(
    Variable_1 = x,
    Variable_2 = y,
    Correlacion_Pearson = round(correlacion_pearson, 4),
    Correlacion_absoluta = round(correlacion_abs, 4)
  )

tabla_corr_top15 <- tabla_corr_completa %>%
  slice_head(n = 15)

bloques_corr <- partir_tabla_en_bloques(tabla_corr_completa, tam_bloque = 20)

# ------------------------------------------------------------
# 3.4) Municipios representativos
# ------------------------------------------------------------
tabla_municipios_repr <- tab_mostrar_seleccion

# ============================================================
# 4) Preparación de flextables
# ============================================================

ft_desc_vv <- crear_flextable_legible(tabla_desc_vv, landscape = FALSE, font_size = 9)
ft_desc_alquiler <- crear_flextable_legible(tabla_desc_alquiler, landscape = FALSE, font_size = 9)
ft_desc_renta_pob <- crear_flextable_legible(tabla_desc_renta_pob, landscape = FALSE, font_size = 9)

ft_municipios_repr <- crear_flextable_legible(tabla_municipios_repr, landscape = TRUE, font_size = 8)

ft_corr_top15 <- crear_flextable_legible(tabla_corr_top15, landscape = FALSE, font_size = 9)

fts_desc_anios <- lapply(bloques_desc_anios, function(x) {
  crear_flextable_legible(x, landscape = TRUE, font_size = 8)
})

fts_corr <- lapply(bloques_corr, function(x) {
  crear_flextable_legible(x, landscape = TRUE, font_size = 8)
})

# ============================================================
# 5) Guardado temporal de gráficos principales
# ============================================================

dir.create("figuras_informe", showWarnings = FALSE)

ggsave(
  filename = "figuras_informe/grafico_vv_disp.png",
  plot = grafico_vv_disp,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "figuras_informe/grafico_alquiler_m2_col.png",
  plot = grafico_alquiler_m2_col,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "figuras_informe/grafico_renta_neta.png",
  plot = grafico_renta_neta,
  width = 8,
  height = 5,
  dpi = 300
)

top_corr <- tabla_correlaciones_ordenada %>%
  slice_head(n = 3)

for (i in seq_len(nrow(top_corr))) {
  grafico_i <- crear_grafico_dispersion(
    datos  = tab_total,
    xvar   = top_corr$x[i],
    yvar   = top_corr$y[i],
    titulo = top_corr$titulo[i],
    xlab   = top_corr$xlab[i],
    ylab   = top_corr$ylab[i]
  )
  
  ggsave(
    filename = paste0("figuras_informe/grafico_bivariante_", i, ".png"),
    plot = grafico_i,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# ============================================================
# 6) Creación del documento Word
# ============================================================

doc <- read_docx()

doc <- doc %>%
  body_add_par("Informe interno de análisis descriptivo", style = "heading 1") %>%
  body_add_par(
    paste(
      "Este documento recoge resultados internos del análisis descriptivo",
      "realizado sobre la tabla integrada tab_total a nivel municipio-año",
      "para el periodo 2019-2023. Las tablas se presentan en formatos",
      "adaptados para facilitar su revisión exploratoria."
    ),
    style = "Normal"
  )

# ------------------------------------------------------------
# 6.1) Estadística descriptiva global
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("1. Estadística descriptiva global", style = "heading 2") %>%
  body_add_par("1.1 Variables de viviendas vacacionales", style = "heading 3") %>%
  body_add_flextable(ft_desc_vv) %>%
  body_add_par("1.2 Variables de alquiler", style = "heading 3") %>%
  body_add_flextable(ft_desc_alquiler) %>%
  body_add_par("1.3 Variables de renta y población", style = "heading 3") %>%
  body_add_flextable(ft_desc_renta_pob)

# ------------------------------------------------------------
# 6.2) Estadística descriptiva por año
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("2. Estadística descriptiva por año", style = "heading 2") %>%
  body_add_par(
    "La tabla completa se presenta en bloques para facilitar la lectura en Word.",
    style = "Normal"
  )

for (i in seq_along(fts_desc_anios)) {
  doc <- anadir_tabla_apaisada(
    doc,
    titulo = paste("2.", i, "Bloque de descriptivos por año", sep = ""),
    flextable_obj = fts_desc_anios[[i]]
  )
}

# ------------------------------------------------------------
# 6.3) Municipios representativos
# ------------------------------------------------------------
doc <- anadir_tabla_apaisada(
  doc,
  titulo = "3. Municipios representativos seleccionados",
  flextable_obj = ft_municipios_repr
)

doc <- doc %>%
  body_add_par(
    paste(
      "Se han seleccionado seis municipios representativos a partir de la variable",
      "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL: dos con valores altos,",
      "dos con valores bajos y dos cercanos a la mediana."
    ),
    style = "Normal"
  )

# ------------------------------------------------------------
# 6.4) Gráficos temporales
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("4. Evolución temporal en municipios representativos", style = "heading 2") %>%
  body_add_par("4.1 Viviendas vacacionales disponibles por 1000 habitantes", style = "heading 3") %>%
  body_add_img(src = "figuras_informe/grafico_vv_disp.png", width = 6.5, height = 4.2) %>%
  body_add_par("4.2 Alquiler mediano por metro cuadrado (vivienda colectiva)", style = "heading 3") %>%
  body_add_img(src = "figuras_informe/grafico_alquiler_m2_col.png", width = 6.5, height = 4.2) %>%
  body_add_par("4.3 Renta neta media por hogar", style = "heading 3") %>%
  body_add_img(src = "figuras_informe/grafico_renta_neta.png", width = 6.5, height = 4.2)

# ------------------------------------------------------------
# 6.5) Correlaciones
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("5. Correlaciones lineales", style = "heading 2") %>%
  body_add_par("5.1 Relaciones con mayor correlación absoluta", style = "heading 3") %>%
  body_add_flextable(ft_corr_top15)

for (i in seq_along(fts_corr)) {
  doc <- anadir_tabla_apaisada(
    doc,
    titulo = paste("5.", i + 1, "Tabla completa de correlaciones (bloque)", sep = ""),
    flextable_obj = fts_corr[[i]]
  )
}

# ------------------------------------------------------------
# 6.6) Diagramas de dispersión principales
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("6. Diagramas de dispersión seleccionados", style = "heading 2")

for (i in seq_len(nrow(top_corr))) {
  doc <- doc %>%
    body_add_par(
      paste0("6.", i, " ", top_corr$titulo[i]),
      style = "heading 3"
    ) %>%
    body_add_img(
      src = paste0("figuras_informe/grafico_bivariante_", i, ".png"),
      width = 6.5,
      height = 4.2
    )
}

# ============================================================
# 7) Guardar documento
# ============================================================
print(doc, target = ruta_salida)

cat("Documento Word generado correctamente en:", ruta_salida, "\n")