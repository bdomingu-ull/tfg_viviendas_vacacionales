# ============================================================
# Script: control_calidad_datos.R
# ============================================================

# --- Carga de paquetes necesarios ---
pkgs <- c("dplyr", "tidyr")
inst <- setdiff(pkgs, rownames(installed.packages()))
if (length(inst)) install.packages(inst, dependencies = TRUE,
                                   type = ifelse(.Platform$OS.type == "windows", "binary", "both"))
invisible(lapply(pkgs, library, character.only = TRUE))

# ============================================================
# 1) Cobertura y completitud de los datos
# ============================================================

# Número de municipios disponibles por año
cobertura_municipios <- tab_total %>%
  group_by(TIME_PERIOD_CODE) %>%
  summarise(
    n_municipios = n_distinct(TERRITORIO_CODE),
    .groups = "drop"
  )

cobertura_municipios

# Valores perdidos (NA) por variable y año
na_por_variable_anio <- tab_total %>%
  group_by(TIME_PERIOD_CODE) %>%
  summarise(
    across(
      .cols = everything(), # R ya sabe que debe excluir TIME_PERIOD_CODE por estar en group_by
      .fns  = ~ sum(is.na(.)),
      .names = "na_{.col}"
    ),
    .groups = "drop"
  )
na_por_variable_anio
# ============================================================
# 2) Coherencia temporal
# ============================================================
estabilidad_temporal <- tab_total %>%
  group_by(TERRITORIO_CODE) %>%
  arrange(TIME_PERIOD_CODE) %>%
  mutate(across(
    .cols = where(is.numeric) & !contains("TIME"),
    .fns = list(
      #diff = ~ . - lag(.), # Diferencia absoluta
      pct_change = ~ (. - lag(.)) / lag(.) * 100 # Variación porcentual
    ),
    .names = "{.col}_{.fn}"
  ))
estabilidad_temporal

saltos_sospechosos <- estabilidad_temporal %>%
  filter(if_any(.cols = contains("pct_change"), .fns  = ~ abs(.) > 50  # Aquí buscamos que el valor absoluto sea mayor a 50
    )
  )
saltos_sospechosos


# ============================================================
# 3) Coherencia interna entre variables
# ============================================================
# Resultado: Una tabla con los municipios donde la relación plazas/vivienda es imposible.
coherencia_ratios <- tab_total %>%
  mutate(
    plazas_por_vivienda = PLAZAS_DISPONIBLES_M_PER_MIL / VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL,
    es_incoherente = plazas_por_vivienda > 15 | plazas_por_vivienda < 1
  ) %>%
  filter(es_incoherente) %>%
  select(TERRITORIO_CODE, TIME_PERIOD_CODE, VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL, PLAZAS_DISPONIBLES_M_PER_MIL, plazas_por_vivienda)



# Filtra precios 5 veces más altos que la media
coherencia_precios <- tab_total %>%
  group_by(TIME_PERIOD_CODE) %>%
  summarise(
    municipio = TERRITORIO_CODE,
    precio = ALQUILER_MED_M_T_INMU_COL,
    media_anual = mean(ALQUILER_MED_M_T_INMU_COL, na.rm = TRUE),
    desviacion = ALQUILER_MED_M_T_INMU_COL / media_anual
  ) %>%
  filter(desviacion > 5 | desviacion < 0.2) 


# ============================================================
# 4) Heterogeneidad y valores extremos
# ============================================================
anomalias_z <- tab_total %>%
  group_by(TIME_PERIOD_CODE) %>%
  mutate(across(
    .cols = where(is.numeric),
    .fns = ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
    .names = "z_{.col}"
  )) %>%
  rowwise() %>%
  mutate(columna_anomala = paste(
    names(.)[starts_with("z_", vars = names(.))][which(abs(c_across(starts_with("z_"))) > 3)],
    collapse = ", "
  )) %>%
  ungroup() %>%
  # Filtramos para quedarnos solo con las filas que tienen al menos una anomalía
  filter(columna_anomala != "")
anomalias_z %>% 
  select(TERRITORIO_CODE, TIME_PERIOD_CODE, columna_anomala)
