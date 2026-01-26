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
      .cols = -c(TERRITORIO_CODE, TIME_PERIOD_CODE),
      .fns  = ~ sum(is.na(.)),
      .names = "na_{.col}"
    ),
    .groups = "drop"
  )

na_por_variable_anio

# ============================================================
# 2) Coherencia temporal
# ============================================================


# ============================================================
# 3) Coherencia interna entre variables
# ============================================================


# ============================================================
# 4) Heterogeneidad y valores extremos
# ============================================================

