# ============================================================
# Script: generar_tabla_integracion_fuentes.R
# Objetivo: Filtrar y seleccionar variables las fuentes de datos 
#           por municipio y periodo, como paso previo a la agregación anual
# ============================================================

# Fuente 1: Viviendas vacacionales

# --- Carga de paquetes necesarios ---
pkgs <- c("dplyr", "stringr","tidyr")
inst <- setdiff(pkgs, rownames(installed.packages()))
if (length(inst)) install.packages(inst, dependencies = TRUE,
                                   type = ifelse(.Platform$OS.type == "windows", "binary", "both"))
invisible(lapply(pkgs, library, character.only = TRUE))


# ============================================================
# 1) Filtrar el periodo temporal (2019–2023)
# ============================================================

vv_filtrada <- vv %>%
  # Mantener solo el intervalo total "_T"
  filter(INTERVALOS_PLAZAS_CODE == "_T") %>%
  
  # Filtrar municipios: códigos de 5 dígitos que empiezan por 35 o 38
  filter(grepl("^(35|38)[0-9]{3}$", TERRITORIO_CODE)) %>%
  
  # Extraer el año de TIME_PERIOD_CODE
  mutate(
    anio = suppressWarnings(parse_integer(substr(TIME_PERIOD_CODE, 1, 4)))
  ) %>%
  
  # Filtrar el periodo temporal 2019–2023
  filter(anio >= 2019, anio <= 2023)

# Nos quedamos solo con las columnas que nos interesan

vv_filtrada <- vv_filtrada %>%
  dplyr::select(
    TERRITORIO_CODE,
    TIME_PERIOD_CODE,
    MEDIDAS_CODE,
    OBS_VALUE
  )

# ============================================================
# 2) Filtrar medidas de interes
# ============================================================
# Además del código de municipio y el año, nos quedaremos con las variables
# relacionadas con:
#   - Viviendas vacacionales disponibles
#   - Viviendas vacacionales reservadas
#   - Plazas disponibles
#

medidas_seleccionadas <- c(
  "VIVIENDAS_VACACAIONALES_DISPONIBLES",   
  "VIVIENDAS_VACACIONALES_RESERVADAS",    
  "PLAZAS_DISPONIBLES" 
)

vv_filtrada <- vv_filtrada %>%
  filter(MEDIDAS_CODE %in% medidas_seleccionadas)

# ============================================================
# 3) Transponemos filas a columnas
# ============================================================

vv_filtrada <- vv_filtrada %>%
  tidyr::pivot_wider(
    names_from = MEDIDAS_CODE,   
    values_from = OBS_VALUE      
  )

# ============================================================
# 4) Calcular medias anuales
# ============================================================