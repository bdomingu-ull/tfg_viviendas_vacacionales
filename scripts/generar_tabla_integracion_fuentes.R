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
  filter(grepl("^(35|38)[0-9]{3}(_.+)?$", TERRITORIO_CODE)) %>%
  
  mutate(TERRITORIO_CODE = (substr(TERRITORIO_CODE, 1, 5))) %>%
  
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
  "VIVIENDAS_VACACIONALES_DISPONIBLES",   
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

#Primero crearemos una función para que nos calcule la media de los 12 meses
calcular_media <- function(datos, columna, nombre) {
  datos %>%
    # Convertir la columna a numérico seguro
    mutate("{columna}" := as.numeric(as.character(.data[[columna]]))) %>%
    # Crear grupos de tamaño tam_grupo
    mutate(grupo = (row_number() - 1) %/% 12 + 1) %>%
    # Calcular media por grupo y asignarla a todas las filas del grupo
    group_by(grupo) %>%
    mutate("{nombre}" := round(mean(.data[[columna]], na.rm = TRUE),2)) %>%
    ungroup() %>%
    select(-grupo)
}
#Está función lo que hará será añadir una nueva columna donde la media se va a repetir en los doce mese.
# Esro lo haremos para uego filtrarla quedarnos solo con el primera línea y quitar el resto de columnas

vv_filtrada <- vv_filtrada %>%
  calcular_media("VIVIENDAS_VACACIONALES_RESERVADAS","VIVIENDAS_VACACIONALES_RESERVADAS_M")  %>%
  calcular_media("VIVIENDAS_VACACIONALES_DISPONIBLES","VIVIENDAS_VACACIONALES_DISPONIBLES_M")  %>%
  calcular_media("PLAZAS_DISPONIBLES","PLAZAS_DISPONIBLES_M") %>%
  mutate(TIME_PERIOD_CODE = suppressWarnings(parse_integer(substr(TIME_PERIOD_CODE, 1, 4)))) %>%
  slice(seq(1, nrow(.), by = 12))  # selecciona cada 12 filas
#Por último nos quedamos solo con las columnas que nos interesan

vv_filtrada <- vv_filtrada %>%
  dplyr::select(
    TERRITORIO_CODE,
    TIME_PERIOD_CODE,
    VIVIENDAS_VACACIONALES_RESERVADAS_M,
    VIVIENDAS_VACACIONALES_DISPONIBLES_M,
    PLAZAS_DISPONIBLES_M
  )




# ============================================================

# Fuente 2: Renta

renta_filtrada <- renta %>%
  # Filtrar municipios: códigos de 5 dígitos que empiezan por 35 o 38
  filter(grepl("^(35|38)[0-9]{3}(_.+)?$", TERRITORIO_CODE)) %>%
  
  mutate(TERRITORIO_CODE = (substr(TERRITORIO_CODE, 1, 5))) %>%
  
  # Filtrar el periodo temporal 2019–2023
  filter(as.numeric(TIME_PERIOD_CODE) >= 2019, as.numeric(TIME_PERIOD_CODE) <= 2023)

#Ahora seleccionaremos las columnas que nos interesan de la renta  
renta_filtrada <- renta_filtrada %>%
  dplyr::select(
    TERRITORIO_CODE,
    TIME_PERIOD_CODE,
    MEDIDAS_CODE,
    OBS_VALUE
  )
# Además del código de municipio y el año, nos quedaremos con las variables
# relacionadas con:
#   - Renta bruta media por hogar
#   - Renta neta media por hogar

medidas_seleccionadas_2 <- c(
  "RENTA_BRUTA_MEDIA_HOGAR",
  "RENTA_NETA_MEDIA_HOGAR"
)
  
renta_filtrada <- renta_filtrada %>%
  filter(MEDIDAS_CODE %in% medidas_seleccionadas_2)

#Por ultimo trasponemos la columna para que nos aparezca como columnas la renta neta y bruta media del hogar

renta_filtrada <- renta_filtrada %>%
  tidyr::pivot_wider(
    names_from = MEDIDAS_CODE,   
    values_from = OBS_VALUE      
  )


# Fuente 3: Población
#Repetiremos todo los pasos pero solo nos vamos a quedar con la Poblacion total

pob_filtrada <- pob %>%
  filter(SEXO_CODE == "_T") %>%
  # Filtrar municipios: códigos de 5 dígitos que empiezan por 35 o 38
  filter(grepl("^(35|38)[0-9]{3}(_2007)?$", TERRITORIO_CODE)) %>%
  
  mutate(TERRITORIO_CODE = (substr(TERRITORIO_CODE, 1, 5))) %>%
  
  # Filtrar el periodo temporal 2019–2023
  filter(as.numeric(TIME_PERIOD_CODE) >= 2019, as.numeric(TIME_PERIOD_CODE) <= 2023)

#pob_filtrada <- pob_filtrada %>%
  #arrange(as.numeric(TIME_PERIOD_CODE), factor(as.numeric(SEXO_CODE), levels = c("F", "M")))

pob_filtrada <- pob_filtrada %>%
  dplyr::select(
    TERRITORIO_CODE,
    TIME_PERIOD_CODE,
    MEDIDAS_CODE,
    OBS_VALUE
  )
pob_filtrada <- pob_filtrada %>%
  filter(MEDIDAS_CODE == "POBLACION")

pob_filtrada <- pob_filtrada %>%
  tidyr::pivot_wider(
    names_from = MEDIDAS_CODE,   
    values_from = OBS_VALUE      
  )


# Por último procederemos a unir las tres tablas donde crearemos una nueva

tab_total <- vv_filtrada %>%
  left_join(renta_filtrada, by= c("TERRITORIO_CODE", "TIME_PERIOD_CODE")) %>%
  left_join(pob_filtrada, by=c("TERRITORIO_CODE", "TIME_PERIOD_CODE"))
# ============================================================
# Fuente 4: SERPAVI 
# ============================================================

# 1) Filtrar municipios de Canarias y quedarnos con variables de interés 
serpavi_filtrada <- datos_serpavi %>%
  # Municipios de Canarias: códigos 35*** o 38*** (CUMUN suele ser el código municipal)
  dplyr::filter(grepl("^(35|38)[0-9]{3}$", as.character(CUMUN))) %>%
  # Nos quedamos solo con identificadores + las 4 variables de interés (para todos los años)
  dplyr::select(
    CUMUN, NMUN,
    dplyr::matches("^ALQM2_LV_M_VU_\\d{2}$|^ALQTBID12_M_VU_\\d{2}$|^ALQM2_LV_M_VC_\\d{2}$|^ALQTBID12_M_VC_\\d{2}$")
  )

# 2) Reestructurar: pasar de columnas por año a una columna TIME_PERIOD_CODE (formato largo)
#    
serpavi_t <- serpavi_filtrada %>%
  dplyr::rename(
    TERRITORIO_CODE = CUMUN
  ) %>%
  tidyr::pivot_longer(
    cols = -c(TERRITORIO_CODE, NMUN),
    names_to = c("variable", "aa"),
    names_pattern = "^(.*)_(\\d{2})$",
    values_to = "valor"
  ) %>%
  dplyr::mutate(
    TIME_PERIOD_CODE = 2000 + as.integer(aa)
  )


# Filtrar TIME_PERIOD_CODE entre 2019 y 2023
serpavi_t <- serpavi_t %>%
  filter(as.numeric(TIME_PERIOD_CODE) >= 2019, as.numeric(TIME_PERIOD_CODE) <= 2023)

# Transformar de nuevo a formato ancho: una fila por (TERRITORIO_CODE, TIME_PERIOD_CODE) y 4 columnas de alquiler de SERPAVI

serpavi_t <- serpavi_t %>%
  dplyr::select(TERRITORIO_CODE,variable,valor,TIME_PERIOD_CODE) %>%
  tidyr::pivot_wider(
    names_from = "variable",
    values_from = "valor"
  ) %>%
  # Renombrar columnas SERPAVI a nombres más claros
  rename(ALQUILER_MED_M_T_INMU_U = ALQTBID12_M_VU) %>%
  rename(ALQUILER_MED_M_T_INMU_COL = ALQTBID12_M_VC) %>%
  rename(ALQUILER_MED_M_M2_U= ALQM2_LV_M_VU) %>%
  rename(ALQUILER_MED_M_M2_COL= ALQM2_LV_M_VC)
  
# Hacer LEFT JOIN con tab_total por municipio y año
tab_total <- tab_total %>%
  left_join(serpavi_t, by=c("TERRITORIO_CODE", "TIME_PERIOD_CODE"))

# Calcular nuevas variables
tab_total <- tab_total %>%
  mutate(VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL = (VIVIENDAS_VACACIONALES_RESERVADAS_M * 1000) / (POBLACION)) %>%
  mutate(VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL = (VIVIENDAS_VACACIONALES_DISPONIBLES_M * 1000) / (POBLACION)) %>%
  mutate(PLAZAS_DISPONIBLES_M_PER_MIL = (PLAZAS_DISPONIBLES_M * 1000) / (POBLACION)) %>%
  relocate(ends_with("PER_MIL"), .after = PLAZAS_DISPONIBLES_M)
