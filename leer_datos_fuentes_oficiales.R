# ============================================================
# Script: leer_datos_fuentes_oficiales.R
# Objetivo: Descargar e importar datos oficiales (ISTAC, SERPAVI, INE)
# ============================================================

# --- Instalación y carga automática de paquetes necesarios ---
pkgs <- c("readr","dplyr","readxl","httr","jsonlite","tidyr","purrr")
inst <- setdiff(pkgs, rownames(installed.packages()))
if (length(inst)) install.packages(inst, dependencies = TRUE,
                                   type = ifelse(.Platform$OS.type == "windows", "binary", "both"))
invisible(lapply(pkgs, require, character.only = TRUE))

# ============================================================
# Función leer_istac_tsv
# Lee archivos TSV desde la API del ISTAC
# ============================================================
leer_istac_tsv <- function(url_tsv, decimal_mark = ".") {
  # Lee todo como texto para evitar errores en la detección de tipos
  df_chr <- read_tsv(
    url_tsv,
    col_types = cols(.default = col_character()),
    locale = locale(encoding = "UTF-8"),
    na = c("", "NA")
  )
  # Detecta qué columnas son completamente numéricas
  es_num <- function(x) all(grepl("^[-+]?[0-9]*[\\.,]?[0-9]+$", x[!is.na(x)]))
  num_cols <- names(df_chr)[vapply(df_chr, es_num, logical(1))]
  # Convierte solo las columnas numéricas detectadas
  for (nc in num_cols) {
    df_chr[[nc]] <- parse_number(df_chr[[nc]], locale = locale(decimal_mark = decimal_mark))
  }
  df_chr
}

# ============================================================
# Descarga de datos desde la API del ISTAC
# ============================================================
# Cada URL apunta a un conjunto de datos distinto
url_vv   <- "https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/C00065A_000061/1.14.tsv"
url_adr  <- "https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/C00065A_000062/1.10.tsv"
url_pob  <- "https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/E30245A_000002/1.4.tsv"
url_renta <- "https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/E30325A_000001/2.1.tsv"

# Descarga y lectura de cada dataset (ajustar decimal_mark si es necesario)
vv    <- leer_istac_tsv(url_vv, ".")    # Viviendas vacacionales


# ============================================================
# Función leer_excel
# Importa una hoja concreta desde un fichero Excel
# ============================================================
leer_excel <- function(ruta_fichero, hoja) {
  # Verifica existencia del fichero y la hoja especificada
  if (!file.exists(ruta_fichero)) stop("El fichero no existe. Verifica la ruta.")
  hojas <- readxl::excel_sheets(ruta_fichero)
  if (!(hoja %in% hojas))
    stop(paste0("La hoja '", hoja, "' no se encuentra. Hojas disponibles: ",
                paste(hojas, collapse = ", ")))
  # Devuelve la hoja como tibble
  readxl::read_excel(path = ruta_fichero, sheet = hoja)
}

# ============================================================
# Ejemplo de importación: datos SERPAVI (Ministerio de Vivienda)
# ============================================================
# Modifica la ruta del fichero para adaptarla a la carpeta en la que tienes almacenados los datos
# En la ruta, debemos usar barras del tipo '/' o no compilará el código
ruta_excel_serpavi <- "C:/Users/benco/Documents/ULL/Curso 2025-2026 ULL/TFG/Ficheros/2025-09-10_bd_SERPAVI_2011-2023.xlsx"
hoja_excel_serpavi <- "Municipios"
datos_serpavi <- leer_excel(ruta_excel_serpavi, hoja_excel_serpavi)

# ============================================================
# Descarga del Censo de Viviendas 2021 (INE, API JSON)
# ============================================================
url_ine <- "https://servicios.ine.es/wstempus/js/ES/DATOS_TABLA/59525"

# Realiza la solicitud y convierte el JSON en un objeto R
resp <- httr::GET(url_ine)
stop_for_status(resp)
datos_json <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))

# Convierte el JSON en tabla y añade variables de interés
df_censo <- datos_json %>%
  tidyr::unnest_wider(Data) %>%           # Expande la lista interna de valores
  dplyr::rename_with(tolower) %>%         # Pone los nombres de columnas en minúsculas
  dplyr::mutate(
    nombre_municipio = sub(",.*", "", nombre),   # Extrae el nombre del municipio
    tipo_vivienda    = sub(".*, ", "", nombre),  # Extrae el tipo de vivienda
    anio             = 2021                      # Año único del censo
  ) %>%
  dplyr::select(nombre_municipio, tipo_vivienda, anio, valor)
