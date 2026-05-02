# ============================================================

# MODELOS EN PANEL

# ============================================================

# --- Carga de paquetes necesarios ---
pkgs <- c("dplyr", "stringr","tidyr","plm","stargazer","lmtest","sandwich","dotwhisker","broom")
inst <- setdiff(pkgs, rownames(installed.packages()))
if (length(inst)) install.packages(inst, dependencies = TRUE,
                                   type = ifelse(.Platform$OS.type == "windows", "binary", "both"))
invisible(lapply(pkgs, library, character.only = TRUE))


# ============================================================
# Analisis con el modelo 1: POOLED OLS
# ============================================================

# ------------------------------------------------------------
# 4.1) Crear lista de las variables de estudio
# ------------------------------------------------------------

pares_bivariantes <- tibble::tribble(
  ~x, ~y, ~control,
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_M2_COL",
  "RENTA_NETA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL", "ALQUILER_MED_M_M2_COL",
  "RENTA_NETA_MEDIA_HOGAR",
  
  "PLAZAS_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_M2_COL",
  "RENTA_NETA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_M2_U",
  "RENTA_NETA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL", "ALQUILER_MED_M_M2_U",
  "RENTA_NETA_MEDIA_HOGAR",
  
  "PLAZAS_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_M2_U",
  "RENTA_NETA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_M2_COL",
  "RENTA_BRUTA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL", "ALQUILER_MED_M_M2_COL",
  "RENTA_BRUTA_MEDIA_HOGAR",
  
  "PLAZAS_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_M2_COL",
  "RENTA_BRUTA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_M2_U",
  "RENTA_BRUTA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL", "ALQUILER_MED_M_M2_U",
  "RENTA_BRUTA_MEDIA_HOGAR",
  
  "PLAZAS_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_M2_U",
  "RENTA_BRUTA_MEDIA_HOGAR",
  
  
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_T_INMU_COL",
  "RENTA_NETA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL", "ALQUILER_MED_M_T_INMU_COL",
  "RENTA_NETA_MEDIA_HOGAR",
  
  "PLAZAS_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_T_INMU_COL",
  "RENTA_NETA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_T_INMU_U",
  "RENTA_NETA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL", "ALQUILER_MED_M_T_INMU_U",
  "RENTA_NETA_MEDIA_HOGAR",
  
  "PLAZAS_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_T_INMU_U",
  "RENTA_NETA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_T_INMU_COL",
  "RENTA_BRUTA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL", "ALQUILER_MED_M_T_INMU_COL",
  "RENTA_BRUTA_MEDIA_HOGAR",
  
  "PLAZAS_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_T_INMU_COL",
  "RENTA_BRUTA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_T_INMU_U",
  "RENTA_BRUTA_MEDIA_HOGAR",
  
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL", "ALQUILER_MED_M_T_INMU_U",
  "RENTA_BRUTA_MEDIA_HOGAR",
  
  "PLAZAS_DISPONIBLES_M_PER_MIL", "ALQUILER_MED_M_T_INMU_U",
  "RENTA_BRUTA_MEDIA_HOGAR",

)

# ------------------------------------------------------------
# 4.2) Función para hacer modelo
# ------------------------------------------------------------

ejecutar_estudio_OLS <- function(x_var, y_var, control) {
  
  formula_simple <- as.formula(paste(y_var, "~", x_var))
  formula_renta  <- as.formula(paste(y_var, "~", x_var, "+ ",control))
  
  poolmod_simple <- plm(formula_simple, data = tab_total, model = "pooling")
  poolmod_renta  <- plm(formula_renta,  data = tab_total, model = "pooling")
  
  return(list(simple = poolmod_simple, renta = poolmod_renta))
}

# ------------------------------------------------------------
# 4.3) Crear lista donde se agrupe el resultado todos las comparaciones
# ------------------------------------------------------------

list_modelos_OLS <- lapply(seq_len(nrow(pares_bivariantes)), function(i) {
  ejecutar_estudio_OLS(
    x_var = pares_bivariantes$x[i],
    y_var = pares_bivariantes$y[i],
    control = pares_bivariantes$control[i]
  )
})

#Primera forma de ver los resultados

summary(lis_modelos_OLS[[3]]$simple)



#Segunda forma de ver los resultados

#stargazer(list_modelos_OLS[[7]]$simple, lis_modelos_OLS[[7]]$renta, 
          #type = "text", 
          #column.labels = c("Sin Control", "Con Renta"),
          #title = "Efecto de Viviendas Vacacionales sobre el Alquiler")

#Tercera forma de ver los resultados

#coeftest(modelo_pool_1, vcov = vcovHC(modelo_pool_1, type = "HC1", cluster = "group"))


# ------------------------------------------------------------
# 4.4) Crear una taba donde de muestran los resultados
# ------------------------------------------------------------

tab_model_OLS <- do.call(rbind, lapply(seq_along(list_modelos_OLS), function(i) {
  
  # 1. Extraer métricas de ambos modelos
  m_simple <- glance(list_modelos_OLS[[i]]$simple)
  m_renta  <- glance(list_modelos_OLS[[i]]$renta)
  
  # 2. Fila para el modelo SIN CONTROL (Lleva el ID)
  fila_simple <- data.frame(
    ID = as.character(2*i-1),           
    Variable_X = pares_bivariantes$x[i],
    Variable_Y = pares_bivariantes$y[i],
    Especificación = "Sin Control",
    Control_Aplicado = "Ninguno",
    `R²` = m_simple$r.squared,
    `Adj_R²` = m_simple$adj.r.squared,
    p_valor = m_simple$p.value,
    check.names = FALSE
  )
  
  # 3. Fila para el modelo CON CONTROL (ID en blanco)
  fila_renta <- data.frame(
    ID = as.character(i * 2),
    Variable_X = pares_bivariantes$x[i],
    Variable_Y = pares_bivariantes$y[i],
    Especificación = "Con Control",
    Control_Aplicado = pares_bivariantes$control[i],
    `R²` = m_renta$r.squared,
    `Adj_R²` = m_renta$adj.r.squared,
    p_valor = m_renta$p.value,
    check.names = FALSE
  )
  
  return(rbind(fila_simple, fila_renta))
}))
