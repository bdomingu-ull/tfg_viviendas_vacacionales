# ============================================================

# MODELOS EN PANEL

# ============================================================

# --- Carga de paquetes necesarios ---
pkgs <- c("dplyr", "stringr","tidyr","plm","stargazer","lmtest","sandwich","dotwhisker","broom")
inst <- setdiff(pkgs, rownames(installed.packages()))
if (length(inst)) install.packages(inst, dependencies = TRUE,
                                   type = ifelse(.Platform$OS.type == "windows", "binary", "both"))
invisible(lapply(pkgs, library, character.only = TRUE))


# ------------------------------------------------------------
# 1.1) Crear lista de las variables de estudio
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

# ============================================================
# Analisis con el modelo 1: POOLED OLS
# ============================================================


# ------------------------------------------------------------
# 1.1) Función para hacer modelo
# ------------------------------------------------------------

ejecutar_estudio_OLS <- function(x_var, y_var, control) {
  
  formula_simple <- as.formula(paste(y_var, "~", x_var))
  formula_renta  <- as.formula(paste(y_var, "~", x_var, "+ ",control))
  
  poolmod_simple <- plm(formula_simple, data = tab_total, model = "pooling")
  poolmod_renta  <- plm(formula_renta,  data = tab_total, model = "pooling")
  
  return(list(simple = poolmod_simple, renta = poolmod_renta))
}

# ------------------------------------------------------------
# 1.2) Crear lista donde se agrupe el resultado todos las comparaciones
# ------------------------------------------------------------

list_modelos_OLS <- lapply(seq_len(nrow(pares_bivariantes)), function(i) {
  ejecutar_estudio_OLS(
    x_var = pares_bivariantes$x[i],
    y_var = pares_bivariantes$y[i],
    control = pares_bivariantes$control[i]
  )
})

#Primera forma de ver los resultados

summary(list_modelos_OLS[[3]]$simple)



#Segunda forma de ver los resultados

#stargazer(list_modelos_OLS[[7]]$simple, lis_modelos_OLS[[7]]$renta, 
          #type = "text", 
          #column.labels = c("Sin Control", "Con Renta"),
          #title = "Efecto de Viviendas Vacacionales sobre el Alquiler")

#Tercera forma de ver los resultados

#coeftest(modelo_pool_1, vcov = vcovHC(modelo_pool_1, type = "HC1", cluster = "group"))


# ------------------------------------------------------------
# 1.3) Crear una taba donde de muestran los resultados
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



# ============================================================
# Analisis con el modelo 2: EFECTOS FIJOS
# ============================================================


# ------------------------------------------------------------
# 2.1) Función para hacer modelo
# ------------------------------------------------------------

ejecutar_estudio_FE <- function(x_var, y_var, control) {
  
  formula_simple <- as.formula(paste(y_var, "~", x_var))
  formula_renta  <- as.formula(paste(y_var, "~", x_var, "+", control))
  
  # Usamos model = "within" para Efectos Fijos
  femod_simple <- plm(formula_simple, data = tab_total, model = "within")
  femod_renta  <- plm(formula_renta,  data = tab_total, model = "within")
  
  return(list(simple = femod_simple, renta = femod_renta))
}


# ------------------------------------------------------------
# 2.2) Crear lista donde se agrupe el resultado todos las comparaciones
# ------------------------------------------------------------

list_modelos_FE <- lapply(seq_len(nrow(pares_bivariantes)), function(i) {
  ejecutar_estudio_FE(
    x_var = pares_bivariantes$x[i],
    y_var = pares_bivariantes$y[i],
    control = pares_bivariantes$control[i]
  )
})

stargazer(list_modelos_OLS[[1]]$renta, list_modelos_FE[[1]]$renta, 
          type = "text",
          column.labels = c("Pooling", "Efectos Fijos"),
          model.names = FALSE,
          keep.stat = c("n", "rsq", "adj.rsq"))



# ------------------------------------------------------------
# 2.3) Crear una taba donde de muestran los resultados
# ------------------------------------------------------------


tab_model_FE <- do.call(rbind, lapply(seq_along(list_modelos_FE), function(i) {
  
  # Extraemos los sumarios
  m_simple <- summary(list_modelos_FE[[i]]$simple)
  m_renta  <- summary(list_modelos_FE[[i]]$renta)
  
  # Fila para el modelo SIN CONTROL
  fila_simple <- data.frame(
    ID = as.character(2*i-1),
    Variable_X = pares_bivariantes$x[i],
    Variable_Y = pares_bivariantes$y[i],
    Especificación = "FE Sin Control",
    # Acceso por posición: 1 suele ser marginal (Within) y 2 el ajustado
    `R² (Within)` = as.numeric(m_simple$r.squared[1]), 
    `Adj_R²` = as.numeric(m_simple$r.squared[2]),
    p_valor = as.numeric(m_simple$fstatistic$p.value),
    check.names = FALSE
  )
  
  # Fila para el modelo CON CONTROL
  fila_renta <- data.frame(
    ID = as.character(2*i),
    Variable_X = pares_bivariantes$x[i],
    Variable_Y = pares_bivariantes$y[i],
    Especificación = "FE Con Control",
    `R² (Within)` = as.numeric(m_renta$r.squared[1]),
    `Adj_R²` = as.numeric(m_renta$r.squared[2]),
    p_valor = as.numeric(m_renta$fstatistic$p.value),
    check.names = FALSE
  )
  
  return(rbind(fila_simple, fila_renta))
}))



# ============================================================
# Analisis con el modelo 3: EFECTOS FIJOS BIDIRECCIONAL
# ============================================================


# ------------------------------------------------------------
# 3.1) Función para hacer modelo
# ------------------------------------------------------------


ejecutar_estudio_2way <- function(x_var, y_var, control) {
  
  formula_simple <- as.formula(paste(y_var, "~", x_var))
  formula_renta  <- as.formula(paste(y_var, "~", x_var, "+", control))
  
  # effect = "twoways" activa efectos fijos de individuo y de tiempo
  mod_simple <- plm(formula_simple, data = tab_total, model = "within", effect = "twoways")
  mod_renta  <- plm(formula_renta,  data = tab_total, model = "within", effect = "twoways")
  
  return(list(simple = mod_simple, renta = mod_renta))
}

# ------------------------------------------------------------
# 3.2) Crear lista donde se agrupe el resultado todos las comparaciones
# ------------------------------------------------------------

list_modelos_2way <- lapply(seq_len(nrow(pares_bivariantes)), function(i) {
  ejecutar_estudio_2way(
    x_var = pares_bivariantes$x[i],
    y_var = pares_bivariantes$y[i],
    control = pares_bivariantes$control[i]
  )
})


# ------------------------------------------------------------
# 3.3) Crear una taba donde de muestran los resultados
# ------------------------------------------------------------


tab_model_2way <- do.call(rbind, lapply(seq_along(list_modelos_2way), function(i) {
  
  m_simple <- summary(list_modelos_2way[[i]]$simple)
  m_renta  <- summary(list_modelos_2way[[i]]$renta)
  
  fila_simple <- data.frame(
    ID = as.character(2*i-1),
    Variable_X = pares_bivariantes$x[i],
    Variable_Y = pares_bivariantes$y[i],
    Especificación = "2-Way Sin Control",
    `R² (Within)` = as.numeric(m_simple$r.squared[1]), 
    `Adj_R²` = as.numeric(m_simple$r.squared[2]),
    p_valor = as.numeric(m_simple$fstatistic$p.value),
    check.names = FALSE
  )
  
  fila_renta <- data.frame(
    ID = as.character(2*i),
    Variable_X = pares_bivariantes$x[i],
    Variable_Y = pares_bivariantes$y[i],
    Especificación = "2-Way Con Control",
    `R² (Within)` = as.numeric(m_renta$r.squared[1]),
    `Adj_R²` = as.numeric(m_renta$r.squared[2]),
    p_valor = as.numeric(m_renta$fstatistic$p.value),
    check.names = FALSE
  )
  
  return(rbind(fila_simple, fila_renta))
}))


# ============================================================
# Analisis con el modelo 4: EFECTOS ALEATORIO
# ============================================================


# ------------------------------------------------------------
# 4.1) Función para hacer modelo
# ------------------------------------------------------------


ejecutar_estudio_RE <- function(x_var, y_var, control) {
  
  formula_simple <- as.formula(paste(y_var, "~", x_var))
  formula_renta  <- as.formula(paste(y_var, "~", x_var, "+", control))
  
  # Model = "random" para Efectos Aleatorios
  mod_simple <- plm(formula_simple, data = tab_total, model = "random")
  mod_renta  <- plm(formula_renta,  data = tab_total, model = "random")
  
  return(list(simple = mod_simple, renta = mod_renta))
}



# ------------------------------------------------------------
# 4.2) Crear lista donde se agrupe el resultado todos las comparaciones
# ------------------------------------------------------------



list_modelos_RE <- lapply(seq_len(nrow(pares_bivariantes)), function(i) {
  ejecutar_estudio_RE(
    x_var = pares_bivariantes$x[i],
    y_var = pares_bivariantes$y[i],
    control = pares_bivariantes$control[i]
  )
})

# ------------------------------------------------------------
# 4.3) Crear una taba donde de muestran los resultados
# ------------------------------------------------------------


tab_model_RE <- do.call(rbind, lapply(seq_along(list_modelos_RE), function(i) {
  
  m_simple <- summary(list_modelos_RE[[i]]$simple)
  m_renta  <- summary(list_modelos_RE[[i]]$renta)
  
  fila_simple <- data.frame(
    ID = as.character(2*i-1),
    Variable_X = pares_bivariantes$x[i],
    Variable_Y = pares_bivariantes$y[i],
    Especificación = "RE Sin Control",
    `R²` = as.numeric(m_simple$r.squared[1]), 
    `Adj_R²` = as.numeric(m_simple$r.squared[2]),
    p_valor = as.numeric(m_simple$fstatistic$p.value),
    check.names = FALSE
  )
  
  fila_renta <- data.frame(
    ID = as.character(2*i),
    Variable_X = pares_bivariantes$x[i],
    Variable_Y = pares_bivariantes$y[i],
    Especificación = "RE Con Control",
    `R²` = as.numeric(m_renta$r.squared[1]),
    `Adj_R²` = as.numeric(m_renta$r.squared[2]),
    p_valor = as.numeric(m_renta$fstatistic$p.value),
    check.names = FALSE
  )
  
  return(rbind(fila_simple, fila_renta))
}))

