# ============================================================
# Script: modelos_panel_informe_word.R
# Objetivo: Estimar modelos de datos en panel y generar un
#           informe Word bien formateado con los resultados,
#           incorporando análisis principal y análisis de
#           robustez con distintas variables de alquiler
# ============================================================

# ------------------------------------------------------------
# 0) Carga de paquetes necesarios
# ------------------------------------------------------------
pkgs <- c(
  "dplyr", "plm", "lmtest", "sandwich", "car",
  "stringr", "officer", "flextable", "tidyr"
)

inst <- setdiff(pkgs, rownames(installed.packages()))
if (length(inst)) {
  install.packages(
    inst,
    dependencies = TRUE,
    type = ifelse(.Platform$OS.type == "windows", "binary", "both")
  )
}
invisible(lapply(pkgs, library, character.only = TRUE))

# ============================================================
# 1) Preparación de los datos
# ============================================================

tab_panel <- tab_total %>%
  filter(TIME_PERIOD_CODE >= 2019, TIME_PERIOD_CODE <= 2023) %>%
  mutate(
    TERRITORIO_CODE = as.character(TERRITORIO_CODE),
    TIME_PERIOD_CODE = as.integer(TIME_PERIOD_CODE)
  ) %>%
  arrange(TERRITORIO_CODE, TIME_PERIOD_CODE)

# ------------------------------------------------------------
# 1.1) Comprobaciones estructurales del panel
# ------------------------------------------------------------
duplicados_panel <- tab_panel %>%
  count(TERRITORIO_CODE, TIME_PERIOD_CODE) %>%
  filter(n > 1)

if (nrow(duplicados_panel) > 0) {
  stop("Existen duplicados en la combinación TERRITORIO_CODE-TIME_PERIOD_CODE.")
}

resumen_panel <- tab_panel %>%
  summarise(
    n_filas = n(),
    n_municipios = n_distinct(TERRITORIO_CODE),
    n_anios = n_distinct(TIME_PERIOD_CODE)
  )

balance_panel <- tab_panel %>%
  count(TERRITORIO_CODE, name = "n_anios_observados") %>%
  arrange(TERRITORIO_CODE)

panel_balanceado <- length(unique(balance_panel$n_anios_observados)) == 1
resumen_panel$panel_balanceado <- panel_balanceado

# ============================================================
# 2) Variables del análisis
# ============================================================

# Dependientes principales
dependientes_principales <- c(
  "ALQUILER_MED_M_M2_COL",
  "ALQUILER_MED_M_M2_U"
)

# Dependientes de robustez
dependientes_robustez <- c(
  "ALQUILER_MED_M_T_INMU_COL",
  "ALQUILER_MED_M_T_INMU_U"
)

# Todas las dependientes
dependientes_todas <- c(dependientes_principales, dependientes_robustez)

# Explicativas principales (siempre separadas)
explicativas_principales <- c(
  "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL",
  "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL",
  "PLAZAS_DISPONIBLES_M_PER_MIL"
)

control_principal <- "RENTA_NETA_MEDIA_HOGAR"

# ============================================================
# 3) Revisión simple de colinealidad
# ============================================================

vars_colinealidad <- c(
  explicativas_principales,
  "RENTA_NETA_MEDIA_HOGAR",
  "RENTA_BRUTA_MEDIA_HOGAR"
)

tabla_correlaciones_explicativas <- cor(
  tab_panel[, vars_colinealidad],
  use = "complete.obs",
  method = "pearson"
)

formula_vif <- as.formula(
  paste(
    "ALQUILER_MED_M_M2_COL ~",
    paste(c(explicativas_principales, "RENTA_NETA_MEDIA_HOGAR"), collapse = " + ")
  )
)

modelo_vif_aux <- lm(formula_vif, data = tab_panel)
vif_aux <- car::vif(modelo_vif_aux)

# ============================================================
# 4) Conversión a panel data frame
# ============================================================

pdata <- pdata.frame(
  tab_panel,
  index = c("TERRITORIO_CODE", "TIME_PERIOD_CODE"),
  drop.index = FALSE,
  row.names = TRUE
)

# ============================================================
# 5) Funciones auxiliares de estimación
# ============================================================

ajustar_modelos_panel <- function(formula_modelo, datos_panel) {
  
  modelo_pool <- plm(
    formula = formula_modelo,
    data = datos_panel,
    model = "pooling"
  )
  
  modelo_fe <- plm(
    formula = formula_modelo,
    data = datos_panel,
    model = "within",
    effect = "individual"
  )
  
  modelo_fe_tw <- plm(
    formula = formula_modelo,
    data = datos_panel,
    model = "within",
    effect = "twoways"
  )
  
  modelo_re <- plm(
    formula = formula_modelo,
    data = datos_panel,
    model = "random"
  )
  
  list(
    pool = modelo_pool,
    fe = modelo_fe,
    fe_tw = modelo_fe_tw,
    re = modelo_re
  )
}

resumen_robusto_plm <- function(modelo, tipo_modelo = c("pool", "fe", "fe_tw", "re")) {
  
  tipo_modelo <- match.arg(tipo_modelo)
  
  vcov_mat <- vcovHC(
    modelo,
    method = "arellano",
    type = "HC1",
    cluster = "group"
  )
  
  test_coef <- coeftest(modelo, vcov = vcov_mat)
  
  salida <- data.frame(
    termino = rownames(test_coef),
    estimacion = test_coef[, 1],
    error_std = test_coef[, 2],
    estadistico = test_coef[, 3],
    p_valor = test_coef[, 4],
    row.names = NULL
  )
  
  salida$modelo <- tipo_modelo
  salida
}

extraer_estadisticos_modelo <- function(modelo, nombre_modelo, dep, explicativa, especificacion, tipo_dependiente) {
  
  r2_val <- tryCatch({
    rsq <- summary(modelo)$r.squared
    if (length(rsq) >= 1) as.numeric(rsq[1]) else NA_real_
  }, error = function(e) NA_real_)
  
  r2_adj_val <- tryCatch({
    rsq <- summary(modelo)$r.squared
    if (length(rsq) >= 2) as.numeric(rsq[2]) else NA_real_
  }, error = function(e) NA_real_)
  
  data.frame(
    dependiente = dep,
    tipo_dependiente = tipo_dependiente,
    explicativa = explicativa,
    especificacion = especificacion,
    modelo = nombre_modelo,
    nobs = nobs(modelo),
    r2 = r2_val,
    r2_ajustado = r2_adj_val
  )
}

ejecutar_tests_panel <- function(modelos) {
  
  test_pool_vs_fe <- tryCatch(
    pFtest(modelos$fe, modelos$pool),
    error = function(e) e
  )
  
  test_pool_vs_fe_tw <- tryCatch(
    pFtest(modelos$fe_tw, modelos$pool),
    error = function(e) e
  )
  
  test_hausman <- tryCatch(
    phtest(modelos$fe, modelos$re),
    error = function(e) e
  )
  
  list(
    pool_vs_fe = test_pool_vs_fe,
    pool_vs_fe_tw = test_pool_vs_fe_tw,
    hausman = test_hausman
  )
}

extraer_info_test <- function(test_obj, nombre_test, nombre_modelo, dep, explicativa, especificacion, tipo_dependiente) {
  if (inherits(test_obj, "error")) {
    return(data.frame(
      dependiente = dep,
      tipo_dependiente = tipo_dependiente,
      explicativa = explicativa,
      especificacion = especificacion,
      modelo_id = nombre_modelo,
      test = nombre_test,
      estadistico = NA_real_,
      p_valor = NA_real_,
      decision_orientativa = paste("No se pudo calcular:", test_obj$message)
    ))
  }
  
  estad <- tryCatch(as.numeric(test_obj$statistic[[1]]), error = function(e) NA_real_)
  pval <- tryCatch(as.numeric(test_obj$p.value), error = function(e) NA_real_)
  
  decision <- if (is.na(pval)) {
    "No concluyente"
  } else if (nombre_test %in% c("pool_vs_fe", "pool_vs_fe_tw")) {
    if (pval < 0.05) {
      "Se rechaza pooled OLS a favor del modelo con efectos fijos"
    } else {
      "No se rechaza pooled OLS"
    }
  } else if (nombre_test == "hausman") {
    if (pval < 0.05) {
      "Se rechaza RE a favor de FE"
    } else {
      "No se rechaza RE frente a FE"
    }
  } else {
    "No concluyente"
  }
  
  data.frame(
    dependiente = dep,
    tipo_dependiente = tipo_dependiente,
    explicativa = explicativa,
    especificacion = especificacion,
    modelo_id = nombre_modelo,
    test = nombre_test,
    estadistico = estad,
    p_valor = pval,
    decision_orientativa = decision
  )
}

# ============================================================
# 6) Estimación de modelos
# ============================================================

resultados_coeficientes <- list()
resultados_estadisticos <- list()
resultados_tests_tabla <- list()
modelos_guardados <- list()

contador <- 1

for (dep in dependientes_todas) {
  
  tipo_dep <- ifelse(dep %in% dependientes_principales, "principal", "robustez")
  
  for (xvar in explicativas_principales) {
    
    # -----------------------------------------
    # Especificación 1: sin control de renta
    # -----------------------------------------
    formula_sin_control <- as.formula(
      paste(dep, "~", xvar)
    )
    
    modelos_sin_control <- ajustar_modelos_panel(
      formula_modelo = formula_sin_control,
      datos_panel = pdata
    )
    
    nombre_modelo_sc <- paste(dep, xvar, "sin_control", sep = "__")
    modelos_guardados[[nombre_modelo_sc]] <- modelos_sin_control
    
    coef_pool_sc <- resumen_robusto_plm(modelos_sin_control$pool, "pool")
    coef_fe_sc <- resumen_robusto_plm(modelos_sin_control$fe, "fe")
    coef_fe_tw_sc <- resumen_robusto_plm(modelos_sin_control$fe_tw, "fe_tw")
    coef_re_sc <- resumen_robusto_plm(modelos_sin_control$re, "re")
    
    coef_todos_sc <- bind_rows(coef_pool_sc, coef_fe_sc, coef_fe_tw_sc, coef_re_sc) %>%
      mutate(
        dependiente = dep,
        tipo_dependiente = tipo_dep,
        explicativa_principal = xvar,
        especificacion = "sin_control"
      ) %>%
      relocate(dependiente, tipo_dependiente, explicativa_principal, especificacion, modelo, termino)
    
    resultados_coeficientes[[contador]] <- coef_todos_sc
    
    est_pool_sc <- extraer_estadisticos_modelo(modelos_sin_control$pool, "pool", dep, xvar, "sin_control", tipo_dep)
    est_fe_sc <- extraer_estadisticos_modelo(modelos_sin_control$fe, "fe", dep, xvar, "sin_control", tipo_dep)
    est_fe_tw_sc <- extraer_estadisticos_modelo(modelos_sin_control$fe_tw, "fe_tw", dep, xvar, "sin_control", tipo_dep)
    est_re_sc <- extraer_estadisticos_modelo(modelos_sin_control$re, "re", dep, xvar, "sin_control", tipo_dep)
    
    resultados_estadisticos[[contador]] <- bind_rows(est_pool_sc, est_fe_sc, est_fe_tw_sc, est_re_sc)
    
    tests_sc <- ejecutar_tests_panel(modelos_sin_control)
    
    resultados_tests_tabla[[length(resultados_tests_tabla) + 1]] <- bind_rows(
      extraer_info_test(tests_sc$pool_vs_fe, "pool_vs_fe", nombre_modelo_sc, dep, xvar, "sin_control", tipo_dep),
      extraer_info_test(tests_sc$pool_vs_fe_tw, "pool_vs_fe_tw", nombre_modelo_sc, dep, xvar, "sin_control", tipo_dep),
      extraer_info_test(tests_sc$hausman, "hausman", nombre_modelo_sc, dep, xvar, "sin_control", tipo_dep)
    )
    
    contador <- contador + 1
    
    # -----------------------------------------
    # Especificación 2: con control de renta
    # -----------------------------------------
    formula_con_control <- as.formula(
      paste(dep, "~", xvar, "+", control_principal)
    )
    
    modelos_con_control <- ajustar_modelos_panel(
      formula_modelo = formula_con_control,
      datos_panel = pdata
    )
    
    nombre_modelo_cc <- paste(dep, xvar, "con_control", sep = "__")
    modelos_guardados[[nombre_modelo_cc]] <- modelos_con_control
    
    coef_pool_cc <- resumen_robusto_plm(modelos_con_control$pool, "pool")
    coef_fe_cc <- resumen_robusto_plm(modelos_con_control$fe, "fe")
    coef_fe_tw_cc <- resumen_robusto_plm(modelos_con_control$fe_tw, "fe_tw")
    coef_re_cc <- resumen_robusto_plm(modelos_con_control$re, "re")
    
    coef_todos_cc <- bind_rows(coef_pool_cc, coef_fe_cc, coef_fe_tw_cc, coef_re_cc) %>%
      mutate(
        dependiente = dep,
        tipo_dependiente = tipo_dep,
        explicativa_principal = xvar,
        especificacion = "con_control"
      ) %>%
      relocate(dependiente, tipo_dependiente, explicativa_principal, especificacion, modelo, termino)
    
    resultados_coeficientes[[contador]] <- coef_todos_cc
    
    est_pool_cc <- extraer_estadisticos_modelo(modelos_con_control$pool, "pool", dep, xvar, "con_control", tipo_dep)
    est_fe_cc <- extraer_estadisticos_modelo(modelos_con_control$fe, "fe", dep, xvar, "con_control", tipo_dep)
    est_fe_tw_cc <- extraer_estadisticos_modelo(modelos_con_control$fe_tw, "fe_tw", dep, xvar, "con_control", tipo_dep)
    est_re_cc <- extraer_estadisticos_modelo(modelos_con_control$re, "re", dep, xvar, "con_control", tipo_dep)
    
    resultados_estadisticos[[contador]] <- bind_rows(est_pool_cc, est_fe_cc, est_fe_tw_cc, est_re_cc)
    
    tests_cc <- ejecutar_tests_panel(modelos_con_control)
    
    resultados_tests_tabla[[length(resultados_tests_tabla) + 1]] <- bind_rows(
      extraer_info_test(tests_cc$pool_vs_fe, "pool_vs_fe", nombre_modelo_cc, dep, xvar, "con_control", tipo_dep),
      extraer_info_test(tests_cc$pool_vs_fe_tw, "pool_vs_fe_tw", nombre_modelo_cc, dep, xvar, "con_control", tipo_dep),
      extraer_info_test(tests_cc$hausman, "hausman", nombre_modelo_cc, dep, xvar, "con_control", tipo_dep)
    )
    
    contador <- contador + 1
  }
}

tabla_coeficientes_panel <- bind_rows(resultados_coeficientes)
tabla_estadisticos_panel <- bind_rows(resultados_estadisticos)
tabla_tests_panel <- bind_rows(resultados_tests_tabla)

# Tabla de coeficientes principales: término vacacional y renta
tabla_coeficientes_principales <- tabla_coeficientes_panel %>%
  filter(termino %in% c(
    "VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL",
    "VIVIENDAS_VACACIONALES_RESERVADAS_M_PER_MIL",
    "PLAZAS_DISPONIBLES_M_PER_MIL",
    "RENTA_NETA_MEDIA_HOGAR"
  )) %>%
  arrange(tipo_dependiente, dependiente, explicativa_principal, especificacion, modelo, termino)

# Tabla resumen específica para el efecto de la variable vacacional
tabla_efecto_vacacional <- tabla_coeficientes_panel %>%
  filter(termino == explicativa_principal) %>%
  mutate(significativo_5 = ifelse(!is.na(p_valor) & p_valor < 0.05, "Sí", "No")) %>%
  arrange(tipo_dependiente, dependiente, explicativa_principal, especificacion, modelo)

# ============================================================
# 7) Exportación opcional a CSV
# ============================================================

write.csv(
  tabla_coeficientes_panel,
  file = file.path("data", "output", "resultados_coeficientes_panel.csv"),
  row.names = FALSE
)

write.csv(
  tabla_estadisticos_panel,
  file = file.path("data", "output", "resultados_estadisticos_panel.csv"),
  row.names = FALSE
)

write.csv(
  tabla_coeficientes_principales,
  file = file.path("data", "output", "resultados_coeficientes_principales_panel.csv"),
  row.names = FALSE
)

write.csv(
  tabla_tests_panel,
  file = file.path("data", "output", "resultados_tests_panel.csv"),
  row.names = FALSE
)

write.csv(
  tabla_efecto_vacacional,
  file = file.path("data", "output", "resultados_efecto_vacacional_panel.csv"),
  row.names = FALSE
)

# ============================================================
# 8) Funciones auxiliares para Word
# ============================================================

crear_tabla_word <- function(df, tam_letra = 9) {
  flextable(df) %>%
    theme_booktabs() %>%
    autofit() %>%
    fontsize(size = tam_letra, part = "all") %>%
    align(align = "center", part = "all") %>%
    bold(part = "header")
}

# ============================================================
# 9) Preparación de tablas para el informe
# ============================================================

tabla_resumen_panel_word <- resumen_panel

tabla_balance_word <- balance_panel

tabla_corr_explicativas_word <- as.data.frame(round(tabla_correlaciones_explicativas, 4))
tabla_corr_explicativas_word <- cbind(
  Variable = rownames(tabla_corr_explicativas_word),
  tabla_corr_explicativas_word
)
rownames(tabla_corr_explicativas_word) <- NULL

tabla_vif_word <- data.frame(
  Variable = names(vif_aux),
  VIF = round(as.numeric(vif_aux), 4)
)

tabla_coef_principales_word <- tabla_coeficientes_principales %>%
  mutate(
    estimacion = round(estimacion, 4),
    error_std = round(error_std, 4),
    estadistico = round(estadistico, 4),
    p_valor = round(p_valor, 4)
  ) %>%
  select(
    tipo_dependiente,
    dependiente,
    explicativa_principal,
    especificacion,
    modelo,
    termino,
    estimacion,
    error_std,
    estadistico,
    p_valor
  )

tabla_estad_word <- tabla_estadisticos_panel %>%
  mutate(
    r2 = round(r2, 4),
    r2_ajustado = round(r2_ajustado, 4)
  ) %>%
  select(
    tipo_dependiente,
    dependiente,
    explicativa,
    especificacion,
    modelo,
    nobs,
    r2,
    r2_ajustado
  )

tabla_tests_word <- tabla_tests_panel %>%
  mutate(
    estadistico = round(estadistico, 4),
    p_valor = round(p_valor, 4)
  ) %>%
  select(
    tipo_dependiente,
    dependiente,
    explicativa,
    especificacion,
    modelo_id,
    test,
    estadistico,
    p_valor,
    decision_orientativa
  )

tabla_efecto_vacacional_word <- tabla_efecto_vacacional %>%
  mutate(
    estimacion = round(estimacion, 4),
    error_std = round(error_std, 4),
    estadistico = round(estadistico, 4),
    p_valor = round(p_valor, 4)
  ) %>%
  select(
    tipo_dependiente,
    dependiente,
    explicativa_principal,
    especificacion,
    modelo,
    estimacion,
    error_std,
    estadistico,
    p_valor,
    significativo_5
  )

# Tablas separadas para facilitar lectura
tabla_efecto_principal_word <- tabla_efecto_vacacional_word %>%
  filter(tipo_dependiente == "principal")

tabla_efecto_robustez_word <- tabla_efecto_vacacional_word %>%
  filter(tipo_dependiente == "robustez")

# ============================================================
# 10) Creación del documento Word
# ============================================================

doc <- read_docx()

doc <- doc %>%
  body_add_par("Resultados de modelos de datos en panel", style = "heading 1") %>%
  body_add_par(
    "Análisis del efecto de las viviendas vacacionales sobre el precio del alquiler a nivel municipio-año (2019–2023).",
    style = "Normal"
  )

# ------------------------------------------------------------
# 10.1) Estructura del panel
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("1. Estructura del panel", style = "heading 2") %>%
  body_add_par(
    "Se comprueba la estructura básica del panel antes de la estimación de los modelos.",
    style = "Normal"
  ) %>%
  body_add_flextable(crear_tabla_word(tabla_resumen_panel_word, 9)) %>%
  body_add_par("Balance del panel por municipio", style = "heading 3") %>%
  body_add_flextable(crear_tabla_word(tabla_balance_word, 8))

# ------------------------------------------------------------
# 10.2) Colinealidad orientativa
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("2. Revisión orientativa de colinealidad", style = "heading 2") %>%
  body_add_par(
    "Se incluye una matriz de correlaciones entre variables explicativas candidatas y una revisión orientativa mediante VIF.",
    style = "Normal"
  ) %>%
  body_add_par("2.1 Matriz de correlaciones entre explicativas", style = "heading 3") %>%
  body_add_flextable(crear_tabla_word(tabla_corr_explicativas_word, 8)) %>%
  body_add_par("2.2 Factores de inflación de la varianza (VIF)", style = "heading 3") %>%
  body_add_flextable(crear_tabla_word(tabla_vif_word, 9))

# ------------------------------------------------------------
# 10.3) Descripción de modelos
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("3. Especificaciones estimadas", style = "heading 2") %>%
  body_add_par(
    paste(
      "Para cada variable dependiente se estiman cuatro especificaciones:",
      "pooled OLS, efectos fijos por municipio, efectos fijos bidireccionales",
      "y efectos aleatorios. Además, para cada combinación se estiman",
      "dos versiones: una sin control y otra con control por renta neta media del hogar."
    ),
    style = "Normal"
  ) %>%
  body_add_par(
    paste(
      "Las variables dependientes principales son el alquiler mediano por metro cuadrado",
      "en vivienda colectiva y en vivienda unifamiliar. Como análisis de robustez",
      "se incorporan también las variables de alquiler mediano total del inmueble",
      "para ambos tipos de vivienda."
    ),
    style = "Normal"
  )

# ------------------------------------------------------------
# 10.4) Resumen del efecto de la variable vacacional
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("4. Resumen del efecto estimado de la variable vacacional", style = "heading 2") %>%
  body_add_par(
    "Esta sección resume, para cada modelo, el coeficiente asociado a la variable vacacional utilizada como explicativa principal.",
    style = "Normal"
  ) %>%
  body_add_par("4.1 Modelos principales (alquiler por m²)", style = "heading 3") %>%
  body_add_flextable(crear_tabla_word(tabla_efecto_principal_word, 8)) %>%
  body_add_par("4.2 Modelos de robustez (alquiler total del inmueble)", style = "heading 3") %>%
  body_add_flextable(crear_tabla_word(tabla_efecto_robustez_word, 8))

# ------------------------------------------------------------
# 10.5) Coeficientes principales completos
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("5. Coeficientes principales completos", style = "heading 2") %>%
  body_add_par(
    "La siguiente tabla recoge los coeficientes principales estimados con errores robustos, incluyendo la variable vacacional y, cuando procede, la renta neta media del hogar.",
    style = "Normal"
  ) %>%
  body_add_flextable(crear_tabla_word(tabla_coef_principales_word, 8))

# ------------------------------------------------------------
# 10.6) Estadísticos de ajuste
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("6. Estadísticos de ajuste", style = "heading 2") %>%
  body_add_flextable(crear_tabla_word(tabla_estad_word, 8))

# ------------------------------------------------------------
# 10.7) Tests econométricos
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("7. Tests econométricos", style = "heading 2") %>%
  body_add_par(
    paste(
      "Se incluyen tests F para comparar pooled OLS frente a modelos con efectos fijos,",
      "así como el test de Hausman para comparar efectos fijos y aleatorios."
    ),
    style = "Normal"
  ) %>%
  body_add_flextable(crear_tabla_word(tabla_tests_word, 8))

# ------------------------------------------------------------
# 10.8) Nota metodológica final
# ------------------------------------------------------------
doc <- doc %>%
  body_add_par("8. Nota de interpretación", style = "heading 2") %>%
  body_add_par(
    paste(
      "Este documento recoge resultados cuantitativos de apoyo a la interpretación.",
      "La selección del modelo principal debe basarse conjuntamente en los tests,",
      "la estabilidad de los coeficientes, la comparación entre especificaciones",
      "con y sin controles y la coherencia sustantiva de los resultados."
    ),
    style = "Normal"
  ) %>%
  body_add_par(
    paste(
      "Los modelos de robustez permiten comprobar si el patrón observado cambia",
      "cuando el alquiler se mide en términos totales en lugar de por metro cuadrado."
    ),
    style = "Normal"
  )

# ============================================================
# 11) Guardar documento Word
# ============================================================

ruta_word <- file.path("data", "output", "resultados_modelos_panel.docx")
print(doc, target = ruta_word)

cat("Documento Word generado correctamente en:", ruta_word, "\n")
cat("Archivos CSV generados correctamente en la carpeta: data/output/\n")