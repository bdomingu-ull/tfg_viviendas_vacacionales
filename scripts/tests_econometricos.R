# ============================================================

# TEST DE HIPÓTESIS 

# ============================================================


# --- Carga de paquetes necesarios ---
pkgs <- c("dplyr", "stringr","tidyr","plm","stargazer","lmtest","sandwich","dotwhisker","broom")
inst <- setdiff(pkgs, rownames(installed.packages()))
if (length(inst)) install.packages(inst, dependencies = TRUE,
                                   type = ifelse(.Platform$OS.type == "windows", "binary", "both"))
invisible(lapply(pkgs, library, character.only = TRUE))


# ============================================================
# TEST F (POOLES OLS V FE)
# ============================================================

# Creamos la tabla comparativa recorriendo listas de modelos

realizar_test_f <- function(y_var, x_var, control_var, datos) {
  
  # Creamos las fórmulas
  formula_completa <- as.formula(paste(y_var, "~", x_var))
  formula_completa_control <- as.formula(paste(y_var, "~", x_var, "+", control_var))
  
  # 1. Estimamos el modelo POOLED (Restringido bajo H0) asumiendo que todos los municipios son iguales.
  mod_pooled <- plm(formula_completa, data = datos, model = "pooling")
  mod_pooled_control <- plm(formula_completa_control, data = datos, model = "pooling")
  
  # 2. Estimamos el modelo de EFECTOS FIJOS, permite que cada municipio tenga su propio intercepto (alfa_i).
  mod_fijos <- plm(formula_completa, data = datos, model = "within")
  mod_fijos_control <- plm(formula_completa_control, data = datos, model = "within")
  
  # 3. Ejecutamos el pFtest (Test F de significatividad de efectos individuales)
  test <- pFtest(mod_fijos, mod_pooled)
  test_control <- pFtest(mod_fijos_control, mod_pooled_control)
  
  return(list(test,test_control))
}

# ------------------------------------------------------------
# TEST sin control
# ------------------------------------------------------------

#Esto crea una tabla que nos dira en que caso si se tiene que aceptar o no la Hípotesis de H0

tab_resul_test_f <- do.call(rbind, lapply(seq_len(nrow(pares_bivariantes)), function(i) {
  
  # Ejecutamos el test para el par i
  resultado <- realizar_test_f(
    y_var = pares_bivariantes$y[i],
    x_var = pares_bivariantes$x[i],
    control_var = pares_bivariantes$control[i],
    datos = tab_total
  )
  
  # Extraemos la info para la tabla
  data.frame(
    ID = i,
    Variable_Y = pares_bivariantes$y[i],
    Variable_X = pares_bivariantes$x[i],
    Estadistico_F = as.numeric(resultado[[1]]$statistic),
    p_valor = as.numeric(resultado[[1]]$p.value),
    Decision = ifelse(resultado[[1]]$p.value < 0.05, 
                      "Rechazar H0 (Municipios distintos)", 
                      "No Rechazar H0 (Municipios iguales)"),
    Modelo_Elegido = ifelse(resultado[[1]]$p.value < 0.05, "EFECTOS FIJOS", "POOLED"),
    stringsAsFactors = FALSE
  )
}))


# ------------------------------------------------------------
# TEST con control de renta
# ------------------------------------------------------------

#Esto crea una tabla que nos dira en que caso si se tiene que aceptar o no la Hípotesis de H0

tab_resul_test_f_control <- do.call(rbind, lapply(seq_len(nrow(pares_bivariantes)), function(i) {
  
  # Ejecutamos el test para el par i
  resultado <- realizar_test_f(
    y_var = pares_bivariantes$y[i],
    x_var = pares_bivariantes$x[i],
    control_var = pares_bivariantes$control[i],
    datos = tab_total
  )
  
  # Extraemos la info para la tabla
  data.frame(
    ID = i,
    Variable_Y = pares_bivariantes$y[i],
    Variable_X = pares_bivariantes$x[i],
    Estadistico_F = as.numeric(resultado[[2]]$statistic),
    p_valor = as.numeric(resultado[[2]]$p.value),
    Decision = ifelse(resultado[[2]]$p.value < 0.05, 
                      "Rechazar H0 (Municipios distintos)", 
                      "No Rechazar H0 (Municipios iguales)"),
    Modelo_Elegido = ifelse(resultado[[2]]$p.value < 0.05, "EFECTOS FIJOS", "POOLED"),
    stringsAsFactors = FALSE
  )
}))
