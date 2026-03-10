# ============================================================
# Script: comienzo_regresión.R
# Objetivo: Filtrar y seleccionar variables las fuentes de datos 
#           por municipio y periodo, como paso previo a la agregación anual
# ============================================================

# --- Carga de paquetes necesarios ---
pkgs <- c("dplyr", "stringr","tidyr","ggplot2", "plotly","ggpubr")
inst <- setdiff(pkgs, rownames(installed.packages()))
if (length(inst)) install.packages(inst, dependencies = TRUE,
                                   type = ifelse(.Platform$OS.type == "windows", "binary", "both"))
invisible(lapply(pkgs, library, character.only = TRUE))

#Paso 1: Cálculo de estadística descriptiva básica

# ============================================================
# 1) Calculo de variables estadísticas
# ============================================================

#Con esta tabla calcularemos las variables estádistica de todos los datos de la columna
tab_medidas_total <- tab_total %>%
  reframe(
    Metrica = c("Media", "Mediana","Desviación típica","Mínimo","Máximo","Rango intercuartílico"), # Aquí pones los nombres
    across(-c(1, 2) & where(is.numeric), \(x) c(mean(x, na.rm = TRUE), median(x, na.rm = TRUE),
        sd(x, na.rm = TRUE),min(x, na.rm = TRUE),max(x, na.rm = TRUE),IQR(x, na.rm = TRUE)))
  )
#Con esta tabla calcularemos las variables estádistica de los datos de la columna organizada por años
tab_medidas_años <- tab_total %>%
  group_by(TIME_PERIOD_CODE) %>%
  reframe(
    Metrica = c("Media", "Mediana","Desviación típica","Mínimo","Máximo","Rango intercuartílico"), # Aquí pones los nombres
    across(-c(1, 2) & where(is.numeric), \(x) c(mean(x, na.rm = TRUE), median(x, na.rm = TRUE),
                                                sd(x, na.rm = TRUE),min(x, na.rm = TRUE),max(x, na.rm = TRUE),IQR(x, na.rm = TRUE)))
  )

# ============================================================
# 2) Elección de 6 datos para mostar
# ============================================================

#Creamos una función que calcule la diferencia entre el dato y la mediana
calcular_dist_mediana <- function(dato){
  mediana <- median(dato, na.rm = TRUE)
  abs(dato - mediana)
}

#Creamos la tabla que elija todos los datos usando lo dos minimos, dos máximos, dos minimos respecto de la mediana de la columna VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL


#Primero hacemos el promedio por municipio para todos los datos
tab_mostrar <- tab_total %>%
  group_by(TERRITORIO_CODE) %>%  
  summarise(
    TIME_PERIOD_CODE = paste0(min(TIME_PERIOD_CODE), "-", max(TIME_PERIOD_CODE)),
    across(where(is.numeric) & !TIME_PERIOD_CODE, ~ mean(.x, na.rm = TRUE)),
    .groups = "drop" # Para que la tabla resultante no se quede "agrupada"
  )%>%
  ungroup()
  
#Luego creamos la tabla con los datos
tab_mostrar <- bind_rows(
  "Min Mediana" = tab_mostrar %>% 
    mutate(distancia = calcular_dist_mediana(VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL)) %>%
    slice_max(order_by = pick(13), n = 2) %>%
    select(-distancia) -> m_med, # Guardamos un momento para el siguiente filtro
  
  "Max" = tab_mostrar %>% 
    filter(!TERRITORIO_CODE %in% m_med$TERRITORIO_CODE) %>% # Excluye los anteriores
    slice_max(order_by = pick(4), n = 2) -> m_max,
  
  "Min" = tab_mostrar %>% 
    filter(!TERRITORIO_CODE %in% c(m_med$TERRITORIO_CODE, m_max$TERRITORIO_CODE)) %>% # Excluye ambos
    slice_min(order_by = pick(4), n = 2),
  
  .id = "Extremo"
)

#Ahora hemos creado una tabla donde elegimos todos los variables de los 6 datos que elegimos
tab_tot_mostrar <- tab_total %>%
  semi_join(tab_mostrar, by='TERRITORIO_CODE') %>%
  arrange(TERRITORIO_CODE)

# ============================================================
# 3) Gráfica de dichos datos
# ============================================================

#Representamos los datos de viviendas vacacionales por cada 1000 habitantes de cada municipio
ggplot(tab_tot_mostrar, aes(x = TIME_PERIOD_CODE, y = VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL)) +
  geom_line(color = "blue", size = 1) +  # La línea
  geom_point(color = "red", size = 2) + # Puntos para marcar cada dato (opcional pero recomendado)
  facet_wrap(~ TERRITORIO_CODE,scales = "free_y")+
  theme_minimal() +
  labs(title = "Evolución de viviendas vacacionales por cada mil de habitantes", x = "Año", y = "Viviendas vacacionales")

#Representamos los datos del alquiler mediano por metro cuadrado de cada municipio
ggplot(tab_tot_mostrar, aes(x = TIME_PERIOD_CODE, y = ALQUILER_MED_M_M2_COL)) +
  geom_line(color = "blue", size = 1) +  # La línea
  geom_point(color = "red", size = 2) + # Puntos para marcar cada dato (opcional pero recomendado)
  facet_wrap(~ TERRITORIO_CODE,scales = "free_y")+
  theme_minimal() +
  labs(title = "Evolución de alquiler mediano por metro cuadrado", x = "Año", y = "Alquiler mediano")
  
ggplot(tab_tot_mostrar, aes(x = TIME_PERIOD_CODE, y = RENTA_NETA_MEDIA_HOGAR)) +
  geom_line(color = "blue", size = 1) +  # La línea
  geom_point(color = "red", size = 2) + # Puntos para marcar cada dato (opcional pero recomendado)
  facet_wrap(~ TERRITORIO_CODE,scales = "free_y")+
  theme_minimal() +
  labs(title = "Evolución de renta neta media", x = "Año", y = "Renta neta")
#Pongo estás gráficas aunque podrían faltar otras como viviendas vacacionales disponibles
#Podemos ver que en la primera gráfica entre 2020-21 hay una caída bastante grande

# ============================================================
# 5) Análisis bivariantes preliminar
# ============================================================

#Gráfico de dispersión y regresión lineal
ggplot(tab_tot_mostrar, aes(x = VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL, y = ALQUILER_MED_M_T_INMU_COL)) +
  geom_point(color = "darkgreen",alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~ TERRITORIO_CODE,scales = "free") +
  theme_minimal() +
  labs(title = "Comparación entre viviendas vacacionales y el alquiler", x = "Viviendas vacacionales", y = "Alquiler mediano")
  
# Con esta forma podemos ver datos llevando el cursor a la gráfica
p <- ggplot(tab_tot_mostrar, aes(x = VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL, 
                                 y = ALQUILER_MED_M_T_INMU_COL)) +
  geom_point(aes(text = paste("Territorio:", TERRITORIO_CODE,
                          "<br>Vacacionales:", VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL,
                          "<br>Alquiler:", ALQUILER_MED_M_T_INMU_COL)),color = "darkgreen", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~ TERRITORIO_CODE, scales = "free") +
  theme_minimal() +
  labs(title = "Comparación entre viviendas vacacionales y el alquiler", 
       x = "Viviendas vacacionales", 
       y = "Alquiler mediano")

# 2. Lo convertimos en interactivo
# tooltip = "text" le dice que use lo que escribimos en el paste() de arriba
ggplotly(p, tooltip = "text")

#Haremos lo mismo pero con el alquiler mediano por unidad

ggplot(tab_tot_mostrar, aes(x = VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL, y = ALQUILER_MED_M_T_INMU_COL)) +
  geom_point(color = "darkgreen",alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~ TERRITORIO_CODE,scales = "free") +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top") +
  theme_minimal() +
  labs(title = "Comparación entre viviendas vacacionales y el alquiler", x = "Viviendas vacacionales", y = "Alquiler mediano")

#Con esto calcularemos los coeficientes de correlación de Perason(Solo hay un ejemplo, el resto se calculará depués)
tabla_correlaciones <- tab_tot_mostrar %>%
  group_by(TERRITORIO_CODE) %>%
  summarize(Correlacion = cor(VIVIENDAS_VACACIONALES_DISPONIBLES_M_PER_MIL, 
                              ALQUILER_MED_M_T_INMU_COL, 
                              method = "pearson", 
                              use = "complete.obs"))

#Haremos lo mismo pero con el alquiler mediano por unidad y con la renta media

ggplot(tab_tot_mostrar, aes(x = RENTA_NETA_MEDIA_HOGAR, y = ALQUILER_MED_M_T_INMU_COL)) +
  geom_point(color = "darkgreen",alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~ TERRITORIO_CODE,scales = "free") +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top") +
  theme_minimal() +
  labs(title = "Comparación entre viviendas vacacionales y el alquiler", x = "Viviendas vacacionales", y = "Alquiler mediano")

#Con esto calcularemos los coeficientes de correlación de Perason(Solo hay un ejemplo, el resto se calculará depués)
tabla_correlaciones_NU <- tab_tot_mostrar %>%
  group_by(TERRITORIO_CODE) %>%
  summarize(Correlacion = cor(RENTA_NETA_MEDIA_HOGAR, 
                              ALQUILER_MED_M_T_INMU_COL, 
                              method = "pearson", 
                              use = "complete.obs"))

#Ahora con el alquiler mediano por total inmueble familia colectiva
ggplot(tab_tot_mostrar, aes(x = RENTA_NETA_MEDIA_HOGAR, y = ALQUILER_MED_M_T_INMU_U)) +
  geom_point(color = "darkgreen",alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~ TERRITORIO_CODE,scales = "free") +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top") +
  theme_minimal() +
  labs(title = "Comparación entre viviendas vacacionales y el alquiler", x = "Viviendas vacacionales", y = "Alquiler mediano")

#Con esto calcularemos los coeficientes de correlación de Perason(Solo hay un ejemplo, el resto se calculará depués)
tabla_correlaciones_NC <- tab_tot_mostrar %>%
  group_by(TERRITORIO_CODE) %>%
  summarize(Correlacion = cor(RENTA_NETA_MEDIA_HOGAR, 
                              ALQUILER_MED_M_T_INMU_U, 
                              method = "pearson", 
                              use = "complete.obs"))
