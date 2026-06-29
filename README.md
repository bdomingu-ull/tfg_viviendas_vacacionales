# Análisis del impacto de las viviendas vacacionales en el precio del alquiler en Canarias

Este repositorio contiene los materiales y scripts asociados al Trabajo Fin de Grado (TFG), realizado por Marcos Rodríguez Pérez y dirigido por Bencomo Domínguez Martín, dentro del Departamento de Matemáticas, Estadística e Investigación Operativa de la Universidad de La Laguna (ULL).

El objetivo del proyecto es analizar la posible relación entre la oferta de viviendas vacacionales y la evolución de los precios de alquiler en los municipios de Canarias, utilizando fuentes de datos oficiales y métodos estadísticos reproducibles.

---

## Estructura del repositorio

El repositorio se organiza bajo la siguiente estructura de archivos y directorios para asegurar la correcta ejecución del flujo de trabajo:

```text
├── data/
│   ├── input/                # Archivos de datos originales introducidos para el análisis
│   └── output/               # Resultados generados automáticamente (tablas CSV de coeficientes y reportes finales)
├── figuras_informe/
│   ├── fotos_corr_simple/    # Diagramas de dispersión y correlaciones bivariantes guardadas de forma física
│   └── fotos_evolucion/      # Gráficos de evolución temporal de las variables clave del panel
├── analisis_descriptivo.R    # Script 1: Estadísticos descriptivos, filtrado de datos y análisis de correlación
├── modelos_panel_informe_word.R # Script 2: Estimación econométrica multivariante y generación del informe Word
└── README.md                 # Presentación, documentación e instrucciones del proyecto
