# Detección de Pacientes en Riesgo de Diabetes con Isolation Forest 🍏🔍

Este repositorio contiene un conjunto de scripts en R para analizar datos clínicos de pacientes con diabetes utilizando el algoritmo **Isolation Forest**. El objetivo principal es detectar perfiles atípicos entre los pacientes que podrían requerir atención médica adicional o un manejo más personalizado de su condición. Este proyecto tiene como meta contribuir a la mejora de la prevención, diagnóstico temprano y tratamiento personalizado de la diabetes, a través del análisis eficiente de los datos clínicos. 💡

![image](https://github.com/user-attachments/assets/40d865cc-e213-47c1-8b19-4ada2726347a)


## Contexto y Justificación 🌍

La **diabetes** es una enfermedad crónica que afecta a millones de personas en todo el mundo, con una prevalencia en constante aumento. Según la **Organización Mundial de la Salud (OMS)**, más de 422 millones de personas viven con diabetes, y esta cifra seguirá creciendo en las próximas décadas. 📈

La detección temprana y el manejo adecuado de la diabetes son cruciales para prevenir complicaciones graves como enfermedades cardiovasculares, ceguera, insuficiencia renal y amputaciones de miembros inferiores. Estas complicaciones no solo impactan la calidad de vida de los pacientes, sino que también representan una carga económica significativa para los sistemas de salud. 💰

Identificar a las personas con mayor riesgo de desarrollar diabetes o que necesitan atención especializada puede ser un desafío debido a la complejidad y la heterogeneidad de la enfermedad. Este proyecto busca desarrollar herramientas innovadoras para el análisis de datos clínicos, contribuyendo a la mejora de la atención de la diabetes. 🌟

## Objetivo del Proyecto 🎯

El objetivo principal de este proyecto es aplicar el algoritmo **Isolation Forest** para detectar anomalías en un conjunto de datos de pacientes con diabetes. El análisis se centrará en identificar patrones inusuales en variables clínicas como la glucosa en sangre, la presión arterial, el índice de masa corporal y otros factores de riesgo, para señalar a los individuos que se desvían significativamente de la población general y que podrían beneficiarse de intervenciones específicas. 🧑‍⚕️👩‍⚕️

## Requisitos 🔧

Antes de ejecutar los scripts, asegúrate de tener instaladas las siguientes librerías en R:

- `solitude`: Implementación de Isolation Forest en R.
- `caret`: Herramientas para la evaluación de modelos de clasificación.
- `dplyr`: Manipulación eficiente de datos.
- `gridExtra`: Organización de gráficos en una cuadrícula.
- `ggplot2`: Creación de gráficos de alta calidad.
- `GGally`: Extensiones para `ggplot2`, incluyendo gráficos de pares.
- `VIM`: Imputación de valores faltantes utilizando KNN.

Puedes instalar estas librerías ejecutando:

```r
install.packages(c("solitude", "caret", "dplyr", "gridExtra", "ggplot2", "GGally", "VIM"))
