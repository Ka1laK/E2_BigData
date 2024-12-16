# --------------------------- #
# Librerías utilizadas
# --------------------------- #
if (!require(solitude)) install.packages("solitude", dependencies = TRUE)
if (!require(caret)) install.packages("caret")
if (!require(dplyr)) install.packages("dplyr")
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(GGally)) install.packages("GGally")
if (!require(VIM)) install.packages("VIM")

library(solitude)  # Isolation Forest
library(caret)     # Métricas de clasificación
library(dplyr)     # Manipulación de datos
library(gridExtra) # Organizar gráficos
library(ggplot2)   # Visualización de gráficos
library(GGally)    # Pair plots
library(VIM)       # KNN para imputación

# --------------------------- #
# Cargar y preparar los datos
# --------------------------- #
df <- read.csv(file.choose(), sep = ",")
head(df)

# Descripcion del dataset
print("Estructura del dataset:")
str(df)       
print("Resumen estadistico del dataset:")
summary(df)     
print("Valores nulos por columna:")
colSums(is.na(df))

# --------------------------- #
# Analisis exploratorio inicial
# --------------------------- #
# Diagramas de cajas para todas las columnas
columnas_a_graficar <- colnames(df)[1:(ncol(df))]
graficos <- list()

for (col in columnas_a_graficar) {
  p <- ggplot(df, aes_string(y = col)) +
    geom_boxplot(fill = "steelblue", color = "black") +
    labs(title = col, y = "") +
    theme_minimal()
  graficos[[col]] <- p
}

do.call(grid.arrange, c(graficos, ncol = 3))

# Histograma de variables numericas
df %>%
  select_if(is.numeric) %>%
  pivot_longer(everything(), names_to = "clave", values_to = "valor") %>%
  ggplot(aes(valor)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ clave, scales = 'free') + 
  theme_minimal() +
  labs(title = "Distribucion de las Variables Numericas", x = "Valor", y = "Frecuencia")

# --------------------------- #
# Aplicación de Isolation Forest
# --------------------------- #
# Escalar los datos numericos
X_escalado <- scale(df %>% select_if(is.numeric))

# Modelo Isolation Forest
modelo_iso <- isolationForest$new(sample_size = nrow(X_escalado), num_trees = 100, seed = 42)
modelo_iso$fit(X_escalado)

# Prediccion de outliers y etiquetado
predicciones <- modelo_iso$predict(X_escalado)
df$outlier <- ifelse(predicciones$anomaly_score > quantile(predicciones$anomaly_score, 0.8), "Outlier", "Normal")


# Visualizacion: Pair Plot inicial
ggpairs(df, 
        columns = 1:6,  
        aes(color = outlier, alpha = 0.6),  
        upper = list(continuous = wrap("points", size = 2)),  
        lower = list(continuous = wrap("points", size = 2)),  
        diag = list(continuous = wrap("densityDiag", alpha = 0.6))) +  
  scale_color_manual(values = c("Normal" = "blue", "Outlier" = "red")) +
  labs(title = "Pair Plot de Outliers Detectados por Isolation Forest") +
  theme_minimal()

# ----------- #
# Imputacion
# ----------- #
# Reemplazar 0 por NA en columnas seleccionadas
columnas_para_imputar <- c("Glucosa", "Presion_Arterial", "Grosor_Piel", "Insulina", "IMC")
df[columnas_para_imputar] <- lapply(df[columnas_para_imputar], function(x) ifelse(x == 0, NA, x))

# Imputar valores faltantes con KNN
df_imputado <- kNN(df, variable = columnas_para_imputar)


# --------------------------- #
# Nuevo analisis exploratorio despues de la imputación
# --------------------------- #
# Repetir diagramas de cajas
columnas_a_graficar <- colnames(df_imputado)[1:(ncol(df_imputado))] 
graficos <- list()
for (col in columnas_a_graficar) {
  p <- ggplot(df_imputado, aes_string(y = col)) +
    geom_boxplot(fill = "steelblue", color = "black") +
    labs(title = col, y = "") +
    theme_minimal()
  graficos[[col]] <- p
}

do.call(grid.arrange, c(graficos, ncol = 3))

# Excluir variables imputadas para evitar sobrecarga el grafico
columnas_a_graficar <- setdiff(colnames(df_imputado), c("outlier", "Glucosa_imp", "Presion_Arterial_imp", "Grosor_Piel_imp", "Insulina_imp", "IMC_imp"))

# Creacion de graficos de cajas para las variables restantes
graficos <- list()
for (col in columnas_a_graficar) {
  p <- ggplot(df_imputado, aes_string(y = col)) +
    geom_boxplot(fill = "steelblue", color = "black") +
    labs(title = col, y = "") +
    theme_minimal()
  graficos[[col]] <- p
}

# Mostrar todos los graficos en un arreglo
do.call(grid.arrange, c(graficos, ncol = 3))

# --------------------------- #
# Histograma de variables numericas despues de la imputacion
# --------------------------- #
df_imputado %>%
  select_if(is.numeric) %>%
  pivot_longer(everything(), names_to = "clave", values_to = "valor") %>%
  ggplot(aes(valor)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ clave, scales = 'free') + 
  theme_minimal() +
  labs(title = "Distribucion de las Variables Numericas (imputacion)", x = "Valor", y = "Frecuencia")

# --------------------------- #
# Revision de la imputacion
# --------------------------- #
print("Estructura del dataset imputado:")
str(df_imputado)       
print("Resumen estadistico del dataset imputado:")
summary(df_imputado)     
print("Valores nulos por columna después de imputacion:")
colSums(is.na(df_imputado))

# ------------------------------------------------------------ #
# Nueva aplicacion de Isolation Forest despues de imputación
# ------------------------------------------------------------ #
# Escalar los datos nuevamente
X_nueva_escalado <- scale(df_imputado %>% select_if(is.numeric))

# Nueva aplicacion de Isolation Forest
modelo_iso_nuevo <- isolationForest$new(sample_size = nrow(X_nueva_escalado), num_trees = 100, seed = 42)
modelo_iso_nuevo$fit(X_nueva_escalado)

# Prediccion de outliers y etiquetado
nueva_prediccion <- modelo_iso_nuevo$predict(X_nueva_escalado)
df_imputado$outlier <- ifelse(nueva_prediccion$anomaly_score > quantile(nueva_prediccion$anomaly_score, 0.8), "Outlier", "Normal")

# --------------------------- #
# Visualizacion final después de imputacion
# --------------------------- #
# Pair Plot despues de la imputacion
ggpairs(df_imputado, 
        columns = 1:6,  
        aes(color = outlier, alpha = 0.6),  
        upper = list(continuous = wrap("points", size = 2)),  
        lower = list(continuous = wrap("points", size = 2)),  
        diag = list(continuous = wrap("densityDiag", alpha = 0.6))) +  
  scale_color_manual(values = c("Normal" = "blue", "Outlier" = "red")) +
  labs(title = "Pair Plot de Outliers Detectados (Datos Imputados)") +
  theme_minimal()