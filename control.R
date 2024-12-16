# --------------------------- #
# Instalación y carga de librerías
# --------------------------- #
if (!require(solitude)) install.packages("solitude", dependencies = TRUE)
if (!require(caret)) install.packages("caret")
if (!require(dplyr)) install.packages("dplyr")
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(corrplot)) install.packages("corrplot")
if (!require(GGally)) install.packages("GGally")

library(solitude)  # Isolation Forest
library(caret)     # Métricas de clasificación
library(dplyr)     # Manipulación de datos
library(gridExtra) # Organizar gráficos
library(ggplot2)   # Visualización de gráficos
library(corrplot)  # Heatmap de correlaciones
library(GGally)    # Pair plots

# --------------------------- #
# Cargar y preparar los datos
# --------------------------- #
df <- read.csv(file.choose(), sep = ",")
head(df)

# Seleccionar columnas numéricas excluyendo 'Outcome'
df_diabetes <- df %>% select(-Outcome)

# Escalar los datos
X_scaled <- as.data.frame(scale(df_diabetes))

# --------------------------- #
# Aplicar Isolation Forest
# --------------------------- #
iso_model <- isolationForest$new(sample_size = nrow(X_scaled), num_trees = 100, seed = 42)
iso_model$fit(X_scaled)

# Predecir outliers y agregar columna
predictions <- iso_model$predict(X_scaled)
df$outlier <- ifelse(predictions$anomaly_score > quantile(predictions$anomaly_score, 0.8), -1, 1)
df$predicted_outlier <- ifelse(df$outlier == -1, 1, 0)  # Convertir para métricas

# --------------------------- #
# Evaluar el modelo
# --------------------------- #
conf_matrix <- confusionMatrix(as.factor(df$predicted_outlier), as.factor(df$Outcome), positive = "1")
print("Matriz de Confusión:")
print(conf_matrix$table)

print("Reporte de Clasificación:")
print(conf_matrix)

# --------------------------- #
# Visualización: Boxplots
# --------------------------- #
columns_to_plot <- colnames(df)[1:(ncol(df) - 3)]  # Excluir columnas adicionales

plots <- lapply(columns_to_plot, function(col) {
  ggplot(df, aes_string(y = col)) +
    geom_boxplot(fill = "steelblue", color = "black") +
    labs(title = col, y = "") +
    theme_minimal()
})

do.call(grid.arrange, c(plots, ncol = 3))  # Mostrar en cuadrícula

# --------------------------- #
# Visualización: Heatmap de Correlación
# --------------------------- #
correlation_matrix <- cor(df[, sapply(df, is.numeric)], use = "pairwise.complete.obs")

corrplot(correlation_matrix, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         addCoef.col = "black", number.digits = 2, 
         tl.col = "black", tl.srt = 45)
title("Matriz de Correlación - Heatmap")

# --------------------------- #
# Visualización: Pair Plot
# --------------------------- #
df$outlier <- as.factor(ifelse(df$outlier == -1, "Outlier", "Normal"))

ggpairs(df, 
        columns = 1:6,  
        aes(color = outlier, alpha = 0.6),  
        upper = list(continuous = wrap("points", size = 2)),  
        lower = list(continuous = wrap("points", size = 2)),  
        diag = list(continuous = wrap("densityDiag", alpha = 0.6))) +  
  scale_color_manual(values = c("Normal" = "blue", "Outlier" = "red")) +
  labs(title = "Pair Plot de Outliers Detectados por Isolation Forest") +
  theme_minimal()