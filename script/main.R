# Instalar y cargar librer�as necesarias
# if (!require(solitude)) install.packages("solitude", dependencies = TRUE)
# if (!require(caret)) install.packages("caret")
# if (!require(dplyr)) install.packages("dplyr")

# Cargar las librer�as
library(solitude)  # Para Isolation Forest
library(caret)     # Para validaci�n cruzada
library(dplyr)     # Para manipulaci�n de datos



# Crear una funci�n de evaluaci�n personalizada
custom_scorer <- function(model, data) {
  # Predecir puntajes de anomal�a
  predictions <- model$predict(data)
  # Devolver el puntaje promedio (negativo porque caret maximiza)
  return(-mean(predictions$anomaly_score))
}


# Cargar los datos (reemplaza con tus datos reales)
df <- read.csv(file.choose(), sep = ",")
head(df)

# Seleccionar variables de inter�s (por ejemplo, Glucosa y BMI)
data <- df %>% select(Glucose, BMI)

# Definir el rango de hiperpar�metros a evaluar
contamination_values <- c(0.05, 0.1, 0.15, 0.2)

# Configurar la validaci�n cruzada
cv_folds <- createFolds(1:nrow(data), k = 5, list = TRUE)

# Inicializar un data frame para almacenar resultados
results <- data.frame(contamination = double(), score = double())



#####################


# Loop para evaluar cada valor de contamination
for (c in contamination_values) {
  
  scores <- c()  # Almacenar puntajes por fold
  
  # Realizar validaci�n cruzada
  for (fold in cv_folds) {
    train_data <- data[-fold, ]
    test_data <- data[fold, ]
    
    # Entrenar Isolation Forest
    iso_model <- isolationForest$new(sample_size = nrow(train_data), num_trees = 100)
    iso_model$fit(train_data)
    
    # Evaluar el modelo en los datos de prueba
    score <- custom_scorer(iso_model, test_data)
    scores <- c(scores, score)
  }
  
  # Calcular el puntaje promedio
  avg_score <- mean(scores)
  
  # Guardar los resultados
  results <- rbind(results, data.frame(contamination = c, score = avg_score))
}

# Seleccionar el mejor valor de contamination
best_contamination <- results$contamination[which.min(results$score)]

# Mostrar los resultados
print("Resultados de la validaci�n cruzada:")
print(results)

cat("Mejor valor de contaminaci�n:", best_contamination, "\n")




#################################################### parte 2
if (!require(gridExtra)) install.packages("gridExtra")

library(ggplot2)
library(gridExtra)
library(dplyr)



# Seleccionar todas las columnas excepto la �ltima
columns_to_plot <- colnames(df)[1:(ncol(df) - 1)]

# Crear una lista para almacenar los gr�ficos
plots <- list()

# Generar boxplots en un bucle
for (col in columns_to_plot) {
  p <- ggplot(df, aes_string(y = col)) +
    geom_boxplot(fill = "steelblue", color = "black") +
    labs(title = col, y = "") +
    theme_minimal()
  plots[[col]] <- p
}

##### analisis exploratorio before 
# Mostrar los gr�ficos en una cuadr�cula
do.call(grid.arrange, c(plots, ncol = 3)) #### antes



#########################################################
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)





# Calcular la matriz de correlaci�n
correlation_matrix <- cor(df[, sapply(df, is.numeric)], use = "pairwise.complete.obs")

# Visualizar la matriz de correlaci�n como un heatmap
corrplot(correlation_matrix, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         addCoef.col = "black", number.digits = 2, 
         tl.col = "black", tl.srt = 45)

# Agregar t�tulo
title("Matriz de Correlaci�n - Heatmap")



##########################################################

library(solitude)  # Para Isolation Forest
library(dplyr)     # Para manipulaci�n�de�datos


# Seleccionar solo columnas num�ricas excluyendo 'Outcome'
df_diabetes <- df %>% select(-Outcome)

# Escalar los datos
X_scaled <- as.data.frame(scale(df_diabetes))

# Mostrar las primeras filas de los datos escalados
head(X_scaled)





# Inicializar el modelo Isolation Forest
iso_model <- isolationForest$new(sample_size = nrow(X_scaled), num_trees = 100, seed = 42)

# Ajustar el modelo a los datos escalados
iso_model$fit(X_scaled)

# Predecir los outliers
predictions <- iso_model$predict(X_scaled)

# Agregar las predicciones como nueva columna en el dataframe original
df$outlier <- ifelse(predictions$anomaly_score > quantile(predictions$anomaly_score, 0.8), -1, 1)

# Mostrar el dataframe con la columna 'outlier'
head(df)


###########################
library(caret)


# Convertir etiquetas del Isolation Forest
df$predicted_outlier <- ifelse(df$outlier == -1, 1, 0)

# Ver las primeras filas para confirmar
head(df)




# Generar la matriz de confusi�n
conf_matrix <- confusionMatrix(as.factor(df$predicted_outlier), as.factor(df$Outcome), positive = "1")

# Mostrar la matriz de confusi�n
print("Matriz de Confusi�n:")
print(conf_matrix$table)

# Mostrar las m�tricas de evaluaci�n
print("Reporte de Clasificaci�n:")
print(conf_matrix)





#################################
if (!require(GGally)) install.packages("GGally")

library(GGally)     # Para pair plots
library(ggplot2)    # Gr�ficos
library(dplyr)      # Manipulaci�n�de�datos






# Convertir la columna 'outlier' a factor para colores
df$outlier <- as.factor(ifelse(df$outlier == -1, "Outlier", "Normal"))

# Mostrar las primeras filas para verificar
head(df)


# Crear un pair plot con las variables num�ricas
ggpairs(df, 
        columns = 1:6,  # Seleccionar las columnas num�ricas
        aes(color = outlier, alpha = 0.6),  # Colorear seg�n 'outlier'
        upper = list(continuous = wrap("points", size = 2)),  # Gr�ficos superiores
        lower = list(continuous = wrap("points", size = 2)),  # Gr�ficos inferiores
        diag = list(continuous = wrap("densityDiag", alpha = 0.6))) +  # Gr�ficos de densidad en la diagonal
  scale_color_manual(values = c("Normal" = "blue", "Outlier" = "red")) +
  labs(title = "Pair Plot de Outliers Detectados por Isolation Forest") +
  theme_minimal()





