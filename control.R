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
if (!require(VIM)) install.packages("VIM")

library(solitude)  # Isolation Forest
library(caret)     # Métricas de clasificación
library(dplyr)     # Manipulación de datos
library(gridExtra) # Organizar gráficos
library(ggplot2)   # Visualización de gráficos
library(corrplot)  # Heatmap de correlaciones
library(GGally)    # Pair plots
library(VIM)       # KNN para imputación

# --------------------------- #
# Cargar y preparar los datos
# --------------------------- #
df <- read.csv(file.choose(), sep = ",")
head(df)

# Descripciones estadísticas
summary(df)


print("Estructura del dataset:")
str(df)       print("Resumen estadístico del dataset:")
summary(df)     print("Valores nulos por columna:")
colSums(is.na(df))


# --------------------------- #
# Análisis exploratorio inicial
# --------------------------- #
columns_to_plot <- colnames(df)[1:(ncol(df))]
plots <- list()

# Diagramas de cajas
for (col in columns_to_plot) {
  p <- ggplot(df, aes_string(y = col)) +
    geom_boxplot(fill = "steelblue", color = "black") +
    labs(title = col, y = "") +
    theme_minimal()
  plots[[col]] <- p
}

do.call(grid.arrange, c(plots, ncol = 3))

# Histograma de variables numéricas
df %>%
  select_if(is.numeric) %>%
  pivot_longer(everything(), names_to = "key", values_to = "value") %>%
  ggplot(aes(value)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ key, scales = 'free') + 
  theme_minimal() +
  labs(title = "Distribución de las Variables Numéricas", x = "Valor", y = "Frecuencia")

# --------------------------- #
# Aplicar Isolation Forest
# --------------------------- #
# Escalar los datos numéricos
X_scaled <- scale(df %>% select_if(is.numeric))

# Modelo Isolation Forest
iso_model <- isolationForest$new(sample_size = nrow(X_scaled), num_trees = 100, seed = 42)
iso_model$fit(X_scaled)

# Predecir outliers y etiquetar
predictions <- iso_model$predict(X_scaled)
df$outlier <- ifelse(predictions$anomaly_score > quantile(predictions$anomaly_score, 0.8), "Outlier", "Normal")

# Visualización: Pair Plot inicial
ggpairs(df, 
        columns = 1:6,  
        aes(color = outlier, alpha = 0.6),  
        upper = list(continuous = wrap("points", size = 2)),  
        lower = list(continuous = wrap("points", size = 2)),  
        diag = list(continuous = wrap("densityDiag", alpha = 0.6))) +  
  scale_color_manual(values = c("Normal" = "blue", "Outlier" = "red")) +
  labs(title = "Pair Plot de Outliers Detectados por Isolation Forest") +
  theme_minimal()

# --------------------------- #
# Imputación de valores faltantes
# --------------------------- #
# Reemplazar 0 por NA en columnas seleccionadas
cols_to_impute <- c("Glucosa", "Presion_Arterial", "Grosor_Piel", "Insulina", "IMC")
df[cols_to_impute] <- lapply(df[cols_to_impute], function(x) ifelse(x == 0, NA, x))

# Imputar valores faltantes con KNN
df_imputed <- kNN(df, variable = cols_to_impute)

# Verificar imputación
summary(df_imputed)

# --------------------------- #
# Nuevo análisis exploratorio
# --------------------------- #
# Repetir diagramas de cajas
columns_to_plot <- colnames(df_imputed)[1:(ncol(df_imputed))] # Cambiar si se elimina 'Outcome'
plots <- list()
for (col in columns_to_plot) {
  p <- ggplot(df_imputed, aes_string(y = col)) +
    geom_boxplot(fill = "steelblue", color = "black") +
    labs(title = col, y = "") +
    theme_minimal()
  plots[[col]] <- p
}

do.call(grid.arrange, c(plots, ncol = 3))

### implementar histograma


df_imputed %>%
  select_if(is.numeric) %>%
  pivot_longer(everything(), names_to = "key", values_to = "value") %>%
  ggplot(aes(value)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ key, scales = 'free') + 
  theme_minimal() +
  labs(title = "Distribución de las Variables Numéricas", x = "Valor", y = "Frecuencia")


print("Estructura del dataset:")
str(df_imputed)       print("Resumen estadístico del dataset:")
summary(df_imputed)     print("Valores nulos por columna:")
colSums(is.na(df_imputed))






# --------------------------- #
# Nueva aplicación de Isolation Forest
# --------------------------- #
X_scaled_new <- scale(df_imputed %>% select_if(is.numeric))

iso_model_new <- isolationForest$new(sample_size = nrow(X_scaled_new), num_trees = 100, seed = 42)
iso_model_new$fit(X_scaled_new)

predictions_new <- iso_model_new$predict(X_scaled_new)
df_imputed$outlier <- ifelse(predictions_new$anomaly_score > quantile(predictions_new$anomaly_score, 0.8), "Outlier", "Normal")

# --------------------------- #
# Visualización final
# --------------------------- #
# Pair Plot después de la imputación
ggpairs(df_imputed, 
        columns = 1:6,  
        aes(color = outlier, alpha = 0.6),  
        upper = list(continuous = wrap("points", size = 2)),  
        lower = list(continuous = wrap("points", size = 2)),  
        diag = list(continuous = wrap("densityDiag", alpha = 0.6))) +  
  scale_color_manual(values = c("Normal" = "blue", "Outlier" = "red")) +
  labs(title = "Pair Plot de Outliers Detectados (Datos Imputados)") +
  theme_minimal()





















