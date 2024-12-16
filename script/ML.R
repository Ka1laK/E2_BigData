# -------------------------------- #
# Cargar librerías necesarias
# -------------------------------- #
if (!require(randomForest)) install.packages("randomForest", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)

library(randomForest)
library(dplyr)

# -------------------------------- #
# Leer el dataset
# -------------------------------- #
df <- read.csv(file.choose(), sep = ",")


# -------------------------------- #
# Generar datos de entrenamiento
# -------------------------------- #
set.seed(123)
# Crear una columna ficticia 'resultado' solo para entrenamiento
df$resultado <- sample(c(0, 1), nrow(df), replace = TRUE)

# Dividir datos en entrenamiento y prueba
trainIndex <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

# -------------------------------- #
# Entrenar el modelo Random Forest
# -------------------------------- #
set.seed(123)
modelo_rf <- randomForest(as.factor(resultado) ~ ., 
                          data = train_data, 
                          ntree = 500)

# Predecir los resultados en los datos originales
predicciones <- predict(modelo_rf, newdata = df)

# -------------------------------- #
# Crear el dataframe final
# -------------------------------- #
# Agregar la columna 'resultado' al dataset original
df_final <- df %>% 
  select(-resultado) %>%           # Eliminar columna temporal 'resultado' usada para entrenar
  mutate(resultado = as.numeric(as.character(predicciones)))

# -------------------------------- #
# Guardar el resultado final
# -------------------------------- #
write.csv(df_final, "df_final_con_resultado.csv", row.names = FALSE)

# Mostrar las primeras filas del dataset final
df_final
