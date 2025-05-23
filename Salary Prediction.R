# Paso 1: Crear el dataframe de ejemplo
# Primero, crearemos un dataframe con los datos de empleados:

# Cargar paquetes necesarios
install.packages("tidyverse")
install.packages("caret") 
library(tidyverse)
library(caret) 

# Para particionar los datos y evaluar el modelo
# Crear dataframe

empleados <- tibble(
  nombre = c("Ana", "Luis", "Marta", "Pedro", "Sofía", "Carlos", "María", "Jorge", "Laura", "Fernando"),
  edad = c(23, 30, 22, 34, 28, 45, 33, 29, 31, 40),
  experiencia = c(1, 8, 2, 10, 5, 20, 12, 7, 9, 15),
  ciudad = c("Madrid", "Barcelona", "Valencia", "Sevilla", "Bilbao", "Madrid", "Barcelona", "Valencia",
             "Sevilla", "Bilbao"),
  salario = c(30000, 35000, 32000, 40000, 36000, 45000, 38000, 34000, 37000, 42000)
)
empleados

empleados <- empleados %>%
mutate(ciudad = as.factor(ciudad))
empleados

set.seed(123) 
indice <- createDataPartition(empleados$salario, p = 0.8, list = FALSE)
train_data <- empleados[indice, ]
test_data <- empleados[-indice, ]
train_data
test_data

modelo <- lm(salario ~ edad + experiencia + ciudad, data = train_data)
summary(modelo)
modelo

predicciones <- predict(modelo, newdata = test_data)
predicciones

rmse <- sqrt(mean((test_data$salario - predicciones)^2)) 
r2 <- cor(test_data$salario, predicciones)^2 
rmse
r2

cat("RMSE:", rmse, "\n")
cat("R²:", r2, "\n")
rmse
r2
