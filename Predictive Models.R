# La creación de modelos predictivos en R utilizando datos no financieros sigue el mismo enfoque general
# que cualquier otro tipo de modelado predictivo. En este ejemplo, vamos a utilizar el conjunto de datos
# mtcars para construir un modelo de regresión lineal que prediga el consumo de combustible (mpg)
# en función de otras variables del conjunto de datos.


# El objetivo principal de preparar un modelo de datos es transformar datos crudos en una forma
# que sea adecuada y óptima para el análisis, la exploración y la construcción de modelos predictivos.
# Esto implica una serie de pasos que aseguran que los datos estén limpios, estructurados y listos
# para ser utilizados en diferentes tipos de análisis.


# Preparar el entorno
# Primero, cargamos las bibliotecas necesarias y los datos.

# Instalar y cargar las bibliotecas necesarias
install.packages("tidyverse")
install.packages("caret")

library(tidyverse)
library(caret)

# Cargar el conjunto de datos mtcars
data <- mtcars #es un conjunto de datos incorporado en R que contiene 
#información sobre diferentes características de automóviles.

# Ver los primeros registros del conjunto de datos
head(data)

# Dividir los datos en conjunto de entrenamiento y prueba
# Dividimos los datos en un conjunto de entrenamiento y un conjunto de prueba
# para evaluar el rendimiento del modelo.

# Establecer una semilla para reproducibilidad. (Establecer una semilla garantiza 
# que los resultados que dependen de números aleatorios sean los mismos cada vez que 
# se ejecute el código. Esto es fundamental para la reproducibilidad y la comparación 
# entre diferentes experimentos o análisis. Sin una semilla fija, los resultados pueden 
# variar entre ejecuciones, lo que dificulta la validación y la depuración del código.)
set.seed(123)

#Esta función toma un número entero como argumento y establece el estado inicial del 
#generador de números aleatorios en ese número

# Dividir los datos en entrenamiento (80%) y prueba (20%)
trainIndex <- createDataPartition(data$mpg, p = 0.8, list = FALSE) #crea una particion
trainData <- data[trainIndex, ]#Crear conjuntos de entrenamiento y prueba
testData <- data[-trainIndex, ]

# Crear y entrenar el modelo de regresión lineal
# Construimos un modelo de regresión lineal para predecir el consumo de combustible (mpg).

# Entrenar el modelo de regresión lineal
linear_model <- lm(mpg ~ ., data = trainData) #ajuste del modelo de regresión lineal simple

# Resumen del modelo
summary(linear_model)


# Evaluar el modelo
# Evaluamos el rendimiento del modelo utilizando el conjunto de prueba.

# Realizar predicciones en el conjunto de prueba
predictions <- predict(linear_model, newdata = testData)
#Se utiliza para hacer predicciones utilizando un modelo de regresión lineal
#ajustado (linear_model) en un conjunto de datos nuevo o de prueba (testData). 

# Calcular el error medio cuadrático (MSE) y el coeficiente de determinación (R^2)
mse <- mean((predictions - testData$mpg)^2)
rsquared <- cor(predictions, testData$mpg)^2

# Imprimir los resultados
print(paste("MSE:", mse))
print(paste("R^2:", rsquared))

# Visualizar los resultados
# Visualizamos los resultados para comprender mejor el rendimiento del modelo.
# 
# Gráfico de dispersión de predicciones vs. valores reales

# Crear un gráfico de dispersión de predicciones vs. valores reales
ggplot(testData, aes(x = mpg, y = predictions)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicciones vs. Valores Reales", x = "Valores Reales (mpg)", y = "Predicciones (mpg)") +
  theme_minimal()

