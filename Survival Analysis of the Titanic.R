# Parte 1: Análisis Exploratorio de los Datos
# Cargar librerías necesarias
library(tidyverse)
library(caret)
library(randomForest)

# Cargar el conjunto de datos Titanic
titanic <- read.csv("test.csv")

# Ver estructura y resumen estadístico
str(titanic)
summary(titanic)

# Histograma de edades
ggplot(titanic, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de edades", x = "Edad", y = "Frecuencia")

# Boxplot de tarifas según clase
ggplot(titanic, aes(x = factor(Pclass), y = Fare)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Tarifa pagada según clase", x = "Clase", y = "Tarifa")

# Gráfico de barras de embarque
ggplot(titanic, aes(x = Embarked, fill = Embarked)) +
  geom_bar() +
  labs(title = "Número de pasajeros por puerto de embarque", x = "Puerto", y = "Cantidad")


# Parte 2: Ingeniería de las Características
# Ver valores faltantes
colSums(is.na(titanic))

# Rellenar valores faltantes
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm = TRUE)
titanic$Fare[is.na(titanic$Fare)] <- median(titanic$Fare, na.rm = TRUE)
titanic$Embarked[is.na(titanic$Embarked)] <- "S"

# Conversión de variables categóricas
titanic$Sex <- ifelse(titanic$Sex == "male", 1, 0)

# Convertir 'Embarked' en variables dummy
titanic <- titanic %>%
  mutate(Embarked_C = ifelse(Embarked == "C", 1, 0),
         Embarked_Q = ifelse(Embarked == "Q", 1, 0),
         Embarked_S = ifelse(Embarked == "S", 1, 0)) %>%
  select(-Embarked)  # Eliminar la columna original

# Normalización de la variable 'Fare'
titanic$Fare <- scale(titanic$Fare)

# Convertir Pclass a factor antes de dividir los datos
titanic$Pclass <- factor(titanic$Pclass)

# División de datos en 80% entrenamiento y 20% prueba
set.seed(123)
trainIndex <- createDataPartition(titanic$Pclass, p = 0.8, list = FALSE)
trainData <- titanic[trainIndex, ]
testData <- titanic[-trainIndex, ]

# Asegurar que Pclass sigue siendo factor en ambos conjuntos
trainData$Pclass <- factor(trainData$Pclass, levels = levels(titanic$Pclass))
testData$Pclass <- factor(testData$Pclass, levels = levels(titanic$Pclass))


# Parte 3: Modelos de Predicción
# Modelo Random Forest
rf_model <- randomForest(Pclass ~ Sex + Age + Fare + SibSp + Parch + Embarked_C + Embarked_Q + Embarked_S, 
                         data = trainData, ntree = 500, mtry = 3)

# Predicciones y conversión a factor con los mismos niveles
predictions_rf <- predict(rf_model, testData)
predictions_rf <- factor(predictions_rf, levels = levels(titanic$Pclass))

# Evaluación del modelo con la matriz de confusión
conf_matrix <- confusionMatrix(predictions_rf, testData$Pclass)
print(conf_matrix)
