library(ggplot2)
library(dplyr)
library(DataExplorer)
library(readr)
library(readxl)

head(iris)
tail(iris)

# 150 = N° Filas, 5 = N° Columnas
dim(iris)

colnames(iris)
colnames(iris) <- c("Longitud del sepalo", "Ancho del sepalo", "Longitud del petalo", "Ancho del petalo", "Especie")

colnames(iris)
str(iris) 

# Una columna, en específico
iris$"Ancho del sepalo"

# Varias columnas
head(iris[c("Ancho del sepalo","Longitud del sepalo")])

# Una fila
iris[1,]

# Varias filas
iris[75:80,]

# Un elemento
iris[10,"Especie"]

# Varios elementos
iris[30:37, c("Longitud del petalo", "Ancho del petalo")]

iris$id <- c(1:150)
head(iris)

iris$id <- NULL
# Eliminando la columna que agregamos
head(iris)

nuevafila <- c(3.5, 2.5, 2.2, 1.5, "setosa")
iris1 <- rbind(iris, nuevafila)
# La nueva fila se coloca al final
tail(iris1)

# Eliminando la fila que agregamos
tail(iris1[-151,])

summary(iris)

iris$`Longitud del sepalo` <- as.numeric(iris$`Longitud del sepalo`)
iris$`Ancho del sepalo` <- as.numeric(iris$`Ancho del sepalo`)
iris$`Longitud del petalo` <- as.numeric(iris$`Longitud del petalo`)
iris$`Ancho del petalo` <- as.numeric(iris$`Ancho del petalo`)

summary(iris)

