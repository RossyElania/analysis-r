# 1. Asignación de Variables y Tipos de Datos
# Práctica 1:

#  1. Crea tres variables en R:
#    o Una con un número decimal: 45.8.
#    o Otra con un texto: "Ciencia de Datos".
#    o Otra con un valor lógico: FALSE.

numeroDecimal  <- 45.8
vTexto  <- "Ciencia de Datos"
valorLogico  <- FALSE

numeroDecimal
vTexto
valorLogico

#  2. Usa print(class()) para mostrar el tipo de dato de cada variable.

print(class(numeroDecimal))
print(class(vTexto))
print(class(valorLogico))

#  3. Convierte el valor 100 a carácter y "75" a número.

numero <- 100
vTexto <- "75"

as.character(numero)
as.numeric(vTexto)

numero
vTexto

# 2. Vectores y Operaciones Básicas
# Práctica 2:

#  1. Crea un vector llamado notas con los valores: 72, 88, 94, 67, 80.
notas <- c(72, 88, 94, 67, 80)
notas

#  2. Extrae el cuarto elemento del vector.
notas[4]

#  3. Resta 3 puntos a cada nota del vector.
notasMenosTres <- notas - 3
notasMenosTres

#  4. Crea un segundo vector con los valores: 15, 30, 45, 60, 75.
segundoVector <- c(15, 30, 45, 60, 75)
segundoVector

#  5. Realiza la multiplicación elemento a elemento entre ambos vectores.
multiplicacion <- notasMenosTres * segundoVector
multiplicacion



# 3. Matrices y Acceso a Datos
# Práctica 3:
  
#  1. Crea una matriz de 3x4 con valores del 10 al 21.
matriz <- matrix(10:21, nrow = 3, ncol = 4)
matriz

#  2. Extrae el valor de la primera fila y segunda columna.
matriz [1,2]
  
#  3. Obtén solo la tercera fila de la matriz.
matriz [3,]

#  4. Divide todos los valores de la matriz por 2
divisionMatriz <- matriz / 2
divisionMatriz


# 4. Data Frames y Filtrado de Datos
# Práctica 4:

#  1. Crea un data frame con los siguientes datos:
#        Ejercicios en R
#    Nombre Edad Ciudad Salario
#    Laura 27 Bogotá 2800
#    Pedro 35 Lima 3400
#    Sofía 24 Quito 2200
#    Andrés 29 Santiago 3100
#    Elena 40 Buenos Aires 4500

df <- data.frame(
     Nombre = c("Laura","Pedro","Sofía","Andrés","Elena"),
     Edad = c(27, 35, 24, 29, 40),
     Ciudad = c("Bogotá", "Lima", "Quito", "Santiago", "Buenos Aires"),
     Salario = c(2800, 3400, 2200, 3100, 4500)
)

df
#  2. Accede a la columna Ciudad.
df[3]

#  3. Filtra los empleados con edad menor a 30.
df[df$Edad < 30, ]

#  4. Agrega una nueva columna llamada Descuento que sea el 5% del salario.
dfDescuento <- df$Salario * 0.05

dfSueldoNeto <- df$Salario - dfDescuento
df


# 5. Lectura y Escritura de Archivos CSV
# Práctica 5:

#  1. Guarda el data frame anterior en un archivo CSV llamado empleados_nuevos.csv.
write.csv(df, "empleados_nuevos.csv", row.names = FALSE)

#  2. Luego, lee el archivo CSV y almacénalo en un nuevo data frame llamado df_empleados.
df_empleados <- read.csv("empleados_nuevos.csv")

#  3. Muestra las últimas 5 filas del data frame leído.

tail(df_empleados, 5)



# 6. Estadísticas Descriptivas y Agregación
# Práctica 6:

#  1. Usa el data frame df_empleados y calcula:

#       o La media de la columna Edad.
media_edad <- mean(df_empleados$Edad)
media_edad

#       o La mediana de la columna Salario.
mediana_salario <- median(df_empleados$Salario)
mediana_salario

#       o La desviación estándar de Edad.
desviacion_estandar_edad <- sd(df_empleados$Edad)
desviacion_estandar_edad

#       o La tabla de frecuencia de Ciudad.
tabla_frecuencia_ciudad <- table(df_empleados$Ciudad)
tabla_frecuencia_ciudad




# 7. Visualización de Datos
# Práctica 7:

#    1. Crea un gráfico de dispersión con los siguientes datos:
x <- c(5, 15, 25, 35, 45)
y <- c(8, 18, 28, 38, 48)

plot(x, y, main = "Gráfico de Dispersión", 
     xlab = "Valor de X", ylab = "Valor de Y", 
     pch = 10, col = "red")

#    2. Crea un gráfico de barras con los valores de ventas por producto:
ventas <- data.frame(
  Producto = c("Monitor", "Teclado", "Ratón"),
  Cantidad = c(20, 40, 30)
)

barplot(ventas$Cantidad, 
        names.arg = ventas$Producto, 
        main = "Ventas por Producto :)", 
        xlab = "Producto", 
        ylab = "Cantidad Vendida", 
        col = "pink"
        )

#    3. Genera un histograma de datos aleatorios con rnorm(120, mean = 55, sd = 12).
Histograma <- rnorm(120, mean = 55, sd = 12)

hist(Histograma, 
     main = "Histograma", 
     xlab = "Valor", 
     ylab = "Frecuencia", 
     col = "yellow")



# 8. Uso de dplyr para Manipulación de Datos
# Práctica 8:

#    1. Carga la librería dplyr.
install.packages("dplyr")
library(dplyr)
library(magrittr)

#    2. Usa el dataset mtcars y realiza lo siguiente:

#       o Filtra los autos con más de 150 HP y 8 cilindros.
autos_filtrados <- mtcars %>%
  filter(hp > 150, cyl == 8)
autos_filtrados

#       o Extrae solo las columnas mpg, cyl y hp.
autos_extraidos <- mtcars %>%
  select(mpg, cyl, hp)
autos_extraidos

#       o Crea una nueva columna relación_hp_cyl que sea hp / cyl.
autos_con_relacion <- mtcars %>%
  mutate(relacion_hp_cyl = hp / cyl)
autos_con_relacion

#       o Calcula el promedio de hp agrupado por gear
promedio_hp_por_gear <- mtcars %>%
  group_by(gear) %>%
  summarise(promedio_hp = mean(hp))
promedio_hp_por_gear


