# La manipulación de datos en R es una habilidad fundamental para cualquier análisis de datos.
# Guía paso a paso sobre cómo manipular datos no financierosb. 
#Utilizaremos algunas bibliotecas populares como tidyverse para realizar estas tareas.

# Instalar y cargar las bibliotecas necesarias
# Primero, asegúrate de tener instaladas las bibliotecas necesarias.
# Podemos instalar las bibliotecas usando install.packages() si no las tienes ya.

install.packages("tidyverse")

#Luego, cargamos las bibliotecas.

library(tidyverse)

# Cargar y ver los datos
# Para este ejemplo, utilizaremos el conjunto de datos integrado de mtcars que viene con R.
# Sin embargo, puedes cargar tus propios datos usando funciones como read.csv() para archivos CSV.

# Cargar el conjunto de datos mtcars
data <- mtcars

# Ver los primeros registros del conjunto de datos
head(data)

# Ver estructura del conjunto de datos
str(data)

# Seleccionar y filtrar datos
# Podemos seleccionar ciertas columnas y filtrar filas basadas en condiciones específicas.
# 
# Selección de columnas


# Seleccionar columnas específicas
selected_data <- data %>% select(mpg, cyl, hp, wt)

# Ver los primeros registros del conjunto de datos seleccionado
head(selected_data)

#Filtrado de filas
# Filtrar filas donde cilindrada (cyl) es igual a 6. %>% lo utilizo para encadenar
filtered_data <- data %>% filter(cyl == 6)

# Ver los primeros registros del conjunto de datos filtrado
head(filtered_data)

# Crear nuevas variables
# Podemos crear nuevas columnas basadas en cálculos con las columnas existentes.

# Crear una nueva columna que calcula la relación peso/potencia
data <- data %>% mutate(weight_to_hp = wt / hp)

# Ver los primeros registros del conjunto de datos con la nueva columna
head(data)

# Resumir y agrupar datos
# Podemos calcular estadísticas resumen y agrupar los datos por una o más variables.

#Estadísticas resumen

# Calcular estadísticas resumen para las millas por galón (mpg)
summary_stats <- data %>% summarise(mean_mpg = mean(mpg), sd_mpg = sd(mpg), min_mpg = min(mpg), max_mpg = max(mpg))

resumen_estadistica <- c(mean_mpg = mean(data$mpg), sd_mpg = sd(data$mpg), 
                         min_mpg = min(data$mpg), max_mpg = max(data$mpg))  

resumen_estadistica

# Ver las estadísticas resumen
print(summary_stats)

#Agrupación y resumen
# Agrupar los datos por el número de cilindros y calcular el promedio de mpg y hp
grouped_stats <- data %>% group_by(cyl) %>% summarise(mean_mpg = mean(mpg), mean_hp = mean(hp))

# Ver las estadísticas agrupadas
print(grouped_stats)

# Ordenar datos
# Podemos ordenar los datos basándonos en una o más columnas.


# Ordenar los datos por millas por galón (mpg) en orden descendente
sorted_data <- data %>% arrange(desc(mpg))



# Ver los primeros registros del conjunto de datos ordenado
head(sorted_data)


# Manejo de datos faltantes
# En caso de que tus datos contengan valores faltantes, puedes usar tidyverse para manejarlos.
#Identificación de valores faltantes

# Crear un conjunto de datos con valores faltantes para el ejemplo
data_with_na <- data
data_with_na[1, "mpg"] <- NA
data_with_na[5, "hp"] <- NA

# Verificar cuántos valores faltantes hay en cada columna
na_summary <- data_with_na %>% summarise_all(~ sum(is.na(.)))

# Ver el resumen de valores faltantes
print(na_summary)

#Eliminación de filas con valores faltantes
# Eliminar filas con cualquier valor faltante
cleaned_data <- data_with_na %>% drop_na()

# Ver los primeros registros del conjunto de datos limpiado
head(cleaned_data)

#Rellenar valores faltantes
# Rellenar valores faltantes con la media de la columna correspondiente
data_filled <- data_with_na %>% 
  mutate(mpg = ifelse(is.na(mpg), mean(mpg, na.rm = TRUE), mpg),
         hp = ifelse(is.na(hp), mean(hp, na.rm = TRUE), hp))

# Ver los primeros registros del conjunto de datos con valores rellenados
head(data_filled)

