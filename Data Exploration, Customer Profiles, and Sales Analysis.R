# Cargar librerías
library(readxl)       
library(dplyr)      
library(ggplot2)    
library(skimr)        
library(DataExplorer) 
library(lubridate)


# Leer archivo Excel
datos <- read_excel("Data Proyecto Final Proce Texto.xlsx")

# Ver las primeras filas
head(datos)

# Ver estructura de los datos
str(datos)

# Ver un resumen general
summary(datos)

# Ver nombres de columnas
colnames(datos)

# Revisar valores únicos por columna
sapply(datos, function(x) length(unique(x)))

# Ver si hay valores perdidos
colSums(is.na(datos))

# Resumen completo
skim(datos)

# Crear un reporte automático de exploración
# create_report(datos)


## Perfil de los clientes

# Distribución por género
ggplot(datos, aes(x = `Genero Cliente`)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribución por Género", x = "Género", y = "Cantidad")

# Distribución por país
ggplot(datos, aes(x = `Pais Cliente`)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Clientes por País", x = "País", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Análisis de ventas

# Total de ventas por categoría de producto

ventas_por_categoria <- datos %>%
  group_by(`Categoria de producto`) %>%
  summarise(Total_Venta = sum(`Monto Venta`)) %>%
  arrange(desc(Total_Venta))

ggplot(ventas_por_categoria, aes(x = reorder(`Categoria de producto`, Total_Venta), y = Total_Venta)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Ventas por Categoría de Producto", x = "Categoría", y = "Total de Ventas")


## Relación entre variables

# Promedio de venta por ocupación
datos %>%
  group_by(`Ocupación Cliente`) %>%
  summarise(Promedio_Venta = mean(`Monto Venta`)) %>%
  ggplot(aes(x = reorder(`Ocupación Cliente`, Promedio_Venta), y = Promedio_Venta)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(title = "Promedio de Venta por Ocupación", x = "Ocupación", y = "Promedio de Venta")

## Análisis de fidelidad del cliente

clientes_frecuentes <- datos %>%
  group_by(`Nombre Cliente`) %>%
  summarise(Pedidos = n(),
            Total_Ventas = sum(`Monto Venta`)) %>%
  arrange(desc(Pedidos))


# Calcular la edad de los clientes

datos <- datos %>%
  mutate(`Edad Cliente` = as.integer(interval(ymd(`Fecha de Nacimiento`), Sys.Date()) / years(1)))

# Distribución de la Edad

ggplot(datos, aes(x = `Edad Cliente`)) +
  geom_histogram(binwidth = 5, fill = "#69b3a2", color = "black") +
  labs(title = "Distribución de Edad de los Clientes", x = "Edad", y = "Cantidad de Clientes")

# Agrupar Clientes por Rangos de Edad

datos <- datos %>%
  mutate(Rango_Edad = case_when(
    `Edad Cliente` < 20 ~ "Menores de 20",
    `Edad Cliente` >= 20 & `Edad Cliente` < 30 ~ "20s",
    `Edad Cliente` >= 30 & `Edad Cliente` < 40 ~ "30s",
    `Edad Cliente` >= 40 & `Edad Cliente` < 50 ~ "40s",
    `Edad Cliente` >= 50 ~ "50+"
  ))

# Venta Promedio por Rango de Edad

ventas_por_edad <- datos %>%
  group_by(Rango_Edad) %>%
  summarise(Venta_Promedio = mean(`Monto Venta`, na.rm = TRUE))

ggplot(ventas_por_edad, aes(x = Rango_Edad, y = Venta_Promedio, fill = Rango_Edad)) +
  geom_col() +
  labs(title = "Venta Promedio por Rango de Edad", x = "Rango de Edad", y = "Monto Promedio") +
  theme_minimal()

# Categoría de Producto vs Edad

ggplot(datos, aes(x = `Categoria de producto`, y = `Edad Cliente`)) +
  geom_boxplot(fill = "#FFA07A") +
  labs(title = "Edad de los Clientes por Categoría de Producto", x = "Categoría", y = "Edad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


