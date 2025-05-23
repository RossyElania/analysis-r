# Cargar y ver los datos
# Primero, cargamos el conjunto de datos mtcars y echamos un vistazo a su estructura.
library(tidyverse)
# Cargar el conjunto de datos mtcars
data <- mtcars#es un conjunto de datos incorporado en R que contiene 
#información sobre diferentes características de automóviles.

# Ver los primeros registros del conjunto de datos
head(data)

# Ver la estructura del conjunto de datos
str(data)

# Resumen estadístico del conjunto de datos
summary(data)

# Estadísticas descriptivas básicas
# Podemos calcular estadísticas descriptivas básicas para cada columna numérica en el conjunto de datos.

# Calcular estadísticas descriptivas básicas
descriptive_stats <- data %>%
  summarise(
    mpg_mean = mean(mpg), mpg_sd = sd(mpg), mpg_min = min(mpg), mpg_max = max(mpg),
    cyl_mean = mean(cyl), cyl_sd = sd(cyl), cyl_min = min(cyl), cyl_max = max(cyl),
    hp_mean = mean(hp), hp_sd = sd(hp), hp_min = min(hp), hp_max = max(hp),
    wt_mean = mean(wt), wt_sd = sd(wt), wt_min = min(wt), wt_max = max(wt)
  )

# Ver las estadísticas descriptivas
print(descriptive_stats)

# Visualización de datos
# Las visualizaciones son una parte crucial del análisis descriptivo. Utilizaremos ggplot2, 
#parte del paquete tidyverse, para crear algunas visualizaciones comunes.
# 
# Histograma

# Histograma de millas por galón (mpg)
ggplot(data, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Millas por Galón (mpg)", x = "Millas por Galón (mpg)", y = "Frecuencia") +
  theme_minimal()

#Diagrama de caja (boxplot)
# Diagrama de caja para millas por galón (mpg) según el número de cilindros (cyl)
ggplot(data, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Diagrama de Caja de Millas por Galón (mpg) por Número de Cilindros", x = "Número de Cilindros", y = "Millas por Galón (mpg)") +
  theme_minimal()

#Gráfico de dispersión
# Gráfico de dispersión de peso (wt) vs. millas por galón (mpg)
ggplot(data, aes(x = wt, y = mpg)) +
  geom_point(color = "red") +
  labs(title = "Gráfico de Dispersión de Peso vs. Millas por Galón", x = "Peso (wt)", y = "Millas por Galón (mpg)") +
  theme_minimal()
