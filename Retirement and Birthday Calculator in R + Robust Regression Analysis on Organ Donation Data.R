## Crear un programa (función) en Rstudio, que solicite por pantalla ingresar el nombre y la fecha de  
## nacimiento (día, mes y año) de una persona, y el programa arroje por pantalla cuantos días falta 
## para su cumpleaños, así como los años restante para su jubilación.

calcularCumpleJubilacion <- function() 
  {
  # Solicitar el nombre
  nombre <- readline(prompt = "Ingresa tu nombre: ")
  
  # Solicitar la fecha de nacimiento
  dia <- as.integer(readline(prompt = "Ingresa el día de tu nacimiento (1-31): "))
  mes <- as.integer(readline(prompt = "Ingresa el mes de tu nacimiento (1-12): "))
  anio <- as.integer(readline(prompt = "Ingresa el año de tu nacimiento (e.g., 1990): "))
  
  # Verificar si la fecha es válida
  if (!tryCatch({as.Date(paste(anio, mes, dia, sep = "-")); TRUE}, error = function(e) FALSE)) {
    cat("La fecha ingresada no es válida.\n")
    return(NULL)
  }
  
  # Fecha de nacimiento y fecha actual
  fecha_nacimiento <- as.Date(paste(anio, mes, dia, sep = "-"))
  hoy <- Sys.Date()
  
  # Calcular el próximo cumpleaños
  proximo_cumple <- as.Date(paste(format(hoy, "%Y"), mes, dia, sep = "-"))
  if (proximo_cumple < hoy) {
    proximo_cumple <- as.Date(paste(as.integer(format(hoy, "%Y")) + 1, mes, dia, sep = "-"))
  }
  dias_para_cumple <- as.integer(difftime(proximo_cumple, hoy, units = "days"))
  
  # Calcular la edad actual y los años para la jubilación
  edad_actual <- as.integer(difftime(hoy, fecha_nacimiento, units = "weeks")) %/% 52
  edad_jubilacion <- 60
  anos_para_jubilacion <- max(0, edad_jubilacion - edad_actual)
  
  # Mostrar resultados
  cat("\nResultados para", nombre, ":\n")
  cat("Faltan", dias_para_cumple, "días para tu próximo cumpleaños.\n")
  cat("Te faltan", anos_para_jubilacion, "años para jubilarte (considerando 60 años).\n")
}

# Llamar a la función
calcularCumpleJubilacion()

## Instala y carga la librería socviz, en esta librería se encuentra el conjunto de datos “organdata”, 
## carga esta base datos y realiza el siguiente ejercicio. (Para conocer el significado de las variables 
## solo ejecuta en la consola la instrucción help(organdata) y te llevara a su descriptor, es necesario 
## cargar la librería primero).

# Instalación y carga de las librerías necesarias
# install.packages("socviz")
library(socviz)
library(tseries)
library(MASS)
library(lmtest)
library(car)   
library(dplyr) 
library(ggplot2)

# Carga base de datos organdata
data("organdata")

# Descriptor datos
help(organdata)

# Verificación datos Nulos(NA)
organdata_clean <- na.omit(organdata)

# Transformación de datos: logaritmo del PIB y de la población
organdata_clean <- organdata_clean %>%
  mutate(log_gdp = log(gdp),
         log_pop = log(pop))

# Transformación de variables
organdata_clean <- organdata_clean %>%
  mutate(log_donors = log(donors + 1))

# Modelo robusto
modelo_robusto <- rlm(log_donors ~ log_gdp + log_pop, data = organdata_clean)

# Resumen modelo robusto
summary(modelo_robusto)

# Análisis de los residuales del modelo robusto
par(mfrow=c(2,2))  
plot(modelo_robusto) 

# Verificar homocedasticidad en el modelo robusto
bptest(modelo_robusto)
jbtestprueba <- jarque.bera.test(residuals(modelo_robusto))
print(jbtestprueba)

#Nota: No es necesario realizar el test de Jarque-Bera para este modelo (modelo robusto) ya que no asume normalidad de los residuos


# Relación entre PIB y tasa de donación
ggplot(organdata_clean, aes(x = log_gdp, y = log_donors)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relación entre PIB (log) y Donaciones (log)", x = "Log PIB", y = "Log Donaciones")

# Relación entre población y tasa de donación
ggplot(organdata_clean, aes(x = log_pop, y = log_donors)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relación entre Población (log) y Donaciones (log)", x = "Log Población", y = "Log Donaciones")

#Modelo de regresión normal (lm) 
modelo_lm <- lm(log_donors ~ log_gdp + log_pop, data = organdata_clean)
summary(modelo_lm)

#Análisis más completo de residuos
# Residuos vs valores ajustados
plot(modelo_robusto$fitted.values, modelo_robusto$residuals,
     main = "Residuos vs Ajustados",
     xlab = "Valores ajustados", ylab = "Residuos")
abline(h = 0, col = "red")

# Histograma de residuos
hist(modelo_robusto$residuals, breaks = 20,
     main = "Histograma de residuos",
     xlab = "Residuos")

# Medidas de influencia
influencePlot(modelo_lm)


