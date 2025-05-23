####################################################
###Practica No. 4 Funciones e Iteracion#############
####################################################

#Importa el archivo adjunto "mu_enoe_VF.xlsx" y realizar:
library(readxl)
datos <- read_excel("mu_enoe_VF.xlsx")


#1) crear un bucle que itere por la columna de ingresos_mensual 
#del dataset he imprima por pantalla la siguiente frase:
#"El logaritmo", X , "el cual est? en la posici?n", X, "es igual a:" Y

#Nota: [1] Recordar que no existe logaritmos para valores negativos (Usar tryCatch)
# [2] "X" es el valor/indice en cada observaci?n
# [3] "Y" es el resutlado de aplicar el logatimo a cada observacion

# Por ejemplo:
#El logatimo de 8000 el cual est? en la posici?n 10280 es igual a: 8.98719682066197

if ("ingreso_mensual" %in% names(datos)) {
  for (i in 1:nrow(datos)) {
    valor <- datos$ingreso_mensual[i]
    
    tryCatch({
      if (!is.na(valor) && is.numeric(valor) && valor > 0) {
        logaritmo <- log(valor)
        cat("El logaritmo de", valor, "el cual está en la posición", i, "es igual a:", logaritmo, "\n")
      }
    })
  }
} else {
  cat("No se encontró la columna 'ingreso_mensual'. Revisa el nombre exacto.\n")
}


#2)Crear una funci?n que me retorne la suma total de los logaritmos naturales de los valores del dataset (Todas las columas y filas)

#Nota:
#No existe valores logaritmos para numeros negativos y ceros (manejar esto, sea creativo)
#Debe utilizar recursividad (bucles) para crear esta funcion

# Verificamos que exista la columna correctamente

sumar_logaritmos <- function(data, fila = 1, columna = 1, suma = 0) {
  if (fila > nrow(data)) {
    return(suma)
  }
  
  valor <- data[[fila, columna]]
  
  if (!is.na(valor) && is.numeric(valor) && valor > 0) {
    suma <- suma + log(valor)
  }
  
  if (columna < ncol(data)) {
    return(sumar_logaritmos(data, fila, columna + 1, suma))
  } else {
    return(sumar_logaritmos(data, fila + 1, 1, suma))
  }
}

suma_total <- sumar_logaritmos(datos)
cat("La suma total de los logaritmos naturales es:", suma_total, "\n")





