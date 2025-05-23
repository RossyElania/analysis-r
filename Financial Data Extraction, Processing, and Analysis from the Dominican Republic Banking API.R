# Importar librerias necesarias

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(scales)
library(stringr) 
library(dplyr)
library(ggplot2)
# library(dotenv)



# Cargar la clave API desde un archivo .env
# Nota: Este script asume que posees un archivo .env con las credenciales de tu API. Esto es por motivos de
# seguridad de tu clave privada. Sin embargo, puedes modificar el codigo facilmente si quieres introducir directamente
# la clave en el script.

# dotenv::load_dot_env()
# Sys.getenv(

api_key <- "a068b3e4f33244c583a208475c9c6465"  #Sustituir con tu API Key


# Establecer headers para el request

headers <- add_headers(
  'Ocp-Apim-Subscription-Key' = api_key,
  'User-Agent' = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
)

# Establecer la URL base (o raiz) a partir de la cual se construyen los endpoints y las diferentes consultas

base_url <- "https://apis.sb.gob.do/estadisticas/v2/"

# Definimos la funcion para consultar los datos

obtener_datos_financieros_rd <- function(end_point, params) {
  
  ############################### DOC STRING #####################################
  # Obtiene datos financieros desde una API REST y devuelve un DataFrame de pandas.
  # 
  # Esta función realiza múltiples solicitudes a un endpoint de la API de 
  # datos públicos de la Superintendencia de Bancos de la República Dominicana 
  # para recuperar datos financieros, paginando a través de los resultados. 
  # Los datos se recopilan en un DataFrame de pandas que se devuelve al finalizar.
  # 
  # Parámetros
  # ----------
  #   end_point : str
  # El endpoint de la API que se utilizará para la solicitud de datos.
  # 
  # params : str
  # Filtros a ser aplicados en la petición al endpoint
  # 
  # Returns
  # -------
  #   data.frame
  # Un DataFrame que contiene los datos financieros obtenidos 
  # del API.
  # 
  # 
  # Ejemplo de uso
  # ------
  # params <- list(
  # periodoInicial = "2022-01",  # Fecha Inicial
  # periodoFinal = "2022-02",
  # tipoEntidad = "BM",
  # paginas = 1,
  # registros = 150  
  # )
  #   obtener_datos_financieros_rd('indicadores/morosidad-estresada',params)
  # 
  # Notes
  # -----
  #   - La función gestiona la paginación automáticamente, asegurándose de que 
  # se recuperen todos los registros disponibles en el rango de fechas especificado.
  # - Se asume que la respuesta de la API incluye un encabezado `x-pagination` 
  # con metadatos que indican si hay más páginas de datos disponibles.
  
  ##################################################################################
  
  next_page <- TRUE
  registros_list <- list()
  
  while (next_page) {
    # Concatenar la base de URL con el endpoint especificado
    url <- paste0(base_url, end_point)
    
    # Añadir los parámetros de consulta
    url <- modify_url(url, query = params)
    
    tryCatch({
      # Hacer la solicitud
      response <- GET(url, headers)
      
      # Verificar si la solicitud fue exitosa
      stop_for_status(response)
      
      # Parsear la respuesta JSON
      datos <- fromJSON(content(response, "text"), flatten = TRUE)
      
      # Agregar los datos a una lista de DataFrames
      registros_list <- append(registros_list, list(as.data.frame(datos)))
      
      # Obtener metadatos de la respuesta
      metadatos <- fromJSON(response$headers[['x-pagination']])
      next_page <- metadatos$HasNext
      total_pages <- metadatos$TotalPages
      total_record <- metadatos$TotalRecords
      
      # Imprimir la página actual y el total de páginas
      print(paste('Página:', params['paginas'], '/', total_pages))
      
      # Incrementar 1 para la siguiente página
      params['paginas'] <- as.numeric(params['paginas']) + 1
      
    }, error = function(e) {
      print(paste("Ha ocurrido un error:", e))
      stop(e$message) 
      # next_page <- FALSE
    })
  }
  
  # Combinar los registros en un solo DataFrame
  registros_finales <- bind_rows(registros_list)
  
  return(registros_finales)
}



# Definir los parámetros
params <- list(
  periodoInicial = "2022-01",  # Fecha Inicial
  periodoFinal = "2023-12",
  tipoEntidad = "BM",
  paginas = 1,
  registros = 100  
)

# Llamar a la función
Bancos_multiples <- obtener_datos_financieros_rd("indicadores/financieros", params)

# Guardar datos en .csv
write.csv(resultados, "consulta_api.csv", row.names = FALSE)

print(Bancos_multiples)


# Cargar paquetes necesarios
library(tidyverse)
library(lubridate)
library(scales)
library(stringr) 
library(dplyr)
library(ggplot2)

# Vista general de los datos
head(Bancos_multiples)
str(Bancos_multiples)
summary(Bancos_multiples)
colSums(is.na(Bancos_multiples))

# Asegurar que las columnas estén en el tipo correcto
Bancos_multiples$valor <- as.numeric(Bancos_multiples$valor)
Bancos_multiples$periodo <- as.Date(paste0(Bancos_multiples$periodo, "-01"))

# 1. Evolución de la cartera de crédito total
cartera_total <- Bancos_multiples %>%
  filter(str_detect(indicador, regex("Cartera.*Total", ignore_case = TRUE))) %>%
  group_by(periodo) %>%
  summarise(total = sum(valor, na.rm = TRUE), .groups = "drop")

ggplot(cartera_total, aes(x = periodo, y = total)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Evolución de la Cartera de Crédito Total", y = "Monto (RD$)", x = "Mes") +
  scale_y_continuous(labels = comma)

# 2. Evolución de carteras por tipo
tipos <- c("Hipotecario", "Consumo", "Tarjeta", "Comercial")

df_tipos <- Bancos_multiples %>%
  filter(str_detect(indicador, regex("cartera.*", ignore_case = TRUE))) %>%
  filter(str_detect(indicador, regex(paste(tipos, collapse = "|"), ignore_case = TRUE))) %>%
  mutate(tipo = case_when(
    str_detect(indicador, regex("consumo", ignore_case = TRUE)) ~ "Consumo",
    str_detect(indicador, regex("comercial", ignore_case = TRUE)) ~ "Comercial",
    str_detect(indicador, regex("hipotecario", ignore_case = TRUE)) ~ "Hipotecario",
    TRUE ~ NA_character_
  )) %>%
  group_by(periodo, tipo) %>%
  summarise(total = sum(valor, na.rm = TRUE), .groups = "drop")

ggplot(df_tipos, aes(x = periodo, y = total, color = tipo)) +
  geom_line() +
  labs(title = "Evolución por tipo de Cartera", y = "Monto (RD$)")

# 3. Cantidad de entidades
n_entidades <- Bancos_multiples %>%
  filter(entidad != "TODOS") %>%
  distinct(entidad) %>%
  nrow()

print(paste("Cantidad de entidades evaluadas:", n_entidades))

# 4. Entidad con mayor evolución en cartera
cartera_por_entidad <- Bancos_multiples %>%
  filter(str_detect(indicador, regex("Cartera.*Total", ignore_case = TRUE))) %>%
  group_by(entidad) %>%
  summarise(total = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total))

print(head(cartera_por_entidad, 1))

# 5. Clasificación de riesgo
clas_riesgo <- Bancos_multiples %>%
  filter(str_detect(indicador, regex("Clasificación.*Riesgo", ignore_case = TRUE))) %>%
  group_by(entidad, valor) %>%
  summarise(cantidad = n(), .groups = "drop") %>%
  arrange(entidad, valor)

print(clas_riesgo)

# 6. Morosidad general por entidad
deuda <- Bancos_multiples %>%
  filter(str_detect(indicador, regex("Cartera.*crédito.*Total", ignore_case = TRUE)))

vencida <- Bancos_multiples %>%
  filter(str_detect(indicador, regex("vencida", ignore_case = TRUE)))

morosidad <- deuda %>%
  group_by(entidad) %>%
  summarise(deuda_total = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  inner_join(
    vencida %>%
      group_by(entidad) %>%
      summarise(vencida_total = sum(valor, na.rm = TRUE), .groups = "drop"),
    by = "entidad"
  ) %>%
  mutate(morosidad = vencida_total / deuda_total) %>%
  arrange(desc(morosidad))

print(morosidad)

# 7. Morosidad por tipo de cartera
for (tipo in tipos) {
  deuda_tipo <- Bancos_multiples %>%
    filter(str_detect(indicador, regex(paste0("Cartera.*", tipo), ignore_case = TRUE))) %>%
    group_by(entidad) %>%
    summarise(deuda = sum(valor, na.rm = TRUE), .groups = "drop")
  
  vencida_tipo <- Bancos_multiples %>%
    filter(str_detect(indicador, regex(paste0("vencida.*", tipo), ignore_case = TRUE))) %>%
    group_by(entidad) %>%
    summarise(vencida = sum(valor, na.rm = TRUE), .groups = "drop")
  
  morosidad_tipo <- inner_join(deuda_tipo, vencida_tipo, by = "entidad") %>%
    mutate(morosidad = vencida / deuda) %>%
    arrange(desc(morosidad))
  
  print(paste("Morosidad para", tipo))
  print(head(morosidad_tipo, 5))
}

# 8. Top 5 entidades por monto
top5 <- deuda %>%
  group_by(entidad) %>%
  summarise(total = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  head(5)

print(top5)

# 9. Monto de deuda en diciembre 2023
deuda_dic <- Bancos_multiples %>%
  filter(periodo == as.Date("2023-12-01"),
         str_detect(indicador, regex("Cartera.*crédito.*Total", ignore_case = TRUE))) %>%
  group_by(entidad) %>%
  summarise(total = sum(valor, na.rm = TRUE), .groups = "drop")

ggplot(deuda_dic, aes(x = reorder(entidad, total), y = total)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Monto de deuda - Diciembre 2023", x = "Entidad", y = "Monto (RD$)")

# 10. Gráfico circular - morosidad por tipo de cartera (dic 2023)
morosidad_dic <- data.frame(tipo = character(), morosidad = numeric())

for (tipo in tipos) {
  deuda_val <- Bancos_multiples %>%
    filter(periodo == as.Date("2023-12-01"),
           str_detect(indicador, regex(paste0("Cartera.*", tipo), ignore_case = TRUE))) %>%
    summarise(deuda = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    pull(deuda)
  
  vencida_val <- Bancos_multiples %>%
    filter(periodo == as.Date("2023-12-01"),
           str_detect(indicador, regex(paste0("vencida.*", tipo), ignore_case = TRUE))) %>%
    summarise(vencida = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    pull(vencida)
  
  mor <- ifelse(deuda_val > 0, vencida_val / deuda_val, NA)
  morosidad_dic <- rbind(morosidad_dic, data.frame(tipo = tipo, morosidad = mor))
}

ggplot(morosidad_dic, aes(x = "", y = morosidad, fill = tipo)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Morosidad por tipo - Dic 2023")

# 11. Línea evolución total 2022-2023
deuda_total <- deuda %>%
  group_by(periodo) %>%
  summarise(total = sum(valor, na.rm = TRUE), .groups = "drop")

ggplot(deuda_total, aes(x = periodo, y = total)) +
  geom_line(color = "darkgreen") +
  geom_point() +
  labs(title = "Evolución de Cartera Total (2022-2023)", y = "Monto RD$")


# Gráfico 12. Comparación deuda vs vencida por tipo (Dic 2023)
comparacion_dic <- data.frame()

for (tipo in tipos) {
  deuda_val <- Bancos_multiples %>%
    filter(periodo == as.Date("2023-12-01"),
           str_detect(indicador, regex(paste0("Cartera.*", tipo), ignore_case = TRUE))) %>%
    summarise(deuda = sum(valor, na.rm = TRUE)) %>%
    pull(deuda)
  
  vencida_val <- Bancos_multiples %>%
    filter(periodo == as.Date("2023-12-01"),
           str_detect(indicador, regex(paste0("vencida.*", tipo), ignore_case = TRUE))) %>%
    summarise(vencida = sum(valor, na.rm = TRUE)) %>%
    pull(vencida)
  
  comparacion_dic <- rbind(comparacion_dic, data.frame(
    tipo = tipo,
    tipo_valor = c("Deuda Total", "Vencida"),
    monto = c(deuda_val, vencida_val)
  ))
}

ggplot(comparacion_dic, aes(x = tipo, y = monto, fill = tipo_valor)) +
  geom_col(position = "dodge") +
  labs(title = "Comparación Deuda vs Vencida por Tipo - Dic 2023", x = "Tipo de Cartera", y = "Monto (RD$)") +
  scale_fill_manual(values = c("Deuda Total" = "skyblue", "Vencida" = "tomato")) +
  theme_minimal()


