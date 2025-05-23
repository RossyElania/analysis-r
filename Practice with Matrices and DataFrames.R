###################################
# Matrices                        #
###################################

# Los siguientes vectores contienen la recaudacion de las tres primeras pel?culas de Star Wars en US y fuera de US (non-US)
new_hope <- c(461, 314.4)
empire_strikes <- c(290.5, 247.9)
return_jedi <- c(309.3, 165.8)

# Crea una matriz que contenga toda la informacion (con tres filas)
star_wars_matrix <- rbind(new_hope, empire_strikes, return_jedi)
star_wars_matrix
  
# Ponles nombres a las columnas: "US" y "non-US"
colnames(star_wars_matrix) <- c("US","non-US")
star_wars_matrix
  
# Ponles nombres a las filas: "A New Hope", "The Empire Strikes Back" y "Return of the Jedi"
rownames(star_wars_matrix) <- c("A New Hope","The Empire Strikes Back","Return of the Jedi")
star_wars_matrix
  
# Si el precio de la entrada es de 5$, estima el numero de espectadores de cada pel?cula
visitors <- star_wars_matrix / 5
total_visitors <- sum(visitors)
visitors
total_visitors

# Como el precio de las entradas no es el mismo todos los a?os, creamos una matriz de precios
ticket_prices_matrix <- matrix(c(5, 5, 6, 6, 7, 7), nrow = 3, byrow = TRUE, dimnames = list(rownames(star_wars_matrix), colnames(star_wars_matrix)))
ticket_prices_matrix <- 

# Repite el calculo del numero de espectadores con la matriz anterior
visitors <- star_wars_matrix / ticket_prices_matrix
visitors

# Calcula el numero de espectadores medio en US
average_us_visitors <- mean(visitors[,"US"])
average_us_visitors
  
# Calcula el numero de espectadores medio fuera de US
average_non_us_visitors <- mean(visitors[,"non-US"])
average_non_us_visitors 
  
# Calcula los totales de recaudacion por pel?cula
worldwide_vector <- colSums(star_wars_matrix)
worldwide_vector

# A?ade el vector anterior con los totales por pel?cula como una nueva columna de la matriz
all_wars_matrix <- rbind(star_wars_matrix, worldwide_vector)
all_wars_matrix

# Crea una nueva matriz con las recaudaciones de las siguientes tres pel?culas de Star Wars
phantom_menace <- c(474.5, 552.5)
attack_clones <- c(310.7, 338.7)
revenge_sith <- c(380.3, 468.5)
star_wars_matrix2 <- rbind(phantom_menace, attack_clones, revenge_sith)
star_wars_matrix2

# Ponles nombres a las columnas: "US" y "non-US"
colnames(star_wars_matrix2) <- c("US","non-US")
star_wars_matrix2
  
# Ponles nombres a las filas: "The Phantom Menace", "Attack of the Clones" y "Revenge of the Sith"
rownames(star_wars_matrix2) <- c("The Phantom Menace", "Attack of the Clones", "Revenge of the Sith")
star_wars_matrix2

# Une en una nueva matriz la recaudacion de todas las pel?culas, las tres primeras filas corresponderán
# a las tres primeras pel?culas y las tres siguientes a las últimas pel?culas
all_wars_matrix <- rbind(star_wars_matrix, star_wars_matrix2) 
all_wars_matrix

# Calcula los totales de recaudacion de todas las pel?culas en US y fuera de US
total_revenue_vector <- colSums(all_wars_matrix)
total_revenue_vector

# Calcula la media recaudada de las tres primeras pel?culas fuera de US
non_us_all <- mean(all_wars_matrix[1:3,2])
non_us_all

# Calcula la media recaudada de las 2 primeras pel?culas fuera de US
non_us_some <- mean(all_wars_matrix[c(1,2),2])
non_us_some
  
###################################
# Data frames                     #
###################################

# Crea a partir de los vectores siguientes un data frame
planets <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune");
type <- c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", "Gas giant")
diameter <- c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883); 
rotation <- c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67);
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE);

planets_df  <- data.frame(
  planets = c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"),
  type = c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", "Gas giant"),
  diameter = c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883), 
  rotation = c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67),
  rings = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
)
  
# Comprueba el contenido del data frame
str(planets_df) 
summary(planets_df) 
  
# Selecciona la informacion de los primeros tres planetas (los mas cercanos al sol)
closest_planets_df <- head(planets_df,3)
closest_planets_df 

# Selecciona la informacion de los ultimos tres planetas (los mas lejanos al sol)
furthest_planets_df <- tail(planets_df,3)
furthest_planets_df

# Comprueba la seleccion
  
# Selecciona la columna diameter de los ultimos seis planetas (los mas lejanos al sol)
furthest_planets_diameter <- tail(planets_df$diameter,6)
furthest_planets_diameter  

# Selecciona los planetas que tienen anillos
planets_with_rings_df <- planets_df[planets_df$rings == TRUE, ]
planets_with_rings_df
  
# Selecciona los planetas que tienen un diametro inferior al de la tierra (aquellos que tienen diametro < 1, 
# puesto que la variable es relativa al diametro de la tierra)
small_planets_df  <- planets_df[planets_df$diameter < 1, ]
small_planets_df 

# La funcion order devuelve las posiciones de un vector ordenado ascendentemente
a <- c(4, 10, 3)
order(a)
a[order(a)]

# Ordena el data frame segun el diametro de los planetas ascendentemente
positions <- order(planets_df$diameter)
largest_first_df <- planets_df[positions, ]
largest_first_df