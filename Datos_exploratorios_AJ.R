datos <- read.csv("movies.csv")



# ¿Las películas de qué genero principal obtuvieron mayores ganancias?
datos$genres[1:5]
datos_br <- datos[, c("genres", "budget", "revenue")]
profit <- cbind(datos_br, profit=datos$revenue-datos$budget)

#Quitar los ceros y generos en espacios blancos
profit <- profit[apply(profit!=0, 1, all), ]
profit <- profit[apply(profit!='', 1, all), ]

#Sacar el genero principal de la pelicula
genre_main = c()
for (genre in profit$genres) {
  main_genre <- strsplit(x=genre,split="|", fixed = TRUE)[[1]][1]
  genre_main <- append(genre_main, main_genre)
}
da <- cbind(genre_main, profit)
da <- aggregate(da$profit, list(da$genre_main), FUN=mean)
colnames(da) <- c("Generos", "Ganancia")
pregunta4.7 <- da[order(da$Ganancia, decreasing = TRUE), ]


# 4.8 ¿La cantidad de actores influye en los ingresos de las películas?¿se han hecho películas con más actores en los últimos años?
