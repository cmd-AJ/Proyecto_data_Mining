

library(ggplot2)
datos <- read.csv("movies.csv")


# ¿Las películas de qué genero principal obtuvieron mayores ganancias?
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


#ggplot(pregunta4.7, aes(x=Generos, y=Ganancia, fill=Generos)) + geom_bar(stat="identity" ) +
 # labs(title = "¿La cantidad de actores influye en los ingresos de las películas?", x = "Cantidad de Actores", y = "Ganancias") +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# 4.8 ¿La cantidad de actores influye en los ingresos de las películas?
#¿se han hecho películas con más actores en los últimos años?
datos_br <- datos[, c( "title", "budget", "revenue", "actorsAmount")]
datos_br <- datos_br[apply(datos_br!=0, 1,all),]
datos_br <- datos_br[order(datos_br$actorsAmount, decreasing = TRUE), ]
profit <- cbind(datos_br, profit=datos_br$revenue-datos_br$budget)
a_graficar <- profit[ , c("profit", "actorsAmount") ]
a_graficar <- a_graficar[apply(a_graficar!=565916, 1,all),]
a_graficar <- a_graficar[apply(a_graficar!=0, 1,all),]
# Plot
ggplot(a_graficar, aes(x=actorsAmount, y=profit)) + geom_point() +
labs(title = "¿La cantidad de actores influye en los ingresos de las películas?", x = "Cantidad de Actores", y = "Ganancias") + 
ylim(-102566092, 1022566902)


datos_br <- datos[, c( "title", "releaseDate", "actorsAmount")]
datos_br <- datos_br[order(datos_br$releaseDate, decreasing = TRUE), ]

anios <- format(as.Date(datos_br$releaseDate), "%Y")

pregunta4.8 <- cbind( datos_br, anios)
pregunta4.8 <-aggregate(pregunta4.8$actorsAmount, by = list(Anios = pregunta4.8$anios), FUN=sum)
pregunta4.8 <- pregunta4.8[order(pregunta4.8$x, decreasing = TRUE), ]

#En la pregunta 4.8 SI

#4.9. (3 puntos) ¿Es posible que la cantidad de hombres y mujeres en el reparto influya en la
#popularidad y los ingresos de las películas?

pregunta4.9 <- datos[ , c( "castMenAmount", "castWomenAmount", "revenue", "popularity" ) ]

pregunta4.9 <- na.exclude(pregunta4.9)

#Por alguna razon alguien escribio strings en datos numericos esto solo los convierte las columnas a numericos
pregunta4.9$castMenAmount <- as.numeric(pregunta4.9$castMenAmount)
pregunta4.9$castWomenAmount <- as.numeric(pregunta4.9$castWomenAmount)
pregunta4.9 <- pregunta4.9[order(pregunta4.9$revenue, decreasing = TRUE), ]
pregunta4.9 <- pregunta4.9[apply(pregunta4.9[, c("castMenAmount", "castWomenAmount", "revenue")]!=0, 1, all), ]

pregunta4.9
#Mas hombres si alcanza mas ingresos pero popularidad no



pregunta4.10 <- datos[ 1:20 , c( "director", "voteAvg") ]
pregunta4.10 <- pregunta4.10[order(pregunta4.10$voteAvg, decreasing = TRUE), ]
pregunta4.10 

#4.11. (8 puntos) ¿Cómo se correlacionan los presupuestos con los ingresos? ¿Los altos
#presupuestos significan altos ingresos? Haga los gráficos que necesite, histograma,
#diagrama de dispersión

pregunta4.10 <- datos[ , c( "revenue", "budget") ]
pregunta4.10 <- pregunta4.10[apply(pregunta4.10!=0, 1, all), ]
ggplot(pregunta4.10, aes(y=revenue, x=budget)) + geom_point() +
  labs(x = "Presupuesto", y = "Ganancias")

# Entre mas presupuesto hay existe mas ganancias como se observa en la grafica


#4.12 ¿Se asocian ciertos meses de lanzamiento con mejores ingresos?
pregunta4.12 <- datos[ , c( "releaseDate", "revenue") ]
#Quitar los revenues que tienen 0
pregunta4.12 <- pregunta4.11[apply(pregunta4.11!=0, 1, all), ]
meses <- format(as.Date(pregunta4.11$releaseDate), "%m")

pregunta4.12 <- cbind( pregunta4.11, meses)

pregunta4.12 <- aggregate(pregunta4.11$revenue, list(meses), FUN=mean)


#4.13. (6 puntos) ¿En qué meses se han visto los lanzamientos con mejores ingresos?
 # ¿cuantas películas, en promedio, se han lanzado por mes?
pregunta4.13 <- pregunta4.12[order(pregunta4.12$Ganancia, decreasing = TRUE), ]
pregunta4.13[ 1:4 ,]

pregunta4.11 <- datos[ , c( "releaseDate", "revenue") ]
pregunta4.11 <- pregunta4.11[apply(pregunta4.11!=0, 1, all), ]
meses <- format(as.Date(pregunta4.11$releaseDate), "%m")
mese <- aggregate(pregunta4.11$revenue, list(meses), FUN=length)

mean(mese$x)


#14. ¿Cómo se correlacionan las calificaciones con el éxito comercial?
pregunta4.14 <- datos[ , c( "voteAvg", "voteCount") ]

pregunta4.14 <- aggregate(pregunta4.14$voteCount, list(pregunta4.14$voteAvg), FUN=mean) 
colnames(pregunta4.14) <- c("voteAvg", "voteCount")

ggplot(pregunta4.14, aes(y=voteCount, x=voteAvg, fill=voteCount)) + geom_bar(stat = "identity") +
  labs(x = "Presupuesto", y = "Ganancias")


#15¿Qué estrategias de marketing, como videos promocionales o páginas
#oficiales, generan mejores resultados?

