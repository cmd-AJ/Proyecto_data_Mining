

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
  labs(x = "voteAvg", y = "voteCount")


#15¿Qué estrategias de marketing, como videos promocionales o páginas
#oficiales, generan mejores resultados?
pregunta4.15 <- datos[ , c( "homePage", "video", "voteCount") ]
pregunta4.15$homePage <- !is.na(pregunta4.15$homePage) & pregunta4.15$homePage != ""
pregunta4.15 <- pregunta4.15[order(-pregunta4.15$video), ]
pregunta4.15 <- aggregate(voteCount ~ video + homePage, data = pregunta4.15, FUN = sum)
pregunta4.15

#generan mejores resultados si hay video o pagina 


# 16: ¿La popularidad del elenco está directamente correlacionada con el éxito
#de taquilla?

pregunta4.16 <- datos[, c("actorsPopularity", "revenue")]
for (i in 1:nrow(pregunta4.16)) {
  for (j in 1:ncol(pregunta4.16)) {
    if (is.character(pregunta4.16[i, j])) {
      split_values <- strsplit(pregunta4.16[i, j], "\\|")[[1]]
      numeric_values <- as.numeric(split_values)
      mean_value <- mean(numeric_values, na.rm = TRUE)
      pregunta4.16[i, j] <- mean_value
    }
  }
}

pregunta4.16 <- pregunta4.16[order(pregunta4.16$revenue, decreasing = TRUE), ]
pregunta4.16 <- pregunta4.16[apply(pregunta4.16!="NaN", 1, all), ]
pregunta4.16 <- pregunta4.16[apply(pregunta4.16!=0, 1, all), ]
pregunta4.16
ggplot(pregunta4.16, aes(x = actorsPopularity, y = revenue)) +
  geom_point() +
  labs( x = "actorsPopularity", y = "revenue")


#NO

#EXTRAS 
#¿Existe una correlación entre el número de compañías productoras involucradas en una película y su presupuesto total?
company_x_budget  <- datos[, c("productionCoAmount" , "budget")]
company_x_budget <- company_x_budget[order(company_x_budget$productionCoAmount, decreasing = TRUE), ]
company_x_budget <- company_x_budget[apply(company_x_budget!=0, 1, all), ]
company_x_budget <- aggregate(company_x_budget$budget, list(company_x_budget$productionCoAmount), FUN=mean)
colnames(company_x_budget) <- c("productionCoAmount", "budget")
company_x_budget
ggplot(company_x_budget, aes(y=productionCoAmount, x=budget)) +   geom_point() +
  geom_smooth(method = "lm", col = "red") +
 labs(title = "¿La cantidad de productores afectan con el presupuesto?", x = "Presupuesto", y = "Cantidad de Productores") 

#Existe que si hay mas productores menos presupuesto van a proporcionar en las peliculas


#El lenguaje afecta con el exito comercial ya que genera una mayor cantidad?
lenguaje <- datos[, c("originalLanguage", "revenue")]
lenguaje
lenguaje <- aggregate(lenguaje$revenue, list(lenguaje$originalLanguage), FUN=sum)
colnames(lenguaje) <- c("Lenguaje", "Total_Ganancia_Por_Lenguaje")
lenguaje <- lenguaje[order(lenguaje$Ganancia, decreasing = TRUE), ]
lenguaje <- lenguaje[apply(lenguaje!=0, 1, all), ]
ggplot(lenguaje, aes(x=Lenguaje, y=Total_Ganancia_Por_Lenguaje, fill=Lenguaje)) + 
  geom_bar(stat="identity") +
  labs(title = "El lenguaje afecta con el éxito comercial", 
       x = "Lenguaje", 
       y = "Total Ganancia Por Lenguaje") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position = "none") +
  scale_y_log10()


#Que pais es el mas utilizado para producir una pelicula?#Que pais esLenguaje el mas utilizado para producir una pelicula?
paises <- datos$productionCountry
paises_list <- strsplit(paises, split = "\\|", fixed = FALSE)
todos_paises <- unlist(paises_list)
todos_paises <- todos_paises[!is.na(todos_paises) & todos_paises != ""]
all_pais <- as.data.frame(table(todos_paises), stringsAsFactors = FALSE)
colnames(all_pais) <- c("Pais", "Total")
all_pais <- all_pais[order(-all_pais$Total), ]
all_pais <- all_pais[apply(all_pais!=0, 1, all), ]
ggplot(all_pais, aes(x = reorder(Pais, -Total), y = Total, fill = Pais)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de Producciones por País",
       x = "País",
       y = "Total de Producciones") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(legend.position = "none") +   scale_y_log10()
# paises
