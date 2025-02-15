if(! "arules" %in% installed.packages()) install.packages("arules", depend = TRUE)
if(! "arulesViz" %in% installed.packages()) install.packages("arulesViz", depend = TRUE)
library(arules)
library(dplyr)
library(arulesViz)

#Cargar el dataset
datos <- read.csv("movies.csv")
#TOMAR NOTA CAST WOMEN Y MAN son strings porque contiene datos como titulos de pelicula por eso se pone en cols
cols <- c("id","genres","homePage","productionCompany","productionCompanyCountry","productionCountry",
          "video","director","actors","actorsPopularity","actorsCharacter", "originalTitle", "title", "originalLanguage",
          "releaseDate", "castWomenAmount", "castMenAmount")

#Convitiendo en factor las categóricas y Discretizando las variables numéricas
datos <- datos %>%
  mutate(across(all_of(cols), as.factor)) %>%
  mutate(across(!all_of(cols), discretize))

#Quitaremos productioncountries amount y video porque los datos son muy frecuentes ya que superan mas de 80% en base a la grafica propuesta anteriormente  
#con la finalidad de obtener mas insights


datos <- datos[,-10]
datos <- datos[,-23]

#Convirtiendolas en transacciones
trans <- as(datos,"transactions")



summary(trans)


itemLabels(trans)
as(trans[1:2, 1:10], "matrix")

# Se observa que el mayor porcentaje es production countries amount 
itemFrequencyPlot(trans, topN=40,  cex.names=.5)


reglas<-apriori(trans, parameter = list(support = 0.35,
                                        target = "frequent", 
                                        minlen=3, maxlen=4))

inspect(reglas)


quality(reglas)$lift <- interestMeasure(reglas, measure="lift", trans = trans)
inspect(head(sort(reglas, by = "lift"), n=10))

plot(head(sort(reglas, by = "lift"), n=50), method = "graph", control=list(cex=.8))  
