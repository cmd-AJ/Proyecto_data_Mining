if(! "psych" %in% installed.packages()) install.packages("psych", depend = TRUE)
if(! "FactoMineR" %in% installed.packages()) install.packages("FactoMineR", depend = TRUE)
if(! "corrplot" %in% installed.packages()) install.packages("corrplot", depend = TRUE)
if(! "fpc" %in% installed.packages()) install.packages("fpc", depend = TRUE)
if(! "factoextra" %in% installed.packages()) install.packages("factoextra", depend = TRUE)
if(! "PCAmixdata" %in% installed.packages()) install.packages("PCAmixdata", depend = TRUE)


library(psych)
library(FactoMineR)
library(fpc)
library(factoextra)
library(corrplot)
library(PCAmixdata)


datos<-read.csv("movies.csv", stringsAsFactors = F)

columns_list <- c("budget", "revenue", "runtime", "popularity", "voteAvg", "voteCount", "genresAmount", "productionCoAmount" )
datos <- datos[, columns_list]

rcor<-cor(datos,use = "pairwise.complete.obs")
det(rcor)#Si el determinante de la matriz de correlación es cercano a 0 significa que hay multicolinealidad


KMO(as.matrix(datos)) 
cortest.bartlett(datos) 


cortest.bartlett(datos[,-1])



cor(datos,use = "pairwise.complete.obs")


#Esta función normaliza los datos de una vez
compPrinc<-prcomp(datos, scale = T)
compPrinc

summary(compPrinc)
# Importance of components:
#                         PC1   PC2   PC3   PC4    PC5    PC6     PC7
# Standard deviation     1.65 1.230 1.181 0.944 0.5889 0.3167 0.15973
# Proportion of Variance 0.39 0.216 0.199 0.127 0.0495 0.0143 0.00364
# Cumulative Proportion  0.39 0.606 0.805 0.932 0.9820 0.9964 1.00000
compPrincPCA<-PCA(datos,ncp=ncol(datos), scale.unit = T)

summary(compPrincPCA)

#Se obtiene el scree plot de las componentes principales.
# Como se ve hacen falta 4 de las 7 componentes para explicar m?s del 80% de la variabilidad
fviz_eig(compPrinc, addlabels = TRUE, ylim = c(0, 80))
fviz_eig(compPrinc, addlabels = TRUE, choice = c("eigenvalue"), ylim = c(0, 3))

fviz_pca_biplot(compPrinc,repel = F)

# En la siguiente gráfica se ilustra la calidad de la representación de los componentes en las dos primeras dimensiones.
fviz_pca_var(compPrinc, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#Contribución de las variables a las 3 primeras dimensiones
fviz_contrib(compPrinc, choice = "var", axes = 1, top = 10) #Dimensión 1
fviz_contrib(compPrinc, choice = "var", axes = 2, top = 10) #Dimensión 2
fviz_contrib(compPrinc, choice = "var", axes = 3, top = 10) #Dimensión 3

var<-get_pca_var(compPrinc)
corrplot(var$cos2, is.corr = F)


# TRABAJANDO CON IRIS
KMO(as.matrix(iris[,1:4])) #La adecuaci?n muestral no es buena

cortest.bartlett(iris[,1:4])

irisPCA <- PCA(iris[,1:4])
summary(irisPCA)
# Con las primeras 2 dimensiones se explica el 95% de la variancia del conjunto de datos
# En la primera dimensión están muy bien representadas las variables Petal.Length, Petal.Width y Petal.Width
# En la segunda dimensión se pueden incluir las variables Sepal.Width 

#Scree Plot
fviz_eig(irisPCA, addlabels =  TRUE, ylim = c(0, 80))
fviz_eig(irisPCA, addlabels = TRUE,choice = c("eigenvalue"), ylim = c(0, 3))


#Representación de las variables en cada componente

fviz_contrib(irisPCA, choice = "var", axes = 1, top = 10)
fviz_contrib(irisPCA, choice = "var", axes = 2, top = 10)


fviz_pca_var(irisPCA, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#Representación de cada variable en cada componente
var<-get_pca_var(irisPCA)

corrplot(var$cos2, is.corr = F)
#Según la representación de las variables en las componentes se podr?a incluir en la dimensi?n 1 pero la interpretabilidad del componente principal ser?a m?s complicada.
