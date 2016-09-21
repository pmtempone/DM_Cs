-----###librerias-----

library(cluster)
library(fpc)
library(MASS)

----#carga dataset----
  
link <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00334/wiki4HE.csv"

set_encuesta <- read.table(link,header = TRUE,sep = ';',stringsAsFactors = FALSE)
encuesta_bkp <- set_encuesta
-----#preparacion de datos-----

dim(set_encuesta)

head(set_encuesta)

summary(set_encuesta)

str(set_encuesta)


table(set_encuesta$DOMAIN)

sapply(set_encuesta[,c(3,5,7:53)],table) #el ? es el NA

set_encuesta$GENDER <- factor(ifelse(set_encuesta$GENDER==1,'F','M'))

set_encuesta$DOMAIN[set_encuesta$DOMAIN=='?'] <- NA

set_encuesta$DOMAIN <- factor(set_encuesta$DOMAIN) 

#funcion para resumir el trabajo

cambio_dato <- function (x){
  vector <- factor(ifelse(x=='?',NA,x))
  return(vector)
}

set_encuesta$OTHER_POSITION <- cambio_dato(set_encuesta$OTHER_POSITION)

set_encuesta[,7] <- cambio_dato(set_encuesta[,7])
set_encuesta$USERWIKI <- as.logical(as.numeric(set_encuesta$USERWIKI))

set_encuesta[,11:53] <- sapply(set_encuesta[,11:53],as.numeric)

set_encuesta.ouc <- subset(set_encuesta,UNIVERSITY==1,select = -c(6:9))

table(complete.cases(set_encuesta.ouc))

set_encuesta.ouc <- set_encuesta.ouc[complete.cases(set_encuesta.ouc),]

set_encuesta.ouc$YEARSEXP <- as.numeric(set_encuesta.ouc$YEARSEXP)

set_encuesta.ouc <- set_encuesta.ouc[complete.cases(set_encuesta.ouc),]

#partimos en 2, datos personales y preguntas
pers <- set_encuesta.ouc[,1:6]
pregs <- set_encuesta.ouc[,7:49]

-----#analisis de datos-----

#matriz de distancia de gower

pers.gower <- daisy(pers,metric = "gower",stand = TRUE)

plot(hclust(pers.gower))

pam.pers <- pam(pers.gower,k=6,diss = TRUE) #ventaja de pam, ver prototipos

names(pam.pers)

pam.pers$clustering

pam.pers$medoids #prototipos, devuelve el id

pers[pam.pers$medoids,]

pers.medoids <- pam.pers$medoids[pam.pers$clustering] #obtengo el medoide de cada valor

#levanto de la matriz gower la distancia entre cada uno de los objetos y su medoide, es una matriz triangular

#transformamos la matriz de gower (triangular) en una matriz cuadrada

pers.matrix.gower <- as.matrix(pers.gower)[cbind(pers.medoids,names(pam.pers$clustering))] #cbind para formar un df de x e y

#sumamos valores de la matriz para tener valor aprox de suma de los errores

sum(pers.matrix.gower)

#valores silhouette

pam.pers$silinfo$avg.width

#medida de separacion de cluster: L* separacion es muy buena,L separacion media, no: cluster superpuestos, no estan separados
#esto se explica xq el silhouette de 0.35 (no es bueno, pero tampoco malo)
pam.pers$isolation

plot(silhouette(pam.pers), col = "red")

clusplot(pam.pers)
