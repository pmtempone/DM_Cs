----#librerias----

library(cluster)
library(MASS)
library(fpc)
library(dplyr)
library(FactoMineR)

---#punto 1----

glx_tp2 <- cbind(glx.uso[,"Nr"] ,as.data.frame(norm_glx_esp),(glx.uso %>% select(S280MAG,BjMAG,Rmag,ApDRmag,Mcz)))
colnames(glx_tp2)[1] <- "Nr"

glx_uso_tp2 <- read.csv("~/Documentos/Datamining_Ciencia/glx_uso.csv")

#cluster jerarquico

glx.dist <- dist(scale(glx_uso_tp2[,-1])) 
glx.clus <- hclust(glx.dist) 
plot( as.dendrogram( glx.clus ), leaflab="none", main="Cluster Jerárquico") 



#entre 3 y 4 clusters
#cluster kmeans

glx.kmeans <- kmeans(glx.dist, centers=4) 

# salida grafica 
plot(silhouette(glx.kmeans$cluster, glx.dist),col=3, border=NA)


#matriz de distancia de gower

glx.gower <- daisy(glx_uso_tp2[,-1],metric = "gower",stand = TRUE)

plot(hclust(glx.gower))

pam.pers <- pam(glx.gower,k=4,diss = TRUE) #ventaja de pam, ver prototipos

names(pam.pers)

pam.pers$clustering

pam.pers$medoids #prototipos, devuelve el id

glx_uso_tp2[pam.pers$medoids,-1]

pers.medoids <- pam.pers$medoids[pam.pers$clustering] #obtengo el medoide de cada valor

#levanto de la matriz gower la distancia entre cada uno de los objetos y su medoide, es una matriz triangular

#transformamos la matriz de gower (triangular) en una matriz cuadrada

pers.matrix.gower <- as.matrix(glx.gower)[cbind(pers.medoids,names(pam.pers$clustering))] #cbind para formar un df de x e y

#sumamos valores de la matriz para tener valor aprox de suma de los errores

sum(pers.matrix.gower)

#valores silhouette

pam.pers$silinfo$avg.width

#medida de separacion de cluster: L* separacion es muy buena,L separacion media, no: cluster superpuestos, no estan separados
#esto se explica xq el silhouette de 0.35 (no es bueno, pero tampoco malo)
pam.pers$isolation

plot(silhouette(pam.pers), col = "red" ,border = NA)

clusplot(pam.pers)
