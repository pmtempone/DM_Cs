----#librerias----

library(cluster)
library(MASS)
library(fpc)
library(dplyr)
library(FactoMineR)
if (!require("RANN")) install.packages("RANN") ## Loading required package: RANN 
library(RANN)
library(ggplot2)
library(dplyr)
library(broom)
library(psych)
library(dbscan)
library(lattice)
library(gridExtra)
---#punto 1----

glx_tp2 <- cbind(glx.uso[,"Nr"] ,as.data.frame(norm_glx_esp),(glx.uso %>% select(S280MAG,BjMAG,Rmag,ApDRmag,Mcz)))
colnames(glx_tp2)[1] <- "Nr"

glx_uso_tp2 <- read.csv("~/Documentos/Datamining_Ciencia/glx_uso.csv")

#cluster jerarquico

glx.dist <- dist(scale(glx_uso_tp2[,-1])) 
glx.clus <- hclust(glx.dist) 
plot( as.dendrogram( glx.clus ), leaflab="none", main="Cluster Jerárquico") 

#optimos k

kclusts <- data.frame(k=1:8) %>% group_by(k) %>% do(kclust=kmeans(glx.dist, .$k))
clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))

assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], glx_uso_tp2[,-1]))

clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

ggplot(clusterings, aes(k, tot.withinss)) + geom_line() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme_bw()
#entre 3 y 4 clusters
#cluster kmeans

glx.kmeans <- kmeans(glx.dist, centers=3) 

# salida grafica 
plot(silhouette(glx.kmeans$cluster, glx.dist),col=3, border=NA)


#matriz de distancia de gower

glx.gower <- daisy(glx_uso_tp2[,-1],metric = "gower",stand = TRUE)

plot(hclust(glx.gower))

pam.pers <- pam(glx.gower,k=3,diss = TRUE) #ventaja de pam, ver prototipos

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
#esto se explica xq el silhouette de 0.4 (no es bueno, pero tampoco malo)
pam.pers$isolation

plot(silhouette(pam.pers), col = "red" ,border = NA)

clusplot(pam.pers) #tarda en generarse el grafico

K <- 2:5
pseudo.sse <- vector()
silhou <- vector()
for (i in seq_along(K)) {
  pam.clust <- pam(glx.dist, diss = F, k = K[i])
  pers.meds <- pam.clust$medoids[pam.clust$clustering]
  m.pers.gower <- as.matrix(glx.dist)[cbind(pers.meds,names(pam.clust$clustering))]
  
  pseudo.sse[i] <- sum(m.pers.gower)
  silhou[i] <- pam.clust$silinfo$avg.width
}

sse <- xyplot(pseudo.sse ~ as.factor(K),  type = 'l', xlab = NULL)
sil <- barchart(silhou ~ as.factor(K), horizontal = F)
grid.arrange(sse, sil)

#calidad de cluster, distancia cofenetica
cor(dist(dat.nrm), cophenetic(dat.clus)) ## [1] 0.9250305 cor(dist(dat.r1.nrm), cophenetic(dat.r1.clus)) ## [1] 0.9030483


#test dbscan

## use the numeric variables in the iris dataset

x <- as.matrix(glx_uso_tp2[,-1])

## DBSCAN
db <- dbscan(x, eps = .4)
db
## visualize results (noise is shown in black)
pairs(x, col = db$cluster + 1L)

## LOF (local outlier factor) 
lof <- lof(x, k = 4)
## larger bubbles in the visualization have a larger LOF
pairs(x, cex = lof)

## OPTICS
opt <- optics(x, eps = 1, minPts = 4)
opt

## extract DBSCAN-like clustering 
opt <- extractDBSCAN(opt, eps_cl = .4)

## create a reachability plot (extracted DBSCAN clusters at eps_cl=.4 are colored)
plot(opt)

## plot the extracted DBSCAN clustering
pairs(x, col = opt$cluster + 1L)

## extract a hierarchical clustering using the Xi method (captures clusters of varying density)
opt <- extractXi(opt, xi = .05)
opt
plot(opt)