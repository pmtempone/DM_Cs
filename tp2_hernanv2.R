library(igraph)
library(sp)
library(rworldmap)
library(rworldxtra)
library(ggplot2)
library(dplyr)

download.file("http://snap.stanford.edu/data/loc-brightkite_edges.txt.gz", destfile = "brkred.gz")
download.file("http://snap.stanford.edu/data/loc-brightkite_totalCheckins.txt.gz",destfile = "brkchk.gz")

red <- read.table(gzfile("brkred.gz"), header=F, stringsAsFactors = F)
checkins <- read.table(file = gzfile("brkchk.gz"), header=F, fill=T, stringsAsFactors = F)

# nombres de checkins

names(checkins) <- c("id", "time", "lat", "lon", "loc_id")


## modifico numeracion de nodos para trabajar con enteros

checkins$id <- checkins$id + 1

red$V1 <- red$V1 + 1

red$V2 <- red$V2 + 1


## me quedo unicamente con casos que no tengan datos faltantes

checkins_c <- checkins[complete.cases(checkins),]


## veo la cantidad de registros por id, verificamos que hay muchos con loc_id desconocido

head(sort(table(checkins_c$loc_id), decreasing=T), n=14)

head(checkins_c[,3:5][checkins_c$loc_id == "00000000000000000000000000000000",])


# elimino elementos que esten repetidos (para optimizar tiempos de procesamiento)

uniq_checks <- unique(checkins_c[,3:5])


coords_pais = function(points){
  # Una función para recuperar el código ISO 3
  # de pais de una locación
  countriesSP <- getMap(resolution='high')
  # convertir las coordenadas en puntos espaciales
  pointsSP = SpatialPoints(points,proj4string=CRS(proj4string(countriesSP)))
  # recuperar el poligono-pais de cada punto espacial
  indices = over(pointsSP, countriesSP)
  # recuperar el nombre de cada pais por código ISO
  # de tres letras
  indices$ISO3
}

pais_iso <- coords_pais(uniq_checks[, 2:1])

#top 12 ranking paises con mas checkins 
head(sort(table(pais_iso), decreasing=T), 12)

# appendeo el codigo ISO3 de pais a los checkins
uniq_checks <- data.frame(uniq_checks, ISO3 = as.character(pais_iso))

# me quedo con los checkins de Noruega
uniq_checks_noruega <- filter(uniq_checks, ISO3 == "NOR")

# inner join, para agregar los datos faltantes para checkins de Noruega
checkins_noruega <- inner_join(checkins_c, uniq_checks_noruega[,3:4], by = "loc_id")

#usuarios que usan la red social en Noruega
uniq_usuarios_noruega <- unique(checkins_noruega$id)

red.amistad <- graph.edgelist(as.matrix(red), directed = F)

summary(red.amistad)

#imagen del grafo de conexiones de Noruega
red_noruega <- induced_subgraph(red.amistad, vids = uniq_usuarios_noruega)

# elimino aristas multiples y ciclos

red_noruega <- simplify(red_noruega, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = igraph_opt("edge.attr.comb"))


summary(red_noruega) # cantidad de nodos y aristas


plot(red_noruega, layout=layout.fruchterman.reingold, vertex.size=3, vertex.label=NA)

# descripciones de la red

vcount(red_noruega) # numero de nodos
ecount(red_noruega) # numero de aristas
graph.density(red_noruega)  # densidad

V(red_noruega) #nodos
E(red_noruega) #aristas

is.simple(red_noruega) # tiene bucles
is.connected(red_noruega) # como se ve, esta compuesto por mas de una componente conexa

# Red de interacciones observadas
diameter(red_noruega)

degree(red_noruega) # grados de los nodos
summary( degree(red_noruega)) # se puede ver que la mediana es gr(n)=4


## histograma de frecuencias
deg <- degree(red_noruega, mode="all")
hist(deg, breaks=vcount(red_noruega), main="Histograma de Frecuencia de Grados", ylab = "Frecuencia", xlab= "Grado", col='red')

## componentes conexas
cfc <- clusters(red_noruega, mode="strong")  
# Cargo los colores de acuerdo a los clusters 
V(red_noruega)$color <- rainbow(cfc$no)[cfc$membership]
plot(red_noruega, mark.groups = lapply(seq_along(cfc$csize)[cfc$csize > 1], function(x) V(red_noruega)[cfc$membership %in% x]),layout=layout.fruchterman.reingold, vertex.size=3, vertex.label=NA)

## Grafico tamaño componentes conexas / % de clientes en cada componente
distribucion_componentes = cfc$csize[order(cfc$csize)]
count_componentes = cbind(unique(distribucion_componentes),table(distribucion_componentes))
plot(x= count_componentes[,1], y = count_componentes[,2], col= "red",pch=5, cex=0.5,xlab="Tamaño Componente conexa", ylab="Cantidad",main="Grafico de componentes")

#coeficiente de clustering

coef_clustering = transitivity(red_noruega, type = "local")
coef_clustering[is.nan(coef_clustering)] <- NA

hist(coef_clustering, main = "Coeficiente de Clustering", breaks = seq(0.0, 1, 0.05), xlab = "Coeficiente", ylab="Frecuencia", col= "red")


## Probabilidad de grado
grados_nodos =degree.distribution(red_noruega,mode='all')
plot(grados_nodos, pch=5, cex=0.5, col="Blue", xlab="Grado", ylab="Prob grado = x")


# probabilidad acumulada
prob_grados_acum <- degree_distribution(red_noruega, cumulative=T, mode="all")
plot( x=0:max(deg), y= prob_grados_acum, pch=6, cex=0.5, col="Blue", xlab="Grado", ylab="Prob grado > x")


# Centralidad 
degree(red_noruega, mode="in")
centr_degree(red_noruega, mode="in", normalized=T)

# Mundos pequeños #######

red_noruega.plf <- power.law.fit(degree(red_noruega))

red_noruega.plf$xmin
red_noruega.plf$alpha

red_noruega.plf$KS.p
rg.transitivity.barabasi <- array()
rg.transitivity.erdos <- array()
#1.35, m=5
for(i in 1:1000){
  rg.1 <- barabasi.game(529, power = 6.27, m=11, directed = F)
  rg.2 <- sample_gnm(529, 843)
  rg.transitivity.barabasi[i] <- mean(transitivity(rg.1, "local", isolates="zero"))
  rg.transitivity.erdos[i] <- mean(transitivity(rg.2, "local", isolates="zero"))
}

red.transitivity <- mean(transitivity(red_noruega.1, "local", isolates="zero"))

#La comparación del promedio de los coeficientes de clusters contra los grafos con el modelo de Barabási–Albert:

table(red.transitivity > rg.transitivity.barabasi)
hist(rg.transitivity.barabasi, main = "coef. clustering, grafos de Barabasi-Albert", xlim=0:1)
abline( v = red.transitivity, col ="red", lwd= 2)

#comparo con Erdos-Renyi
table(red.transitivity > rg.transitivity.erdos)
hist(rg.transitivity.erdos, xlim=c(0.0, 0.2), main = "coef. clustering, grafos al azar")
abline( v = red.transitivity, col ="red", lwd= 2)


# Intermediación

View(data.frame( betweenness((red_noruega))))
hist(betweenness((red_noruega)))

# Cercania
View(data.frame( closeness(red_noruega)) )
hist(closeness((red_noruega)))


# Centralidad de autovectores
View(data.frame( eigen_centrality(red_noruega)$vector) )
hist(eigen_centrality(red_noruega)$vector)
