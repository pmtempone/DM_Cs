----#librerias----

library(kohonen)
library(cluster)
library(RColorBrewer)

---#carga de datos-----

tinto <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", header=T, sep=";")

head(tinto, n = 3)

tinto.st <- scale( tinto[, -12] )
tinto.som <- som(as.matrix( tinto.st), somgrid(4,4,"hexagonal"))
plot(tinto.som, type="codes")


plot(tinto.som, type="mapping", col = tinto$quality, pch=19)

display.brewer.all(type="div")

par(mfrow = c(2,1) )
display.brewer.pal(11, name="Spectral")
paleta <- colorRampPalette(brewer.pal(11,"Spectral"))(6)
plot(1:6, rep(0,6), cex=4, pch=19, col=paleta, xlab="", ylab="")

par(mfrow = c(1,1) )
plot(tinto.som, type="mapping", col = paleta[tinto$quality], pch=19)      

tinto.kmeans <- kmeans( tinto.st, 12)

paleta2 <- colorRampPalette(brewer.pal(11,"Spectral"))(12)
plot(tinto.som, type="mapping", col = paleta2[tinto.kmeans$cluster], pch=19, main="Clusters")
leyenda <- sort(unique(tinto.kmeans$cluster))
legend("left", legend = leyenda, col=paleta2[leyenda], pch=19, ncol =1, cex=0.8)

tt <- table(tinto.som$unit.classif, tinto.kmeans$cluster, dnn = c("celda SOM", "cluster k-mean"))
tt[ order(rowSums(tt)), order(colSums(tt)) ]

par(mfrow=c(2,2))
plot(tinto.som, type="mapping", col = paleta[tinto$quality], pch=19, main="Calidad del vino")
plot(tinto.som, type="mapping", col = paleta2[tinto.kmeans$cluster], pch=19, main="Clusters por k-medias")
plot(tinto.som, type="property", property = tinto.som$codes[,6], main=names(tinto)[6])
plot(tinto.som, type="property", property = tinto.som$codes[,3], main=names(tinto)[3])


par(mfrow=c(1,1)) # Para revertir al layout original
plot(tinto.som, "changes")

par(mfrow=c(1,2))
plot(tinto.som, type = "dist.neighb")
plot(tinto.som, "quality")
Neighbour
