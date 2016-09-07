library(RANN)

a <- c(rep(2,40), rep(6,40), rep(12,40), rep(19,40), rep(26,40))
b <- c(rep(3,40), rep(6,40), rep(14,120))
c <- c(rep(12,80), rep(18,40), rep(2,40), rep(30,40))
a.r1 <- a + rnorm(200, 0, 0.5)
b.r1 <- b + rnorm(200, 0, 0.5)
c.r1 <- c + rnorm(200, 0, 0.5)
a.r2 <- a + rnorm(200, 0, 3)
b.r2 <- b + rnorm(200, 0, 3) 
c.r2 <- c + rnorm(200, 0, 4)

dat.clase <- c(rep("a",40), rep("b",40), rep("c",40), rep("d", 40), rep("e",40))

# aleatorizamos el orden de los registros (para hacerlos parecer más "reales", no es necesario) 
randord <- sample(1:200, 200) 
# dat es la matriz original: 
dat <- as.matrix(cbind(a,b,c)) 
row.names(dat) <- paste0("obj", 1:200) 
dat <- dat[randord,] 
# dat.r1 es la matriz con ruido: 
dat.r1 <- as.matrix(cbind(a.r1, b.r1, c.r1)) 
row.names(dat.r1) <- paste0("obj", 1:200) 
dat.r1 <- dat.r1[randord,] 
# dat.r2 es la matriz con más ruido: 
dat.r2 <- as.matrix(cbind(a.r2, b.r2, c.r2)) 
row.names(dat.r2) <- paste0("obj", 1:200) 
dat.r2 <- dat.r2[randord,] 
# ordenamos las etiquetas de clase 
dat.clase <- dat.clase[randord] 
# Escalamos todos los datos entre 0 y 1 para simplificar los pasos que siguen es
esc01 <- function(x) { (x - min(x)) / (max(x) - min(x))} 
dat.nrm <- apply(dat, 2, esc01) 
dat.r1.nrm <- apply(dat.r1, 2, esc01) 
dat.r2.nrm <- apply(dat.r2, 2, esc01)
#Podemos mirar rápidamente el efecto del agregado de ruido en los tres datasets: 
dat.nrm.dist <- dist(dat.nrm) 
dat.clus <- hclust(dat.nrm.dist) 
dat.r1.dist <- dist(dat.r1.nrm) 
dat.r1.clus <- hclust(dat.r1.dist)
dat.r2.dist <- dist(dat.r2.nrm) 
dat.r2.clus <- hclust(dat.r2.dist) 
par(mfrow=c(1,3)) 
plot( as.dendrogram( dat.clus ), leaflab="none", main="dat.nrm") 
plot( as.dendrogram( dat.r1.clus ), leaflab="none", main="dat.r1.nrm") 
plot( as.dendrogram( dat.r2.clus ), leaflab="none", main="dat.r2.nrm") 
par(mfrow=c(1,1))

cant.muestras <- 20 
rnd.pts <- cbind(runif(cant.muestras, 0, 1), runif(cant.muestras, 0, 1), runif(cant.muestras, 0, 1))

smp <- sample(nrow(dat.nrm), 20) 
smp.dat <- dat.nrm[smp,] 
smp.r1 <- dat.r1.nrm[smp,] 
smp.r2 <- dat.r2.nrm[smp,]


# calculo de las distancias al vecino más cercano para los datos reales 
smp.pts.dist <- nn2(as.data.frame(dat.nrm), as.data.frame(smp.dat), k=2)$nn.dist[,2] 
# calculo de las distancias al vecino más cercano para datos al azar 
rnd.pts.dist <- nn2(as.data.frame(dat.nrm), as.data.frame(rnd.pts), k=1)$nn.dists 
# calculo del estadistico 
sum(smp.pts.dist) / (sum(smp.pts.dist) + sum(rnd.pts.dist)) 
## [1] 0
#Ahora para los datos con ruido intermedio: 
smp.pts.dist <- nn2(as.data.frame(dat.r1.nrm), as.data.frame(smp.r1), k=2)$nn.dist[,2] 
rnd.pts.dist <- nn2(as.data.frame(dat.r1.nrm), as.data.frame(rnd.pts), k=1)$nn.dists 
sum(smp.pts.dist) / (sum(smp.pts.dist) + sum(rnd.pts.dist))


dat.kmeans <- kmeans(dat.nrm, centers=5) 
head(dat.kmeans$cluster) 
## obj127 obj114 obj45 obj128 obj194 obj175 
## 3 1 2 3 4 4 
dat.conf <- table(dat.kmeans$cluster, dat.clase, dnn = c("cluster", "clase")) 
dat.conf
dat.r2.kmeans <- kmeans(dat.r2.nrm, centers=5) 
dat.r2.conf <- table(dat.r2.kmeans$cluster, dat.clase, dnn =c("cluster", "clase")) 
dat.r2.conf

dat.r2.kmeans.mal.1 <- kmeans(dat.r2.nrm, centers=2) 
dat.r2.mal.1.conf <- table(dat.r2.kmeans.mal.1$cluster, dat.clase, dnn = c("cluster", "clase")) 
dat.r2.mal.1.conf
