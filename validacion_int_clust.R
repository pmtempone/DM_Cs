library(cluster) 
dat.r2.kmeans.sil <- silhouette(dat.r2.kmeans$cluster, dat.r2.dist) 
# salida tabulada: 
summary(dat.r2.kmeans.sil) 
## Silhouette of 200 units in 5 clusters from silhouette.default(x = dat.r2.kmeans$cluster, dist = dat.r2.dist) : 
## Cluster sizes and average silhouette widths: ## 36 46 38 40 40 ## 0.4888149 0.3341678 0.4606900 0.2159676 0.5762980 ## Individual silhouette widths: ## Min. 1st Qu. Median Mean 3rd Qu. Max. ## 0.0006365 0.2676000 0.4501000 0.4108000 0.5610000 0.7040000 
summary(dat.r2.kmeans.sil)$avg.width 
## [1] 0.4108295

summary(dat.r2.kmeans.sil)$clus.avg.widths

# salida grafica 
plot(silhouette(dat.r2.kmeans$cluster, dat.r2.dist))
