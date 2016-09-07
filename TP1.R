----#librerias----
library(funModeling)
library(ggplot2)
require(reshape2)
library(corrplot)
----#carga archivo-----
glx <- read.csv("http://astrostatistics.psu.edu/datasets/COMBO17.csv", header = T, stringsAsFactors = F)

str(glx)
df_status(glx)

----#tarea 1----
str(glx)
df_status(glx)
# TAREA 2 (deteccion de outliers)
glx.uso <- glx
with(glx.uso, boxplot(BjMAG)) ; which.max(glx.uso$BjMAG) # Outlier en BjMAG, idx = 2
with(glx.uso, boxplot(ApDRmag)) ; which.min(glx.uso$ApDRmag) # Outlier en BjMAG, idx = 1186

glx.uso <- glx.uso[-c(2,1186),] # elimino ambos casos

# TAREA 3 (deteccion de datos faltantes y eliminarlos)

sapply(glx.uso, function(x) {sum(is.na(x))}) # variabels con datos faltantes VnMAG y S280MAG

c(which(is.na(glx.uso$VnMAG)),which(is.na(glx.uso$S280MAG))) # casos faltantes

glx.uso <- glx.uso[complete.cases(glx.uso),] # elimino los datos faltantes

---#tarea 4---
  
  espectrales <- c(10,12,14,16,18,20,22,24,26,28)
head( glx[, espectrales] )

glx_esp <- glx.uso[, espectrales]

cor(glx_esp,use = "pairwise.complete.obs")
corrplot(cor(glx_esp,use = "pairwise.complete.obs"),addCoef.col = "black")

glx_esp

norm_glx_esp <- apply(glx_esp[,1:9], 2,function (x) x-glx_esp[,10])

corrplot(cor(norm_glx_esp,use = "pairwise.complete.obs"),addCoef.col = "black")
