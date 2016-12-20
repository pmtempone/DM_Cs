install.packages("rgdal")
library(rgdal)
ogrInfo("NOR_adm", "NOR_adm1")
rus_shp1 <- readOGR("NOR_adm", "NOR_adm2")


dim(rus_shp1@data)
[1] 88 16
names(rus_shp1@data)
plot(rus_shp1, xlim=c(27,155))

plot(rus_shp1)
points(uniq_checks_noruega$lon, uniq_checks_noruega$lat, pch=20,
       col="darkred")
