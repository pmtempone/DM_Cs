###TP 2####

----#Librarias-----
library(sp)
library(rworldmap)
library(rworldxtra)
library(dplyr)
library(igraph)
----#descarga dataset---- 

download.file("http://snap.stanford.edu/data/loc-brightkite_edges.txt.gz", destfile = "brkred.gz")

red <-  read.table(gzfile("brkred.gz"), header=F, 
             stringsAsFactors = F)
download.file("http://snap.stanford.edu/data/loc
              -
              brightkite_totalCheckins.txt.gz", destfile = "brkchk.gz")
checkins <-  read.table(file = gzfile("brkchk.gz"), header=F, 
             fill=T, stringsAsFactors = F)

----#preparacion  datos ---- # Wed Dec 14 16:25:40 2016 ------------------------------

names(checkins) <- c("id", "time", "lat", "lon", "loc_id")
checkins$id <- checkins$id + 1
red$V1 <- red$V1 + 1
red$V2 <- red$V2 + 1

checkins_c <- checkins[complete.cases(checkins),]
head(sort(table(checkins_c$loc_id), decreasing=T), n=14)
head(checkins_c[,3:5][checkins_c$loc_id == "00000000000000000000000000000000",])


uniq_checks <- unique(checkins_c[,3:5])


coords_pais = function(points){
  # Una función para recuperar el código ISO 3
  # de pais de una locación
  countriesSP <- getMap(resolution='high')
  # convertir las coordenadas en puntos espaciales
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))
  # recuperar el poligono-pais de cada punto espacial
  indices = over(pointsSP, countriesSP)
  # recuperar el nombre de cada pais por código ISO
  # de tres letras
  indices$ISO3
}


pais_iso <- coords_pais(uniq_checks[, 2:1])
head(sort(table(pais_iso), decreasing=T), 12)
table(pais_iso)["ARG"]

uniq_checks <- data.frame(uniq_checks, ISO3 = as.character(pais_iso))


head(uniq_checks)

----#ejemplo rusia---- # Wed Dec 14 16:31:51 2016 ------------------------------

uniq_checks_rusia <- filter(uniq_checks, ISO3 == "RUS")
checkins_rusia <- inner_join(checkins_c, uniq_checks_rusia[,3:4], by = "loc_id")
uniq_usuarios_rusia <- unique(checkins_rusia$id)

red.amistad <- graph.edgelist(as.matrix(red), directed = F)
summary(red.amistad)
red_rusia <- induced_subgraph(red.amistad, vids = uniq_usuarios_rusia)
summary(red_rusia)

walktrap_rusia <- walktrap.community(red_rusia, steps = 6)
walktrap_rusia
names(walktrap_rusia)
walktrap_rusia$modularity
walktrap_rusia$membership

sort(table(walktrap_rusia$membership), decreasing=T)
is.connected(red_rusia)

color_grado <- ifelse(degree(red_rusia) > 0, "red", "blue")
plot(red_rusia, vertex.label = NA,
     vertex.size = 8, vertex.color = color_grado)
