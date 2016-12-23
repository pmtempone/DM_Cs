## MAPAS : POSICIONES 
library(ggmap)
#busco los unique; no necesito calcular para los 22300, con calcular los no repetidos despues puedo pegar el resultado de cada uno de esos en los demas que tambien se repitan.

pos_noruega <- unique(cbind(checkins_noruega$lat,checkins_noruega$lon))
pos_noruega <- cbind(1:nrow(pos_noruega), pos_noruega)
pos_noruega <- data.frame(pos_noruega)
colnames(pos_noruega) <- c('id','lat','lon')

#me quedo con la tercera parte, en mi caso tomo los primeros 1597 casos.

pos_noruega.2 <- pos_noruega[pos_noruega$id >1597 & pos_noruega$id <=(1597+2500),]

#obtengo los datos usando la libreria

library(ggmap)

pos_noruega.2$textAddress <- mapply(FUN = function(lon, lat) revgeocode(c(lon, lat)), pos_noruega.2$lon, pos_noruega.2$lat) 


save(pos_noruega.2, file='C:/Users/Pablo/Google Drive/TP DM Tecno/pos-noruega-2.rda')


ciudades <- rbind(pos_noruega_,pos_noruega.2,pos_noruega.1[,1:4])

# Funcion para extraer lugares
extraer.ciudad <- function(x) {
  options(warn = -1)
  lista.lugares <- strsplit(x, split = ' |,')
  ciudad <- unlist(
    lapply(lista.lugares, function(x) { if (length(which(x == 'Norway')) > 0) {x[which(x == 'Norway')-2]} else { NA } })
  )
  ciudad <- ifelse(is.na(as.numeric(ciudad)), ciudad, NA)
  options(warn = 0)
  return(ciudad)
  
}

# Ejemplo de uso
a <- extraer.ciudad(ciudades$textAddress)

ciudades <- data.frame(ciudades,ciudad=a)

save(ciudades,file='C:/Users/Pablo/Google Drive/TP DM Tecno/ciudades.rda')
