install.packages("parsedate")
library(parsedate)
# Prueba:
date_test <- parse_iso_8601( checkins_c[1:2, 2] )
date_test
"2010-10-17 01:48:53 UTC"
"2010-10-16 06:02:04 UTC"

install.packages("geonames")
library(geonames)
options(geonamesUsername= "pmtempone")
options(geonamesHost="api.geonames.org")
tz.test <- GNtimezone(uniq_checks_rusia$lat[1], uniq_checks_rusia
                      $lon[1], radius = 0)

tz.nor <- GNtimezone(uniq_checks_noruega$lat[1], uniq_checks_noruega
                      $lon[1], radius = 0)

w_map_r <- getMap(resolution = "low")
plot(w_map_r)
points(uniq_checks_noruega$lon, uniq_checks_noruega$lat, col = "red", cex =
         .7, pch=20)

nor_map_r <- getMap(resolution = "low")
plot(nor_map_r, xlim = c(30, 31), ylim = c(45, 75), asp = 1)
points(uniq_checks_noruega$lon, uniq_checks_noruega$lat, col = "red", cex =
         .7, pch=20)


install.packages("ggmap")
library("ggmap")
noruega_mapa_1 <- get_map(location = c(17,66), zoom = 4)
ggmap(noruega_mapa_1)
noruega_checkin_map_1 <- ggmap(noruega_mapa_1) +
  geom_point(aes(x = lon, y = lat),
             alpha = 0.5,
             data = uniq_checks_noruega)
noruega_checkin_map_1


noruega_checkin_map_2 <- ggmap(noruega_mapa_1) +
  geom_point(aes(x = lon, y = lat, col = log10(n)),
             alpha = 0.5,
             data = uniq_checks_noruega) +
  scale_color_gradient(low="blue", high="red")
noruega_checkin_map_2


cantidades_nor <-  checkins_noruega %>%  
  mutate(cantidad=as.numeric(loc_id)) %>%  
  group_by(lon,lat) %>%
  summarise(total.count=n(), 
            count=sum(is.na(cantidad)))


mapPoints <- ggmap(noruega_mapa_1) + geom_point(aes(x = lon, y = lat, size = count,col=count), data = cantidades_nor, alpha = .5)

mapPoints


m <- get_map("Norway",zoom=4,maptype="toner",source="stamen")
ggmap(m) + geom_point(aes(x=lon,y=lat,color=count),data=cantidades_nor) + geom_point(size=3,alpha=0.3)
g <- ggmap(m)
g <- g+stat_density2d(aes(x = lon, y = lat, fill=..level..), data=checkins_noruega,geom="polygon", alpha=0.2)
g + scale_fill_gradient(low = "yellow", high = "red")

m <- get_map("Norway",zoom=6)
ggmap(m) + geom_point(aes(x=lon,y=lat,color=count,size = count),data=cantidades_nor) + geom_point(size=3,alpha=0.3)
g <- ggmap(m)+stat_density2d(aes(x = lon, y = lat, fill=..level..), data=cantidades_nor,geom="polygon", alpha=0.2)

g

# install.packages("fossil")
library(fossil)
noruega_dist <- earth.dist(checkins_noruega[,4:3]) #ojo con la memoria!
plot(hclust(noruega_dist))
rusia_clusts <- cutree(hclust(noruega_dist), h = 20)
unique(rusia_clusts)
checkins_rusia <- data.frame(checkins_rusia, geo_cluster = rusia_clusts)
cluster_cons <- checkins_rusia %>% group_by(geo_cluster) %>%
  summarize(lat=mean(lat), lon=mean(lon))

