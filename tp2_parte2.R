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
plot(nor_map_r, xlim = c(10, 20), ylim = c(44, 85), asp = 1)
points(uniq_checks_rusia$lon, uniq_checks_rusia$lat, col = "red", cex =
         .7, pch=20)