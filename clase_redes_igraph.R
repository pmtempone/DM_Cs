install.packages("igraph")
library(igraph)
library(ggplot2)

download.file("http://moreno.ss.uci.edu/beach.dat", destfile = "windsurfers.dat")
ws <-  read.table("windsurfers.dat", skip = 7)
dim(ws)

ws.obs  <-  as.matrix(ws[1:43, ])
ws.per <-  as.matrix(ws[44:86, ])

ws.obs.red <-  graph.adjacency(ws.obs, mode="undirected", diag=FALSE, weighted = T)
plot(ws.obs.red)


hist(ws.per[lower.tri(ws.per)], main = "histograma de las interacciones percibidas")

umbral <‐ 0.5
ws.per.2 <‐ ws.per
ws.per.2[which(ws.per.2 <= umbral)] <‐ 0
ws.per.red <‐ graph.adjacency(ws.per.2, mode="undirected", diag=FALSE, weighted = T)
plot(ws.per.red)

summary(ws.obs.red)
#info dice que no es dirigido y que tiene peso

summary(ws.per.red)


# Para información más detallada
# str(ws.obs.red)
# Si necesito sólo los recuentos
vcount(ws.obs.red)

ecount(ws.obs.red)

# Para ver los nodos y aristas
V(ws.obs.red)

E(ws.obs.red)


V(ws.per.red)


E(ws.per.red)


is.simple(ws.obs.red)

is.connected(ws.obs.red) #Si es conexo o no

is.connected(ws.per.red)


# Red de interacciones observadas
diameter(ws.obs.red)
get.diameter(ws.obs.red)
# Red de interacciones percibidas
diameter(ws.per.red)
#da con decimales porque usa el peso

get.diameter(ws.per.red) #cuales son los nodos 


graph.density(ws.obs.red)


graph.density(ws.per.red)


head (transitivity(ws.obs.red, type = "local"))

# red de interacciones observadas
transitivity(ws.obs.red, type = "global")

# red de interacciones percibidas
transitivity(ws.per.red, type = "global")


par(mfrow = c(1,2))
hist(transitivity(ws.obs.red, type = "local"), main = "observada",
     breaks = seq(0.2, 1, 0.1), xlab = "coefs. de clustering")
hist(transitivity(ws.per.red, type = "local"), main = "percibida",
     breaks = seq(0.2, 1, 0.1), xlab = "coefs. de clustering")

library(ggplot2)


degree(ws.obs.red, mode = "in")
degree(ws.obs.red, mode = "out")
qplot(degree(ws.per.red), degree(ws.obs.red))

head( degree.distribution(ws.obs.red ), 15)

par( mfrow = c(1,2) )
plot( sort(degree.distribution(ws.obs.red)),
      xlab = "grados", ylab = "proporción de nodos", type = "h", main = "observadas")
plot( sort(degree.distribution(ws.per.red)),
      xlab = "grados", ylab = "proporción de nodos", type = "h", main = "percibidas")

par( mfrow = c(1,2) )
plot( sort(degree.distribution(ws.obs.red)),
      xlab = "grados", ylab = "proporción de nodos", type = "h", main = "observadas")
plot( sort(degree.distribution(ws.per.red)),
      xlab = "grados", ylab = "proporción de nodos", type = "h", main = "percibidas")

# El display gráfico vuelve a la configuración de un gráfico por panel
par( mfrow = c(1,2) )
par( mfrow = c(1,2) )
plot( degree.distribution(ws.obs.red, cumulative = T), type = "l", xlab = "grado", ylab = "propo
      rción de nodos con grado > x", main = "observadas")
plot( degree.distribution(ws.per.red, cumulative = T), type = "l", xlab = "grado", ylab = "propo
      rción de nodos con grado > x", main = "percibidas")

#mundos pequeños

ws.obs.red.plf <‐ power.law.fit(degree(ws.obs.red))
ws.per.red.plf <‐ power.law.fit(degree(ws.per.red))

ws.obs.red.plf$alpha #el alpha tiene q ser mayor a 1

ws.per.red.plf$alpha
ws.obs.red.plf$KS.p
ws.per.red.plf$KS.p

#se quiere demostrar que ajusto bien a la ley de potencia, h nula que tienen la misma distribucion,como se tienen pocos datos se no se le cree
#porque el x minimo es alto, se agrega x minimo bajos

ws.obs.red.plf$xmin

ws.obs.red.plf.2 <‐ power.law.fit(degree(ws.obs.red), xmin = 9)
ws.obs.red.plf.2$alpha


#simulacion erdos-renyi

rg.transitivity.barabasi <‐ array()
rg.transitivity.erdos <‐ array()
for(i in 1:1000){
  rg.1 <‐ barabasi.game(43, power = 2.4, m=8, directed = F)
  rg.2 <‐ sample_gnm(43, 336)
  rg.transitivity.barabasi[i] <‐ mean(transitivity(rg.1, "local", isolates="zero"))
  rg.transitivity.erdos[i] <‐ mean(transitivity(rg.2, "local", isolates="zero"))
}
red.transitivity <‐ mean(transitivity(ws.obs.red, "local", isolates="zero"))
table(red.transitivity > rg.transitivity.barabasi)

hist(rg.transitivity.barabasi, main = "coef. clustering, grafos de Barabasi‐Albert")
abline( v = red.transitivity, col ="red", lwd= 2)
par(mfrow=c(1,1))

table(red.transitivity > rg.transitivity.erdos)

hist(rg.transitivity.erdos, xlim=c(0.34, 0.7), main = "coef. clustering, grafos al azar")
abline( v = red.transitivity, col ="red", lwd= 2) #coeficiente es mayor a lo esperado para un grafo al azar


