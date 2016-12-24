walktrap_nor <- walktrap.community(red_noruega, steps = 6)
walktrap_nor
names(walktrap_nor)
walktrap_nor$modularity
walktrap_nor$membership


sort(table(walktrap_nor$membership), decreasing=T)
is.connected(red_noruega)

color_grado <- ifelse(degree(red_noruega) == 0 , "red", ifelse(degree(red_noruega)>0 & degree(red_noruega) <=2,  "blue","green"))
plot(red_noruega, vertex.label = NA,
     vertex.size = 8, vertex.color = color_grado)
legend("topright",legend = c("bajo grado","grado medio","grado alto"),fill = c("red","blue","green"),cex = 0.5, lwd = 2)


color_grado[walktrap_nor$membership == 4] <- "green"
color_grado[walktrap_nor$membership == 2] <- "orange"
plot(red_noruega, vertex.label = NA,
     vertex.size = 8, vertex.color = color_grado)


modularity(red_noruega, walktrap_nor$membership)


# Un array de modularidades al azar
random.membership <- array()
# Valores de modularidad para 1000 agrupamientos al azar
for(i in 1:1000) random.membership[i] <- modularity( red_noruega, sample(1:2, 723, replace = T) )
# test del clustering basado en las modularidades
table( modularity(red_noruega, walktrap_nor$membership) > random.membership )


walktrap_fast_greedy <- cluster_fast_greedy(red_noruega)

walktrap_fast_greedy

table( modularity(red_noruega, walktrap_fast_greedy$membership) > random.membership )

sin0 <- seq(1,723)[-which(degree(red_noruega) == 0)]

red_noruega <- induced.subgraph(red_noruega, sin0)

walktrap_fast_greedy <- cluster_fast_greedy(red_noruega)

walktrap_fast_greedy

table( modularity(red_noruega, walktrap_fast_greedy$membership) > random.membership )


plot(red_noruega, vertex.label = NA,
     vertex.size = 8, vertex.color = walktrap_fast_greedy$membership)

walktrap_spin <- cluster_spinglass(red_noruega,spin=8)

walktrap_spin

table( modularity(red_noruega, walktrap_fast_greedy$membership) > random.membership )


plot(red_noruega, vertex.label = NA,
     vertex.size = 8, vertex.color = walktrap_fast_greedy$membership)





