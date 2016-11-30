----#librerias----

library(ggplot2)
library(lattice)
library(gridExtra)
library(cluster)
library(dplyr)
library(MASS)
library(fpc)

----#punto 3 carga de dataset-----

load('table3.dat')

extraer.v <- function(inicio, fin){
  v.cruda <- substr(t3,inicio,fin)
  # Lo que sigue saca los espacios en blanco al inicio y fin
  v.proc <- gsub("(^\\s+|\\s+$)", "", v.cruda)
  return(v.proc)
}

lista <- list(
  Seq = c(1,5),RAh = c(7,8),RAm = c(10,11),RAs = c(13,18),DE = c(20,20),DEd = c(21,22),DEm = c(24,25),DEs = c(27,31),x = c(33,39),y = c(41,47),Rmag = c(49,54),e_Rmag = c(56,60),Ap_Rmag = c(62,67),ApD_Rmag = c(69,75),mu_max = c(77,82),MajAxis = c(84,89),MinAxis = c(91,96),PA = c(98,102),phot_flag = c(104,107),var_flag = c(109,109),stellarity = c(111,115),MC_class = c(117,131),MC_z = c(133,137),e_MC_z = c(139,143),MC_z2 = c(145,149),e_MC_z2 = c(151,155),MC_z_ml = c(157,161),dl = c(163,169),chi2red = c(171,177),UjMag = c(179,184),e_UjMag = c(186,190),BjMag = c(192,197),e_BjMag = c(199,205),VjMag = c(207,212),e_VjMag = c(214,220),usMag = c(222,227),e_usMag = c(229,233),gsMag = c(235,240),e_gsMag = c(242,248),rsMag = c(250,255),e_rsMag = c(257,263),UbMag = c(265,270),e_UbMag = c(272,276),BbMag = c(278,283),e_BbMag = c(285,291),VbMag = c(293,298),e_VbMag = c(300,306),S280Mag = c(308,313),e_S280Mag = c(315,320),S145Mag = c(322,327),e_S145Mag = c(329,332),W420F_E = c(334,343),e_W420F_E = c(345,354),W462F_E = c(356,365),e_W462F_E = c(367,376),W485F_D = c(378,387),e_W485F_D = c(389,398),W518F_E = c(400,409),e_W518F_E = c(411,420),W571F_D = c(422,431),e_W571F_D = c(433,442),W571F_E = c(444,453),e_W571F_E = c(455,464),W571F_S = c(466,475),e_W571F_S = c(477,486),W604F_E = c(488,497),e_W604F_E = c(499,508),W646F_D = c(510,519),e_W646F_D = c(521,530),W696F_E = c(532,541),e_W696F_E = c(543,552),W753F_E = c(554,563),e_W753F_E = c(565,574),W815F_E = c(576,585),e_W815F_E = c(587,596),W815F_G = c(598,607),e_W815F_G = c(609,618),W815F_S = c(620,629),e_W815F_S = c(631,640),W856F_D = c(642,651),e_W856F_D = c(653,662),W914F_D = c(664,673),e_W914F_D = c(675,684),W914F_E = c(686,695),e_W914F_E = c(697,706),UF_F = c(708,717),e_UF_F = c(719,728),UF_G = c(730,739),e_UF_G = c(741,750),UF_S = c(752,761),e_UF_S = c(763,772),BF_D = c(774,783),e_BF_D = c(785,794),BF_F = c(796,805),e_BF_F = c(807,816),BF_S = c(818,827),e_BF_S = c(829,838),VF_D = c(840,849),e_VF_D = c(851,860),RF_D = c(862,871),e_RF_D = c(873,882),RF_E = c(884,893),e_RF_E = c(895,904),RF_F = c(906,915),e_RF_F = c(917,926),RF_G = c(928,937),e_RF_G = c(939,948),RF_S = c(950,959),e_RF_S = c(961,970),IF_D = c(972,981),e_IF_D = c(983,992)
)

# Completo la lista con los valores
for (i in seq_along(lista)) {
  lista[[i]] <- do.call('extraer.v', as.list(lista[[i]]))
}

# Me quedo con estas variables

nom.var <- c('Seq','x','y','Rmag','ApD_Rmag','phot_flag','MC_class','MC_z','UjMag','BjMag','VjMag','S280Mag')

lista <- lista[which(names(lista) %in% nom.var)]

galaxias <- as.data.frame(do.call('cbind', args = lista, quote = F))

galaxias[,-7] <- apply(galaxias[,-7], 2, as.numeric) ; 
rm(list = ls()[-2]) # convierto a numeric las variables
remove(lista)

# Me quedo unicamente con elementos de la clase galaxy

galaxias <- subset(galaxias, MC_class == 'Galaxy')
galaxias$MC_class <- as.factor(as.character(galaxias$MC_class)) ; table(galaxias$MC_class) # check

# Me quedo unicamente con elementos con Rmag < 24 y phot_flag < 8

galaxias <- subset(galaxias, Rmag < 24 & phot_flag < 8)

hist(galaxias$Rmag) ; hist(galaxias$phot_flag) # check

# Convierto a 0 los valores negativos de ApD_Rmag, ya que no debería tenerlos

galaxias <- transform(galaxias, ApD_Rmag = ifelse(ApD_Rmag < 0, 0, ApD_Rmag))
hist(galaxias$ApD_Rmag) # check

# Borro los casos que tengan al menos un NA

galaxias <- galaxias[complete.cases(galaxias),]

# Identifico outliers

grid.arrange(densityplot(~ Rmag, data = galaxias), bwplot(~ Rmag, data = galaxias))# Rmag
grid.arrange(densityplot(~ ApD_Rmag, data = galaxias), bwplot(~ ApD_Rmag, data = galaxias)) # ApD_Rmag
densityplot(~ MC_z, data = galaxias) # MC_z
grid.arrange(densityplot(~ UjMag, data = galaxias), bwplot(~ UjMag, data = galaxias)) # UjMag
grid.arrange(densityplot(~ BjMag, data = galaxias), bwplot(~ BjMag, data = galaxias)) # BjMag
grid.arrange(densityplot(~ VjMag, data = galaxias), bwplot(~ VjMag, data = galaxias)) # VjMag
densityplot(~ S280Mag, data = galaxias) # S280Mag

# outlier para las cuatro variables 4753

which.min(galaxias$UjMag) ; which.min(galaxias$BjMag) ; which.min(galaxias$VjMag) ; which.min(galaxias$Rmag)

xyplot(UjMag ~ BjMag, data = galaxias,
       panel = function(x,y,...) {
         panel.xyplot(x, y, ...)
         ltext(x= min(galaxias$UjMag, na.rm = T),
               y= min(galaxias$BjMag, na.rm = T),
               labels = '4753',
               cex = 0.8, pos = 3)
       }
)

galaxias <- galaxias[-4753,] # saco el outlier

# Identifico outliers en ApD_Rmag

# # Identifico outliers en ApD_Rmag con tecnica de Bootstrap
#
# n <- length(galaxias$ApD_Rmag)
# B <- 100
# boots <- matrix(galaxias$ApD_Rmag[sample(1:nrow(galaxias), n * B, replace = T)], nrow = n, ncol = B)
#
# for (i in seq(0.4,1,0.05)) {
#         print(paste('Prob(>',i,') = ', mean(apply(boots, 2, function(x) {sum(x > i)})/n), sep = ''))
# }
# which(galaxias$ApD_Rmag > 0.6) # consdiero estos numeros outliers por estar muy alejados de los datos


----#punto 3---------

#parte1

galaxias_pt3 <- galaxias %>% select(Seq,x,y,Rmag,ApD_Rmag,MC_z,UjMag,BjMag,VjMag,S280Mag) %>% mutate(UjMag_d=UjMag-S280Mag, Bjmag_d= BjMag-S280Mag,VjMag_d=VjMag-S280Mag)
galaxias_pt3 <- galaxias_pt3 %>% select(-UjMag,-VjMag)

# Selecciono variables


glx.uso_pt3 <- galaxias_pt3[,c(4:11)]


#splom(glx.uso[,c('Rmag','ApDRmag','Mcz','UjMAG.norm','VjMAG.norm','BjMAG','S280MAG')])

#heatmap(as.matrix(similarity.m))

glx.uso_pt3.std <- scale(glx.uso_pt3)
dist.m <- dist(glx.uso_pt3.std, method = 'euclidean')

validacion.sse <- 0      
validacion.silhouette <- 0
Ks <- 2:30
for (i in 1:length(Ks)) {
  set.seed(432748)
  k.cluster <- kmeans(glx.uso_pt3.std, Ks[i], nstart = 20)
  validacion.sse[i] <-  sum(k.cluster$withinss)
  validacion.silhouette[i] <- mean(silhouette(k.cluster$cluster, dist.m)[,3])
  print(i)
  }


# Elijo K = 4
sse <- xyplot(validacion.sse ~ as.factor(Ks),  type = 'l', xlab = NULL)
sil <- barchart(validacion.silhouette ~ as.factor(Ks), horizontal = F)
grid.arrange(sse, sil)

#se hace kmeans

k.cluster.final <- kmeans(glx.uso_pt3.std, 3, nstart = 20)

plot(silhouette(k.cluster.final$cluster, dist.m),col=3, border=NA)


#PAM no correr!

glx.gower <- daisy(glx.uso_pt3.std,metric = "gower",stand = FALSE)
galaxias_pt3.2 <- cbind(galaxias_pt3,cluster=k.cluster.final$cluster)

p <- ggplot(galaxias_pt3.2, aes(x, y))
p + geom_point(aes(colour=factor(cluster)))+labs(colour = "Clusters")
