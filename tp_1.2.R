library(reshape)
library(base)
library(ggplot2)
library(plotly)
#Levanto el dat
t3 <- readLines("http://www.mpia.de/COMBO/table3.dat")

#Extraigo del dataset las variables necesarias. uso desplazamientos por campos para leer caracteres.

nom.var <- c('Seq','x','y','Rmag','ApD_Rmag','phot_flag','MC_class','MC_z','UjMag','BjMag','VjMag','S280Mag')

desplazam <- matrix(c(1,33,41,49,69,104,117,133,179,192,207,308,5,39,47,54,75,107,131,137,184,197,212,313),  nrow=12, ncol=2)

extraer.v <- function(tabla.dat, inicio, fin){
  v.cruda <- substr(tabla.dat,inicio,fin)
  v.proc <- gsub("(^\\s+|\\s+$)", "", v.cruda)
  return(v.proc)
}

armar.dataset <- function(data, nv, desp){
  dataset <- c()
  for(i in 1:nrow(desp)) {
    col1 <- extraer.v(t3, desp[i,1],desp[i,2])
    col_format <- if(nv[i]=='MC_class'){as.character(col1)}else{as.numeric(col1)}
    dataset <- cbind(dataset, col_format)
  }
  colnames(dataset) <- nv
  return (data.frame(dataset))
}

#Armo el dataset

galaxias <- armar.dataset(t3, nom.var, desplazam)

#Me quedo unicamente con elementos de la clase galaxy

galaxias <- subset(galaxias,MC_class=='Galaxy')

#Guardo una copia sin normalizar y otra normalizada, para poder trabajar con los datos.

galaxias_final <- galaxias
galaxias_final[,-c(1,7)] <- apply(galaxias[,-c(1,7)],2,function (x) {as.numeric(x)})

galaxias[,-c(1,7)] <- apply(galaxias[,-c(1,7)],2,function (x) {scale(as.numeric(x))})

#Analizo los datos para encontrar outliers:

anal.out = melt(galaxias, id=c('Seq'))

anal.out[,1] <- NULL

#grafico boxplots para ver graficamente la distribucion de las variables:

anal.out.1 <- anal.out[!(anal.out$variable %in% c('x','y')),]
anal.out.1$value = as.numeric(anal.out.1$value)


#Rmag

ggplot(anal.out.1[anal.out.1$variable=='Rmag',], aes(x=variable, y=value, fill=variable)) + geom_boxplot()
ggplotly()
plot_ly(x = galaxias$Rmag, type = "histogram", histnorm = "probability") %>%
  layout(xaxis = list(type = "numeric"))

out.Rmag <- rownames(galaxias[galaxias$Rmag< (-6.5),])


#Phot_flag 

ggplot(anal.out.1[anal.out.1$variable=='phot_flag',], aes(x=variable, y=value, fill=variable)) + geom_boxplot()
ggplotly()
plot_ly(x = galaxias$phot_flag, type = "histogram", histnorm = "probability")%>%
  layout(xaxis = list(type = "numeric"))

out.phot_flag <- rownames(galaxias[galaxias$phot_flag>5,])


#MC_z 

ggplot(anal.out.1[anal.out.1$variable=='MC_z',], aes(x=variable, y=value, fill=variable)) + geom_boxplot()
ggplotly()

plot_ly(x = galaxias$MC_z, type = "histogram", histnorm = "probability")%>%
  layout(xaxis = list(type = "numeric"))

out.MC_z <- rownames(galaxias[galaxias$MC_z > 2.8,])



#UjMag 

ggplot(anal.out.1[anal.out.1$variable=='UjMag',], aes(x=variable, y=value, fill=variable)) + geom_boxplot()
ggplotly()

plot_ly(x = galaxias$UjMag, type = "histogram", histnorm = "probability")%>%
  layout(xaxis = list(type = "numeric"))

out.UjMag <- rownames(galaxias[galaxias$UjMag<= -3.175 | galaxias$UjMag>5,])


#BjMag 

ggplot(anal.out.1[anal.out.1$variable=='BjMag',], aes(x=variable, y=value, fill=variable)) + geom_boxplot()
ggplotly()

plot_ly(x = galaxias$BjMag, type = "histogram", histnorm = "probability")%>%
  layout(xaxis = list(type = "numeric"))

out.BjMag <- rownames(galaxias[galaxias$BjMag<= -4 | galaxias$BjMag>4.4,])


#VjMag

ggplot(anal.out.1[anal.out.1$variable=='VjMag',], aes(x=variable, y=value, fill=variable)) + geom_boxplot()
ggplotly()

plot_ly(x = galaxias$VjMag, type = "histogram", histnorm = "probability")%>%
  layout(xaxis = list(type = "numeric"))

out.VjMag <- rownames(galaxias[galaxias$VjMag<= -4 | galaxias$VjMag>4.5,])


#S280Mag

ggplot(anal.out.1[anal.out.1$variable=='S280Mag',], aes(x=variable, y=value, fill=variable)) + geom_boxplot()
ggplotly()

plot_ly(x = galaxias$S280Mag, type = "histogram", histnorm = "probability")%>%
  layout(xaxis = list(type = "numeric"))

out.S280Mag <- rownames(galaxias[galaxias$S280Mag>7,])


#ApD_Rmag

ggplot(anal.out.1[anal.out.1$variable=='ApD_Rmag',], aes(x=variable, y=value, fill=variable)) + geom_boxplot()
ggplotly()

plot_ly(x = galaxias$ApD_Rmag, type = "histogram", histnorm = "probability")%>%
  layout(xaxis = list(type = "numeric"))

out.ApD_Rmag <- rownames(galaxias[galaxias$ApD_Rmag<= -7.4 | galaxias$ApD_Rmag>4,])

####### Extraigo los valores outlier


out.BjMag <- as.numeric(out.BjMag)
out.MC_z <- as.numeric(out.MC_z)
out.phot_flag <- as.numeric(out.phot_flag)
out.Rmag <- as.numeric(out.Rmag)
out.VjMag <- as.numeric(out.VjMag)
out.UjMag <- as.numeric(out.UjMag)
out.ApD_Rmag <- as.numeric(out.ApD_Rmag)
out.S280Mag <- as.numeric(out.S280Mag)

outliers <- c(out.BjMag, out.MC_z, out.phot_flag, out.Rmag, out.UjMag, out.VjMag,out.ApD_Rmag,out.S280Mag)

galaxias_final <- galaxias_final[!(rownames(galaxias_final) %in% outliers),]

#Además, los autores del trabajo original informan que las galaxias con Rmag < 24

galaxias_final <- galaxias_final[galaxias_final$Rmag < 24,]

# es conveniente trabajar con registros que para la variable phot_flag tomen valores menores de 8

galaxias_final <- galaxias_final[galaxias_final$phot_flag < 8,]

# La variable ApD_Rmag no debería tomar valores menores que cero. En los casos que
# sea negativa, se deberían convertir los valores a cero, sin descartar el registro.

galaxias_final$ApD_Rmag <- ifelse(galaxias_final$ApD_Rmag<0,0,galaxias_final$ApD_Rmag)


####### Elimino casos faltantes (?????????)

faltantes <-galaxias_final[!complete.cases(galaxias_final),] 

# Da 3040 registros incompletos..    
