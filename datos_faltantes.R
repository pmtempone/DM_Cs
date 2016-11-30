####DATOS FALTANTES####

  
  # primero creamos una matriz con datos faltantes
  a <- 1:50 + runif(50,-5,5)
  b <- a*0.3 + 4 + runif(50, -5,5)
  c <- 25:-24 + rnorm(50,0,6)
  a[sample(1:50,5)] <- NA
  b[sample(1:50,5)] <- NA
  c[sample(1:50,10)] <- NA
  abc <- cbind(a,b,c)
  # Podemos obtener un dataset "limpio" (eliminación de casos completos)
  na.omit(abc)
  attr(na.omit(abc), "na.action")
  # Información sobre datos faltantes
  is.na(abc)
  which(is.na(abc))
  apply(abc,2,function(x) which(is.na(x)))
  apply(abc,2,function(x) table(is.na(x)))
  cor(abc)
  cor(abc, use="complete.cases")
  cor(abc, use="pairwise.complete.obs")