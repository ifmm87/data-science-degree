# Ejercicio 21
error_estandar = function (muestra, desviacion) {
  err =  desviacion / sqrt(muestra)
  cat(err)
}
error_estandar(50, 10)
#A
# Ejercicio 22
# A
miu = 51800
desv = 4000
n = 60
desv_muestral = desv / sqrt(n)
desv_muestral
plot_normal(51800,516.3978)
#B
n = 120
desv_muestral = desv / sqrt(n)
desv_muestral
plot_normal(miu,desv_muestral)

# 3
# Error
error = function (muestra, desviacion, nivel) {
  p=(1+(nivel/100))/2
  zc<-qnorm(c(p),mean=0,sd=1,lower.tail=TRUE)
  cat("El Error es,\n")
  err = zc * desviacion / sqrt(muestra)
  cat(err)
}
error(60, 4000, 95)

# 4

intervalo_media_desv_desconocido=function(muestra,media, desviacion,nivel)
{
  p=(1+(nivel/100))/2
  tc = qt(c(p),df=muestra) 
  i<-(tc*desviacion)/sqrt(muestra)
  cat("El intervalo de confianz para la media,\n")
  cat("al nivel de confianza",nivel,"%es,\n")
  cat("(",media-i,",",media+i,")\n")
}
intervalo_media_desv_desconocido(100,49,8.5,95)

# 5

#A calculo de n
tamano_muestra_media=function(error,desviacion,nivel)
{
  p=(1+(nivel/100))/2
  zc<-qnorm(c(p),mean=0,sd=1,lower.tail=TRUE)
  zc
  n<-((zc*desviacion)/(error))^2
  cat("El tamanio de la muestra para,\n")
  cat("un nivel de confianza de ",nivel,"%,\n")
  cat("con un error y una desviacion","es\n")
  cat(ceiling(n),"\n")
}
tamano_muestra_media(0.05,0.15,95)
# 6
tamano_muestra_media(200,3750,95)

#
#9
n = 400
mu = 910
sigma = 1600
alfa = 0.05
z = -1.83
P = 0.0336
z =(910-1056)/(1600/sqrt(400))
z

#10
n = 40
x = 118000
sigma = 30333
alfa = 0.05
z =(118000-125500)/(30000/sqrt(40))
z

# 11
n = 100
mu = 231
sigma = 80
alfa = 0.05
t = (mu - 238)/(sigma/sqrt(n))
t
