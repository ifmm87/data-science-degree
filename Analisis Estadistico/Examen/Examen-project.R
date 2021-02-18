# Examen Ivan Mujica
# Pregunta 1
# Leemos el archivo
datos  = read.csv("./datos_18_p.csv", header=TRUE)
datos

precio = datos$precio
ingreso = datos$ingreso
autos = datos$autos
ventas = datos$ventas
# estadisticos
datos
#
hist(precio, main = "Precios de los automoviles ",
     xlab = "precio", ylab = "cantidad",col = "steelblue")

hist(autos, main = "Cantidad de autos",
     xlab = "autos", ylab = "cantidad",
     col = "steelblue")

plot(precio, ingreso)

# Pregunta 2
# determinar los estadisticos de las 4 variables
# no summary
#variables
names(datos)


# precio
precio.minimo = min(precio) 
cat("El precio minimo es ", precio.minimo , "\n")
precio.maximo = max(precio)
cat("El el precio maximo es ", precio.maximo, "\n") # el valor maximo de la muestra es 195.7
precio.media = mean(precio)
cat("La media del precio es" , precio.media,  "\n")
precio.varianza = var(precio)
cat("La varianza del precio es", precio.varianza, "\n")
precio.mediana = median(precio)
cat("La mediana del precio es ", precio.mediana, "\n")
precio.desviacion = sd(precio)
cat("La desviacion estandar del precio ", precio.desviacion, " al rededor de la media", "\n")
precio.rango = IQR(precio)
cat("El rango del precio es ", precio.rango, "\n")
precio.cuartiles = quantile(precio)
cat("Los cuartiles del precio son", precio.cuartiles, "\n")

# ingreso
ingreso.minimo = min(ingreso) 
cat("El Ingreso minimo es ", ingreso.minimo , "\n")
ingreso.maximo = max(ingreso)
cat("El  Ingreso maximo es ", ingreso.maximo, "\n")
ingreso.media = mean(ingreso)
cat("La media del Ingreso es" , ingreso.media, "\n")
ingreso.varianza = var(ingreso)
cat("La varianza del Ingreso es", ingreso.varianza, "\n")
ingreso.mediana = median(ingreso)
cat("La mediana del Ingreso es ", ingreso.mediana, "\n")
ingreso.desviacion = sd(ingreso)
cat("La desviacion estandar del Ingreso ", ingreso.desviacion, "al rededor de la media", "\n")
ingreso.rango = IQR(ingreso)
cat("El rango del Ingreso es ", ingreso.rango, "\n")
ingreso.cuartiles = quantile(ingreso)
cat("Los cuartiles del Ingreso son", ingreso.cuartiles, "\n")

# autos
autos.minimo = min(autos) 
cat("El minimo de autos es ", autos.minimo , "\n")
autos.maximo = max(autos)
cat("El maximo numero de autos es ", autos.maximo, "\n")
autos.media = mean(autos)
cat("La media del numero de autos es" , autos.media, "\n")
autos.varianza = var(autos)
cat("La varianza del numero de autos es", autos.varianza, "\n")
autos.mediana = median(autos)
cat("La mediana del numero de autos es ", autos.mediana, "\n")
autos.desviacion = sd(autos)
cat("La desviacion estandar del numero de autos ", autos.desviacion, "al rededor de la media", "\n")
autos.rango = IQR(autos)
cat("El rango del numero de autos es ", autos.rango, "\n")
autos.cuartiles = quantile(autos)
cat("Los cuartiles del numero de autos son", autos.cuartiles, "\n")

# ventas
ventas.minimo = min(ventas) 
cat("El minimo valor  de ventas es ", ventas.minimo , "\n")
ventas.maximo = max(ventas)
cat("El maximo valor de ventas es ", ventas.maximo, "\n")
ventas.media = mean(ventas)
cat("La media del valor  de ventas es" ,ventas.media, "\n")
ventas.varianza = var(ventas)
cat("La varianza valor  de ventas es", ventas.varianza, "\n")
ventas.mediana = median(ventas)
cat("La mediana delvalor  de ventas es ",ventas.mediana, "\n")
ventas.desviacion = sd(ventas)
cat("La desviacion estandar valor  de ventas ", ventas.desviacion, "al rededor de la media", "\n")
ventas.rango = IQR(ventas)
cat("El rango del valor  de ventas es ", ventas.rango, "\n")
ventas.cuartiles = quantile(ventas)
cat("Los cuartiles del valor  de ventas", ventas.cuartiles, "\n")

##
# 3 PROBABILIDAD
# A
n=10
p=0.30
x=5
dbinom(x,n,p)


n=10
p=0.30
sum(dbinom(4:10,n,p))

n=10
p=0.30
sum(dbinom(0:5,n,p))

#B

media = 3.52
k = 3
dpois(k, media)

media = 3.52
k = 3:7
sum(dpois(k, media))

media = 3.52
k = 4
dpois(k, media)

# 4 Intervalos de confianza, se conoce sigma
intervalo_media=function(muestra,media,desviacion,nivel)
{
  p=(1+(nivel/100))/2
  zc<-qnorm(c(p),mean=0,sd=1,lower.tail=TRUE)
  i<-(zc*desviacion)/sqrt(muestra)
  cat("El intervalo de confianza para la media,\n")
  cat("al nivel de confianza",nivel,"%es,\n")
  cat("(",media-i,",",media+i,")\n")
}

##   Para precio
n=16
confianza =99
intervalo_media(n, precio.media, precio.desviacion, confianza)

n=16
confianza =99
intervalo_media(n, ingreso.media, ingreso.desviacion, confianza)

n=16
confianza =99
intervalo_media(n, autos.media, autos.desviacion, confianza)

n=16
confianza =0.99
intervalo_media(n, ventas.media, ventas.desviacion, confianza)


# 5

# A
#H0: mu_precio = 150, con significancia=0.06 

desviacion = precio.desviacion   
n  = 16 
media = precio.media
mu = 150     
zc = (media-mu)/(desviacion/(sqrt(n))) 
pnorm(zc)

#B
#H0: mu_ingreso = 160, con significancia=0.03
desviacion = ingreso.desviacion
n = 16        
media = ingreso.media
mu = 160      
zc = (media-mu)/(desviacion/(sqrt(n))) 
pnorm(zc)


