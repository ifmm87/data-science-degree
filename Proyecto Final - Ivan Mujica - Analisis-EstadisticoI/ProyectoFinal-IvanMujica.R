# Trabajo Final
# Leemos el archivo
# padron por anios
# importante!!! setear el directorio de trabajo!!!!
datos  = read.csv("./padron_anios.csv", header=TRUE)
datos
summary(datos$Cantidad)
var(datos$Cantidad)
sd(datos$Cantidad)

# para el incremento
data <- data.frame( gestion= datos$Gestion,
                    cantidad = datos$Cantidad)

data$incremento <-  c(0,diff(data$cantidad)/data$cantidad[-length(data$cantidad)]*100)
# calculando la columna del logaritmo de la cantidad de empresas
data$logcantidad <-  log(data$cantidad)

par(lab=c(12,12,12))

# grah1
plot(data$gestion, 
     data$cantidad, 
     xlab="Gestión", 
     ylab="Empresas", 
     main="Número de empresas",
     pch=19, cex=2)
grid()

# grah2
plot(data$gestion, 
     log(data$cantidad), 
     xlab="Gestión", 
     ylab="Empresas", 
     pch=19, cex=2)
grid()


# graph3
plot(data$gestion, 
     data$incre, 
     xlab="Gestión", 
     ylab="Empresas", 
     main="Crecimiento porcentual de nuevas empresas por año",
     pch=19, cex=2, type="o")
grid()

# data del 2017
datos2017  = read.csv("./empresas_2017.csv", header=TRUE)
datos2017
(datos$cantidad)
pedazos <- c(datos2017$cantidad)
departamentos <- c(datos2017$departamento)

pct <- round(pedazos/sum(pedazos)*100)
pct
departamentos <- paste(departamentos, pct) 
departamentos <- paste(departamentos,"%",sep="") 

pie(pedazos, 
    labels = departamentos, 
    main="Participación porcentual según departamento 2017",
    col=rainbow(length(departamentos)))


# modelo lineal
# modelo de cantidad de empressa vs la gestion
mod0 <- lm(data$cantidad ~ data$gestion, data = data)
summary(mod0)

log(data$cantidad)
mod <- lm(log(data$cantidad) ~ data$gestion, data = data)
?log
summary(mod)

# El coeficiente de determinacion (R cuadrado) es de 97.5% 
# significa que el modelo explica el 97.5% del numero empresas 
# desde el 2005 al 2017.

# el nuevo  Modelo de regresion y final
# Y = -270.9720 + 0.1406 X
# x : es el tiempo
# Y: cantidad
# el valor 0.1406 se interpreta como porcentaje de incremento


# modelo lineal
# modelo de cantidad de empressa vs la gestion
mod0 <- lm(cantidad ~ gestion, data = data)
summary(mod0)

mod <- lm(log(cantidad) ~ gestion, data = data)

summary(mod)

# El coeficiente de determinacion (R cuadrado) es de 97.5% 
# significa que el modelo explica el 97.5% del numero empresas 
# desde el 2005 al 2017.

# el nuevo  Modelo de regresion y final
# Y = -270.9720 + 0.1406 X
# x : es el tiempo
# Y: cantidad
# el valor 0.1406 se interpreta como porcentaje de incremento

gest <- seq(2016,2022,1)
dat<- data.frame(gestion=gest)

pronostico <- exp(predict(mod,dat))
names(pronostico) <-  gest
pronostico

# grah2
plot(gest, 
     pronostico, 
     xlab="Gestión", 
     ylab="Empresas", 
     pch=19, cex=2)
grid()

# analisis de regresion
# tabla de regresión anova
summary(aov(mod))

# la hipotesis es que los betas (coeficientes de la regresion) 
# son iguales a cero vs que no lo sean.
# como el pvalor es 4.09e-10  significa que se rechaza la hipotesis nula
# Existe evidencia estadística que los coeficientes son distintos a cero 
# y por lo tanto son significativos.

