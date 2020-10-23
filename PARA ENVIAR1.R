## importacion de datos
## i) de archivo txt

Cavendish <- read.table("c:/Cavendish.txt", header=TRUE)
Cavendish
summary(Cavendish)

Cavendish <- read.table("f:/Cavendish.txt", header=TRUE)
Cavendish
summary(Cavendish)
plot(Cavendish)

## ii) de archivo excel  ####Esto funciona con R-32bits

install.packages("RODBC")
library(RODBC)
venta = odbcConnectExcel(file.choose()) 
# almacenamos el resultado en la variable mydat
mydat = sqlFetch(venta, 'Sheet1')
#cerramos la conexion
odbcClose(venta)
#imprimimos los datos importados
mydat

######otro ejemplo####Esto funciona con R-32bits
install.packages("RODBC")
library(RODBC)
empleado = odbcConnectExcel(file.choose()) 
# almacenamos el resultado en la variable mydat
mydat = sqlFetch(empleado, 'Hoja1')
#cerramos la conexion
odbcClose(empleado)
#imprimimos los datos importados
mydat


## iii)  #### otra manera de improtar a excel####Esto funciona con R-64bits

install.packages("readxl")
library("readxl")
b<-read_excel("C:/venta.xls",'Sheet1')
b






## iv)  Generando vectores para trabajo estadistico
## suponiendo que x es el gasto semanal de familias
x<-c(765,856,678,567,854,980,456,1080)
x
summary(x)
plot(x)
hist(x)

###########################
##Creación de un dataframe  ##  Los dataframes son una clase de objetos especial en R.
##Normalmente, cuando se realiza un estudio estadístico sobre los sujetos u objetos de
##una muestra, la información se organiza precisamente, en un dataframe: 
## una hoja de datos, en los que cada fila corresponde a un sujeto y cada columna a una variable.
##dataframe podemos incluir también datos alfanuméricos.  
## EJEMPLO
edad <- c(22, 34, 29, 25, 30, 33, 31, 27, 25, 25)
tiempo <- c(14.21, 10.36, 11.89, 13.81, 12.03, 10.99, 12.48, 13.37, 12.29, 11.92)
sexo <- c("M","H","H","M","M","H","M","M","H","H")
misDatos <- data.frame(edad,tiempo,sexo)
misDatos
str(misDatos)    # Estructura de 'misDatos'
names(misDatos)  # Nombre de las variables contenidas en 'misDatos'

##Acceso a variables dentro de un dataframe
##data.frames R dispone de algunas funciones 
##que facilitan la tarea de seleccionar o filtrar datos.
##por ejemplo, si queremos ver sólo los datos de los 
##sujetos (filas) 3 a 6, escribiríamos:

misDatos[3:6,]

##Si queremos seleccionar los datos de edad (primera columna), 
##podemos tratar a misDatos igual que si fuese una matriz:

misDatos[,1]

##Aunque también podemos referirnos a la columna por su nombre:

misDatos$edad

##Así pues, los siguientes comandos son equivalentes y dan el mismo resultado:

mean(misDatos[,1])
mean(misDatos$edad)
mean(misDatos[,"edad"])
mean(misDatos[["edad"]])

#####################  Los comandos attach y detach

##deseamos construir tablas de frecuencias de cada una de las variables 
##que contiene, una tabla de frecuencias cruzadas para el nivel de estudios 
##por sexo, y que además queremos calcular la edad medio de los individuos 
##de cada sexo. La sintaxis a utilizar sería la siguiente:

table(misDatos$estudios)
table(misDatos$sexo)
table(misDatos$edad)
table(misDatos$sexo,misDatos$edad)
mean(misDatos$edad[misDatos$sexo=="M"])
mean(misDatos$edad[misDatos$sexo=="H"])


##escribir tantas veces misDatos resulta tedioso, al margen de que se multiplica
## el riesgo de cometer errores en la redacción de los comandos.
##Para evitar este problema podemos utilizar el comando attach(),
##cuyo objetivo consiste básicamente en “enganchar” el contenido del dataframe
## al entorno donde R busca los nombres de variable; de esta forma se puede 
##acceder directamente a las variables del dataframe por su nombre, 
##sin necesidad de que éste tenga que ser precedido con el nombre del 
##dataframe y el símbolo $; una vez que hayamos acabado nuestra tarea
##desenganchamos” el dataframe con detach(). La tarea anterior, utilizando
## estos comandos, se puede llevar a cabo mediante:


attach(misDatos)
#table(estudios)
table(sexo)
table(edad)
table(sexo,edad)
mean(edad[sexo=="M"])
mean(edad[sexo=="H"])
detach(misDatos)


##########################################################




################


altura<-rnorm(10,1.70, 0.1)
altura
sexo<-sample(factor(c("M","F")),size=10, replace=TRUE)
sexo
sexo[3]
altura[3]
datospoblacion<-data.frame(sexo, altura)
datospoblacion
datospoblacion[3,]
datospoblacion[3,2]
datospoblacion[,2]
datospoblacion[2]
#cuando el dataframe es grande, conviene usar View()
View(datospoblacion)
#tambien puede acceder a las columnas por nombre utilizandoel signo"$":
datospoblacion$altura
datospoblacion$altura[3]
#cada columna y fila , son vectores hechos y derechos, asi que se puede aplicarles filtros


#xxxxxxxxxxxxxxxxxxxxx
#segunda clase#
#Funcion if

x<-rnorm(1)
x
if(x>0.02) print("Este valor es mayor que el 96 percentil ")
print()

#Funcion for()

n<-50
x<-vector()
z<-vector()
for(i in 1:100){
x[i]<-mean(rexp(n, rate=0.5))
z[i]<-(x[i]-2)/sqrt(2/n)
}
x[i]
z[i]

#Funcion while()

eps<-1; s<-0; n<-0
while(eps>0.001){
      n<-n+1
      x<-runif(1,-1,1)
      y<-runif(1,-1,1)
      if (x^2+y^2<1) s<-s+1
      pihat<-4*s/n
      eps = abs(pihat-pi)
}
pihat
n
pihat# es la estimacion

###########
mysuma<-function(x,y){
      x+y
}
#se acaba de 
mysuma(20,40)
##return

algunafuncion<-mysuma
algunafuncion(4,5)

myresta<-function(x,y){
   x-y
}
myresta(45,15)

myoperacion<-function(x,y,cualquierotrafuncion){
cualquierotrafuncion(x,y)
}
myoperacion(2,3,mysuma)
myoperacion(2,3,myresta)

secuencia<-seq(1:20)
mysuma(secuencia,100)

mygraficador<-function(x,y){
     plot(x,y)
}
mygraficador(25,4)
mygraficador(seq(0:200)-100, ((seq(0:200)-100)^2))



################################### PROGRAMA PARA FILTRAR UNA VARIABLE

install.packages("UsingR")
install.packages("dplyr")
library(dplyr)

#El paquete UsingR tiene instalado una base de dato de la maraton de
#New York en el año 2002
# rescatamos esa base de datos llamando data

data<-UsingR::nym.2002
data
# en esta base de datos hay 5 variables y 1000(corredores por cada variable)
# en el concurso de maraton cuya variables son
# 1. place (el lugar que se qeue do en la maraton)
# 2. gender (sexo del corredor)
# 3. age (edad del corredor)
# 4. home (lugar de procedencia)
# 5. Time (tiempo que hicieron en minutos en terminar la maraton)
#
#
# PARA filtrar una variable que se quiere se hace lo siguiente
#Ejemplo se quiere filtrar la varialbe edad, entonce se hace

edades<-select(data, age)
#Como se tien selccionado la varaible, entonces se puede hacer algunas descriptivas

mean(edades)
# este da como resultado un error, por que son elemetos de una matriz
# por lo tanto se tiene que hacer es DESENLISTAR LOS ELEMENTOS DFE LA MATRIZ

edades<-select(data, age)%>% unlist
edades #vemos solamente edades
mean(edades)
sd(edades)
hist(edades)