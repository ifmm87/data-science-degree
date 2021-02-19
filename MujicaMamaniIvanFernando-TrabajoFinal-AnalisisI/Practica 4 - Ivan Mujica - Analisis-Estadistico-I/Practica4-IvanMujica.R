### pag 554 ejer 6
# a)Trace un diagrama de dispersión con estos datos tomando como variable independiente los
#  salarios.
# b) ¿Qué indica el diagrama de dispersión del inciso a) acerca de la relación entre salario y gra-
#    tificación?
# c). Use el método de mínimos cuadrados para obtener la ecuación de regresión estimada.
# d). Dé una interpretación de la ecuación de regresión estimada.
# e). ¿Cuál será la gratificación de un vicepresidente que tenga un salario anual de $120 000?


# datos ;

data <- data.frame(salario = c(135,115,146,167,165,176,98,136,163,119),
                    gratifi = c(12,14,16,19,22,24,7,17,18,11))
# a)
plot( data$salario, data$gratifi, xlab = "X", ylab = "y")

# b) Gratificacion o bonos anuales vs salario de vicepresidentes.
#    se observa una aparente relación directa entre salarios y los bonos 
#    anuales.



# c) modelo Y = A + B X
mod <- lm(gratifi ~ salario, data=data)
mod
# d)  La relación entre el salario recibido por los vicepresidentes y los 
#     bonos anuales es directa, a medida que aumenta el salario aumenta los bonos 
#     anuales.
#     
#     Se osbserva que por cada unidad del salario correspondido al vicepresidente
#     los bonos anuales se incrementan en 0.18 $us.


# e) 
120*0.1843-10.1641
11.9 
# la respuesta es 11.9 mil $us.


### ---------------- pag 556 ejer 9 ------------
# Un gerente de ventas recolectó los datos siguientes sobre ventas anuales y años de experiencia.
# Años de Ventas anuales


#a. Elabore un diagrama de dispersión con estos datos,
#   en el que la variable independiente sean
#   los años de experiencia.
#b. Dé la ecuación de regresión estimada que puede emplearse 
#   para predecir las ventas anuales cuando se conocen los 
#   años de experiencia.
#c. Use la ecuación de regresión estimada para pronosticar 
#   las ventas anuales de un vendedor  de 9 años de experiencia.


# a)
data <-  data.frame(experi = c(1,3,4,4,6,8,10,10,11,13),
                    ventas = c(80,97,92,102,103,111,119,123,117,136))

plot( data$experi, data$ventas, xlab = "X(experiencia)", ylab = "y(ventas)")

# b) 
(mod<-lm(ventas ~ experi, data=data))
# y = 80 + 4 x
y = 80 + 4*9
y
# aprox 116 miles de $us



## --------------------  pag 565  ejer 20--------
#Consumer Reports publica pruebas y evaluaciones sobre televisores de alta definición. 
#Para cada modelo se elaboró una evaluación general basada principalmente en la 
# calidad de la imagen. Una evaluación más alta indica un mejor funcionamiento. 
# En los datos siguientes se dan evaluación general y precio de 
# televisores de plasma de 45 pulgadas (Consumer Reports, marzo 2006).



# a. Use estos datos para obtener una ecuación de regresión 
#    estimada que pueda emplearse para estimar la puntuación
#    en la evaluación general de una televisión de 42 pulgadas dado
#    el precio.
#b. Calcule r 2. 
#    ¿Proporcionó un buen ajuste la ecuación de regresión estimada?
#c. Estime la puntuación en la evaluación general de un televisor
#    cuyo precio es $3200.


# a) 
data <-  data.frame(marca  =c("Dell","Hisense","Hitachi","JVC",
                              "LG","Maxent","Panasonic","Phillips",
                              "Proview","Samsung"),
                    precio =c(2800,2800,2700,3500,3300,2000,
                              4000,3000,2500,3000),
                    puntc  =c(62,53,44,50,54,39,66,55,34,39))

plot( data$precio, data$puntc, xlab = "X(precio)", ylab = "y(puntuación)")

(mod <-  lm(puntc~precio, data=data))

summary(mod)

# b) se tiene un coeficiente de determinacion del 45% lo que significa que 
# el modelo puede explica solo el 45% de la variación de la variable precio.

# c) 

y <- 12.01749+0.01270*3200
y

# La puntuacion estimada es 52.7.


