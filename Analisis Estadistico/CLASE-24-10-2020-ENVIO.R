install.packages(pkgs="distrEx")
library(distrEx)
#Combianciones Ejercicio 1
#en una clase de 35 estudiantes se quiere elegir un comite de 3 estudiantes
#cuantos comites diferentes se pueden formar?
choose(35,3)
#Combianciones Ejercicio 2
#cuantas combinaciones de flores tomadas de 3 en 3
choose(7,3)
#Combianciones Ejercicio 3
# au una reunion asisten 3 personas y se salundan entre todos, cuantos saludos se han intercambiado
choose(10,2)

#probabilidad condicional Ejercicio 1
#al 25% de tus amigos les gusta fresa y chocolate
#mientras que al 60 % le gusta chocolate
#cual es la probabilidad de que a un amigo le guste chocolate?

pB <- 60
pA_B <-  25
pAB <- pA_B/pB
pAB
#probabilidad condicional Ejercicio 2
# el 76 porciento de los estudiante de ingenieria ha aprbado resistencia de materiales
# y el 45 % aprobaron estatica, ademas el 30% aprobaron resistencia de materiales
#y estatica
# si camilo aprobo resitencia de materiales que probabilidad tiene de 
# haber tomado estatica?
pA <- 76
pB <- 45
pA_B <- 30
pB_A <- pA_B
pBA <- pA_B/pA
pBA
#probabilidad condicional Ejercicio 3
#
alumno_frances = 5
alumna_frances = 5
alumnos = 5
alumnas = 5
total = alumno_frances+
  alumna_frances+alumnos+alumnas
  #respuesta 1
personaAzarProb <- (alumnos+alumna_frances+alumno_frances)/
  total
personaAzarProb
  #respuesta 2
personaAzarProb <- (alumna_frances)/
  total
personaAzarProb

#teorema de bayes Ejercicio 1
# en una sala de pediatria de un hosp, el 60% son ninias, de los 35% son menores de 24
# meses  el 20% de las ninias tienen  un pediatra ingresa a la sala y selecciona uno al azar
# determine la probabilidad de que el menor sea menor de 9 meses
pH <- 0.6
pM_H <- 0.2
pV <- 0.4
pM_V <- 0.35
pM <- pH*pM_H+pV*pM_V
pM

#teorema de bayes Ejercicio 2
#un medico cirujano se especializa en cirujias esteticas
#entre sus pacientes, el 20% se realizan correcciones faciales, un 35% implantes mamas
# y el restante otras cirujias correctivas, se sabe ademas
# 15% de implantes mamarios
pF <- 0.2
pH_F <- 0.25
pM <- 0.35
pH_M <- 0.15
pO <- 0.45
pO_H <- 0.40
pH <- pF*pH_F+pM*pH_M+pO*pO_H
pH

#teorema de bayes Ejercicio 3
pP <- 0.25
pE_P <- 0.01
pS <- 0.35
pE_S <- 0.02
pT <- 0.4
pE_T <- 0.03
pp_E <- pP*pE_P/(pP*pE_P+pS*pE_S+pT*pE_T)
pp_E


#Ejercicio 1 hipergeom?trica 
dhyper(2,4,10-4,5)

#Ejercicio 2 binomial 
dbinom(2,6,0.1)

#Ejercicio 3 binomial 
x=5
lambda=2
dpois(x,lambda)

#Variable aleatoria continua Ejercicio 1
f <- function(x) (3/4)*(-x^2+4*x-3)
integrate(f,lower =1.7,upper = 2.4)

#Variable aleatoria continua Ejercicio 2
f <- function(x) 100/x^2
P_X <- 1-integrate(f,lower = 100,upper = 200)$value
P_X

#Distribuci?n Normal Ejercicio 1
punif(22, min=20, max=25, lower.tail=T)

#Distribuci?n Normal Ejercicio 2
prob=0.898
media=650
desv=100
qnorm(prob,media,desv)

#Distribuci?n Exponencial Ejercicio 1
x=400
lambda=1/360
1-pexp(x,lambda)

#Distribuci?n Exponencial Ejercicio 2
x=4
rate=1/3
dexp(x,rate)


#Distribuci?n Normal Ejercicio 1
x=500
media=650
desv=100
pnorm(x,media,desv)

#Distribuci?n Normal Ejercicio 2
prob=0.898
media=650
desv=100
qnorm(prob,media,desv)

#Distribuci?n Normal Ejercicio 3
x=5
n=7
prob=0.5
dnorm(x,n,prob)

#Distribuci?n Normal Ejercicio 4
media=6
desv_est=1.1
pnorm(6.6, mean = media, sd = desv_est, lower.tail = TRUE)
