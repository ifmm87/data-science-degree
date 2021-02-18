# Practica 2

#Ejercicio 27
n = 13429
nBigTen = 2961
nPac10 = 4494
nNoBigTenNoPac10 = 6823
# A
probabilidadNinguno = nNoBigTenNoPac10 /  n 
probabilidadNinguno
# B
probabilidadCualquiera = 1 - probabilidadNinguno
probabilidadCualquiera
# C
probabilidadBigTen = 2961 / 13429
probabilidadPac10 = 4494 / 13429
probabilidadAmbos = probabilidadBigTen + probabilidadPac10 - probabilidadCualquiera
probabilidadAmbos

# Ejercicio 28

probabilidadTrabajo = 0.458
probabilidadPersonal = 0.54
probabilidadAmbos = 0.30

#A
probabilidadCualquiera  = probabilidadTrabajo + probabilidadPersonal - probabilidadAmbos
probabilidadCualquiera
# B
probabilidadNinguno = 1 - probabilidadCualquiera
probabilidadNinguno

# Ejercicio Probabilidad Condicional
probablidadB = 0.40
probabilidadAUnionB = 0
probabilidadADadoB = probabilidadAUnionB / probablidadB 
probabilidadADadoB

# Ejercicio probailidad posterior con el teorema de bayess
probIncumple = 0.05 
probFalleDadoNoIncumple = 0.20
probFalleDadoIncumple = 1
probFalleDadoIncumple = (probIncumple * probFalleDadoIncumple) / ( probIncumple * probFalleDadoIncumple + (1-probIncumple) * probFalleDadoNoIncumple )
probFalleDadoIncumple

# Ejercicio distribuciones
# Binomial
# A
p=0.23
n=6
dbinom(2,n,p)
# B
p = 0.23
n=10
dbinom(0, n, p)
# Poison
# B
media = 2.5
k = 3
dpois(k, media)
# C
media = 2.5
k = 0
dpois(k, media)
# Hipergeometrica
# A
n = 3 
r = 7
N = 10
#B
f2 = dhyper(2,r,N -r,n)
f3 = dhyper(3,r,N -r,n)
f2 + f3
#Uniforme
# A
a = 11.975
b = 12.100
min = 12
max=12.05
fx = 8

prob1205 = punif(12.05,min=a, max=b, lower.tail=TRUE)
prob12 = punif(12,min=a, max=b, lower.tail=TRUE)
prob1205 - prob12
# B
prob121 = punif(12.100,min=a, max=b, lower.tail=TRUE)
prob1202 = punif(12.02,min=a, max=b, lower.tail=TRUE)
prob121 - prob1202

# Exponencial
x=3
rate = 1/2
pexp(3, rate=rate, lower.tail=F)

# Normal
#A
media = 30
desviacion = 8.20
1- pnorm(40, media, desviacion)


