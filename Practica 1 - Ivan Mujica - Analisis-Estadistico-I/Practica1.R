# Practica 1
# Leemos el archivo
install.packages("readxl")
library("readxl")
ejercicio29 <- read_excel("/home/ivan/Data-science-degree/data-science-degree/DATOS DEL LIBRO-ANDERSON/Ch 02 Descriptive/Crosstab.xls",'Data')
x <- ejercicio29$x # recuperamos la columna x
y <- ejercicio29$y # recuperamos la columna y
dataframe <- data.frame(x, y)
dataframe # dataframe principal
# contando las frecuencias
(sinTotales<-table(misDatos$x , misDatos$y)) # sin totales
(conTotales<-addmargins(sinTotales, margin=1)) # con totales y
(conTotales<-addmargins(conTotales, margin=2)) # con totales x

# margin.table(a, 1) # B frequencies (summed over A)
#margin.table(a, 2) # B frequencies (summed over A)
prop.table(conTotales) # cell percentages

b <-prop.table(sinTotales,1) # porcentaje de los renglones
(addmargins(b * 100, margin=2))

c <-prop.table(sinTotales,2) # porcentaje de las columnas
(addmargins(c * 100, margin=1))
