data(iris)
iris
str(iris)
hist(iris$Sepal.Length)
install.packages("ggplot2")
library(ggplot2) #primero activamos la librer�a
ggplot(iris, aes(x = Sepal.Length)) +
geom_histogram()
# diferenciando las especies por color
ggplot(iris, aes(x = Sepal.Length, fill=Species)) +
geom_histogram()

### HACER REFERENCIA DE GRANTICA DE LOS GRAFICOS Y
### LA CONFIGURACION (aes)

#Ejemplo () sin a�adir ningun grafico 
diamonds
str (diamonds)
ggplot(diamonds)#(solo introducimos referencia de los datos)
ggplot(diamonds, aes(x=carat))  #solo inicamos el eje X
ggplot(diamonds, aes(x=carat, y=price))#Especificamos ambos ejes X e Y, 
                                       #que ahora ser�n fijos en todas las capas.
ggplot(diamonds, aes(x=carat, color=cut))#Cada categor�a de la variable
                                         # �cut� tendr� un color diferente,
                                         #una vez le indiquemos la geometr�a.
ggplot(diamonds, aes(x=carat), color="steelblue")#Si desea tener el color, 
                                                 #el tama�o, etc, fijo (es decir, 
                                                 #no var�a bas�ndose en una variable del marco de
                                                 #datos), es necesario especificarlo fuera de los aes(), 
                                                 #como en este caso.
###las Capas(geom)(ver pag 8)
ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
geom_point() +
geom_smooth()


ggplot(diamonds) +
geom_point(aes(x=carat, y=price, color=cut)) +
geom_smooth(aes(x=carat, y=price, color=cut))

## con geom_smooth()

ggplot(diamonds) +
geom_point(aes(x=carat, y=price, color=cut)) +
geom_smooth(aes(x=carat, y=price))

## o mas sencilo
ggplot(diamonds, aes(x=carat, y=price)) +
geom_point(aes(color=cut)) +
geom_smooth()

##Ahora vamos a hacer que la forma de los puntos cambie con la caracter�stica �color� del conjunto de
##datos.

ggplot(diamonds, aes(x=carat, y=price, color=cut, shape=color)) +
geom_point()

#### Las etiquetas (labs):Las etiquetas ayudan a visualizar lo que queremos representar en un gr�fico. Las m�s comunes son el t�tulo
##principal del gr�fico, los t�tulos de los ejes y las leyendas

gg <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
geom_point() +
labs(title="Diagrama de puntos", x="Tama�o del diamante", y="Precio")
gg


##El Tema (theme)
###El ajuste del tama�o de las etiquetas se puede hacer usando la funci�n theme() configurando
###plot.title, axis.text.x y axis.text.y, que deben ser especificados dentro del element_text().
###el t�tulo de la leyenda debe establecer el nombre utilizando scale_color_discrete(),

gg1 <- gg +
theme(plot.title=element_text(size=30, face="bold"),
axis.text.x=element_text(size=15),
axis.text.y=element_text(size=15),
axis.title.x=element_text(size=20),
axis.title.y=element_text(size=20)) +
scale_color_discrete(name="Corte del diamante")

### El aspecto (facet)
#facet_wrap(f�rmula) toma una f�rmula como argumento. Los elementos que coloquemos a la derecha
#corresponden a las columnas, y los que coloquemos a la izquierda definen las filas.
#Por ejemplo, si definimos las columnas por �cut�.

gg1 +
facet_wrap( ~ cut, ncol=3)




#Si definimos las filas por �color� y columnas �cut�.

gg1 +
facet_wrap(color ~ cut)
#En facet_wrap, las escalas de los ejes X e Y se fijan para acomodar todos los puntos de forma
#predeterminada. es posible hacer que las escalas sean libres haciendo que las gr�ficas parezcan distribuidas
#m�s uniformemente estableciendo el argumento scales = free.

gg1 +
facet_wrap(color ~ cut, scales="free")

###La leyenda: eliminaci�n y cambio de posici�n.

#Si introducimos theme(legend.position = "none"), se puede quitar la leyenda. Mediante theme(legend.position
#= "top") se puede mover la leyenda alrededor de la trama.
#legend.justification denota el punto de anclaje de la leyenda, es decir, el punto que se colocar� en
#las coordenadas dadas por legend.position.

#Ejemplo sin leyenda:

p1 <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
geom_point() +
geom_smooth() +
theme(legend.position="none") +
labs(title="legend.position='none'")
p1

#Leyenda arriba:
p2 <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
geom_point() +
geom_smooth() +
theme(legend.position="top") +
labs(title="legend.position='top'")
p2

#Leyenda dentro del gr�fico:
ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
geom_point() +
geom_smooth() +
labs(title="legend.position='coords inside plot'") +
theme(legend.justification=c(1,0), legend.position=c(1,0))

## pagina 22 no seguimos

####histograma28

ggplot(diamonds,aes(x=price)) +
geom_histogram(fill="white",color="black")

#Cambiamos la anchura de los intervalos dentro de la geometr�a con el argumento binwidth, al aumentar
#el n�mero cada vez tenemos menos intervalos:

ggplot(diamonds,aes(x=price)) +
geom_histogram(fill="white",color="black",binwidth=1000)

#Los histogramas representan la frecuencia absoluta, si queremos que aparezca la frecuencia relativa,
#debemos especificarlo mediante y = (..count..)/sum(..count..):

ggplot(diamonds,aes(x=price)) +
geom_histogram(aes(y = (..count..)/sum(..count..)),
fill="white",
color="black",
binwidth=1000)

##Diagramas de cajas30

p <- ggplot(diamonds,aes(cut,price))
p +
geom_boxplot(aes(fill=cut))

#Podemos superponer los puntos correspondientes a las obervaciones a�adiendo geom_point()

p +
geom_boxplot(aes(fill=cut))+
geom_point()

#En muchos casos es interesante considerar distintos factores. En los datos del precio de los diamantes
#podemos considerar la calidad del corte cut y el color del diamante color. Para ello, basta con a�adir una
#instrucci�n a la est�tica del boxplot.

p +
geom_boxplot(aes(fill=factor(color)))

#Tambi�n podemos resaltar los outliers

p +
geom_boxplot(aes(fill=factor(color)),
outlier.colour = "red",
outlier.size = 1.5)

#Gr�ficos de barras

#Por ejemplo, la base de datos birthwt contiene informaci�n acerca del peso de recien nacidos en funci�n
#de distintas caracter�sticas de la madre.

library(MASS)
data(birthwt)
head(birthwt)

str(birthwt)

#Podemos realizar una tabla de frecuencias que indique cuantos ni�os nacen con poco peso en relaci�n a
#la raza de la madre (contar el n�mero de medidas que hay en cada combinaci�n).

with(birthwt,
table(race,low))

#Podemos obtener la gr�fica mediante el siguiente procedimiento:
ggplot(birthwt,aes(x=factor(low),fill=factor(race)))+
geom_bar(position=position_dodge())

#Ahora realizamos el mismo gr�fico sin incluir position_dodge():

ggplot(birthwt,aes(x=factor(low),fill=factor(race)))+
geom_bar()

#Podemos cambiar el color de las barras haciendo:

ggplot(birthwt,aes(x=factor(low),fill=factor(race)))+
geom_bar(position=position_dodge(),color="black") +
scale_fill_manual(values=c("#999999", "#E69F00", "#E70F00"))


#En general, es m�s interesante representar los porcentajes. En las tablas podemos hacer, por ejemplo, el
#procentaje de bajo peso por raza:

t <- with(birthwt,
table(race,low))
tp <- round(prop.table(t,1),1)
tp

# Para poder utilizar los resultados en ggplot2, debemos transformar la tabla a un data.frame:
tp <- as.data.frame(round(prop.table(t,1),1))
tp

#Ahora podemos representar estos porcentajes

ggplot(tp,aes(x=factor(race),fill=factor(low), Freq)) +
   geom_bar(stat="identity",
            position=position_dodge(),
            color="black")

#Gr�ficas de medias e intervalos de confianza

#Necesitamos instalar y activar plyr para cargar los siguientes datos que vamos a utilizar (ToothGrowth)
#yRmsic�para utilizar la funci�n resumen.

#El conjunto de datos ToothGrowth corresponde a 60 observaciones del largo de los dientes (len),
#realizadas sobre 10 conejillos de Indias a los que se le suplementa uno de los tres niveles de dosis de vitamina
#C (0.5, 1, y 2 mg; dose) con dos tipos de m�todos de administraci�n (jugo de naranja o �cido asc�rbico;
#supp).

install.packages("plyr")
library(plyr)
install.packages("Rmisc")
library(Rmisc)

## Loading required package: lattice
##Realizamos un resumen de los datos:
df <- ToothGrowth
dfc <- summarySE(df, measurevar="len", groupvars=c("supp","dose"))
dfc

#Se ha creado un data.frame con la media de longitud por dosis y suplemento. Tambi�n se ha calculado la
#correspondiente desviaci�n estandard y el valor de la amplitud del intervalo de confianza. Una vez calculados,
#podemos representarlos

ggplot(dfc, aes(x=dose, y=len, colour=supp)) +
geom_errorbar(aes(ymin=len-ci, ymax=len+ci), width=.1,size=1) +
geom_line(size=1) +
geom_point(size=3)

## Hemos creado un gr�fico en el que se representa la media con un punto y su intervalo de confianza.
##Tambi�n se ha a�adido un geom con l�neas.

#En algunos casos, se prefiere representar las medias con barras. Podemos hacer lo siguiente:
#primero, definimos la dosis como un factor.

dfc2 <- dfc
dfc2$dose <- factor(dfc2$dose)

#Ahora podemos obtener la gr�fica haciendo:

ggplot(dfc2, aes(x=dose, y=len, fill=supp)) +
geom_bar(stat="identity",position=position_dodge()) +
geom_errorbar(aes(ymin=len-ci, ymax=len+ci),
width=.2, # Ancho de las barras de error
position=position_dodge(.9))

####46

