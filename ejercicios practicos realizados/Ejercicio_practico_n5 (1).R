library(pwr)
library(ggpubr)


# Integrantes:
# - Ekaterina Cornejo 20.187.903-5
# - Catalina Y��ez 19.516.593-9
# - Aldo Castillo 19.839.621-4

# Pr�ctico 5 problema D ---------------------------------------------------

# -------------------------------------------------------------------------

"
Contexto

Los siguientes datos vienen de un estudio hecho por Rothman & Keller (1972) 
(Journal of chronic diseases, 25(12),711-716) sobre la incidencia de la cantidad 
de alcohol y de tabaco que se consume en el riesgo de padecer c�ncer oral.

"

# -------------------------------------------------------------------------


"
1. Suponiendo que la diferencia en la proporci�n de personas que desarrollan la
   enfermedad entre quienes fuman de 20 a 39 cigarrillos al d�a y aquellos que fuman 
   40 o m�s unidades por d�a es de 0.18. �Cu�nta gente deber�amos entrevistar para 
   obtener un intervalo de confianza del 95% y poder estad�stico de 80%?

"


# Aplicando una diferencia en la proporci�n de 0.18, con un intervalo de confianza 
# del 95% y poder estad�stico del 80%


poder1 <- pwr.p.test(h = 0.18, n = NULL, sig.level = 0.05, power = 0.8, alternative = "two.sided")
print(poder1)

"
Utilizando la funci�n pwr.p.test se puede obtener que el tama�o de la poblaci�n
requerido para conseguir un 80% de poder estad�stico con un intervalo de confianza 
del 95% es de 243 personas aproximadamente (242.2488). 
"

# -------------------------------------------------------------------------


" 
2. Estudios previos hab�an determinado que la incidencia de c�ncer oral en la 
   poblaci�n general que fuma entre 20 y 39 cigarrillos al d�a era de 5%. �Respaldan 
   estos datos tal estimaci�n?
"

# Hip�tesis
# H0: La incidencia de c�ncer oral en la poblaci�n general que fuma
# entre 20 y 39 cigarrillos al d�a es de 5% p = 0.05
#
# H1: La incidencia de c�ncer oral en la poblaci�n general que fuma
# entre 20 y 39 cigarrillos al d�a es distinta de 5% p != 0.05

# Tama�o, proporci�n de �xito y cantidad de �xitos de la muestra.

# Tama�o total de la muestra
n <- 930

# Personas con c�ncer que fuman entre 20 y 39 cigarrillos al dia
exitos <- 248


# Valor nulo y nivel de significaci�n.

# Porcentaje
p0 <- 0.05

# Nivel de significaci�n
alfa <- 0.05


# Prueba de hip�tesis
prueba <- prop.test(exitos, n = n, p = p0, alternative = "two.sided", conf.level = 1-alfa)
print(prueba)

"
Se obtuvo un porcentaje estimado de la muestra de 26.6667% con un intervalo de 
confianza de 0.2387303 - 0.2965589 utilizando un alpha de 0.05.
Se rechaza la hip�tesis nula a favor de HA, por lo que NO se respalda la estimaci�n 
de que la incidencia de c�ncer oral en la poblaci�n general que fuma entre 20 
y 39 cigarrillos al d�a es de 5%.
"

# -------------------------------------------------------------------------

"
3. Seg�n estos datos, �da lo mismo fumar diariamente entre 1 y 2 paquetes de 
   cigarrillos que hacerlo m�s de dos paquetes?
"

# Construir la matriz de datos

# Hip�tesis
# H0: Aquellos que consumen entre uno y dos paquetes, y los que 
# consumen m�s, tienen las mismas probabilidad de contraer o no c�ncer oral
#
# H1: Aquellos que consumen entre uno y dos paquetes, y 
# los que consumen m�s, no tienen las mismas probabilidad de contraer o no c�ncer oral

cero<- c(26, 85)
unodiezynueve<- c(66, 97)
veintetreintaynueve<- c(248, 197)
paquetes <- c(314,294)
cuarentamas<- c(143, 68)

tabla <- as.table(rbind (paquetes , cuarentamas ))
dimnames(tabla) <- list(cigarros=c('paquetes','cuarentamas'),datos=c('Casos','Controles'))

#Hacer la prueba chi - cuadrado de homogeneidad
prueba <- chisq.test(tabla, correct=FALSE)
print(prueba)


"
Con un alpha igual a 0.05, y un p-value = 0.00004816 (p-value < alpha) se rechaza 
la Hip�tesis Nula , y por lo tanto se concluye que no da lo mismo la diferencia 
de consumo de entre uno y dos paquetes, a m�s paquetes de cigarrillos consumidos.
Se concluye este resultado con un 95% de confianza.
"

