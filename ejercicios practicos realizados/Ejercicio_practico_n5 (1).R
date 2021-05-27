library(pwr)
library(ggpubr)


# Integrantes:
# - Ekaterina Cornejo 20.187.903-5
# - Catalina Yáñez 19.516.593-9
# - Aldo Castillo 19.839.621-4

# Práctico 5 problema D ---------------------------------------------------

# -------------------------------------------------------------------------

"
Contexto

Los siguientes datos vienen de un estudio hecho por Rothman & Keller (1972) 
(Journal of chronic diseases, 25(12),711-716) sobre la incidencia de la cantidad 
de alcohol y de tabaco que se consume en el riesgo de padecer cáncer oral.

"

# -------------------------------------------------------------------------


"
1. Suponiendo que la diferencia en la proporción de personas que desarrollan la
   enfermedad entre quienes fuman de 20 a 39 cigarrillos al día y aquellos que fuman 
   40 o más unidades por día es de 0.18. ¿Cuánta gente deberíamos entrevistar para 
   obtener un intervalo de confianza del 95% y poder estadístico de 80%?

"


# Aplicando una diferencia en la proporción de 0.18, con un intervalo de confianza 
# del 95% y poder estadístico del 80%


poder1 <- pwr.p.test(h = 0.18, n = NULL, sig.level = 0.05, power = 0.8, alternative = "two.sided")
print(poder1)

"
Utilizando la función pwr.p.test se puede obtener que el tamaño de la población
requerido para conseguir un 80% de poder estadístico con un intervalo de confianza 
del 95% es de 243 personas aproximadamente (242.2488). 
"

# -------------------------------------------------------------------------


" 
2. Estudios previos habían determinado que la incidencia de cáncer oral en la 
   población general que fuma entre 20 y 39 cigarrillos al día era de 5%. ¿Respaldan 
   estos datos tal estimación?
"

# Hipótesis
# H0: La incidencia de cáncer oral en la población general que fuma
# entre 20 y 39 cigarrillos al día es de 5% p = 0.05
#
# H1: La incidencia de cáncer oral en la población general que fuma
# entre 20 y 39 cigarrillos al día es distinta de 5% p != 0.05

# Tamaño, proporción de éxito y cantidad de éxitos de la muestra.

# Tamaño total de la muestra
n <- 930

# Personas con cáncer que fuman entre 20 y 39 cigarrillos al dia
exitos <- 248


# Valor nulo y nivel de significación.

# Porcentaje
p0 <- 0.05

# Nivel de significación
alfa <- 0.05


# Prueba de hipótesis
prueba <- prop.test(exitos, n = n, p = p0, alternative = "two.sided", conf.level = 1-alfa)
print(prueba)

"
Se obtuvo un porcentaje estimado de la muestra de 26.6667% con un intervalo de 
confianza de 0.2387303 - 0.2965589 utilizando un alpha de 0.05.
Se rechaza la hipótesis nula a favor de HA, por lo que NO se respalda la estimación 
de que la incidencia de cáncer oral en la población general que fuma entre 20 
y 39 cigarrillos al día es de 5%.
"

# -------------------------------------------------------------------------

"
3. Según estos datos, ¿da lo mismo fumar diariamente entre 1 y 2 paquetes de 
   cigarrillos que hacerlo más de dos paquetes?
"

# Construir la matriz de datos

# Hipótesis
# H0: Aquellos que consumen entre uno y dos paquetes, y los que 
# consumen más, tienen las mismas probabilidad de contraer o no cáncer oral
#
# H1: Aquellos que consumen entre uno y dos paquetes, y 
# los que consumen más, no tienen las mismas probabilidad de contraer o no cáncer oral

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
la Hipótesis Nula , y por lo tanto se concluye que no da lo mismo la diferencia 
de consumo de entre uno y dos paquetes, a más paquetes de cigarrillos consumidos.
Se concluye este resultado con un 95% de confianza.
"

