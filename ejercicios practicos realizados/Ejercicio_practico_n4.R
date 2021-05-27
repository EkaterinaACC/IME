library(TeachingDemos)
library(ggpubr)
library(datasets)

# Integrantes:
# - Ekaterina Cornejo 20.187.903-5
# - Catalina Yáñez 19.516.593-9
# - Aldo Castillo 19.839.621-4




# Practico 4 problema B ---------------------------------------------------


# -------------------------------------------------------------------------

"
Contexto

Se sabe que el proceso de fabricación de barras de acero para concreto reforzado producen barras con
medidas de dureza que siguen una distribución normal con desviación estándar de 10 kilogramos de
fuerza por milímetro cuadrado. Usando una muestra aleatoria de tamanio 25, un ingeniero quiere averiguar
si una linea de produccion esta generando barras con dureza media de 170 [kgf mm^-2].
"
# Datos
# sd = 10 [kgf mm^-2]
# n = 25

# Hipotesis
# H0: Dureza media de las barras de acero U = 170 [kgf mm^-2]
# H1: Dureza media de las barras de acero U != 170 [kgf mm^-2]

# Delta = 2
# Delta es la diferencia entre la media (170) con los parametros que el ingeniero tomara para rechazar hipotesis nula

# -------------------------------------------------------------------------

"
1. Si el ingeniero está seguro que la verdadera dureza media no puede ser menor a los 170 [kgf mm-2] y piensa
   rechazar la hipótesis nula si la muestra presenta una media mayor a 172 [kgf mm-2], pero la verdadera dureza
   media de la línea de producción fuera 173 [kgf mm-2], ¿cuál sería la probabilidad de que el ingeniero, que
   obviamente no conoce este dato, tome la decisión correcta de rechazar la hipótesis nula en favor de la 
   hipótesis alternativa?
"

poder1 <- power.t.test(n = 25, sd = 10, delta = 2, sig.level = 0.05, power = NULL, alternative = "one.sided", strict = TRUE, type = "one.sample")
print(poder1)


"
Existe aproximadamente un 75% de probabilidades de que el ingeniero no cometa error tipo 1 (No rechazar H0 en favor de H1)
"

# -------------------------------------------------------------------------

"
2. ¿A cuánto cambiaría esta probabilidad si se pudiera tomar una muestra de 64 barras?
"

poder2 <- power.t.test(n = 64, sd = 10, delta = 2, sig.level = 0.05, power = NULL, alternative = "one.sided", strict = TRUE, type = "one.sample")
print(poder2)

"
La probabilidad cambia a un 52% de no cometer error tipo 1 ( 1 - 0.48)
"

# -------------------------------------------------------------------------

"
3. ¿Cuántas barras deberían revisarse para conseguir un poder estadístico de 0,90 y un nivel de significación de
0,05?
" 

poder3 <- power.t.test(n = NULL, sd = 10, delta = 2, sig.level = 0.05, power = 0.9, alternative = "one.sided", strict = TRUE, type = "one.sample")
print(poder3)

"
Para conseguir un poder estadistico de 0.9 y un nivel de significación 0.05 se necesitan 215.4562 muestras. Pero como son barras de acero
y no existen fracciones de estas, n debería ser 216.
"

# -------------------------------------------------------------------------

"
4. ¿Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error de tipo 1 a un 1% solamente?
"

poder4 <- power.t.test(n = 25, sd = 10, delta = 2, sig.level = 0.01, power = NULL, alternative = "one.sided", strict = TRUE, type = "one.sample")
print(poder4)

"
Al bajar la probabilidad de cometer error tipo 1 al 1% existe un 92% de probabilidades de cometer error tipo 2.
"







