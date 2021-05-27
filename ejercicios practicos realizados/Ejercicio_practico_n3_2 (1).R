library(TeachingDemos)
library(ggpubr)
library(datasets)

# Integrantes:
# - Ekaterina Cornejo 20.187.903-5
# - Catalina Yáñez 19.516.593-9
# - Aldo Castillo 19.839.621-4


# Problema A parte I--------------------------------------------------------

# El artículo Automatic Segmentation of Medical Images Using Image Registration: Diagnostic and
# Simulation Applications (Journal of Medical Engeeniering and Technology 2005) propuso una nueva
# técnica para la identificación automática de los bordes de estructuras significativas en una imagen médica
# utilizando desplazamiento lineal promedio (ALD, por sus siglas en ingles). El artículo dio las siguientes
# observaciones de ALD con una muestra de 49 razones (en pixeles y usando punto en vez de coma decimal).

texto <- "1.38 0.98 1.09 0.77 0.66 1.28 0.61 1.49 0.81 0.36 0.84 0.83 0.61 0.64 1.30 0.57 0.43 0.62 1.00 1.05 0.82 1.10 0.65 0.99 0.56 0.66 0.64 1.45 0.82 1.06 0.41 0.58 0.66 1.14 0.73 0.59 0.51 1.04 0.85 0.45 0.82 1.01 1.11 0.34 1.25 0.38 1.44 1.28 0.91"
file <- textConnection(texto)
sample <- scan(file)

# Los autores comentaron que el ALD medio estaría alrededor de 1.0. ¿Los datos soportan esta afirmación?


# Hipótesis

# H0: Media es igual a 1: U = 1
# H1: Media es no es igual a 1: U != 1


# Condiciones

# Se sabe que son 49 muestras por lo que se usara z.test (n > 30)
# Además se asume que la muestra es independiente y menor al 10% de la "población"

hist(sample)

desviacion <- sd(sample)
media <- mean(sample)
alfa <- 0.05

z <- z.test(x = sample, sd = desviacion, alternative = "two.sided", mu = 1, conf.level = (1 - alfa))
print(z)


# Conclusion
"
 Ya que el valor p es menor a alfa, existen suficientes datos estadísticos para rechazar H0 en favor de H1. 
 El ALD medio es distinto de 1.
"

# -------------------------------------------------------------------------


# Problema B parte I--------------------------------------------------------------


# El artículo "Engineering Properties of Soil" (Soil Science 1998) puso a prueba la idea generalizada 
# de que el 3% del suelo corresponde a materia orgánica. Para esto, los autores obtuvieron una 
# muestra aleatoria de especímenes de suelo, determinando que el porcentaje de materia orgánica 
# presente en cada espécimen era (usando punto en vez de coma decimal):
  
texto2 <- "3.10 5.09 2.97 1.59 4.60 3.32 0.55 1.45 0.14 4.47 0.80 3.50 5.02 4.67 5.22 2.69 3.98 3.17 3.03 2.21 2.69 4.47 3.31 1.17 2.76 1.17 1.57 2.62 1.66 2.05"
file2 <- textConnection(texto2)
sample2 <- scan(file2)

# ¿Qué conclusión sugeriría a los autores?

# Hipótesis

# H0: La conclusión que se sugeriría es que el porcentaje de materia orgánica en el suelo es de 3% (U = 3)
# H1: La conclusion que se sugeriría es que el porcentaje de materia orgánica en el suelo NO es de 3% (U != 3)

# Condiciones

# Se sabe que son 30 muestras por lo que se usara t.test (n !> 30)
# Ademas se asume que la muestra es independiente y menor al 10% de la "población"

hist(sample2)

alfa2 <- 0.05

t1 <- t.test(x = sample2, alternative = "two.sided", mu = 3, conf.level = (1 - alfa2))
print(t1)

# Conclusión
"
 Ya que el valor p es mayor a alfa, no existen suficientes datos estadisticos para rechazar H0 en favor de H1. 
 La conclusion que se sugiere es que el porcentaje de materia organica en el suelo es del 3%. Cabe decir que 
 el p valor se encuentra muy cercano al borde, por lo que no es estrictamente concluyente.
"

# -------------------------------------------------------------------------

# Problema A parte II --------------------------------------------------------------

# Se sabe que la lactancia estimula una perdida de masa ósea para proporcionar cantidades de calcio
# adecuadas para la producción de leche. Un estudio intenta determinar si madres adolescentes podrían
# recuperar niveles mas normales a pesar de no consumir suplementos (Amer. J. Clinical Nutr., 2004; 1322-1326).
# El estudio obtuvo las siguientes medidas del contenido total de minerales en los huesos del cuerpo
# (en gramos) para una muestra de madres adolescentes tanto durante la lactancia (6-24 semanas
# postparto) y posterior a ella (12-30 semana postparto):

texto3 <- "1928 2549 2825 1924 1628 2175 2114 2621 1843 2541"
file3 <- textConnection(texto3)
lactancia <- scan(file3)

texto4 <- "2126 2885 2895 1942 1750 2184 2164 2626 2006 2627"
file4 <- textConnection(texto4)
posdestete <- scan(file4)

# ¿Sugieren los datos que el contenido total de minerales en los huesos del cuerpo durante el posdestete
# excede el de la etapa de lactancia por mas de 25 g? 

# Hipótesis
# Up = media posdestete
# Ul = media lactancia

# H0: Up - Ul = 25
# H1: Up - Ul > 25

alfa <- 0.01

grafico.lactancia <- gghistogram(lactancia, bins = 10, title = "Histograma de lactancia")
grafico.lactancia

grafico.posdestete <- gghistogram(posdestete, bins = 10, title = "Histograma de posdestete")
grafico.posdestete


t2 <- t.test(x = posdestete, y = lactancia, alternative = "greater", mu = 25, conf.level = (1 - alfa), paired = TRUE)
print(t2)


# Conclusión
"
 Ya que el valor p es mayor a alfa, no existen suficientes datos estadísticos para rechazar H0. 
 El contenido total de minerales en los huesos del cuerpo durante el posdestete NO excede el de 
 la etapa de lactancia por mas de 25g. Cabe decir que el p valor se encuentra muy cercano al borde,
 por lo que no es estrictamente concluyente.
"


# Problema B parte II--------------------------------------------------------------


# La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al rapido crecimiento de
# los pollitos es beneficioso, tanto para las avícolas como para los consumidores. En el paquete datasets
# de R estan los datos (chickwts) de un experimento hecho para medir la efectividad de varios
# suplementos alimenticios en la tasa de crecimiento de las aves. Pollitos recién nacidos se separaron
# aleatoriamente en 6 grupos, y a cada grupo se le dio un suplemento distinto. Para productores de la 7ma
# region, es especialmente importante saber si existe diferencia en la efectividad entre el suplemento
# basado en linaza (linseed) y el basado en habas (horsebean).


# Hipotesis
# Ul = media linaza
# Uh = media habas

# H0: La diferencia en la efectividad de los suplementos alimenticios es 0 (Ul - Uh = 0) 
# H1: La diferencia en la efectividad de los suplementos alimenticios es distinto de 0 (Ul - Uh != 0)



data <- chickwts

data.linseed <- data[data$feed == "linseed", 1]
data.horsebean <- data[data$feed == "horsebean", 1]

grafico.linseed <- gghistogram(data.linseed, bins = 10, title = "Histograma de sumplemento 'linseed'")
grafico.linseed

grafico.horsebean <- gghistogram(data.horsebean, bins = 10, title = "Histograma de suplemento 'horsebean'")
grafico.horsebean



t <- t.test(x = data.linseed, y = data.horsebean, alternative = "two.sided", mu = 0, conf.level = (1 - alfa))
print(t)

# Conclusión
"
 Ya que el valor p es menor a alfa, existen suficientes datos estadísticos para rechazar H0 en favor de H1. 
 Si existe diferencia sustancial entre la efectividad de los alimentos en base a linaza y en base a habas.
"


