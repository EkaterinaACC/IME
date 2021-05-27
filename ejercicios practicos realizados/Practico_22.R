library(ggplot2)
library(ggpubr)
library(tidyr)

# Integrantes:
# - Ekaterina Cornejo 20.187.903-5
# - Catalina Yáñez 19.516.593-9
# - Aldo Castillo 19.839.621-4

# Se setea la dirección de trabajo en donde se encuentra nuestro archivo .R
dirstudio <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirstudio)

basename <- "Casen 2017.csv"
file <- file.path(dirstudio, basename)
población <- read.csv(file = file)

# Se guarda el tamaño de la población
tamaño <- nrow(población)

ingreso <- as.numeric(población[["ytot"]])

poda <- 0.3

q20 <- quantile(ingreso, poda)

q80 <- quantile(ingreso, 1 - poda)

ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]

tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)

sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )

set.seed(133)

ingreso.normal <- rnorm(1000, mean = media.ingreso, sd = sd.ingreso)


############################# Distribución Z ##################################

Z <- (ingreso.normal - media.ingreso) / sd.ingreso

hist(Z)


# Pregunta 2 --------------------------------------------------------------


######################## Distribución Chi Cuadrado ############################

#Sabemos que una distribución Chi cuadrado está dada por la sumatoria de distri-
#buciones Z^2 desde 1 hasta los grados de libertad que demos a la distribución,
#aunque debemos tener en cuenta que las distribuciones Z deben ser aleatorias e
#independientes

# Función que entrega un random de elementos de una distribución Z^2
Z_random <- function(x){
  set.seed(sample(1:500,1))
  sample(Z^2) 
}  


# Función que genera un arreglo que corresponde a la distribución Chi con N gra-
# dos de libertad
chi_dist <- function(grados){
  # Generamos un vector de largo grados al cual aplicamos la función Z_random
  array_chi_cuadrado <- sapply(1:grados, Z_random)
  # sumamos los componentes del vector generado
  dist <- rowSums(array_chi_cuadrado, na.rm=TRUE)
}

# Grados de libertad distribuciones
grad_lib_1 = 4
grad_lib_2 = 8

# Primera distribución
distri_chi_1 <- chi_dist(grad_lib_1)
distri_chi_2 <- chi_dist(grad_lib_2)

# Histogramas distribuciones
hist(distri_chi_1,main ="Distribución Chi Cuadrado (4 grados de libertad)")
hist(distri_chi_2,main ="Distribución Chi Cuadrado (8 grados de libertad)")


# Pregunta 3 --------------------------------------------------------------


############################ Distribución F ####################################

# Como ya tenemos las dos distribuciones necesarias dadas en el ejercicio ante-
# rior podemos aplicar la distribución F, que está dada por:
#
#                 F = (distribución_chi_1 / grados_lib_1 )
#                     --------------------------------------
#                     (distribución_chi_2 / grados_lib_2 )
#

# se hace la división de los chi con sus respectivos grados de libertad
F_dist = ((distri_chi_1 /grad_lib_1) / (distri_chi_2/grad_lib_2))

# Histograma distribución F
hist(F_dist,main ="Distribución F")



# Pregunta 4 --------------------------------------------------------------

ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)

veinte.repeticiones <- sapply(1:20, ensayo)


########################## Distribución Binomial ##############################

# Distribución de probabilidad discreta que nos dice el porcentaje en que es 
# probable obtener un resultado entre dos variables al realizar un número n de 
# pruebas.

# p -> probabilidad de éxito en un solo intento
# k -> cantidad de sucesos
# n -> intentos independientes

funB <- function(k)
{
  p <- 0.65
  n <- 20
  out <- (factorial(n) * (p^k) * ((1 - p)^(n-k))) / (factorial(k) * factorial(n - k))
}

binomial <- sapply(1:20, funB)

plot(binomial, main = "Distribución Binomial")



# Pregunta 5 --------------------------------------------------------------


######################### Distribución Geométrica #############################

# La distribución geométrica es un modelo adecuado para aquellos procesos en los 
# que se repiten pruebas hasta la consecución del éxito a resultado deseado.

# p -> probabilidad de éxito
p <- 0.65 # Basado en veinte repeticiones con 13 aciertos sobre 20 intentos
# 1 - p -> probabilidad de fallo (f)
f <- 1- p

funG <- function(n)
{
  p <- 0.65
  out <- ((1 - p) ^ (n-1)) * p
}

geometrica <- sapply(1:20, funG)

plot(geometrica, main = "Distribución Geométrica")


# Pregunta 6 --------------------------------------------------------------


###################### Distribución Binomial Negativa ##########################

# Esta distribución puede considerarse como una extensión o ampliación de la 
# distribución geométrica. La distribución binomial negativa es un modelo 
# adecuado para tratar aquellos procesos en los que se repite un determinado 
# ensayo o prueba hasta conseguir un número determinado de resultados favorables.

funBn <- function(k)
{
  p <- 0.65
  n <- 20
  out <- (factorial(k + n - 1) * (p^n) * ((1 - p)^k)) / (factorial(n-1) * factorial(k))
}

binomialN <- sapply(1:20, funBn)

plot(binomialN, main = "Distribución Binomial negativa")
