library(ggplot2)
library(ggpubr)
library(tidyr)

# Integrantes:
# - Ekaterina Cornejo 20.187.903-5
# - Catalina Y��ez 19.516.593-9
# - Aldo Castillo 19.839.621-4

# Se setea la direcci�n de trabajo en donde se encuentra nuestro archivo .R
dirstudio <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirstudio)

basename <- "Casen 2017.csv"
file <- file.path(dirstudio, basename)
poblaci�n <- read.csv(file = file)

# Se guarda el tama�o de la poblaci�n
tama�o <- nrow(poblaci�n)

ingreso <- as.numeric(poblaci�n[["ytot"]])

poda <- 0.3

q20 <- quantile(ingreso, poda)

q80 <- quantile(ingreso, 1 - poda)

ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]

tama�o.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)

sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tama�o.podado )

set.seed(133)

ingreso.normal <- rnorm(1000, mean = media.ingreso, sd = sd.ingreso)


############################# Distribuci�n Z ##################################

Z <- (ingreso.normal - media.ingreso) / sd.ingreso

hist(Z)


# Pregunta 2 --------------------------------------------------------------


######################## Distribuci�n Chi Cuadrado ############################

#Sabemos que una distribuci�n Chi cuadrado est� dada por la sumatoria de distri-
#buciones Z^2 desde 1 hasta los grados de libertad que demos a la distribuci�n,
#aunque debemos tener en cuenta que las distribuciones Z deben ser aleatorias e
#independientes

# Funci�n que entrega un random de elementos de una distribuci�n Z^2
Z_random <- function(x){
  set.seed(sample(1:500,1))
  sample(Z^2) 
}  


# Funci�n que genera un arreglo que corresponde a la distribuci�n Chi con N gra-
# dos de libertad
chi_dist <- function(grados){
  # Generamos un vector de largo grados al cual aplicamos la funci�n Z_random
  array_chi_cuadrado <- sapply(1:grados, Z_random)
  # sumamos los componentes del vector generado
  dist <- rowSums(array_chi_cuadrado, na.rm=TRUE)
}

# Grados de libertad distribuciones
grad_lib_1 = 4
grad_lib_2 = 8

# Primera distribuci�n
distri_chi_1 <- chi_dist(grad_lib_1)
distri_chi_2 <- chi_dist(grad_lib_2)

# Histogramas distribuciones
hist(distri_chi_1,main ="Distribuci�n Chi Cuadrado (4 grados de libertad)")
hist(distri_chi_2,main ="Distribuci�n Chi Cuadrado (8 grados de libertad)")


# Pregunta 3 --------------------------------------------------------------


############################ Distribuci�n F ####################################

# Como ya tenemos las dos distribuciones necesarias dadas en el ejercicio ante-
# rior podemos aplicar la distribuci�n F, que est� dada por:
#
#                 F = (distribuci�n_chi_1 / grados_lib_1 )
#                     --------------------------------------
#                     (distribuci�n_chi_2 / grados_lib_2 )
#

# se hace la divisi�n de los chi con sus respectivos grados de libertad
F_dist = ((distri_chi_1 /grad_lib_1) / (distri_chi_2/grad_lib_2))

# Histograma distribuci�n F
hist(F_dist,main ="Distribuci�n F")



# Pregunta 4 --------------------------------------------------------------

ensayo <- function(x)
  ifelse(sample(poblaci�n[["sexo"]], 1) == "Mujer", 1, 0)

veinte.repeticiones <- sapply(1:20, ensayo)


########################## Distribuci�n Binomial ##############################

# Distribuci�n de probabilidad discreta que nos dice el porcentaje en que es 
# probable obtener un resultado entre dos variables al realizar un n�mero n de 
# pruebas.

# p -> probabilidad de �xito en un solo intento
# k -> cantidad de sucesos
# n -> intentos independientes

funB <- function(k)
{
  p <- 0.65
  n <- 20
  out <- (factorial(n) * (p^k) * ((1 - p)^(n-k))) / (factorial(k) * factorial(n - k))
}

binomial <- sapply(1:20, funB)

plot(binomial, main = "Distribuci�n Binomial")



# Pregunta 5 --------------------------------------------------------------


######################### Distribuci�n Geom�trica #############################

# La distribuci�n geom�trica es un modelo adecuado para aquellos procesos en los 
# que se repiten pruebas hasta la consecuci�n del �xito a resultado deseado.

# p -> probabilidad de �xito
p <- 0.65 # Basado en veinte repeticiones con 13 aciertos sobre 20 intentos
# 1 - p -> probabilidad de fallo (f)
f <- 1- p

funG <- function(n)
{
  p <- 0.65
  out <- ((1 - p) ^ (n-1)) * p
}

geometrica <- sapply(1:20, funG)

plot(geometrica, main = "Distribuci�n Geom�trica")


# Pregunta 6 --------------------------------------------------------------


###################### Distribuci�n Binomial Negativa ##########################

# Esta distribuci�n puede considerarse como una extensi�n o ampliaci�n de la 
# distribuci�n geom�trica. La distribuci�n binomial negativa es un modelo 
# adecuado para tratar aquellos procesos en los que se repite un determinado 
# ensayo o prueba hasta conseguir un n�mero determinado de resultados favorables.

funBn <- function(k)
{
  p <- 0.65
  n <- 20
  out <- (factorial(k + n - 1) * (p^n) * ((1 - p)^k)) / (factorial(n-1) * factorial(k))
}

binomialN <- sapply(1:20, funBn)

plot(binomialN, main = "Distribuci�n Binomial negativa")
