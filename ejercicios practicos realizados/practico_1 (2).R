library(ggplot2)
library(ggpubr)
library(dplyr)

# Integrantes:
# - Ekaterina Cornejo 20.187.903-5
# - Catalina Ya�ez 19.516.593-9
# - Aldo Castillo 19.839.621-4


# Pregunta b) -------------------------------------------------------------
# �Se encuest� m�s o menos la misma cantidad de gente en cada provincia de la RM?

# Se setea la direcci�n de trabajo en donde se encuentra nuestro archivo .R
dirstudio <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirstudio)

# Se guarda el nombre del archivo y se abre con File.path
basename <- "Casen 2017.csv"
file <- file.path(dirstudio, basename)

# Se lee el archivo .csv y se guarda en la variable "poblaci�n"
poblaci�n <- read.csv(file = file, encoding = "UTF-8")

# Se genera la columna ingresos
poblaci�n$ingresos <- poblaci�n$ytot / 1000
poblaci�n.b <- poblaci�n %>% group_by(provincia) %>% summarize(totalPersonas=n())

# Se rescata el tama�o de la poblaci�n
tama�o.poblaci�n <- nrow(poblaci�n)

# Se define la semilla y el tama�o de muestra
semilla <- 113
tama�o.muestra <- 100

# Se setea la semila y rescata una muestra del tama�o de muestra antes definido
set.seed(semilla)
muestra <- poblaci�n[sample(1:tama�o.poblaci�n, tama�o.muestra), ]
datos.b <- muestra

# Se toma cada comuna y se rescata la cantidad de personas que participaron por provincia
new <- datos.b %>% group_by(provincia) %>% summarize(totalPersonas=n())

# Se grafica cantidad de personas vs provincia en base a la muestra
p <- ggboxplot(
  new,
  x = "provincia", y = "totalPersonas",
  add = "mean", add.params = list(color = "#FC4E07"),
  color = "provincia", fill = "provincia",
  title = "Muestra de personas por provincia en Regi�n Metropolitana",
  ylab = "Cantidad de personas"
)
print(p)


cat("\n")
cat("Personas pregistradas por provincia en una muestra\n")
cat("-----------------------------\n")
print(new)


# Se grafica cantidad de personas vs provincia en el total de la poblaci�n
p2 <- ggboxplot(
  poblaci�n.b,
  x = "provincia", y = "totalPersonas",
  add = "mean", add.params = list(color = "#FC4E07"),
  color = "provincia", fill = "provincia",
  title = "Poblaci�n de personas por provincia en Regi�n Metropolitana",
  ylab = "Cantidad de personas"
)
print(p2)


cat("\n")
cat("Personas pregistradas por provincia en la poblaci�n total\n")
cat("-----------------------------\n")
print(poblaci�n)


# Conclusi�n
# Analizando el gr�fico generado "Muestra de personas por provincia en Region Metropolitana" se puede ver que
# existe una clara diferencia en cuanto a las personas que participaron en la provincia de Santiago contra
# las dem�s. Adem�s, se puede ver en el resumen que nos muestra al final del c�digo, que si sacamos Santiago
# de los datos, s� se mantiene dentro de un rango de 1 a 10 participantes en cada provincia.


# Pregunta g) -------------------------------------------------------------

# �Van los ingresos de los chilenos increment�ndose con la experiencia y de forma similar entre hombres y
# mujeres?
  
datos.g <- muestra
datos.g$ingresos <- datos.g$ytot / 1000

datos.g.muj <- subset(datos.g, datos.g$sexo == "Mujer")
datos.g.hom <- subset(datos.g, datos.g$sexo == "Hombre")

regresion_hombres <- lm(log(datos.g$ingresos) ~ datos.g$edad, data = datos.g, subset = sexo == "Hombre")
regresion_mujeres <- lm(log(datos.g$ingresos) ~ datos.g$edad, data = datos.g, subset = sexo == "Mujer")

plot (log(datos.g$ingresos) ~ datos.g$edad, 
                col = rainbow(2),
                main = "Ingresos en funci�n de la experiencia de la muestra",
                xlab = "Edad",
                ylab = "Ingresos"
)

legend("topright", pch=16, col=rainbow(2), legend=c("Hombres","Mujeres"))

abline(regresion_hombres, col="red")
abline(regresion_mujeres, col="cyan")



regresion_hombres_total <- lm(log(poblaci�n$ingresos) ~ poblaci�n$edad, data = poblaci�n, subset = sexo == "Hombre")
regresion_mujeres_total <- lm(log(poblaci�n$ingresos) ~ poblaci�n$edad, data = poblaci�n, subset = sexo == "Mujer")

plot (log(poblaci�n$ingresos) ~ poblaci�n$edad, 
      col = rainbow(2),
      main = "Ingresos en funci�n de la experiencia de la poblacion",
      xlab = "Edad",
      ylab = "Ingresos"
)

legend("topright", pch=16, col=rainbow(2), legend=c("Hombres","Mujeres"))

abline(regresion_hombres_total, col="red")
abline(regresion_mujeres_total, col="cyan")

# Conclusi�n
# En primer lugar, notar que se hace uso de la curva de regresi�n lineal en el gr�fico. Esta es utilizada
# para mostrar la relaci�n de dependencia de las variables edad e ingresos. En este caso, la edad representa
# la experiencia (a mayor edad, mayor experiencia). Entonces, a partir del gr�fico generado, podemos concluir
# que existe una tendencia positiva de los ingresos de ambos g�neros a medida aunmenta la experiencia, pero
# desde el inicio se puede notar que los ingresos de las mujeres son m�s bajos que los de los hombres.


# Nota --------------------------------------------------------------------

# El gr�fico de poblaci�n total de la parte b no siempre se muestra, se busco el error pero no encontramos 
# soluci�n, sin ejecutar el programa de nuevo si refresca el gr�fico o le hace zoom, este aparece sin problemas.
