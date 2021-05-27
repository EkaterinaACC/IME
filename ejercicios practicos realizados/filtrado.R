library(ggplot2)
library(ggpubr)
library(knitr)
library(tidyr)
require(ez)
library(datasets)
library(car)
library(rcompanion)

# Indicar directorio
dir <- "~/Descargas"
basename <- "Datos-Casen-v2.csv"
file <- file.path(dir, basename)
poblacion <- read.csv(file = file, encoding = "UTF-8")
tamaño.poblacion <- nrow(poblacion)


distinto <- subset(poblacion,
                   subset = poblacion$region=="Región de Magallanes y de la Antártica Chilena" | poblacion$region=="Región Metropolitana de Santiago",
                   select = c("region","ytotcorh"))
tamaño.poblacion <- nrow(distinto)

semilla <- 113
tamaño.muestra <- 100

set.seed(semilla)
muestra <- distinto[sample(1:tamaño.poblacion, tamaño.muestra), ]

personas <- factor(1:nrow(muestra))

datos.long <- data.frame(
  personas,
  region = factor(muestra[["region"]]),
  sueldo = muestra[["ytotcorh"]]
)

p1 <- ggboxplot(
  datos.long,
  x = "region", y = "sueldo",
  fill = "region"
)
print(p1)


##verificaciones

p2 <- ggqqplot(
  muestra,
  x = "ytotcorh",
  color = "region"
)
p2 <- p2 + facet_wrap(~ region)
p2


#normalizando datos
muestra$ytotcorh <- transformTukey(datos.long$sueldo)
p2 <- ggqqplot(
  muestra,
  x = "ytotcorh",
  color = "region"
)
p2 <- p2 + facet_wrap(~ region)
p2
shapiro.test(muestra[["ytotcorh"]])


lts <- leveneTest(sueldo ~ region, datos.long)
print(lts)


ez.aov <- ezANOVA(
  data = datos.long, 
  dv = sueldo,
  wid = personas,
  between = region,
  type = 2,
  return_aov = TRUE
)
tabla.aov <- summary(ez.aov[["aov"]])[[1]]
tabla.aov[1, "Pr(>F)"] <- sub(
  pattern = "0.",
  replacement = ".",
  x = sprintf("%1.3f", tabla.aov[1, "Pr(>F)"])
)
tabla.aov <- cbind(tabla.aov, GES = c(ez.aov[["ANOVA"]][["ges"]], NA))
options(knitr.kable.NA = '')
kt <- kable(
  tabla.aov,
  format = "pandoc"
  # format.args = list(zero.print = FALSE)
  
)
print(kt)



ezp <- ezPlot(
  data = datos.long,
  dv = sueldo,
  wid = personas,
  between = region,
  type = 3,
  x = region
)
print(ezp)
mt <- TukeyHSD(ez.aov[["aov"]])
print(mt)

##########################################
#sin normalizar se utilizara kruskall-wallis
t5 <- kruskal.test(sueldo  ~ region, datos.long)

print(t5)
Ranking <- rank(datos.long[["sueldo"]])
datos.long <- data.frame(datos.long, Ranking)

p4 <- ggboxplot(
  datos.long,
  x = "region", y = "Ranking",
  xlab = "region",
  color = "region",
  add = "jitter"
)
p4
ph1 <- pairwise.wilcox.test(datos.long[["sueldo"]], datos.long[["region"]],
                            p.adjust.method = "BH")

cat("\n\n")
cat("Post-hoc entre pares de cremas\n")
cat("------------------------------\n")
print(ph1)
