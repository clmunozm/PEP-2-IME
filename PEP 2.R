# PEP 2 Inferencia de Modelos Estadísticos
# Integrantes: 
# David Morales P. (19.881.480-6)
# Claudio Muñoz M. (20.003.395-7)

# Librerías
require (dplyr)


# Lord Vader desea saber si los niveles de exigencia con que los instructores de las diferentes divisiones
# evalúan a los nuevos soldados son similares, por lo que le ha solicitado estudiar si existen diferencias significativas en el
# promedio de la evaluación realizada por el instructor entre las distintas divisiones. El Lord Sith ha sido muy claro al
# solicitar un reporte de aquellas divisiones en las que se observen diferencias

# Se cargan los datos
datos <- read.csv2(file.choose(),header=TRUE)

datos2<- datos %>% group_by(division) %>%
  summarise(mean = mean(eval_instructor), sum= sum(eval_instructor), n = n())

# Se Establece un nivel de significación
alfa <- 0.025

cavetrooper <- datos[datos$division=="Cavetrooper", "eval_instructor"]
snowtrooper <- datos[datos$division=="Snowtrooper", "eval_instructor"]
lavatrooper <- datos[datos$division=="Lavatrooper", "eval_instructor"]
shoretrooper <- datos[datos$division=="Shoretrooper", "eval_instructor"]
spacetrooper <- datos[datos$division=="Spacetrooper", "eval_instructor"]
sandtrooper <- datos[datos$division=="Sandtrooper", "eval_instructor"]
flametrooper <- datos[datos$division=="Flametrooper", "eval_instructor"]
recontrooper <- datos[datos$division=="Recontrooper", "eval_instructor"]

new_datos <- data.frame("Cavetrooper" = cavetrooper, "Snowtrooper" = snowtrooper, 
                        "Lavatrooper" = lavatrooper, "Shoretropper" = shoretrooper, 
                        "Spacetrooper" = spacetrooper,"Sandtrooper"= sandtrooper, 
                        "Flametrooper" = flametrooper,
                        "Recontrooper" = recontrooper)


# Llevar data frame a formato largo.
new_datos <- new_datos %>% pivot_longer(c("Cavetrooper", "Snowtrooper", 
                                       "Lavatrooper", "Shoretropper", 
                                       "Spacetrooper","Sandtrooper", 
                                       "Flametrooper",
                                       "Recontrooper"),
                                        names_to = "division" ,
                                        values_to = "eval_instructor" )

new_datos[["division"]] <- factor(new_datos[["division"]])

new_datos[["instancia"]] <- factor(1:nrow(new_datos))
# ComprobaciÃ³n de normalidad mediante un grafico Q-Q
g <- ggqqplot(new_datos , x = "eval_instructor", y = "division", color = "division")
g <- g + facet_wrap (~ division)
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print (g)

cat("\n\ nProcedimiento ANOVA usando ezANOVA \n\n")
prueba <- ezANOVA(data = new_datos,
                   dv = eval_instructor,
                   between = division,
                   wid = instancia,
                   return_aov = TRUE)

print(prueba)

# Prueba de homocedasticidad
leveneTest(y = new_datos$eval_instructor, group = new_datos$division, center = "median")
# H0: Las varianzas de las muestras son iguales.
# HA: Al menos una de las muestras tiene varianza diferente a alguna de las demás.

# Al ser realizada la prueba de Levene para la homogeneidad de Varianzas, se obtiene
# un p= 0.5035376, mucho mayor al nivel de significación por lo que se puede concluir
# con un 97,5% de confianza que las varianzas de las muestras son iguales.

cat("Procedimiento ANOVA usando aov\n\n")
prueba2 <- aov(eval_instructor ~ division, data = new_datos)
print(summary(prueba2))

# Gráfico del tamaño del efecto .

g2 <- ezPlot(data = new_datos,
             dv = eval_instructor,
             wid = instancia ,
             between = division ,
             y_lab = "eval promedio",
             x = division)

print(g2)


post_hoc <- TukeyHSD(prueba2,
                     "division",
                     ordered = TRUE,
                     conf.level = 1 - alfa)

print(post_hoc)


