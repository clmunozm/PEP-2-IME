# PEP 2 Inferencia de Modelos Estadísticos
# Integrantes: 
# David Morales P. (19.881.480-6)
# Claudio Muñoz M. (20.003.395-7)

# Librerías
require (dplyr)

# Pregunta 1
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


# Formulación de Hipótesis.
# H0: No existen diferencias significativas en el promedio de la evaluación realizadas por intructor 
# en las distintas divisiones

# HA: Existen diferencias significativas en el promedio de la evaluación realizadas por intructor 
# en las distintas divisiones


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

# Comprobación de normalidad mediante un grafico Q-Q
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
# un p= 0.5035376, mucho mayor al nivel de significancia de 0.025 por lo que se puede concluir
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

# Conclusión prueba Anova
# Al obtener un valor p de 2.576e-107, muy inferior a un nivel de significancia de 0.025
# se rechaza la hipótesis nula en favor de la alternativa, concluyendo así que
# al menos una de las divisiones presenta un promedio de evaluación por parte del instructor
# con una diferencia significativa en relación con las demas divisiones. Esto además
# se puede ver en el gráfico del tamaño del efecto.



# Prueba Post-Hoc

post_hoc <- TukeyHSD(prueba2,
                     "division",
                     ordered = TRUE,
                     conf.level = 1 - alfa)

print(post_hoc)

# Conclusión prueba Post Hoc
# Como se pudo ver en los resultados de la prueba post-hoc, los valores p
# asociados a las diferencias de cada una de las evaluaciones de los troopers, 
# solo las diferencias de los troopers correspondientes a Cavetrooper y Spacetroopers
# presentaron un valor p menor a un nivel de significancia de 0.025, por lo que
# es posible concluir que los troopers de las divisiones Cavetroopers y Spacestroopers
# poseen diferencias significativas en el promedio de las evaluaciones realizadas por el instructor

# Pregunta 2


# Pregunta 3
# La Universidad de Santiago de Chile consta con una gran cantidad de alumnos, 
# al encontrarse todas las carreras en el mismo espacio existen diversas opiniones 
# y creencias respecto a la vuelta a la normalidad, la presencialidad en tiempos de 
# covid es un tema de que hablar y cada estudiante tiene su opinión respecto a cada 
# ambito del que hacer en la universidad de forma presencial. El siguiente estudio 
# pretende determinar si existe diferencia en como es de necesario volver a realizar 
# distintas actividades de forma presencial. Para ello, se seleccionaron a 10 alumnos  
# de distintas carreras y facultades  respondiendo del 1 al 5 (siendo 1 para nada 
# necesario, 2 poco necesario, 3 indiferente, 4 necesario y 5 muy necesario ) ir de 
# forma presencial a: "rendir evaluaciones", "clases presenciales" y "ir al foro".

# H0: Las actividades presenciales tienen preferencias similares por parte de los alumnos
# H1: Al menos una actividad presencial tiene una preferencia distinta a las demás por parte de los alumnos

