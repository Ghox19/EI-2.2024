library(ez)
library(dplyr)
library(ggplot2)
library(ggpubr)

## PREGUNTA 1

# Leemos los datos del CSV
datos <- read.csv2("Dataset PE1.csv")

# Mostramos los datos iniciales
head(datos)

#Debido a que se están midiendo cuatro grupos independientes, que en este caso corresponden a los usuarios de ChaoCaspa
#de diferentes edades (40-49, 50-59, 60-69, 70 o superior), y además la escala del H2O2 en el cabello de los participantes
#presenta una escala de intervalos iguales. Debido a estos datos, la prueba seleccionada preliminarmente es el método de ANOVA para muestras
#independientes. Para realizarla correctamente, es necesario verificar que las condiciones se cumplan correctamente.

#- Observaciones independientes: Debido al formato del estudio, se puede deducir razonablemente que las observaciones son
#independientes, pues corresponden a distintas personas utilizando un producto para el cabello.
#- La variable medida corresponde a una escala de intervalos iguales: De la misma forma que se mencionó en el análisis preliminar,
#la medida de microgramos de H2O2 es de intervalos iguales.
#- Las observaciones provienen de una distribución normal: Para esta condición, la solución escogida es realizar un test
#test de Shapiro-Wilk en cada uno de los grupos.

datos_filtradosP1 <- datos %>% filter(SEXO == "M") %>% filter(SHAMPOO == "ChaoCaspa") %>% select(ID, SEXO, EDAD, SHAMPOO, NIVEL_3)
datos_filtradosP1[["ID"]] <- factor(datos_filtradosP1[["ID"]])
datos_filtradosP1[["EDAD"]] <- factor(datos_filtradosP1[["EDAD"]])

head(datos_filtradosP1)

edad40 <- datos_filtradosP1 %>% filter(EDAD == "40-49")
edad50 <- datos_filtradosP1 %>% filter(EDAD == "50-59")
edad60 <- datos_filtradosP1 %>% filter(EDAD == "60-69")
edad70 <- datos_filtradosP1 %>% filter(EDAD == "70 o superior")

normalidad40 <- shapiro.test(edad40$NIVEL_3)
print(normalidad40)

normalidad50 <- shapiro.test(edad50$NIVEL_3)
print(normalidad50)
normalidad60 <- shapiro.test(edad60$NIVEL_3)
print(normalidad60)
normalidad70 <- shapiro.test(edad70$NIVEL_3)
print(normalidad70)

g <- ggqqplot(datos_filtradosP1, x = "NIVEL_3", y = "EDAD", color = "EDAD")

g <- g + facet_wrap(~ EDAD)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

#Recordando que esta prueba posee una hipótesis nula que implica que las muestras provienen de una distribución normal,
#se obtuvo que todos los casos presentan un p-valor notablemente mayor al nivel de significancia seleccionado. Por ende, se puede 
#concluir con seguridad que todas las observaciones provienen de una distribución normal. 

#Finalmente, queda verificar la condición de homocedasticidad. Es decir, si las muestras provienen de poblaciones distintas,
#todas deben tener la misma varianza. Para esto, en primera instancia se utiliza la función de R EZAnova, que, además de
#entregar los resultados de la prueba de ANOVA, también incluye un test de Levene para la condición buscada.

alfa <- 0.05

omnibus <- ezANOVA(
  data = datos_filtradosP1,
  dv = NIVEL_3, between = EDAD, wid = ID,
  return_aov = TRUE
)

cat("Resultado de la prueba de Levene:\n")
print(omnibus[2])

#El test de Levene entrega un p-valor de 0.09, el cual está peligrosamente cercano al nivel de significancia dado, 
#pero que sigue siendo mayor, por lo que se puede concluir que la homocedasticidad se cumple, sin embargo, se recomienda
#al químico farmacéutico realizar otra prueba con una muestra más grande.

varianza_por_edad <- datos_filtradosP1 %>%
  group_by(EDAD) %>%
  summarise(varianza_muestral = var(NIVEL_3, na.rm = TRUE))

# Mostrar el resultado
print(varianza_por_edad)

# Calcular la razón entre la máxima y mínima varianza muestral
max_var <- max(varianza_por_edad$varianza_muestral)
min_var <- min(varianza_por_edad$varianza_muestral)
razon_varianza <- max_var / min_var

print(razon_varianza)

#Adicionalmente, se realizó una prueba adicional de varianzas, analizando la relación entre la máxima y mínima varianza.
#Como el resultado entrega 1.24, por lo que, al ser menor que 1.5, se posee otra verificación adicional de la condición,
#permitiendo, finalmente, utilizar la prueba de ANOVA.

#AL realizar el test de ANOVA, se tienen las siguientes hipótesis:
#  - H0: No existe una diferencia entre los microgramos de H2O2 entre los casos de distintas edades que utilizaron ChaoCaspa.
#- HA: Sí existe una diferencia entre los microgramos de H2O2 entre los casos de distintas edades que utilizaron
#ChaoCaspa

print(summary(omnibus[["aov"]]))

#Con un p-valor obtenido muchísimo menor que el nivel de significancia, entonces se puede rechazar la hipótesis nula y 
#aceptar la hipótesis alternativa, concluyendo que existen diferencias entre los grupos de distintas edades.

#Ahora, para determinar en cuáles grupos se encuentran diferencias, se realiza un análisis POST-HOC utilizando la prueba
#HSD de Tukey.

post_hoc <- TukeyHSD(omnibus[["aov"]], which = "EDAD",
                     ordered = TRUE, conf.level = 1 - alfa)
print(post_hoc)

g_efecto <- ezPlot(data = datos_filtradosP1, x = EDAD,
                   dv = NIVEL_3, wid = ID, between = EDAD,
                   y_lab = " Micromolares presentes en el cuero cabelludo [uM]")

g_efecto <- g_efecto + theme_pubr()
print(g_efecto)

#A partir de los resultados, se concluye que existen diferencias en. 

#Concluyendo, se puede determinar con un 95% de confianza que sí existe una diferencia entre los niveles de H2O2 de los hombres que
#utilizaron ChaoCaspa entre los distintos grupos etarios, por lo que el producto sí afecta de manera distinta según la edad
#de la persona que lo utiliza. Sin embargo, debido a la fragilidad del test de homocedasticidad, se recomienda utilizar
#muestras de mayor tamaño, para asegurar la fiabilidad de los resultados

## Pregunta 2

datos_filtradosP2 <- datos %>% filter(SEXO == "F") %>% filter(SHAMPOO == "Placebo") %>% select(ID, SEXO, SHAMPOO, PERCEPCION_3, PERCEPCION_6)

head(datos_filtradosP2)

alfa <- 0.05

# Hacer la prueba de Mann-Whitney.
prueba <- wilcox.test(datos_filtradosP2$PERCEPCION_3, datos_filtradosP2$PERCEPCION_6,paired = TRUE, alternative = "two.sided", conf.level = 1 - alfa)
print(prueba)



