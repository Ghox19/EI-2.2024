#Cuando existe menos de 5 observaciones en un grupo, se tiene como remplazo a la prueba chicuadrado de Independencia

#Ejemplo 
#Argh y Grrr, son igualmente efectivas para inmunizar a la población ante una mordida de vampiro. 
#Para ello, los investigadores reclutaron a 17 voluntarios de todo el mundo, de los cuales 6 recibieron 
#la vacuna Argh y los 11 restantes, la Grrr. Al cabo de tres meses, sometieron a cada uno de los
#participantes a una mordida de vampiro y observaron que ninguno de los voluntarios que recibieron 
#la vacuna Argh resultó afectado,

vacuna <- c(rep("A", 6), rep("B", 11))
resultado <- c(rep("C", 12), rep("D", 5))

prueba <- fisher.test(vacuna, resultado)
print(prueba)