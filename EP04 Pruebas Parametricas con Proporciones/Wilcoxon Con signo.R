
Interfaz_A <- c(2.9, 6.1, 6.7, 4.7, 6.4, 5.7, 2.7, 6.9, 1.7, 6.4) Interfaz_B <- c(6.0, 2.8, 1.3, 4.7, 3.1, 1.8, 2.9, 4.0, 2.3, 1.6)
# Establecer nivel de significación.
alfa <- 0.05
# Hacer la prueba de rangos con signo de Wilcoxon.
prueba <- wilcox.test(Interfaz_A, Interfaz_B, paired = TRUE, alternative = "greater", conf.level = 1 - alfa)
print(prueba)