# Dados
aditivo = c(1,2,3,4,5,6)
octanagem = c(80.5,81.6,82.1,83.7,83.9,85.0)
# Modelo de regressão linear
modelo = lm(octanagem ~ aditivo)
summary(modelo)
anova(modelo)
