Experiencia = c(1,2,3,4,5,5,8,10)
Salario = c(3156,3248,3650,3689,3779,3907,3849,4118)
plot(Experiencia, Salario, lwd=6)

# Coeficientes do modelo
lm(Salario ~ Experiencia)

# Reta de regressão no modelo
plot(Experiencia, Salario, lwd=6)
abline(lm(Salario ~ Experiencia))

# Ex1
idade = c(56,42,72,36,47,55,49,38,42,68,60,63)
pressao = c(147,125,160,118,128,150,145,115,140,152,155,149)
cor(idade, pressao)

# Coeficientes do modelo
lm(pressao ~ idade)

# Reta de regressão no modelo
plot(idade, pressao, lwd=6)
abline(lm(pressao ~ idade))

80.778 + 1.138*60
