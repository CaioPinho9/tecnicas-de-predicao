library(data.table)
base <- fread(input = paste0("salarios.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(base)

# Modelo de Regressão Múltipla
modelo <- lm(Salario ~ Experiencia + Gerencia + Educacional, data=base)
summary(modelo)

library(car)
Anova(modelo)
