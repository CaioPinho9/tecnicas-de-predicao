library(data.table)
base = fread(input = paste0("risco.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(base)

# Modelo de Regressão Múltipla
modelo = lm(Risco ~ Idade + PA + Tabag, data=base)
summary(modelo)

library(car)
Anova(modelo)
