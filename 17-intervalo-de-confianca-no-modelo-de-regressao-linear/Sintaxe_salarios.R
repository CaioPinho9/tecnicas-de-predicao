# LEITURA DA BASE
library(data.table)
base = fread(input = paste0("salarios.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")

# Classificação das variáveis qualitativas
base$Gerencia_CAT = as.factor(base$Gerencia)
base$Educacional_CAT = as.factor(base$Educacional)

# Modelo
modelo = lm(Salario ~ Experiencia + Gerencia_CAT + Educacional_CAT, data=base)
summary(modelo)
pred1 = data.frame(Experiencia=0,Gerencia_CAT="1",Educacional_CAT="3")
pred2 = data.frame(Experiencia=0,Gerencia_CAT="0",Educacional_CAT="3")
novo = rbind(pred1,pred2)

# Previsão da média
predito = predict(modelo, novo, interval="confidence",level=0.95)
