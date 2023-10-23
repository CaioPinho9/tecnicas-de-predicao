# LEITURA DA BASE
library(data.table)
base = fread(input = paste0("bpd.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 

# Analise Grafica
library(ggplot2)
ggplot(base, aes(x=peso, y=bpd))+
       geom_point() 

# Modelo
modelo = glm(bpd ~ peso, family = binomial(), data=base)
summary(modelo)
prob = predict(modelo, base, type = "response")
base1 = cbind(base,prob)

# Grafico do Ajuste
library(ggplot2)
ggplot(base, aes(x=peso, y=bpd))+
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 

# Predicao
novo = data.frame(peso=900)
predict(modelo, novo, type = "response")

