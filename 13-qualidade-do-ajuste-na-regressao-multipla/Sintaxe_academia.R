library(data.table)
dados <- fread(input = paste0("academia.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(dados)
# Modelo de Regressão Múltipla
modelo <- lm(Perda_Peso ~ Ex_Aerob + Cal_Ing, data=dados)
summary(modelo)

# Análise Gráfica da relação das variáveis independentes com a variável dependente
library(car)
avPlots(modelo)

# Importância de cada variável no modelo
library(relaimpo)
imp<-calc.relimp(modelo)
var.exp<-data.frame(round(imp$lmg*100,1))
colnames(var.exp)<-"imp.lmg"
nome<-rownames(var.exp)
var.exp<-data.frame(nome,var.exp)

library(ggplot2)
ggplot(var.exp,aes(nome,imp.lmg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = imp.lmg), vjust = 1.5, lwd=6, colour = "white")

# Análise de resíduos
plot(fitted(modelo), rstandard(modelo))
abline(0,0)
library(car)
qqPlot(modelo)

