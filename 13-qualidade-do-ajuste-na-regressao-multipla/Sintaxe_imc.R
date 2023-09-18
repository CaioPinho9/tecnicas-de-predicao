library(data.table)
dados = fread(input = paste0("imc.csv"), header = T, na.strings = "NA", data.table = FALSE, dec = ",")
names(dados)
cor(dados)
# Modelo de Regressão Múltipla
modelo = lm(IMC ~ TR + SOMA_DC, data = dados)
summary(modelo)

# Análise Gráfica da relação das variáveis independentes com a variável dependente
library(car)
vif(modelo)
avPlots(modelo)

# Importância de cada variável no modelo
library(relaimpo)
imp = calc.relimp(modelo)
var.exp = data.frame(round(imp$lmg * 100, 1))
colnames(var.exp) = "imp.lmg"
nome = rownames(var.exp)
var.exp = data.frame(nome, var.exp)

library(ggplot2)
ggplot(var.exp, aes(nome, imp.lmg)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = imp.lmg), vjust = 1.5, lwd = 6, colour = "white")

# Análise de resíduos
plot(fitted(modelo), rstandard(modelo))
abline(0, 0)
library(car)
qqPlot(modelo)

library(rgl)
plot3d(dados$IMC, dados$SOMA_DC, dados$TR, col = "blue", size = 2)