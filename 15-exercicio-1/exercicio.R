# Carregando a base de dados
dados <- read.csv("apartamento.csv", sep=";")

# Preparar o modelo regressão linear:
modelo <- lm(Valor ~ Area + Idade + Energia + Local, data = dados)

summary(modelo)

############### a)

## Retirar a variável "Energia", pois p-value>0.2;
modelo <- lm(Valor ~ Area + Idade + Local, data = dados)

summary(modelo)

## Manter a variável "Local", uma vez que ficou significativo ao retirar a variável "Energia";

## Area:  +1.00939  -> Para cada metro de área, o preço sobe em, aproximadamente, mil reais.
## Idade: -2.05789  -> Para cada ano passado, o preço do imovel diminui dois mil.

## Local: -11.11571 -> Quando a região é mais  valorizada, o preço decai em -11.11571 mil reais;
##                  -> Quando a região é menos valorizada, o preço decai em -22.23142 mil reais;

############### b) 

cor(dados[c("Area", "Idade", "Local")])
library(car)  # Precisaremos da função VIF() do pacote 'car'
vif(modelo)

#> vif(modelo)
#Area    Idade    Local 
#1.126257 1.062417 1.191844

#  Os valores de VIF menores quem 10, indicam que não há preocupações
#  signficativas com multicolinearidade no modelo.

############### c)

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
  geom_text(aes(label = imp.lmg), vjust = 1.5, lwd = 6, colour="white")

# Nota-se que a area influencia mais no preço do imovel;

############## d)

# Como o R-squared = 0.7876, entende-se que o modelo explica 78.76% dos casos

library(car)
qqPlot(modelo)

## E o qqplot tá bom.

plot(fitted(modelo), rstandard(modelo))
abline(0,0)

## E a analise de residuo tá boa;