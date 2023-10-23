# LEITURA DA BASE
library(data.table)
base = fread(input = paste0("apart.csv"), header = T, na.strings = "NA", data.table = FALSE, dec = ",")
library(dplyr)
base$Local = recode_factor(base$Local, `1` = "Mais_Val", `2` = "Menos_Val")
# Modelo
m0 = lm(Valor ~ 1, data = base)
m1 = step(m0, list(lower = ~1,
                upper = ~Area + Idade + Energia + Local),
        direction = "forward")
m2 = step(m0, list(lower = ~1,
                upper = ~Area + Idade + Energia + Local),
        direction = "both")

mc = lm(Valor ~ Area + Idade + Energia + Local, data = base)
m3 = step(mc, list(lower = ~1,
                upper = ~Area + Idade + Energia + Local),
        direction = "backward")

# Modelo Final
modelo = lm(Valor ~ Area + Idade + Local, data = base)
summary(modelo)

# Contribuição de cada variável
library(relaimpo)
imp = calc.relimp(modelo)
var.exp = data.frame(round(imp$lmg * 100, 1))
colnames(var.exp) = "imp.lmg"
nome = rownames(var.exp)
var.exp = data.frame(nome, var.exp)
library(ggplot2)
ggplot(var.exp, aes(nome, imp.lmg)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = imp.lmg), vjust = 1.2, lwd = 5, colour = "white")

