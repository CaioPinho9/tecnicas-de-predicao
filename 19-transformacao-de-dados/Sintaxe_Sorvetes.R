# Leitura da base
library(data.table)
base = fread(input = paste0("sorvetes.csv"), header = T, na.strings = "NA", data.table = FALSE, dec = ",")

# Gráfico dos dados
plot(base$Temperatura, base$Vendas)

# Modelo
modelo = lm(Vendas ~ Temperatura, data = base)
summary(modelo)

# Qualidade do Ajuste
plot(fitted(modelo), rstandard(modelo))
abline(0, 0)

# Modelo 2
base$Temperatura2 = base$Temperatura ^ 2
modelo2 = lm(Vendas ~ Temperatura + Temperatura2, data = base)
summary(modelo2)

# Qualidade do Ajuste
plot(fitted(modelo2), rstandard(modelo2))
abline(0, 0)

# Modelo 3
modelo3 = lm(log(Vendas) ~ Temperatura, data = base)
summary(modelo3)

# Qualidade do Ajuste
plot(fitted(modelo3), rstandard(modelo3))
abline(0, 0)


