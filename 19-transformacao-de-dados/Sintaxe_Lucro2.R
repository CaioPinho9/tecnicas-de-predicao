# Leitura da base
library(data.table)
base <- fread(input = paste0("Lucro2.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(base)

# Gráfico de cada variável independente com a variável dependente
plot(base$Clientes,base$Lucro)
plot(base$Tempo,base$Lucro)

# Modelo
modelo <- lm(Lucro ~ Clientes + Tempo, data=base)
summary(modelo)

# Qualidade do Ajuste
plot(fitted(modelo),rstandard(modelo))
abline(0,0)

# Modelo 2
modelo2 <- lm(log(Lucro) ~ Clientes + Tempo, data=base)
summary(modelo2)

# Qualidade do Ajuste
plot(fitted(modelo2),rstandard(modelo2))
abline(0,0)

