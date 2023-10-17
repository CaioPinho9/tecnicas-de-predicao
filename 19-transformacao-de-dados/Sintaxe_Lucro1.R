# Leitura da base
library(data.table)
base <- fread(input = paste0("Lucro1.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(base)

# Gráfico
plot(base$Tempo,base$Lucro)

# Modelo
modelo <- lm(Lucro ~ Tempo, data=base)
summary(modelo)

# Qualidade do Ajuste
plot(fitted(modelo),rstandard(modelo))
abline(0,0)

# Modelo 2
modelo2 <- lm(log(Lucro) ~ Tempo, data=base)
summary(modelo2)

# Qualidade do Ajuste
plot(fitted(modelo2),rstandard(modelo2))
abline(0,0)

# Previsoes
previsao <- predict(modelo2)
base1 <- cbind(base,previsao)
base1$PrevLucro <- exp(base1$previsao)

novo=data.frame(Tempo=3)
predict(modelo,novo)
exp(predict(modelo2,novo))

