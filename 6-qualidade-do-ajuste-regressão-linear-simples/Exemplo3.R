library(data.table)
library(ggplot2)
Base <- fread(input = paste0("Exercicio.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")

# Gráfico
plot(Base$Cal_Ing, Base$Perda_Peso)
plot(Base$Ex_Aerob, Base$Perda_Peso)
plot(Base)

# Coeficiente de correlação
cor(Base)

# Regressão linear
modelo = lm(Perda_Peso ~ Ex_Aerob, data=Base)

# Reta de regressão no modelo
plot(Base$Ex_Aerob, Base$Perda_Peso)
abline(modelo)

# Coeficiente de determinação (R2)
summary(modelo)$r.squared

# Análise de resíduos
plot(fitted(modelo), rstandard(modelo))
abline(0,0)

# Previsão
novo = data.frame(Ex_Aerob=210)
predict(modelo, novo)
