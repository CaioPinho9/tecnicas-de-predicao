aditivo = c(1,2,3,4,5,6)
octanagem = c(80.5,81.6,82.1,83.7,83.9,85.0)

dados = data.frame(aditivo, octanagem)

rm(aditivo, octanagem)

cor(dados$aditivo, dados$octanagem)

modelo = lm(octanagem ~ aditivo, data=dados)

coef(modelo)

summary(modelo)$r.squared

plot(dados$aditivo, dados$octanagem, lwd=6)
abline(modelo)
