# EXEMPLO 1
# Sintaxe do modelo aditivo
library(data.table)
base = fread(input = paste0("joalheria.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 
base$T = seq(1:nrow(base))
base$I = as.factor(base$TRIM)

# Visualização gráfica
plot(base$T,base$VENDAS,xlab="Período de Tempo", ylab="Vendas")
lines(base$T,base$VENDAS, col = "black")

# Modelo
modelo = lm(VENDAS ~ T + I, data=base)
summary(modelo)

# Visualização gráfica do ajuste
plot(base$T,base$VENDAS,xlab="Período de Tempo", ylab="Vendas")
lines(base$T,base$VENDAS, col = "black")
lines(base$T,modelo$fitted.values, col = "red")

# EXEMPLO 2
# Sintaxe para ajuste do modelo e análises gráficas
library(data.table)
base = fread(input = paste0("passageiros.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 
base$I = as.factor(base$mes)

# Modelo
modelo = lm(y ~ T + I, data=base)
summary(modelo)

# Visualização gráfica do ajuste
plot(base$T,base$y,xlab="Período de Tempo", ylab="Passageiros (x1000)")
lines(base$T,base$y, col = "black",lwd=2)
lines(base$T,modelo$fitted.values, col = "red",lwd=2)

# Análise de Resíduos
plot(modelo$fitted.values,modelo$residuals,xlab="valores ajustados", ylab="resíduos")
abline(0,0)
plot(modelo$fitted.values,rstandard(modelo),xlab="valores ajustados", ylab="resíduos")

# EXEMPLO 2 - Retirando as 6 últimas observações
# Sintaxe para comparar a eficiência do modelo
base1 = base[-c(91:96),]
novo = base[c(91:96),]

# Modelo
modelo1 = lm(y ~ T + I, data=base1)
summary(modelo1)
previsao = predict(modelo1, novo, interval = "prediction")
resultado = cbind(novo,previsao)
resultado

# Visualização gráfica da previsão
plot(resultado$T,resultado$y,xlab="Período de Tempo", ylab="Passageiros (x1000)", ylim=c(220,430))
lines(resultado$T,resultado$y, col = "black",lwd=2)
lines(resultado$T,resultado$fit, col = "red",lwd=2)
lines(resultado$T,resultado$lwr, col = "red",lty=2,lwd=2)
lines(resultado$T,resultado$upr, col = "red",lty=2,lwd=2)
