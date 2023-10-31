# Sintaxe do modelo multiplicativo
library(data.table)
# Leitura e Preparação da base
base <- fread(input = paste0("passageiros.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 
base$I <- as.factor(base$mes)
base1 <- base[-c(91:96),]
novo <- base[c(91:96),]
 
# Visualização gráfica
plot(base$T,base$y,xlab="Periodo de Tempo", ylab="Passageiros (x1000)")
lines(base$T,base$y, col = "black",lwd=2)

# Modelo Aditivo
modelo1 <- lm(y ~ T + I, data=base1)
summary(modelo1)
previsao <- predict(modelo1, novo, interval = "prediction")
resultado1 <- cbind(novo,previsao)
resultado1
plot(resultado1$T,resultado1$y,xlab="Periodo de Tempo", ylab="Passageiros (x1000)",ylim=c(230,450))
lines(resultado1$T,resultado1$y, col = "black",lwd=2)
lines(resultado1$T,resultado1$fit, col = "red",lwd=2)
lines(resultado1$T,resultado1$lwr, col = "red",lwd=2, lty='dashed')
lines(resultado1$T,resultado1$upr, col = "red",lwd=2, lty='dashed')

plot(base1$T,base1$y,xlab="Periodo de Tempo", ylab="Passageiros (x1000)")
lines(base1$T,base1$y, col = "black",lwd=2)
lines(base1$T,modelo1$fitted.values, col = "red",lwd=2)


# Modelo com transformação
modelo2 <- lm(log(y) ~ T + I, data=base1)
summary(modelo2)
previsao <- predict(modelo2, novo, interval = "prediction")
resultado2 <- cbind(novo,exp(previsao))
resultado2
plot(resultado2$T,resultado2$y,xlab="Periodo de Tempo", ylab="Passageiros (x1000)",ylim=c(230,450))
lines(resultado2$T,resultado2$y, col = "black",lwd=2)
lines(resultado2$T,resultado2$fit, col = "red",lwd=2)
lines(resultado2$T,resultado2$lwr, col = "red",lwd=2, lty='dashed')
lines(resultado2$T,resultado2$upr, col = "red",lwd=2, lty='dashed')

plot(base1$T,base1$y,xlab="Periodo de Tempo", ylab="Passageiros (x1000)")
lines(base1$T,base1$y, col = "black",lwd=2)
lines(base1$T,exp(modelo2$fitted.values), col = "red",lwd=2)


# Modelo Multiplicativo 
modelo3 <- lm(y ~ T : I, data=base1)
summary(modelo3)
previsao <- predict(modelo3, novo, interval = "prediction")
resultado3 <- cbind(novo,previsao)
resultado3
plot(resultado3$T,resultado3$y,xlab="Per?odo de Tempo", ylab="Passageiros (x1000)",ylim=c(230,450))
lines(resultado3$T,resultado3$y, col = "black",lwd=2)
lines(resultado3$T,resultado3$fit, col = "red",lwd=2)
lines(resultado3$T,resultado3$lwr, col = "red",lwd=2, lty='dashed')
lines(resultado3$T,resultado3$upr, col = "red",lwd=2, lty='dashed')

plot(base1$T,base1$y,xlab="Periodo de Tempo", ylab="Passageiros (x1000)")
lines(base1$T,base1$y, col = "black",lwd=2)
lines(base1$T,modelo3$fitted.values, col = "red",lwd=2)

# Checagem dos Ajustes
plot(base1$T,base1$y,xlab="Periodo de Tempo", ylab="Passageiros (x1000)")
lines(base1$T,base1$y, col = "black",lwd=2)
lines(base1$T,modelo1$fitted.values, col = "blue",lwd=2)
lines(base1$T,exp(modelo2$fitted.values), col = "red",lwd=2)
lines(base1$T,modelo3$fitted.values, col = "green",lwd=2)
legend(0, 350, legend=c("M1:Aditivo", "M2:Transf.", "M3:Multip."),
       col=c("blue", "red","green"), lty=1:2, cex=0.8)

# Medida de Comparação
novo$Erro1 <- abs(resultado1$y-resultado1$fit)
novo$Erro2 <- abs(resultado2$y-resultado2$fit)
novo$Erro3 <- abs(resultado3$y-resultado3$fit)

plot(novo$T,novo$Erro1,xlab="Periodo de Tempo", ylab="Erro Absoluto")
lines(novo$T,novo$Erro1, col = "blue",lwd=2)
points(novo$T,novo$Erro2, col = "red",lwd=2)
lines(novo$T,novo$Erro2, col = "red",lwd=2)
points(novo$T,novo$Erro3, col = "green",lwd=2)
lines(novo$T,novo$Erro3, col = "green",lwd=2)
legend(94.8, 60, legend=c("M1:Aditivo", "M2:Transf.", "M3:Multip."),
       col=c("blue", "red","green"), lty=1:2, cex=0.8)

Erro_M1 <- sum(abs(resultado1$y-resultado1$fit))/nrow(resultado1)
Erro_M2 <- sum(abs(resultado2$y-resultado2$fit))/nrow(resultado2)
Erro_M3 <- sum(abs(resultado3$y-resultado3$fit))/nrow(resultado3)

