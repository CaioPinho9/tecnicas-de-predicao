# Dados
funcionarios = c(100,200,300,400,500,600,700)
acidentes = c(40,45,50,65,70,70,80)

# Modelo
modelo = lm(acidentes ~ funcionarios)
summary(modelo)
# Intervalo de confiança e de predição para os dados da base
IC1 = predict(modelo, interval="confidence",level = 0.95)
IC2 = predict(modelo, interval="predict",level = 0.95)
new = data.frame(funcionarios,acidentes,IC1,IC2)

# Gráfico
library("ggplot2")
ggplot(new, aes(funcionarios, acidentes)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) + 
  geom_line(aes(y = lwr), color = "black") +
  geom_line(aes(y = upr), color = "black") +
  geom_line(aes(y = lwr.1), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr.1), color = "red", linetype = "dashed")

# Predição para 550 funcionários 
novo = data.frame(funcionarios=550)
predict(modelo, novo, interval="confidence")
predict(modelo, novo, interval="predict")

