library(tidyverse)
library(forecast)
library(lubridate)
library(data.table)
# Leitura e Preparação da base
base <- fread(input = paste0("energia.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 

# INFORMACOES SOBRE A BASE DE DADOS
# 1404	Electric energy consumption – Brazil – industrial
# Mais detalhes: https://ibpad.com.br/comunicacao/deixe-seus-graficos-no-ggplot2-mais-visuais-com-imagens/

#visualização grafica
plot(base$consumo,type="s")

################################################################################
# SUAVIZACAO EXPONENCIAL SIMPLES
alpha1 <- ses(base$consumo, alpha = 0.1)
alpha2 <- ses(base$consumo, alpha = 0.5)
alpha3 <- ses(base$consumo, alpha = 0.9)

# calculando o erro de cada ajuste
list(alpha1, alpha2, alpha3) %>% map(accuracy)

# analise grafica do ajuste
plot(base$consumo,type="s")
lines(fitted(alpha1), col="blue")
lines(fitted(alpha2), col="red")
lines(fitted(alpha3), col="green")
legend("topleft",lty=1, col=c(1,"blue","red","green"),
       c("serie original", expression(alpha == 0.1),
         expression(alpha == 0.5),
         expression(alpha == 0.9)),
       pch=1)

# qual o valor otimo encontrado para alpha nesse caso?
alpha_otimo <- ses(base$consumo)
summary(alpha_otimo)


################################################################################
# SUAVIZACAO EXPONENCIAL DE HOLT
beta1 <- holt(base$consumo, alpha = 0.6, beta = 0.4)
summary(beta1)
beta2 <- holt(base$consumo)
summary(beta2)

# calculando o erro de cada ajuste
list(beta1, beta2) %>% map(accuracy)

# analise grafica do ajuste
plot(base$consumo,type="s")
lines(fitted(beta1), col = "blue")
lines(fitted(beta2), col = "red")


################################################################################
# SUAVIZACAO EXPONENCIAL DE HOLT-WINTER
base_ts <- ts(base$consumo, frequency=12, start=c(1980,1))
base_ts

gama_ad <- hw(base_ts, seasonal = "additive")
gama_mult <- hw(base_ts, seasonal = "multiplicative")

# calculando o erro de cada ajuste
list(gama_ad, gama_mult) %>% map(accuracy)
summary(gama_ad)

# analise grafica do ajuste
plot(base_ts)
lines(fitted(gama_ad), col = "blue")
lines(fitted(gama_mult), col = "red")


################################################################################
# COMPARACAO GERAL

list(alpha_otimo, beta2, gama_ad) %>% map(accuracy)
