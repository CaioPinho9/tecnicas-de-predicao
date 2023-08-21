rbvn = function(n, mu1, s1, mu2, s2, rho) {
  X1 = rnorm(n, mu1, s1)
  X2 = rnorm(n, mu2 + (s2 / s1) * rho *
                (X1 - mu1), sqrt((1 - rho ^ 2) * s2 ^ 2))
  cbind(X1, X2)
}
dados = data.frame(rbvn(1000, 1.60, 0.09, 60, 8, 0.70))
colnames(dados) = c("Altura", "Peso")

library(sampling)
dados$ID = seq(1:nrow(dados))
amostra30 = c()
n = 10000
for (i in 1:n) {
  Amostra = strata(dados, size = 30, method = "srswor")
  colnames(Amostra)[1] = 'ID'
  ASA = merge(Amostra, dados, by = 'ID')
  amostra30[i] = cor(ASA$Altura, ASA$Peso)
}
hist(amostra30, col = 'steelblue', main = 'Simulação de 10.000 amostras')
abline(v = 0.7, col = "red", lwd = 4)


# Dados
anos = c(2, 4, 4, 5, 6, 7, 8, 8, 10, 10)
clientes = c(48, 61, 53, 64, 60, 63, 72, 70, 71, 81)
plot(anos, clientes)

# Intervalo de Confiança
cor.test(anos, clientes, conf.level = 0.95)$conf.int

# Teste de Hipóteses
cor.test(anos, clientes, alternative = "two.sided")


rbvn = function(n, mu1, s1, mu2, s2, rho) {
  X1 = rnorm(n, mu1, s1)
  X2 = rnorm(n, mu2 + (s2 / s1) * rho *
                (X1 - mu1), sqrt((1 - rho ^ 2) * s2 ^ 2))
  cbind(X1, X2)
}
dados = data.frame(rbvn(1000, 1.60, 0.09, 60, 8, 0.10))
colnames(dados) = c("Altura", "Peso")

# Teste de Hipóteses
cor.test(dados$Altura, dados$Peso, alternative = "two.sided")
plot(dados)
