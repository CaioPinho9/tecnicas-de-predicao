# LEITURA DA BASE
library(data.table)
base =
  fread(
    input = paste0("selecao.csv"),
    header = T,
    na.strings = "NA",
    data.table = FALSE,
    dec = ","
  )
library(dplyr)

# Modelo 1
m0 = lm(y ~ 1, data = base)
m1 = step(m0,
          list(
            lower = ~ 1,
            upper = ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
          ),
          direction = "forward")

# Análise de resíduos
plot(fitted(m1), rstandard(m1))
abline(0, 0)
anova(m1)
summary(m1)
# Com essa análise de residuos podemos ver que os pontos não estão aleatóriamente distribuidos

# Transformando todas as variaveis para o quadrado
base$x1Squared = base$x1 ^ 2
base$x2Squared = base$x2 ^ 2
base$x3Squared = base$x3 ^ 2
base$x4Squared = base$x4 ^ 2
base$x5Squared = base$x5 ^ 2
base$x6Squared = base$x6 ^ 2
base$x7Squared = base$x7 ^ 2
base$x8Squared = base$x8 ^ 2
base$x9Squared = base$x9 ^ 2

# Modelo 2
m0 = lm(y ~ 1, data = base)
m2 = step(
  m0,
  list(
    lower = ~ 1,
    upper = ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x1Squared + x2Squared + x3Squared + x4Squared + x5Squared + x6Squared + x7Squared + x8Squared + x9Squared
  ),
  direction = "forward"
)

# Análise de resíduos
plot(fitted(m2), rstandard(m2))
abline(0, 0)
anova(m2)
summary(m2)
# Nesse segundo modelo o resídio já está ajustado, porém algumas das variaveis não possuem significancia e podem ser retiradas

# Modelo Final
m3 = lm(y ~ x2 + x4 + x5 + x7 + x8 + x9 +  x5Squared + x7Squared + x8Squared + x9Squared,
        data = base)

# Análise de resíduos
plot(fitted(m3), rstandard(m3))
abline(0, 0)
anova(m3)
summary(m3)
# Esse modelo usa menos variaveis que o anterior, enquanto mantem o residuo esperado e o mesmo R quadrado.