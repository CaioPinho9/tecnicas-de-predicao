# LEITURA DA BASE
library(data.table)
base = fread(input = paste0("Deslocamento.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 
base <- as.data.frame(lapply(base, as.factor))

colnames(base)
# Modelo
m0 = glm(desloc ~ 1, data = base, family=binomial())
m1 = step(m0, list(lower = ~1,
                   upper = ~escola + sexo + idade + imc + tr + pa + t_livre),
          direction = "forward")

summary(m1)

# Razão de Chances
OR = data.frame(exp(m1$coefficients))
IC = data.frame(exp(confint(m1)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")