# LEITURA DA BASE
library(data.table)
base = fread(input = paste0("bpd.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 

# Categorizacao das variaveis
base$peso_cat = cut(base$peso,
                     breaks=c(0, 950, 1350, 1750),
                     labels=c("F1","F2","F3"))

# Modelo
modelo = glm(bpd ~ peso_cat, family = binomial(), data=base)
summary(modelo)
prob = predict(modelo, base, type = "response")
base1 = cbind(base,prob)

# Razão de Chances
OR = data.frame(exp(modelo$coefficients))
IC = data.frame(exp(confint(modelo)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")

# Reordenando os nives da variavel peso_cat
library(dplyr)
library(forcats)
base = base %>% 
        mutate(peso_cat = peso_cat %>% 
                          fct_relevel("F3"))

# Modelo
modelo = glm(bpd ~ peso_cat, family = binomial(), data=base)
summary(modelo)

# Razão de Chances
OR = data.frame(exp(modelo$coefficients))
IC = data.frame(exp(confint(modelo)))
IC_OR = cbind(OR[-1,],IC[-1,])
colnames(IC_OR) = c("OR","2.5%","97.5%")
