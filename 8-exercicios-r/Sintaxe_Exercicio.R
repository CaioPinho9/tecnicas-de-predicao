library(data.table)
Base =
  fread(
    input = paste0("exercicios.csv"),
    header = T,
    na.strings = "NA",
    data.table = FALSE,
    dec = ","
  )
names(Base)

# Gráfico de dispersão total
plot(Base[-5])

# Correlação entre as variáveis
cor(Base[-5])
cor(Base$Carnes, Base$Gasto) # 0.7400449
cor(Base$Limpeza, Base$Gasto) # 0.1112046
cor(Base$Frutas, Base$Gasto) # 0.1328795
# Isso mostra que a carne aparenta ter uma relação forte nessa amostra

# Intervalo de Confiança
cor.test(Base$Carnes, Base$Gasto, conf.level = 0.95)$conf.int # [0.6841370 0.7873068]

# Criação do modelo
model = lm(Gasto ~ Carnes, data = Base)
summary(model) # Intercept  -92.581, Determinação 60.228
# A cada kilograma a mais de carne é esperado que o gasto final aumente em R$60,22
# O aumento da quantidade de carne explica 54.76% da variação dos Gastos

# Reta de regressão no modelo
plot(Base$Carnes, Base$Gasto)
abline(lm(Gasto ~ Carnes, data = Base))

# Análise de resíduos
plot(fitted(model), rstandard(model)) # O gráfico de resíduos está aleatório mostrando que essa é uma relação linear
abline(0, 0)
anova(model)

# Análise do Sexo
library(ggplot2)
ggplot(Base, aes(x = Carnes, y = Gasto, color = Sexo)) +
  geom_point() +
  labs(title = "Gasto por carne separado por sexo",
       x = "Carne (Kg)",
       y = "Gasto (R$)") +
  theme_minimal()

male_frame = subset(Base, Sexo == 'M')
female_frame = subset(Base, Sexo == 'F')

# Homem
cor(male_frame$Carnes, male_frame$Gasto) # 0.4687519
cor.test(male_frame$Carnes, male_frame$Gasto, conf.level = 0.95)$conf.int # [0.2480219 0.6432132]
plot(male_frame$Carnes, male_frame$Gasto)
abline(model)

# Criação do modelo
model_ma = lm(Gasto ~ Carnes, data = male_frame)
summary(model_ma) # Apenas 21% da variação dos gastos pode ser explicada pela quantidade de carne comprada por homens


# Mulher
cor(female_frame$Carnes, female_frame$Gasto) # 0.758486
cor.test(female_frame$Carnes, female_frame$Gasto, conf.level = 0.95)$conf.int # [0.6987156 0.8077408]
plot(female_frame$Carnes, female_frame$Gasto)
abline(model)
# A correlação é mais forte quando o sexo é feminino

# Criação do modelo
model_fe = lm(Gasto ~ Carnes, data = female_frame)
summary(model_fe) # Intercept  -95.771, Determinação 61.905
# A cada kilograma que uma mulher compre a mais de carne é esperado que o gasto final aumente em R$61,09
# O aumento da quantidade carne comprado por uma mulher explica 57.53% da variação dos Gastos

# Reta de regressão no modelo
plot(female_frame$Carnes, female_frame$Gasto)
abline(lm(Gasto ~ Carnes, data = female_frame))

# Análise de resíduos
plot(fitted(model_fe), rstandard(model_fe)) # O gráfico de resíduos está aleatório mostrando que essa é uma relação linear
abline(0, 0)
anova(model_fe)
