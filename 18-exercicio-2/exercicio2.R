# Caio Broering Pinho
# Igor Zimmer Gonçalves
# Gustavo da Rocha Pereira

# CARREGAR O PACOTE
library(data.table)
# LEITURA DA BASE
base = fread(
  input = paste0("car_base.csv"),
  header = T,
  na.strings = "NA",
  data.table = FALSE,
  dec = ","
)
# Convertendo para valores numéricos
base$price = as.numeric(base$price)
base$carwidth = as.numeric(base$carwidth)

# Classificação das variáveis qualitativas
base$drivewheel = as.factor(base$drivewheel)

# Removendo os valores "4wd" da base, pois tem poucas observações
base = base[base$drivewheel != "4wd",]

# Gráfico do efeito da variável quantitativa
library(ggplot2)
ggplot(data = base, aes(x = carwidth, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Grafico do efeito da largura do carro",
       x = "carwidth",
       y = "price")

# Gráfico do efeito da variável quantitativa
library(ggplot2)
ggplot(data = base, aes(x = drivewheel, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Grafico do efeito da tração",
       x = "drivewheel",
       y = "price")

# Modelo
modelo = lm(price ~ carwidth * drivewheel, data = base)
summary(modelo)

# Com tração dianteira, para cada polegada de largura do carro é esperado que
# o preço aumente em 1710 dólares.

# Com tração traseira, para cada polegada de largura do carro é esperado que
# o preço aumente em 2530 dólares.


library(car)
Anova(modelo)
# Como o p-valor da interação entre largura e tração é menor que 5% ele é significante
# Como o coeficiente da interação entre os dois é positivo, significa que 
# quanto maior a largura maior será a diferença entre as variaveis qualitativas

# Gráfico da interação
ggplot(data = base, aes(x = carwidth, y = price, color = drivewheel)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Grafico da Interacao",
       x = "carwidth",
       y = "price")

# Predição para 70 polegadas e tração dianteira
novo = data.frame(carwidth = 70, drivewheel = "fwd")
predict(modelo, novo, interval = "confidence")
# A média do preço da população de carros com esses parametros deve estar entre 14890.39 e 20486.77 doláres

predict(modelo, novo, interval = "predict")
# O preço de um carros com esses parametros deve estar entre 8013.44 e 27371.72 doláres
