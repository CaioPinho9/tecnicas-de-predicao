library(data.table)
Base = fread(input = paste0("risco.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(Base)

plot(Base)
cor(Base)

modelo = lm(Risco ~ Idade + Pressao, data = Base)
summary(modelo)
library(car)
avPlots(modelo)
plot(modelo)