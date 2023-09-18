library(data.table)
Base = fread(input = paste0("carros.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(Base)

plot(Base)
cor(Base$tempo, Base$valor)

m1 = lm(valor ~ tempo, data=Base)
summary(m1)
