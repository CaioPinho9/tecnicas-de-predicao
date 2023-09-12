#Dados
x1=c(10,8,13,9,11,14,6,4,12,7,5)
y1=c(8.00,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68)

x2=c(10,8,13,9,11,14,6,4,12,7,5)
y2=c(9.14,8.14,8.74,8.77,9.26,8.10,6.13,3.10,9.13,7.26,4.74)

x3=c(10,8,13,9,11,14,6,4,12,7,5)
y3=c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73)

plot(x1, y1)

plot(x2, y2)

plot(x3, y3)

modelo1 = lm(y1 ~ x1)
summary(modelo1)$r.squared

modelo2 = lm(y2 ~ x2)
summary(modelo2)$r.squared

modelo3 = lm(y3 ~ x3)
summary(modelo3)$r.squared

plot(fitted(modelo1), rstandard(modelo1))
abline(0,0)

plot(fitted(modelo2), rstandard(modelo2))
abline(0,0)

plot(fitted(modelo3), rstandard(modelo3))
abline(0,0)