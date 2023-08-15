library(data.table)
Base <- fread(input = paste0("ceramica.csv"), 
              header = T, na.strings = "NA", 
              data.table = FALSE, dec=",")

#Gráfico
plot(Base)

# Coeficiente de correlação
cor(Base)
correlacao <- cor(Base)

# Matriz do correlação
library(ggcorrplot)
ggcorrplot(correlacao)
ggcorrplot(correlacao, hc.order = TRUE, type = "lower",
           lab = TRUE)
