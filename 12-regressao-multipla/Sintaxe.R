library(data.table)
Base <- fread(input = paste0("risco.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(Base)

