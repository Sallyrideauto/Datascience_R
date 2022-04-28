# sPSS파일을 csv파일로 변환
# (1)
install.packages("memisc")
library(memisc)
data <- as.data.set(spss.system.file('drinking_water_example.sav'))
str(data)

# (2)
# R (>= 4.0.0)
install.packages("foreign")
library(foreign)
write.table(read.spss("drinking_water_example.sav"), file="dataout.csv", quote=TRUE, sep=",")
dataout <- read.csv(file="dataout.csv")
str(dataout)
