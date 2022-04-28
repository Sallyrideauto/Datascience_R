# sPSS파일을 csv파일로 변환
# (1)
install.packages("memisc")
library(memisc)
spss1 <- as.data.set(spss.system.file('drinking_water_example.sav'))
str(spss1)

# (2)
# R (>= 4.0.0)
install.packages("foreign")
library(foreign)
write.table(read.spss("drinking_water_example.sav"), file="dataout.csv", quote=TRUE, sep=",")
spss2 <- read.csv(file="dataout.csv")
str(spss2)


# (3)
install.packages("haven")
library(haven)

spss3 <- read_sav("drinking_water_example.sav", encoding='UTF-8')
str(spss3)

# (4) 
install.packages("Hmisc")
library(Hmisc)

spss4 <- spss.get("drinking_water_example.sav", reencode=NA)
str(spss4)





