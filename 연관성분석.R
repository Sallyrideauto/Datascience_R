# 연관성 분석

install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

data(Groceries)
data(package = "arules")
Groceries

inspect(Groceries[1:10])
summary(Groceries)
sort(itemFrequency(Groceries, type = "absolute"), decreasing = T)
