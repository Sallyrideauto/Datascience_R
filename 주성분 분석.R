# iris 데이터셋을 활용한 PCA 분석 수행

data(iris)
head(iris, 3)

log.ir <- log(iris[, 1:4]) # 연속형 자료에 대한 로그 변환 수행
ir.species <- iris[, 5] 
ir.pca <- prcomp(log.ir, center = TRUE, scale = TRUE) # PCA를 수행하기 전에 변수에 대해 중심화와 표준화를 수행
print(ir.pca)

plot(ir.pca, type = "l")  # plot 함수 : 주성분과 관련된 분산을 그려 줌
summary(ir.pca) # 주성분들의 중요도를 제공
predict(ir.pca, newdata=tail(log.ir, 2))  # 새로운 자료에 대해 그들의 주성분 값(PCs)을 예측
biplot(ir.pca)  # PCA의 결과를 시각화

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(
  ir.pca, obs.scale = 1, var.scale = 1,
  groups = ir.species, ellipse = TRUE, circle = TRUE
)
g <- g + scale_color_discrete(name='')
g <- g + theme(legend.direction='horizontal', legend.position = 'top')
print(g)