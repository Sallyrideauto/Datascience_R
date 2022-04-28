# 머신러닝기반 데이터 분석 평가_이혜수

# 평가문항 2

library(rpart)
library(rpart.plot)
data(iris)

rpart_model <- rpart(Species ~ ., data = iris)
rpart_model

rpart.plot(rpart_model)

# 평가문항 3

# 학습데이터와 검정데이터 샘플링
data(iris)
idx <- sample(1 : nrow(iris), nrow(iris) * 0.7)
train <- iris[idx, ]    # 학습 데이터
test <- iris[-idx, ]    # 검정 데이터

# formula(공식) 생성
# 종속변수(꽃의 종) ~ 독립변수(나머지 4개 변수)
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# 테스트 데이터를 이용한 분류
iris_ctree <- ctree(formula, data = train)    # 분류모델 생성
iris_ctree

plot(iris_ctree, type = "simple")   # 시각화

# 평가문항 4

data(iris)

# 유클리드 거리 계산
idist <- dist(iris[1:4])

# 계층형 군집 분석(클러스터링)
hc <- hclust(idist)

# 덴드로그램 시각화
plot(hc, hang = -1)
rect.hclust(hc, k = 3, border = "red") # 3개 군집 수 확인

# 평가문항 5

library(ggplot2)
data(iris)
t <- sample(1:nrow(iris))   # 데이터 샘플링
test <- iris[t, ]           # 표본으로 검정 데이터 생성
dim(test)
head(test)                  # 검정 데이터

# 군집을 위해서 필요한 변수 추출
myiris <- test[c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]   # Species 칼럼 제외
head(myiris)

# 계층적 군집 분석(탐색적 분석)
result <- hclust(dist(myiris), method = "average")    # 평균 거리 이용
result
plot(result, hang = -1)     # -1 이하 값 제거

# 비계층적 군집 분석
result2 <- kmeans(myiris, 3)
names(result2)
result2$cluster     # 각 케이스에 대한 소속 군집 수(1, 2, 3) 생성
# 원형 데이터에 군집 추가
myiris$cluster <- result2$cluster
head(myiris)

# 변수 간의 상관계수 보기
cor(myiris[, -5], method = "pearson")   # 상관계수 보기
plot(myiris[, -5])   # 변수 간 산점도 보기

# 상관계수를 색상으로 시각화
install.packages("mclust")
library(mclust)
library(corrgram)
corrgram(myiris[, -5], upper.panel = panel.conf)  # 수치(상관계수) 추가(위쪽)
corrgram(myiris[, -5], lower.panel = panel.conf)  # 수치(상관계수) 추가(아래쪽)
