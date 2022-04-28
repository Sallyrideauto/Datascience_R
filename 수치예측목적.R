# 수치 예측 목적의 머신러닝 수행하기(Boston 데이터셋)

data(Boston)
head(Boston)

idx <- sample(1:nrow(Boston), size = nrow(Boston) * 0.7, replace = F)
Boston_train <- Boston[idx, ]   # 훈련용 데이터셋 분할
Boston_test <- Boston[-idx, ]   # 평가용 데이터셋 분할
dim(Boston_train) ; dim(Boston_test)
# 훈련용 데이터셋은 354개 행을 가지고 있음(506개 행의 70%)
# 평가용 데이터셋은 152개 행을 가지고 있음(506개 행의 30%)

# 다중회귀분석 기법 사용
lm.fit <- lm(medv ~ ., data = Boston_train)
summary(lm.fit)
lm.fit2 <- step(lm.fit, method = "both")
summary(lm.fit2)

lm.yha2 <- predict(lm.fit2, newdata = Boston_test)  # 평가 데이터 이용, 예측결과 생성
mean((lm.yha2 - Boston_test$medv) ^ 2)  # 예측값과 실제값 간의 평균제곱오차(MSE) 계산

# 의사결정트리 기법 사용

# tree 함수 사용한 의사결정트리 분석
install.packages("tree")
library(tree)

tree.fit <- tree(medv ~ ., data = Boston_train)
summary(tree.fit)

plot(tree.fit)
text(tree.fit, pretty = 0)

tree.yhat <- predict(tree.fit, newdata = Boston_test)   # 평가 데이터 이용, 예측결과 생성
mean((tree.yhat - Boston_test$medv) ^ 2) # 예측값과 실제값 간 평균제곱오차(MSE) 계산 

# rpart 함수 사용한 의사결정트리 분석

library(rpart)

rpart.fit <- rpart(medv ~ ., data = Boston_train)
summary(rpart.fit)

# rpart.plot 패키지를 통한 시각화
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(rpart.fit, digits = 3, type = 0, extra = 1, fallen.leaves = F, cex = 1) # 시각화

rpart.yhat <- predict(rpart.fit, newdata = Boston_test)   # 평가 데이터 이용, 예측결과 생성
mean((rpart.yhat - Boston_test$medv) ^ 2)   # 예측값과 실제값 간 평균제곱오차(MSE) 계산
