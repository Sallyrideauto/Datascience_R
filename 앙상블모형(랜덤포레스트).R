# 랜덤 포레스트

# 랜덤 포레스트 기본 모델 생성

# 패키지 설치 및 데이터셋 가져오기
install.packages('randomForest')
library(randomForest)   # randomForest() 함수 제공
data(iris)

# 랜덤 포레스트 모델 생성
model = randomForest(Species ~ ., data = iris)  # 모델 생성
model

# --------------------------------------------------------------------------

# 파라미터 조정 : 트리 개수 300개, 변수 개수 4개 지정
model2 = randomForest(formula = Species ~ ., data = iris, 
                      ntree = 300, mtry = 4, 
                      na.action = na.omit)
model2

# 중요 변수를 생성하여 랜덤포레스트 모델 생성
model3 = randomForest(Species ~ ., data = iris, 
                      importance = T, na.action = na.omit)
importance(model3)  # 중요 변수 보기
varImpPlot(model3)  # 중요 변수 시각화

# --------------------------------------------------------------------------

# 최적의 파라미터(ntree, mtry) 찾기

# 속성값 생성
ntree <- c(400, 500, 600) # 트리의 개수 지정
mtry <- c(2:4)  # 변수의 개수 지정
param <- data.frame(n = ntree, m = mtry)
param   # n m 

# 이중 for 함수를 이용하여 모델 생성
for(i in param$n){    # ntree 칼럼 : 트리수 지정
  cat('ntree =', i, '\n')
  for(j in param$m){  # mtry 칼럼 : 변수 수 지정
    cat('mtry =', j, '\n')
    model_iris <- randomForest(Species ~ ., data = iris, 
                               ntree = i, mtry = j, 
                               na.action = na.omit)
    print(model_iris)
  }  # 내부 for.j의 끝
} # 외부 for.i의 끝

# --------------------------------------------------------------------------

# 앙상블기법 교재 실습 : 배깅(Bagging)

library(party)
library(caret)

# data sampling
data1 <- iris[sample(1:nrow(iris), replace = T), ]
data2 <- iris[sample(1:nrow(iris), replace = T), ]
data3 <- iris[sample(1:nrow(iris), replace = T), ]
data4 <- iris[sample(1:nrow(iris), replace = T), ]
data5 <- iris[sample(1:nrow(iris), replace = T), ]

# 예측모형 생성
citree1 <- ctree(Species ~ ., data1)
citree2 <- ctree(Species ~ ., data2)
citree3 <- ctree(Species ~ ., data3)
citree4 <- ctree(Species ~ ., data4)
citree5 <- ctree(Species ~ ., data5)

# 예측수행
predicted1 <- predict(citree1, iris)
predicted2 <- predict(citree2, iris)
predicted3 <- predict(citree3, iris)
predicted4 <- predict(citree4, iris)
predicted5 <- predict(citree5, iris)

# 예측모형을 결합하여 새로운 예측모형 생성
newmodel <- data.frame(Species = iris$Species, 
                       predicted1, predicted2, predicted3, 
                       predicted4, predicted5)
head(newmodel)
newmodel

# 최종 모형으로 통합
funcValue <- function(x) {
  result <- NULL
  for(i in 1:nrow(x)){
    xtab <- table(t(x[i, ]))
    rvalue <- names(sort(xtab, decreasing = T)[1])
    result <- c(result, rvalue)
  }
  return(result)
}
newmodel

# 최종 모형의 2번째에서 6번째를 통합하여 최종 결과 생성
newmodel$result <- funcValue(newmodel[, 2:6])
newmodel$result

# 최종결과 비교
table(newmodel$result, newmodel$Species)

# --------------------------------------------------------------------------

# 앙상블기법 교재 실습 : Random Forest

head(iris)

# 70% training 데이터, 30% testing 데이터로 구분
idx <- sample(2, nrow(iris), replace = T, prob = c(0.7, 0.3))

trData <- iris[idx == 1, ]
nrow(trData)
teData <- iris[idx == 2, ]
nrow(teData)

library(randomForest)

# 랜덤포레스트 실행(100개의 트리를 다양한 방법(proximity = T)으로 생성)
RFmodel <- randomForest(Species ~ ., data = trData, ntree = 100, proximity = T)
RFmodel

# 시각화
plot(RFmodel, main = "RamdomForest Model of iris")

# 모델에 사용된 변수 중 중요한 것 확인
importance(RFmodel)

# 중요한 것 시각화
varImpPlot(RFmodel)

# 실제값과 예측값 비교
table(trData$Species, predict(RFmodel))

# 테스트데이터로 예측
pred <- predict(RFmodel, newdata = teData)

# 실제값과 예측값 비교
table(teData$Species, pred)

# 시각화
plot(margin(RFmodel, teData$Species))

# --------------------------------------------------------------------------

# 앙상블기법 교재 실습 : 배깅(Bagging) 2

# adabag 패키지의 bagging() 함수 활용
install.packages("adabag")
library(adabag)

data(iris)
iris.bagging <- bagging(Species~., data = iris, mfinal = 10)
# mfinal = 반복수 또는 트리의 수(디폴트 = 100)
iris.bagging$importance   # 변수의 상대적인 중요도

plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

pred <- predict(iris.bagging, newdata = iris)
table(pred$class, iris[, 5])

# --------------------------------------------------------------------------

# 앙상블기법 교재 실습 : 부스팅(boosting)

library(adabag)
data(iris)

boo.adabag <- boosting(Species ~ ., data = iris, boos = TRUE, mfinal = 10)
boo.adabag$importance

plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])

pred <- predict(boo.adabag, newdata = iris)
tb <- table(pred$class, iris[, 5])
tb

error.rpart <- 1 - (sum(diag(tb)) / sum(tb))
error.rpart

# iris 데이터셋 중 setosa를 제외한 versicolor와 virginica 자료만으로 분석 수행

install.packages("ada")
library(ada)
data(iris)

iris[iris$Species != "setosa", ] -> iris        # setosa 50개 자료 제외
n <- dim(iris)[1]

# 총 100개의 자료를 60개의 훈련용 자료(training data)와 검증용 자료(testing data)로 나누었다.
trind <- sample(1:n, floor(.6*n), FALSE)
teind <- setdiff(1:n, trind)    # set difference(차집합)
iris[, 5] <- as.factor((levels(iris[, 5])[2:3])[as.numeric(iris[, 5])-1]) 
# 0, 1, 2가 차례대로 50개(총 150개)

# 