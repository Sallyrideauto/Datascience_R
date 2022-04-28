# 원 데이터셋과 분할된 데이터셋 각각에서 목적변수 속성들의 빈도 일관성 여부 확인

# doBy 패키지의 sampleBy 함수를 사용
install.packages("doBy")
library(doBy)

iris_train <- sampleBy(~Species, frac=0.7, data=iris)
dim(iris_train)
table(iris_train$Species)

iris_train

# caret 패키지의 createDataPartition 함수를 사용
install.packages("caret")
library(caret)

train.idx <- createDataPartition(iris$Species, p=0.7, list=F)
iris_train <- iris[train.idx, ]
iris_test <- iris[-train.idx, ]

dim(iris_train)
dim(iris_test)
table(iris_train$Species)
table(iris_test$Species)

# 나이브 베이즈 모형 사용

install.packages("e1071")
library(e1071)  # 나이브 베이즈 기법 적용하기 위한 e1071 패키지 로드

naive.result <- naiveBayes(iris_train, iris_train$Species, laplase = 1) # 나이브 베이즈 적합
naive.pred <- predict(naive.result, iris_test, type = "class")  # 테스트 데이터 평가
table(naive.pred, iris_test$Species)  # 분류 결과 도출
confusionMatrix(naive.pred, iris_test$Species)  # caret 패키지의 confusionMatrix 함수를 이용한 혼동행렬 도출

# 로지스틱 회귀분석 기법 사용

library(nnet) # 다항 로지스틱 회귀를 사용하기 위한 nnet 패키지 로딩

multi.result <- multinom(Species~., iris_train)   # 훈련 데이터 통한 모형 적합
multi.pred <- predict(multi.result, iris_test)     # 테스트 데이터 이용한 평가
table(multi.pred, iris_test$Species)              # 분류 결과 도출

confusionMatrix(multi.pred, iris_test$Species)

# 의사결정 트리

# party 패키지 이용 분류분석
# 의사결정 트리 생성 : ctree() 함수 이용

install.packages("party") # party 패키지 설치
library(party)

# airquality 데이터셋 로딩
install.packages("datasets")
library(datasets)
str(airquality)

formula <- Temp ~ Solar.R + Wind + Ozone  # formula 생성
# formula를 이용한 분류모델 생성
air_ctree <- ctree(formula, data = airquality)
air_ctree

# 분류분석 결과
plot(air_ctree)

# 학습데이터와 검정데이터 샘플링으로 분류분석 수행
set.seed(1234)  # 시드값을 적용 시 랜덤 값이 동일하게 생성됨
idx <- sample(1:nrow(iris), nrow(iris)*0.7)
train <- iris[idx, ] # 학습데이터
test <- iris[-idx, ] # 검정데이터

# formula(공식) 생성
# 종속변수(꽃의 종) ~ 독립변수(나머지 4개 변수)
formula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

# 학습데이터 이용 분류모델 생성
iris_ctree <- ctree(formula, data = train)
iris_ctree

# 분류모델 플로팅
plot(iris_ctree, type = "simple") # 간단한 형식으로 시각화
plot(iris_ctree)  # 의사결정 트리로 결과 플로팅

# 분류모델 평가
# 모델의 예측치 생성과 혼돈 매트릭스 생성
pred <- predict(iris_ctree, test)
table(pred, test$Species)
# 분류 정확도 96%
(14+16+13)/nrow(test)

# K겹 교차 샘플링으로 분류 분석하기

# K겹 교차 검정을 위한 샘플링_3겹, 2회 반복
install.packages("cvTools")
library(cvTools)
cross <- cvFolds(nrow(iris), K = 3, R = 2)

# K겹 교차 검정 데이터 보기
str(cross)  # 구조 보기
cross # 3겹 교차 검정 데이터 보기
length(cross$which)
dim(cross$subsets)
table(cross$which)  # 3겹 빈도수

# K겹 교차 검정 수행
R = 1:2 # 2회 반복
K = 1:3 # 3겹
CNT = 0 # 카운터 변수 -> 1차 테스트
ACC <- numeric()  # 분류정확도 저장 -> 2차 모델 생성

for(r in R){      # 2회
  cat('\n R = ', r, '\n')
  for(k in K){    # 3회
    
    # test 생성
    datas_idx <- cross$subsets[cross$which == k, r]
    test <- iris[datas_idx, ]
    cat('test : ', nrow(test), '\n')
    
    # train 생성
    formula <- Species ~ .
    train <- iris[-datas_idx, ]
    cat('train : ', nrow(train), '\n')
    
    # model 생성
    model <- ctree(Species ~ ., data = train)
  }
}