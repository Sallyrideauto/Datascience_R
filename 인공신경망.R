# 간단한 인공신경망 모델 생성

# 패키지 설치
install.packages("nnet")  # 인공신경망 모델 생성 패키지
library(nnet)

# 데이터셋 생성
df = data.frame(          # 데이터프레임 생성 : 입력 변수 x와 출력 변수 y
  x2 = c(1:6), 
  x1 = c(6:1), 
  y = factor(c('no', 'no', 'no', 'yes', 'yes', 'yes'))
)
str(df) # 데이터 구조 보기

# 인공신경망 모델 생성
model_net = nnet(y ~ ., df, size = 1)  # size는 은닉층 수

# 모델 결과 변수 보기
model_net

# 가중치(weights) 보기
summary(model_net)

# 분류모델의 적합값 보기
model_net$fitted.values

# 분류모델의 예측치 생성과 분류 정확도
p <- predict(model_net, df, type = "class")   # 분류모델의 예측치 생성
table(p, df$y)  # 혼돈 matrix 생성

# ---------------------------------------------------------------------------

# iris 데이터셋을 이용한 인공신경망 모델 생성

# 데이터셋 생성
data(iris)  # 데이터셋 로드
idx = sample(1:nrow(iris), 0.7*nrow(iris))  # 7:3 비율로 나눌 색인 생성
training = iris[idx, ]  # 학습데이터(train data)
testing = iris[-idx, ]  # 검정데이터(test data)
nrow(training)
nrow(testing)

# 인공신경망 모델(은닉층 1개와 은닉층 3개) 생성
model_net_iris1 = nnet(Species ~ ., training, size = 1) # 은닉층 1개
model_net_iris1
model_net_iris3 = nnet(Species ~ ., training, size = 3) # 은닉층 3개
model_net_iris3

# 가중치 네트워크 보기
summary(model_net_iris1) # 은닉층 1개 신경망 모델
summary(model_net_iris3) # 은닉층 3개 신경망 모델

# 분류모델 평가
table(predict(model_net_iris1, testing, type = "class"), testing$Species)
(14 + 17 + 14) / nrow(testing)
table(predict(model_net_iris3, testing, type = "class"), testing$Species)
(14 + 16 + 12) / nrow(testing)

# -------------------------------------------------------------------------------

# neuralnet 패키지를 이용한 인공신경망 모델 생성

# 패키지 설치
install.packages("neuralnet")   # 인공신경망 모델 생성을 위한 패키지
library(neuralnet)

# 데이터셋 생성
data(iris)  # 데이터셋 로드
idx = sample(1:nrow(iris), 0.7*nrow(iris))
# 분류 모델을 생성하기 위해서 학습데이터(training_iris)를 70%, 검정데이터(testing_iris)를 30% 비율로 나눈다.
training_iris = iris[idx, ]  # 학습데이터(train data)
testing_iris = iris[-idx, ]  # 검정데이터(test data)
dim(training_iris)
dim(testing_iris)

# 수치형으로 칼럼 생성
training_iris$Species2[training_iris$Species == 'setosa'] <- 1
training_iris$Species2[training_iris$Species == 'versicolor'] <- 2
training_iris$Species2[training_iris$Species == 'virginica'] <- 3
training_iris$Species <- NULL # 기존 칼럼 제거
head(training_iris)
testing_iris$Species2[testing_iris$Species == 'setosa'] <- 1
testing_iris$Species2[testing_iris$Species == 'versicolor'] <- 2
testing_iris$Species2[testing_iris$Species == 'virginica'] <- 3
testing_iris$Species <- NULL # 기존 칼럼 제거
head(testing_iris)

# 데이터 정규화
normal <- function(x) {     # 정규화 함수 정의
  return((x - min(x)) / (max(x) - min(x)))
}
# 정규화 함수를 이용하여 학습데이터/검정데이터 정규화
training_nor <- as.data.frame(lapply(training_iris, normal))
summary(training_nor)   # 0~1 확인
testing_nor <- as.data.frame(lapply(testing_iris, normal))
summary(testing_nor)   # 0~1 확인

# 인공신경망 모델 생성 : 은닉 노드 1개
model_net = neuralnet(Species2 ~ Sepal.Length + Sepal.Width + 
                        Petal.Length + Petal.Width,
                      data = training_nor, hidden = 1)
model_net
plot(model_net)   # 인공신경망 시각화

# 분류모델 성능 평가
# 모델의 예측치 생성 : compute() 함수 이용
model_result <- compute(model_net, testing_nor[c(1:4)])
model_result$net.result   # 분류 예측값 보기
# 상관관계 분석 : 상관계수로 두 변수 간 선형 관계의 강도 측정
cor(model_result$net.result, testing_nor$Species2)

# 분류모델 성능 향상 : 은닉층 노드 2개 지정, backdrop 속성 적용
# 인공신경망 모델 생성
model_net2 = neuralnet(Species2 ~ Sepal.Length + Sepal.Width + 
                         Petal.Length + Petal.Width, 
                       data = training_nor, hidden = 2, 
                       algorithm = "backdrop", learningrate = 0.01)
# 분류모델 예측치 생성과 평가
model_result <- compute(model_net, testing_nor[c(1:4)])
cor(model_result$net.result, testing_nor$Species2)