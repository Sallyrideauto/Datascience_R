# 데이터 가져오기
product <- read.csv("/Users/sallyride/Rwork/Part-Ⅳ/product.csv", header = TRUE)
str(product)

# 독립변수와 종속변수 생성
y = product$제품_만족도 # 종속변수
x = product$제품_적절성 # 독립변수
df <- data.frame(x, y)

# 단순 선형회귀 모델 생성
result.lm <- lm(formula = y ~ x, data = df)
result.lm # 회귀분석의 절편과 기울기

# 모델의 적합값과 잔차 보기
names(result.lm)  # 회귀분석의 결과변수 목록 보기
fitted.values(result.lm) [1:2]  # 적합값 2개 원소 보기
head(df, 1) # 관측값 보기
# 회귀방정식을 적용하여 모델의 적합값 계산
# 회귀방정식 Y = 절편 + 기울기 * X : 3은 X 변수의 첫 번째 분량
Y = 0.7789 + 0.7393 * 4
Y
# 잔차(오차) 계산
3 - 3.735963 # Y관측값 - Y적합값
# 모델의 잔차 보기
residuals(result.lm)[1:2]
# 모델의 잔차와 회귀방정식에 의한 적합값으로부터 관측값 계산
-0.7359630 + 3.735963

# 선형 회귀분석 모델 시각화
plot(formula = y ~ x, data = product) # x, y 산점도 그리기
result.lm <- lm(formula = y ~ x, data = product)  # 선형 회귀모델 생성
abline(result.lm, col = 'red')  # 회귀선

summary(result.lm)  # 선형 회귀분석 결과 보기

# 다중 회귀분석

# 변수 모델링
y = product$제품_만족도   # 종속변수
x1 = product$제품_친밀도  # 독립변수2
x2 = product$제품_적절성  # 독립변수1
df <- data.frame(x1, x2, y) # 데이터프레임 생성

# 다중 회귀분석
result.lm <- lm(formula = y ~ x1 + x2, data = df)
result.lm   # 절편과 기울기 확인

# 다중 공선성 문제 확인

# 패키지 설치
install.packages("car")   # vif() 함수 제공 패키지 설치
library(car)  # 메모리 로딩

vif(result.lm)  # 분산팽창요인(VIF)

# 다중 회귀분석 결과 보기
summary(result.lm)

# 다중 공선성 문제 해결

# install.packages("car") # 패키지 설치 
library(car)  # 메모리 로딩
data(iris)    # 데이터 로딩

# iris 데이터셋으로 다중 회귀분석
model <- lm(formula = Sepal.Length ~ 
              Sepal.Width + Petal.Length, data = iris)
vif(model)
sqrt(vif(model)) > 2 # root(VIF)가 2 이상인 것은 다중 공선성 문제 의심
cor(iris[ , -5])  # 변수간의 상관계수 보기(Species 제외)

# 회귀모델 생성

# 학습 데이터와 검정 데이터 표본 추출
x <- sample(1:nrow(iris), 0.7 * nrow(iris))   # 전체 중 70만 추출
train <- iris[x, ]  # 학습데이터 선정
test <- iris[-x, ]  # 검정데이터 선정

# 변수 제거 및 다중 회귀분석
model <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data = train)
model
summary(model)

model # 회귀방정식을 위한 절편과 기울기 보기
head(train, 1) # 회귀방정식 도출
Y = 1.9057 + 0.6762 * 3 + 0.4901 * 5.2  # 다중 회귀방정식 적용
Y
6.7 - Y

# 검정데이터의 독립변수를 이용한 예측치 생성
pred <- predict(model, test)  # x 변수만 test에서 찾아서 값 예측 생성
pred # test 데이터셋의 y 예측치(회귀방정식 적용)

cor(pred, test$Sepal.Length)  # 상관계수를 이용한 회귀모델 평가

# 회귀분석의 기본 가정 충족으로 회귀분석 수행

formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width # 변수 모델링
model <- lm(formula = formula, data = iris) # 회귀모델 생성
model

# 잔차(오차) 분석
# 독립성 검정 - 더빈 왓슨 값으로 확인
install.packages('lmtest')
library(lmtest)
dwtest(model) # 더빈 왓슨 값 : DW = 2.0604(1~3)
# 등분산성 검정 - 잔차와 적합값의 분포
plot(model, which = 1)
# 잔차의 정규성 검정
attributes(model) # model 결과변수의 속성 보기
res <- residuals(model) # 회귀모델에서 잔차 추출
shapiro.test(res) # 정규성 검정
par(mfrow = c(1,2))
hist(res, freq = F) # 히스토그램의 정규분포 시각화
qqnorm(res) # qq 플롯으로 정규분포 시각화

# 다중 공선성 검사
library(car)
sqrt(vif(model)) > 2 # TRUE

# 회귀모델 생성과 평가
formula = Sepal.Length ~ Sepal.Width + Petal.Length # 변수 모델링
model <- lm(formula = formula, data = iris) # 회귀모델 생성
summary(model) # 모델 평가

# 로지스틱 회귀분석

# 날씨 관련 요인 변수로 비(rain) 유무 예측

weather = read.csv("/Users/sallyride/Rwork/Part-Ⅳ/weather.csv", stringsAsFactors = F)
dim(weather)
head(weather)
str(weather)

# 변수 선택과 더미 변수 생성
# chr 칼럼, Date, RainToday 칼럼 제거
weather_df <- weather[ , c(-1, -6, -8, -14)]
str(weather_df)
# RainTomorrow 칼럼 -> 로지스틱 회귀분석 결과(0, 1)에 맞게 더미 변수 생성
weather_df$RainTomorrow[weather_df$RainTomorrow == 'Yes'] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow == 'No'] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)

# 학습데이터와 검정데이터 생성(7:3 비율)
idx <- sample(1:nrow(weather_df), nrow(weather_df) * 0.7)
train <- weather_df[idx, ]
test <- weather_df[-idx, ]

# 로지스틱 회귀모델 생성
weather_model <- glm(RainTomorrow ~ ., data = train, family = 'binomial')
weather_model
summary(weather_model)

# 로지스틱 회귀모델 예측치 생성
pred <- predict(weather_model, newdata = test, type = "response")
pred # 1에 가까울수록 비가 올 확률이 높음
# 시그모이드 함수 : 0.5 기준 -> 비 유무 판단
result_pred <- ifelse(pred >= 0.5, 1, 0)
result_pred
table(result_pred)

# 모델 평가 - 분류정확도 계산
table(result_pred, test$RainTomorrow)

# ROC Curve를 이용한 모델 평가
install.packages("ROCR")  # Receiver Operating Characterisic 패키지 설치
library(ROCR)
# ROCR 패키지 제공 함수 : prediction() -> performance()
pr <- prediction(pred, test$RainTomorrow)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)