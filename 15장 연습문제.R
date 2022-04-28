# 연습문제 15-3
# mpg 데이터셋을 대상으로 7:3 비율로 학습데이터와 검정데이터로 각각 샘플링한 후
# 단계별로 분류분석을 수행하시오.

# 학습데이터와 검정데이터 샘플링
library(neuralnet)
data(mpg)   # 데이터셋 로드
idx = sample(1 : nrow(mpg), 0.7 * nrow(mpg))  # 7:3 비율로 나눌 색인 생성
training_mpg = mpg[idx, ]   # 학습 데이터(train data)
testing_mpg = mpg[-idx, ]   # 검정 데이터(test data)
dim(training_mpg)
dim(testing_mpg)

# formula(공식) 생성 : 수치형으로 칼럼 생성
training_mpg$cty2[training_mpg$cty == 'displ'] <- 1
training_mpg$cty2[training_mpg$cty == 'cyl'] <- 2
training_mpg$cty2[training_mpg$cty == 'year'] <- 3
training_mpg$cty <- NULL    # 기존 칼럼 제거
head(training_mpg)