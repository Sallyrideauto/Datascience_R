# LHS_분석용 데이터 구축 평가답안

# 문제 2
# MASS 패키지에 있는 Animals 데이터셋에 대해 R의 기본 함수를 이용하여
# body 칼럼을 대상으로 다음의 기술통계량을 구하시오.

library(MASS)
data(Animals)
head(Animals)

str(Animals)        # Animals 데이터셋 구조 보기
summary(Animals)    # 요약통계량 
mean(Animals$body)  # 평균
sqrt(var(Animals$body, na.rm = T))  # 표준편차
table(Animals$body) # Animals 데이터셋의 빈도수 

# 문제 3
# 우리나라 전체 중학교 2학년 여학생 평균 키가 148.5로 알려진 상태에서 
# A 중학교 2학년 전체 500명을 대상으로 10%인 50명을 표본으로 선정하여 
#표본평균 신장을 계산하고 모집단의 평균과 차이가 있는지를 단계별로 분석을 수행하여 점검하시오.

# 데이터셋 가져오기
setwd("/Users/sallyride/Rwork/Part-Ⅲ")
stheight <- read.csv("student_height2.csv", header = TRUE)
height <- stheight$height

summary(height)    # 데이터의 요약기술통계량 계산
plot(height)      # 결측치 발견
shapiro.test(height)  # 정규성 검정

# 귀무가설 : 평균 신장에 차이가 없다.
# 대립가설 : 평균 신장에 차이가 있다.
# 가설 검정
t.test(height, mu = 149.4)
t.test(height, mu = 149.4, alter = "two.side", conf.level = 0.95)
t.test(height, mu = 149.4, alter = "greater", conf.level = 0.95)
# 결론 : 평균신장에 차이가 없다.

# 문제 4

exam_data = data.frame(
  name = c("Anastasia", "Dima", "Katherine", "James", "Emily", "Michael", "Matthew", "Laura", "Kevin", "Jonas", "Kim", "Lee"),
  score = c(12.5, 9, 16.5, 12, 9, 20, 14.5, 13.5, 8, 19, 15, 10),
  attempts = c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1, 1, 3),
  qualify = c("yes", "no", "yes", "no", "no", "yes", "yes", "no", "no", "yes", "yes", "no"),
  country = c("RUS", "CHN", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "KOR", "KOR")
)
print(exam_data)  # 전체 데이터프레임
result <- subset(exam_data, select = -c(qualify))           # Qualify 항목을 제외하고 출력
print(result)
result2 <- exam_data[-c(2, 10),]                              # Dima와 Jonas를 제외하고 출력 
print(result2)
result3 <- data.frame(exam_data$name, exam_data$country)    # 이름과 국적만 출력 
print(result3)