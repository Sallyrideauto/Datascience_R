# 요인 분석(Factor Analysis)

# 공통요인으로 변수 정제

# 과목 변수 생성 : 6개 과목의 점수
# s1 : 자연과학 s2 : 물리화학
# s3 : 인문사회 s4 : 신문방송
# s5 : 응용수학 s6 : 추론통계
# name : 각 과목의 문제에 대한 이름
s1 <- c(1,2,1,2,3,4,2,3,4,5)
s2 <- c(1,3,1,2,3,4,2,4,3,4)
s3 <- c(2,3,2,3,2,3,5,3,4,2)
s4 <- c(2,4,2,3,2,3,5,3,4,1)
s5 <- c(4,5,4,5,2,1,5,2,4,3)
s6 <- c(4,3,4,4,2,1,5,2,4,2)
name <- 1:10

# 과목 데이터프레임 생성
subject <- data.frame(s1, s2, s3, s4, s5, s6)
str(subject)

# 주성분 분석으로 요인 수 알아보기
pc <- prcomp(subject) # 주성분 분석 수행 함수
summary(pc) # 요약통계량

# 고유값으로 요인 수 분석
en <- eigen(cor(subject))  # $values : 고유값, $vectors : 고유벡터
names(en)   # "values" "vectors"
en$values
en$vectors
# 고유값을 이용한 시각화
plot(en$values, type = "o")

# 변수 간의 상관관계 분석과 요인 분석
# 상관관계 분석 : 변수 간의 상관성으로 공통요인 추출
cor(subject)

# 요인 분석 : 요인회전법 적용(Varimax 회전법이 기본)
# 주성분 분석의 가정에 의해서 2개 요인으로 분석
result <- factanal(subject, factors = 2, rotation = "varimax")
result  # p-value is 0.0232 < 0.05
# 고유값으로 가정한 3개 요인으로 분석
result <- factanal(subject,
                   factors = 3, # 요인 개수 지정
                   rotation = "varimax", # 회전 개수 지정
                   scores = "regression") # 요인점수 계산 방법
result
# 다양한 방법으로 요인적재량 보기
attributes(result)  # 결과변수 속성 보기
result$loadings   # 기본 요인적재량 보기
print(result, digits = 2, cutoff = 0.5)   # 요인부하량 0.5 이상, 소수점 2자리 표기
# 모든 요인적재량 보기 : 감추어진 요인적재량 보기
print(result$loadings, cutoff = 0) # display every loadings

# 요인점수를 이용한 요인적재량 시각화
# Factor1과 Factor2 요인적재량 시각화
plot(result$scores[ , c(1:2)], main = "Factor1과 Factor2 요인점수 행렬")
# 산점도에 레이블 표시(문항 이름 : name)
text(result$scores[ , 1], result$scores[ , 2], labels = name, cex = 0.7, pos = 3, col = "blue")
# 요인적재량 추가
points(result$loadings[ , c(1:2)], pch = 19, col = "red")
# 요인적재량의 레이블 표시
text(result$loadings[ , 1], result$loadings[ , 2],
     labels = rownames(result$loadings), cex = 0.8, pos = 3, col = "red")
# Factot1과 Factor3 요인적재량 시각화
plot(result$scores[ , c(1:3)], main = "Factor1과 Factor3 요인점수 행렬")
# 산점도에 레이블 표시(문항 이름 : name)
text(result$scores[ , 1], result$scores[ , 3],
     labels = name, cex = 0.7, pos = 3, col = "blue")
# 요인적재량 추가
points(result$loadings[ , c(1,3)], pch = 19, col = "red")
# 요인적재량의 레이블 표시
text(result$loadings[ , 1], result$loadings[ , 3],
     labels = rownames(result$loadings), cex = 0.8, pos = 3, col = "red")

# 3차원 산점도로 요인적재량 시각화
# 3차원 산점도 패키지 설치 및 로딩
install.packages("scatterplot3d")
library(scatterplot3d)
# 요인점수별 분류 및 3차원 프레임 생성
Factor1 <- result$scores[ , 1]
Factor2 <- result$scores[ , 2]
Factor3 <- result$scores[ , 3]
# scatterplot3d(밑변, 오른쪽변, 왼쪽변, type)
# type = 'p' : 기본 산점도 표시
d3 <- scatterplot3d(Factor1, Factor2, Factor3, type = 'p')
# 요인적재량 표시
loadings1 <- result$loadings[ , 1]
loadings2 <- result$loadings[ , 2]
loadings3 <- result$loadings[ , 3]
d3$points3d(loadings1, loadings2, loadings3,
            bg = 'red', pch = 21, cex = 2, type = 'h')

# 요인별 변수 묶기
# 요인별 과목 변수 이용 데이터프레임 생성
app <- data.frame(subject$s5, subject$s6) # 응용과학
soc <- data.frame(subject$s3, subject$s4) # 사회과학
nat <- data.frame(subject$s1, subject$s2) # 자연과학
# 요인별 산술평균 계산
app_science <- round((app$subject.s5 + app$subject.s6) / ncol(app), 2)
soc_science <- round((soc$subject.s3 + soc$subject.s4) / ncol(soc), 2)
nat_science <- round((nat$subject.s1 + nat$subject.s2) / ncol(nat), 2)
# 상관관계 분석
subject_factor_df <- data.frame(app_science, soc_science, nat_science)
cor(subject_factor_df)

# 잘못 분류된 요인 제거로 변수 정제

# 요인 분석에 사용될 데이터셋 가져오기
# SPSS 데이터셋 가져오기
install.packages('memisc')  # SPSS 데이터를 가져오기 위해 패키지 설치
library(memisc) # 패키지 로딩
setwd("/Users/sallyride/Rwork/Part-Ⅲ")  # 경로 지정
# 파일 가져오기
data.spss <- as.data.set(spss.system.file('drinking_water.sav'))
data.spss[1:11]   # 데이터셋 보기(첫 번째 칼럼부터 11개 칼럼만 보기)
# 데이터프레임으로 변경
drinking_water <- data.spss[1:11] # 11개 변수 선택
drinking_water_df <- as.data.frame(data.spss[1:11]) # 데이터프레임 생성
str(drinking_water_df)  # 데이터셋 구조 보기
# 요인 수를 3개로 지정하여 요인 분석 수행
result2 <- factanal(drinking_water_df, factors = 3, rotation = "varimax")
result2

# 요인별 변수 묶기
# q4를 제외하고 데이터프레임 생성
dw_df <- drinking_water_df[-4]
str(dw_df)
dim(dw_df)
# 요인에 속하는 입력 변수별 데이터프레임 구성
s <- data.frame(dw_df$Q8, dw_df$Q9, dw_df$Q10, dw_df$Q11)   # 제품만족도 저장 데이터프레임
c <- data.frame(dw_df$Q1, dw_df$Q2, dw_df$Q3)               # 제품친밀도 저장 데이터프레임
p <- data.frame(dw_df$Q5, dw_df$Q6, dw_df$Q7)               # 제품적절성 저장 데이터프레임
# 요인별 산술평균 계산
satisfaction <- round((s$dw_df.Q8 + s$dw_df.Q9 + s$dw_df.Q10 + s$dw_df.Q11) / ncol(s), 2)
closeness <- round((c$dw_df.Q1 + c$dw_df.Q2 + c$dw_df.Q3) / ncol(c), 2)
pertinence <- round((p$dw_df.Q5 + p$dw_df.Q6 + p$dw_df.Q7) / ncol(p), 2)
# 상관관계 분석
drinking_water_factor_df <- data.frame(satisfaction, closeness, pertinence)
colnames(drinking_water_factor_df) <- c("제품만족도", "제품친밀도", "제품적절성")
cor(drinking_water_factor_df)