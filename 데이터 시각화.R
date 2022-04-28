# 데이터 시각화 

# 세로 막대 차트
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
# 8개의 벡터에 칼럼명 지정
names(chart_data) <- c("2018 1분기", "2019 1분기", 
                       "2018 2분기", "2019 2분기", 
                       "2018 3분기", "2019 3분기", 
                       "2018 4분기", "2019 4분기")
str(chart_data) # 자료구조 보기
chart_data #벡터 자료 보기 

# 세로 막대 차트 그리기
par(family = 'NanumGothic')
barplot(chart_data, ylim = c(0, 600), 
        ylab = "매출액(단위 : 만원)", 
        xlab = "년도별 분기 현황", 
        col = rainbow(8), 
        main = "2018년도 vs 2019년도 매출현황 비교")

# 가로 막대 차트 그리기
barplot(chart_data, xlim = c(0, 600), horiz = T, 
        ylab = "매출액(단위 : 만원)", 
        xlab = "년도별 분기 현황", 
        space = 1, cex.names = 0.8, 
        main = "2018년도 vs 2019년도 매출현황 비교", 
        col = rep(c(2, 4), 4))

# 누적 막대 차트

data(VADeaths)    # 메모리로 데이터 가져오기
VADeaths
str(VADeaths)     # VADeaths 데이터셋 구조 보기 
mode(VADeaths)

# 개별 차트와 누적 차트 그리기
par(mfrow = c(1, 2))    # 1행 2열 그래프 보기
# 개별 차트 그리기
barplot(VADeaths, beside = T, col = rainbow(5), 
        main = "미국 버지니아주 하위계층 사망 비율")
# 범례 표시 : x 좌표 19, y 좌표 71 위치에 무지개색으로 5개의 범례를 표시한다.
legend(19, 71, c("50-54", "55-59", "60-64", "65-69", "70-74"), 
       cex = 0.8, fill = rainbow(5))

# 누적 차트 그리기
barplot(VADeaths, beside = F, col = rainbow(5))
# 제목 표시
title(main = "미국 버지니아주 하위계층 사망 비율", font.main = 4)
legend(3.8, 200, c("50-54", "55-59", "60-64", "65-69", "70-74"), 
       cex = 0.8, fill = rainbow(5))

# 점 차트 시각화
par(mfrow = c(1, 1))  # 1행 1열 그래프 보기 
dotchart(chart_data, color = c("blue", "red"), 
         lcolor = "black", pch = 1:2, 
         labels = names(chart_data), 
         xlab = "매출액", 
         main = "분기별 판매현황 : 점 차트 시각화", 
         cex = 1.2)

# 원형 차트 시각화
par(mfrow = c(1, 1))    # 1행 1열 그래프 보기
pie(chart_data, labels = names(chart_data), col = rainbow(8), cex = 1.2)
title("2018-2019년도 분기별 매출 현황")   # 제목 추가

# 상자 그래프 시각화

# "notch = FALSE"일 때
# range = 0 속성은 최소값과 최대값을 점선으로 연결
boxplot(VADeaths, range = 0)

# "notch = TRUE"일 때
# notch = T 속성은 중위수 비교 시 사용(허리선)
boxplot(VADeaths, range = 0, notch = T)
abline(h = 37, lty = 3, col = "red")  # 기준선 추가(선 스타일과 선 색상)

# VADeaths 데이터셋의 요약통계량 보기
summary(VADeaths)

# 히스토그램 시각화

# iris 데이터셋 가져오기
data(iris)
names(iris)     # iris의 칼럼명 보기
head(iris)      # iris의 앞부분 데이터 관측치 보기

# iris 데이터셋의 꽃받침 길이(Sepal.Length) 칼럼으로 히스토그램 시각화
summary(iris$Sepal.Length)      # 요약통계량 구하기
hist(iris$Sepal.Length, xlab = "iris$Sepal.Length", col = "magenta", 
     main = "iris 꽃받침 길이 Histogram", xlim = c(4.3, 7.9))

# iris 데이터셋의 꽃받침 너비(Sepal.Width) 칼럼으로 히스토그램 시각화
summary(iris$Sepal.Width)      # 요약통계량 구하기
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "mistyrose", 
     main = "iris 꽃받침 너비 Histogram", xlim = c(2.0, 4.5))

# 빈도수에 의해서 히스토그램 그리기
par(mfrow = c(1, 2))    # plots 영역에 1행 2열의 차트 표현
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", 
     col = "green", 
     main = "iris 꽃받침 너비 히스토그램 : 빈도수", xlim = c(2.0, 4.5))
# 확률 밀도에 의해서 히스토그램 그리기
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", 
     col = "mistyrose", freq = F, 
     main = "iris 꽃받침 너비 Histogram : 확률 밀도", xlim = c(2.0, 4.5))
# 밀도를 기준을 line 추가하기
lines(density(iris$Sepal.Width), col = "red")

# 정규분포 추정 곡선 나타내기

# 계급을 밀도로 표현한 히스토그램 시각화
par(mfrow = c(1, 1))    # 하나의 차트만 표현
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "mistyrose", 
     freq = F, main = "iris 꽃받침 너비 Histogram", xlim = c(2.0, 4.5))
# 히스토그램에 밀도를 기준으로 분포곡선 추가
lines(density(iris$Sepal.Width), col = "red")
# 히스토그램에 정규분포 추정 곡선 추가
x <- seq(2.0, 4.5, 0.1)
curve(dnorm(x, mean = mean(iris$Sepal.Width), 
            sd = sd(iris$Sepal.Width)), 
      col = "blue", add = T)

# 산점도 시각화

# 산점도 그래프에 대각선과 텍스트 추가하기

# 기본 산점도 시각화
price <- runif(10, min = 1, max = 100)  # 난수 발생
plot(price, col = "red")                # 산점도 그래프 그리기
# 대각선 추가
par(new = T)                            # 차트 추가
line_chart = 1:100
# 대각선 추가 : 'axes = F' 속성과 'ann = F' 속성 사용
# x축과 y축의 눈금과 축 이름 제거
plot(line_chart, type = "l", col = "red", axes = F, ann = F)
text(70, 80, "대각선 추가", col = "blue") # 텍스트 추가

# type 속성으로 산점도 그리기
par(mfrow = c(2, 2))    # 2행 2열 그래프
plot(price, type = "l") # 실선
plot(price, type = "o") # 원형과 실선(원형 통과)
plot(price, type = "h") # 직선
plot(price, type = "s") # 꺾은선

# pch 속성으로 산점도 그리기
par(mfrow = c(2, 2))                    # 2행 2열 그래프
plot(price, type = "o", pch = 5)        # 빈 사각형
plot(price, type = "o", pch = 15)       # 채워진 사각형
plot(price, type = "o", pch = 20, col = "blue")
plot(price, type = "o", pch = 20, col = "orange", cex = 1.5)
# lwd(선 굵기) 속성 추가 사용
plot(mfrow = c(1, 1))
plot(price, type = "o", pch = 20, col = "green", cex = 2.0, lwd = 3)

# plot 함수를 이용한 시계열 자료 시각화
methods("plot") # 목록에 표시되는 객체를 인수로 하여 시각화
data("WWWusage") # 시계열 자료 가져오기
str(WWWusage)   # WWWusage 자료구조 보기
plot(WWWusage)  # 시계열 자료를 이용한 추세선 시각화