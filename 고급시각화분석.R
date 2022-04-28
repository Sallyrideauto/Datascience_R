# 고급 시각화 분석

library(extrafont)
font_import()

# lattice 패키지 설치
install.packages("lattice")
library(lattice)

# 실습용 데이터 가져오기
install.packages("mlmRev")
library(mlmRev)
data(Chem97)  # 데이터셋 가져오기
str(Chem97)   # 자료구조 보기
head(Chem97, 30)  # 앞부분 30개 행 보기
Chem97  # 전체 데이터 보기

# 히스토그램

# Chem 97 데이터셋의 gcsescore 변수를 x축으로 하여 히스토그램 그리기
histogram(~gcsescore, data = Chem97)

# score 변수를 조건변수로 지정하여 데이터 시각화하기
histogram(~gcsescore | score, data = Chem97)  # 첫 번째 결과 그래프
# score 변수를 factor 적용
histogram(~gcsescore | factor(score), data = Chem97)  # 두 번째 결과 그래프

# 밀도 그래프

# densityplot() 함수를 사용하여 밀도 그래프 그리기
densityplot(~gcsescore | factor(score), data = Chem97, 
            groups = gender, 
            plot.points = T, auto.key = T)

# 막대 그래프

# barchart() 함수를 사용하여 막대 그래프 그리기
# 기본 데이터셋 가져오기
data(VADeaths)
VADeaths
str(VADeaths) # 데이터셋 구조 보기(num[1:5, 1:4])
class(VADeaths) # matrix array
mode(VADeaths)  # numeric
# 데이터 형식 변경(matrix 형식을 table 형식으로 변경)
dft <- as.data.frame.table(VADeaths)
str(dft)
dft
# 막대 그래프 그리기
barchart(Var1 ~ Freq | Var2, data = dft, layout = c(4, 1))
# origin 속성을 사용하여 막대 그래프 그리기
barchart(Var1 ~ Freq | Var2, data = dft, layout = c(4, 1), origin = 0)

# 점 그래프

# dotplot() 함수를 사용하여 점 그래프 그리기
# layout 속성이 없는 경우
dotplot(Var1 ~ Freq | Var2, dft)  # 2행 2열
# layout 속성을 적용한 경우
dotplot(Var1 ~ Freq | Var2, dft, layout = c(4, 1))

# 점을 선으로 연결하여 시각화하기
dotplot(Var1 ~ Freq, data = dft, 
        groups = Var2, type = "o", 
        auto.key = list(space = "right", points = T, lines = T))  # 범례 추가

# 산점도 그래프

# airquality 데이터셋으로 산점도 그래프 그리기
library(datasets)
str(airquality)
# xyplot() 함수를 사용하여 산점도 그리기
# airquality의 Ozone 변수를 y축, Wind 변수를 x축으로
xyplot(Ozone ~ Wind, data = airquality)
# 조건변수를 사용하는 xyplot() 함수로 산점도 그리기
xyplot(Ozone ~ Wind | Month, data = airquality)
# 조건변수와 layout 속성을 사용하는 xyplot() 함수로 산점도 그리기
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1))
# Month 변수를 factor 타입으로 변환하여 산점도 그리기
# 패널 제목에는 factor 값을 표시
convert <- transform(airquality, Month = factor(Month)) # factor형으로 변환
str(convert)  # Month 변수의 Factor값 확인
xyplot(Ozone ~ Wind | Month, data = convert)

# quakes 데이터셋으로 산점도 그래프 그리기
# quakes 데이터셋 보기
head(quakes)
str(quakes)
par(family = "NanumGothic")
# 지진 발생 진앙지(위도와 경도) 산점도 그리기
xyplot(lat ~ long, data = quakes, pch = ".")
# 산점도 그래프를 변수에 저장하고, 제목 문자열 추가하기
# xyplot() 함수의 결과인 산점도 그래프를 변수에 저장
tplot <- xyplot(lat ~ long, data = quakes, pch = ".", family = "NanumGothic")
# 변수의 내용 업데이트
tplot <- update(tplot, main = "1964년 이후 태평양에서 발생한 지진 위치")
print(tplot)  # 산점도 그래프를 저장하는 변수 출력

# 이산형 변수를 조건으로 지정하여 산점도 그리기
range(quakes$depth) # depth 변수의 범위 확인
# depth 변수 리코딩 : 6개의 범주(100 단위)로 코딩 변경
quakes$depth2[quakes$depth >= 40 & quakes$depth <= 150] <- 1
quakes$depth2[quakes$depth >= 151 & quakes$depth <= 250] <- 2
quakes$depth2[quakes$depth >= 251 & quakes$depth <= 350] <- 3
quakes$depth2[quakes$depth >= 351 & quakes$depth <= 450] <- 4
quakes$depth2[quakes$depth >= 451 & quakes$depth <= 550] <- 5
quakes$depth2[quakes$depth >= 551 & quakes$depth <= 680] <- 6
# 리코딩된 변수(depth2)를 조건으로 산점도 그리기
convert <- transform(quakes, depth2 = factor(depth2)) # factor형으로 변환
xyplot(lat ~ long | depth2, data = convert)

# 동일한 패널에 두 개의 변수값 표현하기
xyplot(Ozone + Solar.R ~ Wind | factor(Month), 
       data = airquality, col = c("blue", "red"), 
       layout = c(5, 1))

# 데이터 범주화

# equal.count() 함수를 사용하여 이산형 변수 범주화하기
# 1~150을 대상으로 겹치지 않게 4개 영역으로 범주화
numgroup <- equal.count(1:150, number = 4, overlap = 0)
numgroup
# 지진의 깊이를 5개 영역으로 범주화
depthgroup <- equal.count(quakes$depth, number = 5, overlap = 0)
depthgroup
# 범주화된 변수(depthgroup)를 조건으로 산점도 그리기
xyplot(lat ~ long | depthgroup, data = quakes, 
       main = "Fiji Earthquakes(depthgroup)", 
       ylab = "latitude", xlab = "longitude", 
       pch = "@", col = "red", family = "NanumGothic")

# 수심과 리히터 규모 변수를 동시에 적용하여 산점도 그리기
# 리히터 영역을 2개 영역으로 구분
magnitudegroup <- equal.count(quakes$mag, number = 2, overlap = 0)
magnitudegroup
# magnitudegroup 변수를 기준으로 산점도 그리기
xyplot(lat ~ long | magnitudegroup, data = quakes, 
       main = "Fiji Earthquakes(magnitude)", 
       ylab = "latitude", xlab = "longitude", 
       pch = "@", col = "blue")
# 수심과 리히터 규모를 동시에 표현(2행 5열 패널 구현)
xyplot(lat ~ long | depthgroup * magnitudegroup, data = quakes, 
       main = "Fiji Earthquakes", 
       ylab = "latitude", xlab = "longitude", 
       pch = "@", col = c("red", "blue"))   # 수심(빨강), 리히터 규모(파랑)

# 이산형 변수를 리코딩한 뒤에 factor 형으로 변환하여 산점도 그리기
# depth 변수 리코딩
quakes$depth3[quakes$depth >= 39.5 & quakes$depth <= 80.5] <- 'd1'
quakes$depth3[quakes$depth >= 79.5 & quakes$depth <= 186.5] <- 'd2'
quakes$depth3[quakes$depth >= 185.5 & quakes$depth <= 397.5] <- 'd3'
quakes$depth3[quakes$depth >= 396.5 & quakes$depth <= 562.5] <- 'd4'
quakes$depth3[quakes$depth >= 562.5 & quakes$depth <= 680.5] <- 'd5'
# mag 변수 리코딩
quakes$mag3[quakes$mag >= 3.95 & quakes$mag <= 4.65] <- 'm1'
quakes$mag3[quakes$mag >= 4.55 & quakes$mag <= 6.45] <- 'm2'
# factor형 변환
convert <- transform(quakes,
                     depth3 = factor(depth3), 
                     mag3 = factor(mag3))
# 산점도 그래프 그리기
xyplot(lat ~ long | depth3 * mag3, data = convert, 
       main = "Fiji Earthquakes", 
       ylab = "latitude", xlab = "longitude", 
       pch = "@", col = c("red", "blue"))

# 조건 그래프

# depth 조건에 의해서 위도와 경도의 조건 그래프 그리기
coplot(lat ~ long | depth, data = quakes)

# 조건의 구간 크기와 겹침 간격 적용 후 조건 그래프 그리기
# 조건의 구간 막대가 0.1 단위로 겹쳐 범주화
coplot(lat ~ long | depth, data = quakes, overlap = 0.1)    # 겹치는 구간 : 0.1
# 조건 구간을 5개로 지정하고, 1행 5열의 패널로 조건 그래프 작성
# 구간 5, 1행 5열
coplot(lat ~ long | depth, data = quakes, number = 5, row = 1)

# 패널과 조건 막대에 색을 적용하여 조건 그래프 그리기
# 패널 영역에 부드러운 곡선 추가
coplot(lat ~ long | depth, data = quakes, 
       number = 5, row = 1, 
       panel = panel.smooth)
# 패널 영역과 조건 막대에 색상 적용
coplot(lat ~ long | depth, data = quakes, 
       number = 5, row = 1, 
       col = 'blue',              # 패널에 색상 적용
       bar.bg = c(num = 'green')) # 조건 막대에 색상 적용

# 3차원 산점도 그래프
cloud(depth ~ lat * long, data = quakes, 
      zlim = rev(range(quakes$depth)), 
      xlab = "경도", ylab = "위도", zlab = "깊이")

# 테두리와 회전 속성을 추가하여 3차원 산점도 그래프 그리기
cloud(depth ~ lat * long, data = quakes, 
      zlim = rev(range(quakes$depth)), 
      panel.aspect = 0.9, 
      screen = list(z = 45, x = -25),
      xlab = "경도", ylab = "위도", zlab = "깊이")

# 기하학적 기법 시각화

# qplot() 함수
library(ggplot2)  # 메모리 로딩
data(mpg)         # 데이터셋 가져오기
str(mpg)          # 데이터셋 구조 보기
head(mpg)         # 데이터셋 앞부분 내용 보기
summary(mpg)      # 데이터셋의 요약 통계량
table(mpg$drv)    # 구동 방식 빈도수

# qplot() 함수의 fill과 bindwidth 속성 적용하기

# 도수분포를 세로 막대 그래프로 표현
qplot(hwy, data = mpg)
qplot(hwy, data = mpg, fill = drv) # fill 속성 적용
qplot(hwy, data = mpg, fill = drv, binwidth  = 2) # bindwidth 속성 적용

# facets 속성을 사용하여 drv 변수값으로 행/열 단위로 패널 생성하기
qplot(hwy, data = mpg, fill = drv, facets = . ~ drv, binwidth = 2)  # 열 단위 패널 생성
qplot(hwy, data = mpg, fill = drv, facets = drv ~ ., binwidth = 2)  # 행 단위 패널 생성

# 두 개 변수 대상으로 qplot() 함수 적용

# qplot() 함수에서 color 속성을 사용하여 두 변수 구분하기
qplot(displ, hwy, data = mpg) # 두 변수로 displ과 hwy 변수 사용
qplot(displ, hwy, data = mpg, color = drv)  # 두 변수로 displ과 hwy 변수 사용, drv 변수에 색상 적용

# displ과 hwy 변수의 관계를 drv 변수로 구분하기
qplot(displ, hwy, data = mpg, color = drv, facets = . ~ drv)  # 행 단위 패널 생성

# 미적 요소 맵핑(mapping)

# mtcars 데이터셋에 색상, 크기, 모양 적용하기
head(mtcars)  # ggplot2 패키지에서 제공하는 데이터셋
qplot(wt, mpg, data = mtcars, color = factor(carb)) # 색상 적용
qplot(wt, mpg, data = mtcars, size = qsec, color = factor(carb))  # 크기 적용
qplot(wt, mpg, data = mtcars, size = qsec, color = factor(carb), shape = factor(cyl)) # 모양 적용

# 기하학적 개체 적용

# diamonds 데이터셋에 막대, 점, 선, 레이아웃 적용하기
head(diamonds)  # ggplot2 패키지에서 제공하는 데이터셋
# geom 속성과 fill 속성 사용하기
# geom = 'bar' 속성으로 막대 그래프 그리기
# fill = cut 속성으로 cut 변수의 값을 색상으로 채우기
qplot(clarity, data = diamonds, fill = cut, geom = "bar")
qplot(clarity, data = diamonds, colour = cut, geom = "bar") # 테두리 색 적용
qplot(wt, mpg, data = mtcars, size = qsec, geom = "point")  # geom = "point" 속성으로 산점도 그래프 그리기
# 산점도 그래프에 cyl 변수의 요인으로 포인트 크기 적용하고, carb 변수의 요인으로 포인트 색 적용하기
qplot(wt, mpg, data = mtcars, size = factor(cyl), color = factor(carb), geom = "point")
# 산점도 그래프에 qsec 변수의 요인으로 포인트 크기 적용하고, cyl 변수의 요인으로 포인트 모양 적용하기
qplot(wt, mpg, data = mtcars, size = qsec, color = factor(carb), shape = factor(cyl), geom = "point")
# geom = "smooth" 속성으로 산점도 그래프에 평활 그리기
qplot(wt, mpg, data = mtcars, geom = c("point", "smooth"))
# 산점도 그래프의 평활에 cyl 변수의 요인으로 색상 적용하기
qplot(wt, mpg, data = mtcars, color = factor(cyl), geom = c("point", "smooth"))
# geom = "line" 속성으로 그래프 그리기
qplot(mpg, wt, data = mtcars, color = factor(cyl), geom = "line")
# geom = c("point", "line") 속성으로 그래프 그리기
qplot(mpg, wt, data = mtcars, color = factor(cyl), geom = c("point", "line"))

# ggplot() 함수

# 미적 요소 맵핑

# aes() 함수 속성을 추가하여 미적 요소 맵핑하기
# aes(x축 변수, y축 변수, color = 변수) 형식으로 미적 요소 맵핑
p <- ggplot(diamonds, aes(carat, price, color = cut))
p + geom_point()    # point 차트 추가

# mtcars 데이터셋에 미적 요소 맵핑
# aes(x축 변수, y축 변수, color = 변수) 형식으로 미적 요소 맵핑
p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl)))
p + geom_point()    # point 차트 추가

# 기하학적 객체 적용

# geom_line()과 geom_point() 함수를 적용하여 레이어 추가하기
# geom_line() 레이어 추가
p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl)))
p + geom_line()   # line 추가
# geom_point() 레이어 추가
p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl)))
p + geom_point()  # point 추가

# 미적 요소 맵핑과 기하학적 객체 적용

# stat_bin() 함수를 사용하여 막대 그래프 그리기
# 기본 미적 요소 맵핑 객체를 생성한 뒤에 stat_bin() 함수 사용
p <- ggplot(diamonds, aes(price))
# 미적 요소 맵핑과 기하학적 객체 적용
p + stat_bin(aes(fill = cut), geom = "bar")

# stat_bin() 함수로 산점도 그래프 그리기
# stat_bin() 함수 적용 영역 나타내기
p <- ggplot(diamonds, aes(price))
p + stat_bin(aes(fill = cut), geom = "area")
# stat_bin() 함수로 산점도 그래프 그리기
p + stat_bin(aes(color = cut, size = ..density..), geom = "point")