par(family = "NanumBarunGothicBold")

# 시계열 분석

data("AirPassengers") # 12년간 항공기 탑승 승객 수

# 차분 적용 - 평균 정상화
par(mfrow = c(1,2))
ts.plot(AirPassengers)  # 시계열 시각화
diff <- diff(AirPassengers)   # 차분 수행
plot(diff)  # 평균 정상화

# 로그 적용 - 분산 정상화
par(mfrow = c(1,2))
plot(AirPassengers) # 시계열 시각화
log <- diff(log(AirPassengers))   # 로그+차분 수행
plot(log) # 분산 정상화

# 시계열 자료 시각화

data("WWWusage")  # 인터넷 사용 시간
str(WWWusage)   # 데이터셋 구조 보기
WWWusage
# 시계열 자료 추세선 시각화
X11()
ts.plot(WWWusage, type = "l", col = "red")

# 다중 시계열 자료 시각화
data("EuStockMarkets")
head(EuStockMarkets)
# 데이터프레임으로 변환
EuStock <- data.frame(EuStockMarkets)
head(EuStock)
# 단일 시계열 자료 추세선 시각화(1,000개 뎅터 대상)
X11()
plot(EuStock$DAX[1:1000], type = "l", col = "red")
# 다중 시계열 자료 추세선 시각화(1,000개 데이터 대상)
plot.ts(cbind(EuStock$DAX[1:1000], EuStock$SMI[1:1000]),
        main = "주가지수 추세선")