# 상관관계 분석(Correalation Analysis)
# 변수들 간의 관련성을 분석하기 위해 사용

# 상관관계 분석 수행 

# 데이터 가져오기 
product <- read.csv("/Users/sallyride/Rwork/Part-Ⅲ/product.csv", header = TRUE)
head(product) # 친밀성 적절성 만족도(등간척도 - 5점 척도)
summary(product) # 요약통계량
sd(product$제품_친밀도); sd(product$제품_적절성); sd(product$제품_만족도)

# 상관계수 보기

# 변수 간의 상관계수 보기
cor(product$제품_친밀도, product$제품_적절성) # 다소 높은 양의 상관관계
cor(product$제품_친밀도, product$제품_만족도) # 다소 높은 양의 상관관계

# 제품_적절성과 제품_만족도의 상관계수 보기
cor(product$제품_적절성, product$제품_만족도) # 다소 높은 양의 상관관계

# (제품_적절성 + 제품_친밀도)와 제품_만족도의 상관계수 보기
cor(product$제품_적절성 + product$제품_친밀도, product$제품_만족도)

# 전체 변수 간의 상관계수 보기
cor(product, method = "pearson") # 피어슨 상관계수

# 동일한 색상으로 그룹을 표시하고 색의 농도로 상관계수 표현
# par(family = "NanumBarunGothicBold") # Mac에서 한글 깨짐을 방지하기 위해 콘솔창에 입력
install.packages("corrgram") # 패키지 설치 
library(corrgram)

corrgram(product)     # 색상 적용 - 동일 색상으로 그룹화 표시
corrgram(product, upper.panel = panel.conf)   # 위쪽에 상관계수 추가
corrgram(product, lower.panel = panel.conf)   # 아래쪽에 상관계수 추가

# 차트에 밀도곡선, 상관성, 유의확률(별표) 추가하기
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
# 상관성, p값(*), 정규분포(모수 검정 조건) 시각화
chart.Correlation(product, histogram = , pch = "+")

# 서열척도 대상 상관계수
cor(product, method = "spearman")


# ------------------------------------------------------------------------------------------


# EuStockMarkets 데이터셋을 통한 기술통계 분석

data("EuStockMarkets")  # 데이터셋을 사용하기 위한 명령어
dim(EuStockMarkets)     # 데이터셋의 구조 파악(row, column)
EuStockMarkets          # 데이터셋 이름을 입력하면 해당 데이터를 출력
EuStockMarkets[,'DAX']
# 'DAX'에 해당하는 데이터만 출력하기 위함
# 데이터셋[행, 열] 형식으로 입력하며, 입력칸을 비우면 전체선택을 의미

summary(EuStockMarkets) # 데이터셋을 요약하여 출력(각 칼럼별 요약데이터)

# 분포 및 중심화 경향 확인
mean(EuStockMarkets[,'DAX'])  # 'DAX' 데이터의 평균
median(EuStockMarkets[,'DAX'])  # 'DAX' 데이터의 중앙값
range(EuStockMarkets[,'DAX'])  # 'DAX' 데이터의 범위
summary(EuStockMarkets[,'DAX']) # 중심화 경향 및 분포 파악 

# 퍼짐 정도 확인 
