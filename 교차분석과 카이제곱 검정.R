# 교차분석

# 변수 리코딩과 데이터프레임 생성
setwd("/Users/sallyride/Rwork/Part-Ⅲ")
data <- read.csv("cleanDescriptive.csv", header = TRUE)
head(data) # 변수 확인
# 변수 리코딩 : 리코딩된 변수 이용
x <- data$level2
y <- data$pass2
# 데이터프레임 생성
result <- data.frame(Level = x, Pass = y) # 데이터프레임 생성
dim(result) # 차원 보기

# 데이터프레임을 이용한 교차분석 수행
# 기본 함수를 이용한 교차 분할표 작성
table(result) # 교차 빈도수
# 교차 분할표 작성을 위한 패키지 설치
install.packages("gmodels") # gmodels 패키지 설치
library(gmodels)  # CrossTable() 함수 사용
# install.packages("ggplot2") # diamonds 데이터셋 사용을 위한 패키지 설치
library(ggplot2)
# 패키지를 이용한 교차 분할표 작성
# diamonds의 cut과 color에 대한 교차 분할표 생성
CrossTable(x = diamonds$color, y = diamonds$cut)
# 패키지를 이용한 교차 분할표 작성 : 부모의 학력 수준과 자녀의 대학 진학 여부
# 변수 모델 : 학력수준(독립변수) -> 진학여부(종속변수)
x <- data$level2  # 부모의 학력수준 리코딩 변수
y <- data$pass2   # 자녀의 대학 진학 여부 레코딩 변수
CrossTable(x, y)  # 교차 분할표 작성

# 카이제곱 검정

# CrossTable() 함수를 이용한 카이제곱 검정
CrossTable(x = diamonds$cut,
           y = diamonds$color,
           chisq = TRUE)

# 주사위 적합도 검정
chisq.test(c(4,6,17,16,8,9))

# 5개의 스포츠음료에 대한 선호도에 차이가 있는지 검정
data <- textConnection(
  "스포츠음료종류 관측도수
  1 41
  2 30
  3 51
  4 71
  5 61
  "
)
x <- read.table(data, header = T)
x
chisq.test(x$관측도수)  # 선호도 분석의 검정 통계량 확인

# 독립성 검정(관련성 검정)
# 부모의 학력수준과 자녀의 대학 진학여부의 독립성(관련성) 검정
setwd("/Users/sallyride/Rwork/Part-Ⅲ")
data <- read.csv("cleanDescriptive.csv", header = TRUE)
x <- data$level2
y <- data$pass2
CrossTable(x, y, chisq = TRUE)

# 동질성 검정
# 교육센터에서 교육 방법에 따라 교육생들의 만족도에 차이가 있는지 검정
setwd("/Users/sallyride/Rwork/Part-Ⅲ")
data <- read.csv("homogenity.csv")
head(data)
data <- subset(data, !is.na(survey), c(method, survey))
# 코딩 변경(변수 리코딩)
# method_ 1 : 방법1, 2 : 방법2, 3 : 방법3
# survey_ 1 : 매우만족, 2 : 만족, 3 : 보통, 4 : 불만족, 5 : 매우불만족
# method2 필드 추가
data$method2[data$method == 1] <- "방법1"
data$method2[data$method == 2] <- "방법2"
data$method2[data$method == 3] <- "방법3"
# survey2 필드 추가
data$survey2[data$survey == 1] <- "1.매우만족"
data$survey2[data$survey == 2] <- "2.만족"
data$survey2[data$survey == 3] <- "3.보통"
data$survey2[data$survey == 4] <- "4.불만족"
data$survey2[data$survey == 5] <- "5.매우불만족"
# 교차 분할표 작성
table(data$method2, data$survey2)   # 교차표 생성 -> table(행, 열)
# 동질성 검정 : 모두 특성치에 대한 추론 검정
chisq.test(data$method2, data$survey2)