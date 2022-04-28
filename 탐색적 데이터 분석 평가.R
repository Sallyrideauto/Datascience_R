# 탐색적 데이터 분석 평가_LHS

# 문제 2
# dply 패키지와 iris 데이터 넷을 대상으로 아래의 문제를 실행하는 R 코드를 작성하여 제출하시오.

library(dplyr)

# 2-1
# iris의 꽃받침의 폭(sepal,Width)이 3.7 이상의 값만 필터링하여 화면 출력하시오.

iris %>% subset(Sepal.Width >= 3.7)

# 2-2 
# 1)의 결과에서 2,4,5번째 컬럼을 선택하시오.
select(iris %>% subset(Sepal.Width >= 3.7), Sepal.Width, Petal.Width, Species)

# 2-3
# 2)의 결과에서 2번 컬럼의 값에서 4번 컬럼의 값을 뺀 diff 파생변수를 만들고, 
# 앞부분 10개만 출력하시오.
head(mutate(iris %>% subset(Sepal.Width >= 3.7), diff = Sepal.Width - Petal.Width), n=10)


# 2-4
# 3)의 결과에서 꽃의 종(Species)별로 그룹화하여 
# Sepal,Width와 Petal.Width 변수의 평균을 계산하시오.


# 문제 3

# “user_data.csv”와 “return_data.csv” 파일을 이용하여
# 고객별 반품사유코드(return_code)를 대상으로
# 다음과 같이 단계별로 실행하여 파생 변수를 추가하시오.

setwd("/Users/sallyride/Desktop/Study/빅데이터 국비교육/R_Practice")

# 3-1 고객 정보 파일

customer_data <- read.csv("user_data.csv", header = T)
str(customer_data)

# 3-2 반품 정보 파일

returndata <- read.csv("return_data.csv",header = T)
str(returndata)

# 3-3
# 고객별 반품사유코드에 따른 파생변수(customer_return) 생성

custom_return <- dcast(returndata, user_id~return_code, length)
names(custom_return) <- c("user_id","return_code1","return_code2","return_code3","return_code4")
head(custom_return,10)

# 3-4
# 고객정보(customer_data)에 파생변수(customer_return, 반품사유 컬럼)를 추가하여
# 고객반품정보(customer_return_data)를 만들고 맨 앞 6개 데이터 화면 출력

customer_return_data <- left_join(customer_data,custom_return)
head(customer_return_data,6)

# 3-5
# 고객반품정보(customer_return_data) 테이블에서
# 맨 밑에서 10개를 화면 출력

tail(customer_return_data, 10)

