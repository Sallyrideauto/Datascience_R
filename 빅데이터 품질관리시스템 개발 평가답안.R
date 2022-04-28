# 빅데이터 품질관리시스템 평가 답안 코드_LHS

# data.table 패키지와 아래의 데이터를 이용하여 조건에 맞게 R로 코딩

# 1. data.table package를 설치 및 실행하시오.
library(dplyr)
install.packages("data.table", type = "binary")
# there is no package called ‘data.table’ 오류 해결
library(data.table)

# 2. pisa2015.csv 파일을 data.table 패키지 내 함수를 이용하여 읽어 변수 pisa에 저장하시오.
system.time({read.csv("/Users/sallyride/data/pisa2015.csv")})
pisa <- fread("/Users/sallyride/data/pisa2015.csv", na.strings = "")

# 3. pisa2015.csv 파일 내 데이터의 size를 보이시오.
print(object.size(pisa), unit = "GB") # GB 단위로 데이터 용량 표시

# 4. pisa 데이터 중 Korea와 Japan 데이터만 eastasia2.csv 파일로 저장하시오.
fwrite(pisa, file = "/Users/sallyride/data/pisa2015.csv")
# 해당 데이터 가공 전 데이터셋의 매핑 과정을 먼저 수행

pisa.tib <- tibble::as_tibble(pisa)
pisa.df <- as.data.frame(pisa)
# tibble을 이용하여 좀 더 직관적인 데이터프레임 생성

eastasia2 <- subset(pisa, CNT %in% c("Korea", "Japan"))
fwrite(eastasia2, file = "/Users/sallyride/data/eastasia2.csv")
# pisa 데이터 중 Korea와 Japan 데이터만 따로 저장함

# 5. pisa 데이터 첫 5줄과 마지막 5줄을 한꺼번에 보이시오.
pisa[, list(CNTRYID)]

# 6. pisa 데이터에서 CNTRYID 맨 마지막 6줄의 데이터를 보이시오.
pisa[order(CNTRYID, decreasing = TRUE)][, head(CNTRYID)]

# 7. 한국과 일본에서 물리시험(ST063Q01NA)에 참가한 학생 수를 보이시오.
pisa[CNTRYID %in% c("Korea", "Japan"), table(ST063Q01NA)]

# 8. 한국과 일본 학생들의 긴장여부(ST118Q04NA)를 tense라는 중간변수(intermediate variable)를 생성하여 화면에 출력하시오. 긴장 여부는 "Strongly disagree", "disagree", "Agree", "Strongly agree"로 구분
pisa[CNTRYID %in% c("Korea", "Japan"), 
     .(tense = factor(ST118Q04NA, levels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree")))
     # 긴장의 정도를 "Strongly disagree", "disagree", "Agree", "Strongly agree"의 순서로 표시
     ][, table(tense)]

# 9. 한국과 일본 학생들의 과학 자기효능감(Science Self-efficacy scale) 항목(SCIEEFF)의 평균, 표준편차, 최소값, 중간값(median), 최대값을 구하여 보이시오(NA는 제외).
pisa[CNTRYID %in% c("Korea", "Japan"), 
     .(xbar = mean(SCIEEFF, na.rm = T), 
       # 한국과 일본 학생들의 과학 자기효능감 평균
       sigma = sd(SCIEEFF, na.rm = T), 
       # 한국과 일본 학생들의 과학 자기효능감 표준편차 
       minimum = min(SCIEEFF, na.rm = T), 
       # 한국과 일본 학생들의 과학 자기효능감 최소값
       med = median(SCIEEFF, na.rm = T), 
       # 한국과 일본 학생들의 과학 자기효능감 중간값
       maximum = max(SCIEEFF, na.rm = T))]
       # 한국과 일본 학생들의 과학 자기효능감 최대값

# 10. 과학 자기효능감 지수(SCIEEFF)와 과학 흥미지수(JOYSCIE) 데이터를 이용하여 산포도(scatter plot)를 그리시오(NA는 제외)
pisa[CNTRYID %in% c("Korea", "Japan"), 
     .(plot(y = SCIEEFF, x = JOYSCIE, 
            # 한국과 일본 학생들의 과학 자기효능감 지수(SCIEEFF)를 y축, 과학 흥미지수(JOYSCIE)를 x축으로 설정
            col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3)), 
       xbar.joyscie = mean(JOYSCIE, na.rm = T))]