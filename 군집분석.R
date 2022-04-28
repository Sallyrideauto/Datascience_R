# K 평균 군집화

# 군집 수 K 결정
library(cluster)
data(iris)

nc2 = pam(iris, 2)    # 데이터를 2개로 군집화 시행
si2 = silhouette(nc2)   # 실루엣값 계산
summary(si2)
plot(si2)   # 실루엣 계산 결과 출력

nc3 = pam(iris, 3)    # 데이터를 3개로 군집화 시행
si3 = silhouette(nc3)   # 실루엣값 계산
summary(si3)
plot(si3)   # 실루엣 계산 결과 출력

# K 평균 군집화 모델 생성
iris.kc = kmeans(iris[, 1:4], 2)  # 군집수 2로 시행
iris.kc   # K = 2일 떄의 결과 출력
iris.kc = kmeans(iris[, 1:4], 3)  # 군집수 3으로 시행
iris.kc   # K = 3일 떄의 결과 출력

# K 평균 클러스터링 분석 수행

data(iris)
iris2 <- iris[, 1:4]    # 목표변수(Species) 제외

km.out.withness <- c()
km.out.between <- c()
for(i in 2:8){          # 군집 수를 k = 2~7까지 변화시켜 가며 클러스터링 시행
  set.seed(1)
  km.out <- kmeans(iris2, centers = i)
  km.out.withness[i - 1] <- km.out$tot.withinss  # 군집 내 제곱합 저장
  km.out.between[i - 1] <- km.out$betweenss     # 군집 간 제곱합 저장
}
data.frame(km.out.withness, km.out.between)

# 군집수 K = 3을 이용하여 클러스터링을 다시 수행하고 그 결과를 탐색
km.out.k3 <- kmeans(iris2, centers = 3)
km.out.k3$centers     # 각 군집의 중심점 출력
km.out.k3$cluster     # 각 관측치의 할당된 군집번호 출력
km.out.k3$size        # 각 군집의 데이터 관측치 개수 출력
table(km.out.k3$cluster, iris$Species)  # 군집결과와 원래 품종 개수 비교

plot(iris2[, 1:2], col = km.out.k3$cluster, 
     pch = ifelse(km.out.k3$cluster == 1, 16, 
                  ifelse(km.out.k3$cluster == 2, 17, 18)), cex = 2) ; 
points(km.out.k3$centers, col = 1:3, pch = 16:18, cex = 5)

