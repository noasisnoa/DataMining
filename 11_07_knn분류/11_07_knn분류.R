install.packages('kknn')
library(kknn)
data(iris)
m<-dim(iris)[1]
m
#150개 데이터 중 1/3을 테스트 데이터
#나머지를 훈련용 데이터로 생성
val<-sample(1:m, size = round(m/3), replace = FALSE)
val
#훈련용 데이터 100 개
iris_train<-iris[-val,]
dim(iris_train)
#테스트 데이터 50개
iris_test<-iris[val,]
dim(iris_test)

#최적의 k값 찾기
cv<-train.kknn(Species~., iris_train, iris_test, ks = seq(1, 50, by = 2), scale = F)
cv
k<-cv$best.parameters$k
k

#맨하탄 거리 기반, 가중치 없음
iris_knn_normal<-kknn(Species~., train=iris_train, test=iris_test, distance = 1, kernel = "rectangular")
summary(iris_knn_normal)
f1<-fitted(iris_knn_normal)
table(iris_test$Species, f1)
## 시각화
#한글로 되어있는 것을 숫자로 바꾼다.->캐릭터로 바꿔준다. 
#숫자이지만 text처럼 나온다. 
pcol<-as.character(as.numeric(iris_test$Species))
pairs(iris_test[1:4], pch = pcol, col=c('green3', 'red')[(iris_test$Species != f1)+1] )

#유클리디안 거리 기반, 가중치 없음
iris_knn_weighted<-kknn(Species~., train=iris_train, test=iris_test, distance = 2, kernel = "triangular")
summary(iris_knn_weighted)
f2<-fitted(iris_knn_weighted)
table(iris_test$Species, f2)

## 시각화
#한글로 되어있는 것을 숫자로 바꾼다.->캐릭터로 바꿔준다. 
#숫자이지만 text처럼 나온다. 
pcol<-as.character(as.numeric(iris_test$Species))
pairs(iris_test[1:4], pch = pcol, col=c('green3', 'red')[(iris_test$Species != f2)+1] )
