#단순베이즈분류 : 사후확률이 큰 쪽으로
install.packages('e1071')
install.packages('MASS')
library(e1071)
library(MASS)
data(iris)
str(iris)

#단순베이즈 분류
m<-naiveBayes(Species~., data=iris)
m

#예측하기
pm<-predict(m, iris)
pm

#정오분류표 만들기
tm<-table(pm, iris$Species)
tm

#정오분류률
p<-sum(diag(tm))/sum(tm)
1-p

#결측값이 있는 데이터, 단순베이즈 분류
install.packages('mlbench')
library(mlbench)
data(HouseVotes84)
summary(HouseVotes84)
str(HouseVotes84)

#누락된 데이터의 개수확인
sum(!complete.cases(HouseVotes84))

#단순베이즈분류
m<-naiveBayes(Class~., data=HouseVotes84)
m

#예측하기
pm<-predict(m, HouseVotes84)
pm

#정오분류표 만들기
tm<-table(pm, HouseVotes84$Class)
tm

#결측값 제거하고 단순베이즈분류
newHouseVotes84<-na.omit(HouseVotes84)
sum(!complete.cases(newHouseVotes84))

#ne데이터 단순베이즈분류
nm<-naiveBayes(Class~., data=newHouseVotes84)
nm

#예측하기
npm<-predict(m, newHouseVotes84)
npm

#정오분류표 만들기
ntm<-table(pm, newHouseVotes84$Class)
ntm

#정오분류률 계산
sum(diag(ntm)/sum(ntm))
1-(sum(diag(ntm)/sum(ntm)))  