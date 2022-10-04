#2022.10.04 데이터마이닝
#4장 의사결정나무
#ADSP / 사회조사 분석사는 스스고 하고 싶으면 하고
#3학년 2학기 때 프로그래밍 
# 100%이론으로 간다., 손코딩도 조금 있어 
#과제는 프로그래밍을 한 것을 해서 실제로 했나 안했나를 확인해 보는 정도
# p 작을수록 이질감이 있다, 지니지수 엔트로피 지수

#엔트로피, 진위지수가 작을 수록 

#정보이득이라는 것은 엔트로피 값을 서로 뺀 것이다. 

#의사결저아누 단계 잘 알고 있어야 해
#cp값 넣은 것이 적합모형

install.packages('rgart')
library(rpart)
data(iris)
# y값만 범주형이고 나머지는 수치형이다.
str(iris)
#summary를 보았을 때 np(결측지)가 없다.
summary(iris)
#걸측지는 업는 걸로
sum(!complete.cases(iris))
#의사경정나무 분석
c <-rpart(Species~., data = iris)
c
plot(c, compress = T, margin = 0.3)
#text를 해야히 글자가 나온다
text(c, cex = 1.5)

#예측
pc<-predict(c, newdata = iris, type = 'class')
summary(pc)
table(pc, iris$Species, dnn =c("Actual", "Predicted"))
mean(iris$Species ==pc)
c$cptable

##cp = 0.01 로
c <-rpart(Species~., data = iris, cp = 0.01)
bc<-predict(c, newdata = iris, type = 'class')
summary(bc)
table(bc, iris$Species, dnn =c("Actual", "Predicted"))
mean(iris$Species ==bc)





#교과서 예제2
install.packages('party')
library(party)
data(stagec)
summary(stagec)
sum(!complete.cases(stagec))

astagec <-(na.omit(stagec))
str(astagec)
#$ ploidy : Factor w/ 3 levels "diploid","tetraploid",..: 1 1 1 2 1 1 2 2 2 1 ...
#범주형이 가 된다.

#test 데이터와 train 데이터를 분류하기
set.seed(1234)

ind <-sample(2, nrow(astagec), replace = TRUE, prob=c(0.7, 0.3))
ind
train<- astagec[ind == 1,]
test <- astagec[ind == 2,]
dim(train)
dim(test)

tree <-rpart(ploidy~., data=train)
plot(tree)
text(tree, cex=1.5)

#예측
ptest<-predict(tree, newdata = test, type = 'class')
#예측 수행수 결과 확인하기
table(test$ploidy, ptest, dnn = c("Actual", "Predicted"))
mean(test$ploidy == ptest)

#cp값 결정 후 의사결정마누 모형 분석
tree <-rpart(ploidy~., data = astagec, cp = 0.010)
ptree<-predict(tree, newdata = astagec, type = 'class')
round(tree$cptable, 3)

table(ptree, astagec$ploidy, dnn = c("Actual", "Predicted"))
mean(astagec$ploidy == ptree)

