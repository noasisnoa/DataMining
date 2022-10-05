#의사결정나무모형이란?

install.packages("rpart")
library(rpart)
data(iris)
head(iris)
str(iris)
## Species(범주형, 이산형)를 분류/ 분류나무
#rpart 분류나무 함수
c<-rpart(Species~., data = iris)
c

plot(c, compress = T, margin = 0.3)
text(c, cex = 1.5)
#분류나무 모형에 적용하기
pc <- predict(c, newdata = iris, type = "class")
summary(pc)
table(pc, iris$Species, dnn =c("Actual", "Predicted"))
#얼마나 잘 분류하는지 알아보는 코드
mean(iris$Species ==pc)
# 0.96정도의 정확성을 가지고 있다. 
#의사결정나무 모형 그리기
install.packages("rpart.plot")
library(rpart.plot)

#### 그림
prp(c, type = 4, extra = 2)
#복잡도를 확인하는 함수
c$cptable

#xerror +-xstd
#0.10 +- 0.03


#### 그림
plotcp(c)

#cp = 0.01로 결정
bc<-rpart(Species~., data = iris,cp= 0.01)
bc$cptable
pbc <- predict(c, newdata = iris, type = "class")
table(pbc, iris$Species, dnn =c("Actual", "Predicted"))
mean(iris$Species ==pbc)
