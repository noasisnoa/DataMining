data(trees)
summary(trees)
head(trees) #6개의 자료까지만 보여준다. 
dim(trees) # 데이터의 크기를 알려준다.
str(trees) # 전체데이터의 값을 보여준다.

#회귀분석 종속변수 높이, 독립변수 둘레
#높이와 둘레
fit1 <-lm(formula = trees$Height~trees$Girth, data=trees)
summary(fit1)
cor(trees)
#높이와 부피
fit2<-lm(trees$Height~trees$Volume, data= trees)
summary(fit2)
#높이에 댜한 부피와 둘레
fit<-lm(trees$Height~trees$Girth+trees$Volume, data= trees)
summary(fit)

#CP 값 구하기
library(olsrr)
full_model <-fit
ols_mallows(fit1, full_model)
ols_mallows(fit2, full_model)