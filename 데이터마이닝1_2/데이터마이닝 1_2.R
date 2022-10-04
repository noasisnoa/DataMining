data(trees)
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

#회귀문석에서 보형 평가 AIC값 구하기
install.packages("cars")
data(states)
cor.plot(states)
#종속병수 : 자살률, 독립변수 : 인구, 수입, 문명률, Frost
fit <-lm(Murder~Population+Income+Illiteracy+Forest, data=states)
summary(fit)

fit1 <-lm(Murder~Population+Income+Illiteracy, data=states)
summary(fit1)

AIC(fit, fit1)
anova(fit, fit1)

# 단계적으로 AIC값 구하기
fit <-lm(Murder~Population+Income+Illiteracy+Forest+HS.Grad+Area, data=states)
library(MASS)
stepAIC(fit, direction = 'backward')

#연속형 반응변수 평가 지표 구하기
install.packages("forecast")
library(forecast)
plot(states$Murder)
lines(fit$fitted.values, col='red')
lines(fit$fitted.values, col='blue')
lines(fit$fitted.values, col='green')

accuracy(fit)
accuracy(fit1)

predicted<-as.factor(c(1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1))
actual <-as.factor(c(1, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1))
confusionMatrix(predicted, actual)

#press 계산하기
install.packages("qpdR")
library(qpcR)
PRESS(fit)
PRESS(fit1)

#모형평가 지표 출력
ols_step_best_subset(fit)

#정오분류포
#예측한 결과가 얼마나 맞았는지 나타내는 표
predicted<-as.factor(c(1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1))
actual <-as.factor(c(1, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1))
confusionMatrix(predicted, actual)

#MAX, MSE, MAPE  구하기
accuracy(fit)
accuracy(fit1)
plot(states$Murder)
lines(fit$fitted.values, col='red')
lines(fit$fitted.values, col='blue')
lines(fit$fitted.values, col='green')