#반응변수 (y/ 범주형)의 범주가 2개인 자료에 대한 로지스틱 분석
str(iris)
#범주형인 Species의 갯수 확인하기
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
levels(iris$Species)

#반응변수 두개만 택하여 로지스틱 회귀분석 실시
a <-subset( iris, Species == "setosa"| Species =="versicolor")
a

#회귀분석 실시
b <-glm(Species~Sepal.Length, data = a, family = "binomial")
summary(b)
#접합도 확인 
round(fitted(b)[c(1:5, 96:100)], 3)

#로지스틱 회귀모형 그래프 그려보기
x = seq(min(a$Sepal.Length), max(a$Sepal.Length), 0.1 )
y = 1 / (1+(exp(-27.831 + 5.140 *x )))
plot(y~x, type = 'l')

y = 1 / (1+(1/exp(-27.831 + 5.140 *x )))
plot(y~x, type = 'l')

#Sepa;.Length의 품종별 평균
aggregate(Sepal.Length ~Species, a, mean)
#    Species Sepal.Length
#1     setosa        5.006
#2 versicolor        5.936


#예측변수(독립변수)가 2개 이상인 경우
iris
c <-glm(Species~Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = a, family = "binomial")
summary(c)
#다 무의미 하다.

#기여도가 낮은 변수부터 제거
reduced <- step(c,direction = 'backward' )
summary(reduced)