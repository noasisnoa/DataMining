install.packages("ade4")
library(ade4)
data("olympic")
str(olympic)
head(olympic)
o<-data.frame(olympic)
head(olympic)

#결측치까지 확인 할 수 있다. 
summary(o)
install.packages("psych")
library(psych)
fa.parallel(o, fm ='ml', fa = 'fa', n.iter = 100)
#3개를 추천

install.packages("nFactors")
library(nFactors)
nScree(o)
#noc naf nparallel nkaiser
#1   2   2         2       2

#아이덴 벨루가 크면 배제할 수 없다. 
cor(o)
eigen(cor(o))$values
#1이상인 것이 2개이다. 
#요인 개수를 2개로 결정
#요인함수 분석

#2)최우추정법
fa <-factanal(o, factors = 2, scores = 'regression')
fa
# 귀무가설은 : Test of the hypothesis that 2 factors are sufficient.
# 채택한다. 
# 요인이 2개 인것이 충분한가요?
# Cumulative Var   0.336   0.593 두 요인을 합쳐서 0.593의 결롸를 낸다. 
# 0.05보다 작기 때문에 기각할 수 있다.

#f1으로 멀리 다리는 것이 관련이 있다. 
#f2는 

print(fa$loadings, cutoff= 0.5)
colnames(fa$scores)<-c("Run", "throw")
round(fa$scores,3)

install.packages("semPlot")
library(semPlot)
#다시해보기
semPaths(fa, what = "est", residuals = FALSE, cut = 0.3, posCol =c("white", "darkgreemn"), negCol("White", "red"), edge.label.cex = 0.75)

fap<-principal(o, nfactors = 2, rotate = 'varimax')
fap
semPaths(fap, what = "est", residuals = FALSE, cut = 0.3, posCol =c("white", "darkgreemn"), negCol("White", "red"), edge.label.cex = 0.75)
biplot(fap, cex = c(0.5, 0.57), main ="FA Biplot by principal")
#o.o5보다 크니까 기각할 수 없다. 
#귀무가설이 유의미 하다. 
