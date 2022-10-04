# 영-과 영근처 -분산 에측변수의 처리
install.packages('caret')
library(caret)
data(mdrr)

#데이터 위치 확인하기
getwd()
#데이터 위치 변경하기
setwd('/Users/ilan/r 2/caret')
#데이터 이픔을 mdrrDescr.csv로 저장하기
write.csv(mdrrDescr, file = 'mdrrDescr.csv')

#데이터 확인하기
str(mdrrDescr)
summary(mdrrDescr)

#데이터 크기 확인하기
dim(mdrrDescr)

#nearZeroVar 함수를 히용하여 freqRatio와 pepcentUnique를 출력하고 nZV가 TURE인 변수 확인
#영-과 영슨처- 분산 예측 변수의 처리
nzv <-nearZeroVar(mdrrDescr, saveMetrics = T, )
nzv

#영-과 영근처-분산 예측변수의 처리
filteredDescr <-mdrrDescr[, -nzv]
filteredDescr
write.csv(filteredDescr, file= 'filteredDescr.csv')
str(filteredDescr)

## nearZeroVar()에서 디폴트값으로 freqRatio=19 이상, precentUnique=10이하로 지정
nzv2 <-nearZeroVar(mdrrDescr, saveMetrics = T, freqCut =21, uniqueCut = 20)
filteredDescr2 <-mdrrDescr[,-nzv2]

# 2.2.2 절대적 상관관계 갖는 변수의 식별
descrCor <-cor(filteredDescr)
descrCor
#findCorrelation()함수를 활용하여 상관계수가 0.75인 변수를 찾아준다. 
highlyCorDescr <-findCorrelation(descrCor, cutoff = 0.75)
filteredDescr<- filteredDescr[, -highlyCorDescr]
descrCor2 <- cor(filteredDescr)
dim(descrCor2)