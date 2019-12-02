#5일차
setwd("D:/WorkR2")
df <- read.table(file="airquality.txt".header=T)
df
class(df)
dim(df)
str(df)
head(df,3)
tail(df,3)


java ->JDK (java 개발환경) JRE 자동설치
      (Java Development Kit)
    ->JRE (실행환경)
웹설치하면 자동으로 실행
      ()

SE(stadard edition)
ee(Enterprise     )
me(mobile         )


install.peckages("xlsx")
install.packages("rJava")

install.packakages 패키지 설치방법 위에 
rjava 는 java 실행환경과 개발환경이 구축이 안됐다는 것 

jdk 설치
os 별 환경설정
환경변수 path에 jdk 설치 위치 등록 

library(rJava)
library(xlsx)

반드시 패키지를 쓰겠다는 함수 library 
rjava 를 먼저 쓰고 xlsx 를 써야함


df.xlsx <- read.xlsx(file="airquality.xlsx",sheetIndex = 1,
                     encoding="UTF-8")

df.xlsx
class(df.xlsx)
str(df.xlsx)
head(df.xlsx)
tail(df.xlsx)

encoding 을 utf-8 로 해야한다.그래야 한글이 안 깨짐

base  -  read.table( ) 일반 텍스트 파일
      - dead.csv() csv 텍스트 파일
rJava: read.xlsx( )excel 파일



score <- c(76,84,69,50,95,6,82,71,88,84)
which(score==69)  #어느 집에 있는지 위치, (지금까지는 값을 알아내는 것이었음)
which(score>=95)
max(score)
which.max(score)
min(score)
which.min(score)


idx <- which(score>=60)    
score[idx] <- 61
score

iris
idx <- which(iris[,1:4]>5.0,arr.ind = TRUE)   #뭔말?
idx


#data 분석절차 ->문제 정의 ->data 수집->data 정제, 전처리->data 탐색 ->결과 보고서
#현상을 분석 ,이면을 찾는 것 

#data 탐색 : 탐색적 data분석 
#머신러닝(ML):Model 구축


#단일변수 (일변량)범주형 자료탐색 


#
favorite <- c('WINTER','SUMMER','SPRING', 'SUMMER','SUMMER','FALL','FALL','SUMMER','SPRING','SPRING')


favorite
class(favorite)
table(favorite)     #도수분포표(몇개있는지)
table(favorite) /length(favorite)  #length 비율
ds <- table(favorite)
dsbar
barplot(ds,main='favorite season') #막대그래프


ds.new <- ds[c(2,3,1,4)]
ds.new
barplot(ds.new,main = 'favorite season')  #main 제목


pie(ds,main='favorite season') 
pie(ds.new,main = 'favorite season')


favorite.color <- c(2,3,2,1,1,2,2,1,3,2,1,3,2,1,2)

ds <- table(favorite.color); ds
barplot(ds,main='favorite season')
colors <- c('green','red','blue')
names(ds) <- colors;     ds
barplot(ds,main = 'favorite season',col=colors)
pie(ds,main = 'favorite season',col=colors) #색상부여하는 인수-col



#단일변수(일변량)연속형 자료탐색 -값의 왜곡이 있을 수 있어서 중앙값이나, 절사평균을쓴다.


weight <- c(60,62,64,65,68,69);weight
weight.heavy <- c(weight,120);weight.heavy
#평균 -데이터분포가 어떻냐에 따라서 다를수 있다.
mean(weight);mean(weight.heavy)

#중앙값  - 가장 가운데 값을 찾음, 큰 차이가 없다. (평균보다 유용)
median(weight);median(weight.heavy)
#절사평균 -가장 높은값 ,가장 낮은 값을 제외한 평균 trim :20% 를 잘라내고 한것 
mean(weight,trim = 0.2)
mean(weight.heavy,trim = 0.2)
#사분위수-데이터를 4등분 한것,2사분위값이 중앙값과 같음. 1~3사분위 까지 대략50%된다.
quantile(weight.heavy)
quantile(weight.heavy,(0:10)/10) 대략25% 는 63점 , 대략 50%는
summary(weight.heavy) #사분위수 , 최대 ,최소 중앙값






#산포(distribution)-값이 퍼져있는 정도파악, -분산값이나 표준편차가 크면 흩어져있다. 
#분산- 
var(weight)
#표준편차-
sd(weight)

#값의 범위(최소값과 최대값)
range(weight)
#최대값과 최소값의 차이
diff(weight)


#histogram :연속형 자료의 분포를 시각화
#연속형 자료에서는 구간을 나누고 구간에 속한 값들의 개수를 세는 방법으로 사용
str(cars)
dist <- cars[,2]
hist(dist, main='Histogram for 제동거리',xlab = '제동거리',ylab='빈도수',
     border='blue',col='green',las=3,breaks=5) #breaks 막대수



#상자그림(boxplot, 상자수염그림)
#사분위수를 시각화하여 그래프 형태로 표시
#상자그림은 하나의 그래프로 데이터의 분포
#형태를 포함한 다양한 정보를 전달
#자료의 전반적인 분포를 이해하는데 도움
#구체적인 최소 /최대/중앙값을 알기는 어렵다

#(상자 밖의 선은 최대최소값, 동그라미 이상적인 수치 굵은선은 중앙값, 상자가 높아지면 평균에서 흩어져있다)
boxplot(dist,main='자동차제동거리')

boxplot.stats(dist)
boxplot.stats(dist)$stats  #정상범위 사분위수
boxplot.stats(dist)$n      #관측치 개수
boxplot.stats(dist)$conf   #중앙값 신뢰구간
boxplot.stats(dist)$out   #이상치(특이값)목록


#일변량중 그룹으로 구성된 자료의 상자그림
boxplot(Petal.Length~Species, #그룹으로 묶일것 물결 다음
        data=iris, 
        main='품종별 꽃잎의 길이')
boxplot(iris$Petal.Length~iris$Species,
        main='품종별 꽃잎의 길이') 


#한 화면에 여러 그래프 작성
par(mfrow=c(1,3)) #1X3 가상화면 분할
barplot(table(mtCars$carb,main="C",
              xlab="carburetors",ylab="freg",
              col="blue")
        
        
par(mfrow=c(1,1)) #가상화면 분할해제
