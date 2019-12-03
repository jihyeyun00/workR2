#6일차

#다중변수 자료탐색- 1.상관관계로 이해한다. 산점도 쓴다.
#두변수 사이의 산점도

#산점도 (scatter plot)-2변수로 구성된 자료의 분포를 알아보는 그래프
#                      관측값들의 분포를 통해 2변수 사이의 관계파악 
#                       matrix or data frame 을 사용한다.
#                       상관관계 파악


wt<-mtcars$wt
mpg<-mtcars$mpg

plot(wt,mpg,main = "중량-연비 그래프",
     xlab="중량", ylab="연비",
     col="red",pch=19)  #0~25 까지 쓸수있,음의 상관관계, 양의 상관관계

#여러변수들 간의 산점도 
vars<-c("mpg","disp","drat","wt")
target<-mtcars[,vars]
head(target)
pairs(target,main="multi plots") #다변량일때 산점도 그리는 방법


plot(wt,mpg,main = "중량-연비 그래프",
     xlab="중량", ylab="연비",
     col="red",pch=19)

plot(mtcars$wt~mtcars$mpg,    #직접지정
     main = "중량-연비 그래프",
     xlab="중량", ylab="연비",
     col="red",pch=19)

plot(mtcars[,c("wt","mpg")],  #요소지정
     main = "중량-연비 그래프",
     xlab="중량", ylab="연비",
     col="red",pch=19)

plot(mpg~wt,data=mtcars,   #물결로 
     main = "중량-연비 그래프",
     xlab="중량", ylab="연비",
     col="red",pch=19)



#그룹정보가 있는 두 변수의 산점도 #(범주형으로 그룹으로 묶어서 보는것)

iris.2<-iris[,3:4]
point<-as.numeric(iris$Species) #as.numeric 함수를 숫자형으로 바꿈
point
color<-c("red","green","blue")
plot(iris.2, main = "Iris plot", #iris.2 에 행렬이 들어가 있음
     pch=c(point),              #pch 점의 모양
     col=color[point])

#상관계수 값 -1<= r <= 1   r= 상관계수, 0.5보다 높거나, 낮을때 상관관계가 높다.
# 마이너스에 있을때, 음의 상관관계 (떨어지는 사선)
# 양수에 있을 때, 양의 상관관계 (올라가는 사선)




#상관분석
beers<-c(5,2,9,8,3,7,3,5,3,5)
bal<-c(0.1,0.03,0.19,0.12,0.04,0.0095,0.07,0.06,0.02,0.05)
tbl<-data.frame(beers,bal)
tbl

plot(bal~beers,data=tbl)  #1)산점도

res<-lm(bal~beers,data=tbl) #2)회귀식 :y= xw + b :y=종속변수, x=독립변수, w=weigth, b=bias 
res

abline(res) #3)회귀선 ,lm을 해야 나옴

cor(tbl[,1:2])  #4)상관 계수  cor(mtcars,mpg)와 같은 형식으로도 할 수 있음
cor(iris[,1:4]) #5)다변량으로 상관계수  -0.5 보다크면 0.5보다 크면 




#상관분석 순서
# 1.상관분석 대상 변수 선정
# 2.산점도 작성(관측값 분포 확인):plot()
# 3.회귀식 도출 :lm()
#    (회귀식 : 두변수의 선형관계를 가장 잘 나타낼수 있는 선의 식)
#    (y=xw + b)
# 4.회귀선을 산점도에 표시 :abline()
#    (회귀선:관측값들의 추세를 가장 잘 나타낼 수 있는 선)
# 5.상관계수 계산:cor()
# 6.상관분석 결과 해석 -가장 중요!!!


#다중변수 2.시계열 data 분석 -선그래프 
#(범주형 :막대, 파이, #연속형:히스토, 상자수염, #상관분석:산점도, #시계열 분석: 선그래프)


#시계열 data -선그래프 , x 축이 시간
month<-1:12
late<-c(5,8,7,9,4,6,12,13,8,6,6,4)

plot(month, late, main="지각생통계",  #lwd = 선의 두께 ,#lty= 선의 모양
     type="l",  lty=1,  lwd=1,        #month :행  late :열
     xlab="Month",  ylab="late cnt")
plot(month, late, main='지각생통계',
       type="b", lty=1, lwd=1,
       xlab="Month", ylab="late cnt")

plot(month, late, main='지각생통계',
       type="o", lty=1, lwd=1,
       xlab="Month", ylab="late cnt")

plot(month, late, main='지각생통계',
       type="s", lty=1, lwd=1,
       xlab="Month", ylab="late cnt")


#복수의 선 그래프
late1<-c(5,8,7,9,4,6,12,13,8,6,6,4)
late2<-c(4,6,5,8,7,8,9,10,11,6,5,7)

plot(month, late1, main='지각생통계',
       type="b", lty=1, col="red",
       xlab="Month", ylab="late cnt",
       ylim=c(1,15))
lines(month,late2,type = "b",lty=1,   
      col="blue")


#자료 탐색 실습-탐색적 데이터 분석

#0단계: 문제 정의
#1단계: 분석 대상 데이터셋 준비

#       BostonHousing 데이터셋(mlbench pac.)
install.packages("mlbench")
library(mlbench)
data("BostonHousing")
myds<-BostonHousing[,c("crim",
                        "rm",
                        "dis",
                        "tax",
                        "medv")] ;myds
#crim : 1인당 범죄율
# rm:주택 1가구당 방수
# dis:보스턴 5개 지역 센터 까지의 거리
# tax:재산세율
# medv:주택가격

class(BostonHousing)
dim(BostonHousing)
str(BostonHousing)
head(BostonHousing)
tail(BostonHousing)
myds<-BostonHousing[,c("crim",
                       "rm",
                       "dis",
                       "tax",
                       "medv")] ;myds



#2단계:파생변수 추가: grp 변수 추가(주책 가격 상중하)  값이 나옴
grp<-c()
for (i in 1:nrow(myds)) {
        if(myds$medv[i]>=25.0){
           grp[i]<-'H'
        }else if (myds$medv[i]<=17.0){
                grp[i]<-'L'
                
        }else {
                grp[i]<-'M'
        }
}



grp<-factor(grp)
grp<-factor(grp,levels=c("H","M","L"))
myds<-data.frame(myds,grp)
head(myds)


#3단계: 데이터셋 형태와 기본적인 내용 파악
str(myds)
head(myds)
table(myds$grp)

#4단계:히스토그램에 의한 관측값의 분포 확인, par=가상화면화면 분할
par(mfrow=c(2,3))
for(i in 1:5){
        hist(myds[,i],
             main=colnames(myds)[i],
             col = "yellow")
}
par(mfrow=c(1,1)) #가상화면 복귀


#5단계:상자그림에 의한 관측값의 분포 확인 ?? 어떻게 읽냐

par(mfrow=c(2,3))
for(i in 1:5) {
        boxplot(myds[,i],
             main=colnames(myds)[i])}

par(mfrow=c(1,1)) #가상화면 복귀

#6단계:그룹별 관측값 분포 확인 ?? 값 안나옴
boxplot(myds$crim~myds$grp,
        main="1인당 범죄율")
boxplot(myds$rm~myds$grp,
        main="방의 개수")

#7단계:다중 산점도를 통한 변수간 상관 관계 확인
pairs(myds[,-6])


#8단계:그룹 정보를 포함한 변수 간 상관 관계 확인

point<-as.interger(myds$grp)
color<-c("red","green","blue")
pairs(myds[,-6],pch=point,
      col=color[point])

#9단계:변수 간 상관 계수 확인
cor(myds[,-6])


#Data 이해:1.data set 에 대한 이해 
          # 2.문제 정의 검증
          # 3.문제 정의에 대한 1차 결과 파악