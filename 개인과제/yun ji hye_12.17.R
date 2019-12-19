# 
library(tidyverse)
library(car)
library(dplyr)



#문1)
# trees 데이터셋에 대해 다음의 문제를 해결하는 R 코드를 작성하시오.
# 
# (1) 나무 둘레(Girth)와 나무의 키(Height)로 나무의 볼륨을 예측하는 다중선형 회귀
# 모델을 만드시오.
# 
head(trees)
str(trees)
newdata1<-trees[,1:3]
head(newdata1)
plot(newdata1,pch=16,col='blue',
     main = 'trees')

tr_model<-lm(Volume~Girth+Height,data=newdata1)
tr_model

# (2) 다중선형 회귀모델을 이용하여 trees 데이터셋의 나무 둘레(Girth)와 나무의 키
# (Height)로 나무의 볼륨을 예측하시오.
#
Volume=(-57.9877)+
    (4.7082*newdata1$Girth)+
    ( 0.3393*newdata1$Height)
Volume    
summary(tr_model)

# (3) (2)에서 예측한 볼륨과 실제 trees 데이터셋의 볼륨(Volume)이 얼마나 차이가
# 나는지 보이시오. (예측값, 실제값, 예측값-실제값을 나타낸다.) 

fitted(tr_model)    
residuals(tr_model) 


#예측갑은 volume 
#실제값은 trees$volume
#예측값-실제값은 volume-trees$volume
# 
# 문2)
# mtcars 데이터셋에서 다른 변수들을 이용하여 연비(mpg)를 예측하는 다중 회귀모델을 만드시오.
# 
# (1) 전체 변수를 이용하여 연비(mpg)를 예측하는 회귀모델을 만들고 회귀식을 나타
# 내시오.
#

head(mtcars)
newdata3<-mtcars[,1:11]
head(newdata3)
mt_model<-lm(mpg~.,data=newdata3)
mt_model

mpg=(12.30337)+
    (-0.11144*newdata3$cyl)+
    (0.01334*newdata3$disp)+
    (-0.02148*newdata3$hp)+
    (0.78711*newdata3$drat)+
    (-3.71530*newdata3$wt)+
    (0.82104*newdata3$qsec)+
    ( 0.31776*newdata3$vs)+
    ( 2.52023*newdata3$am)+
    (0.65541*newdata3$gear)+
    (-0.19942*newdata3$carb)
mpg

# (2) 연비(mpg)를 예측하는 데 도움이 되는 변수들만 사용하여 예측하는 회귀모델을
# 만들고 회귀식을 나타내시오.
# 
library(MASS)
mt_model2<-stepAIC(mt_model)   
summary(mt_model2)
#wt,qsec,am 이 도움이 된다.
mpg=(12.30337)+
    (-3.71530*newdata3$wt)+
    (0.82104*newdata3$qsec)+
    ( 2.52023*newdata3$am)
mpg
summary(mt_model2)
# (3) (1), (2)에서 만든 예측모델의 설명력(Adjusted R-squared)을 비교하시오.
summary(mt_model)
#0.8066
summary(mt_model2)
#0.8336
#더 높아져서 신뢰도있는 데이터가 됌


# 문3) 
# UCLA 대학원의 입학 데이터를 불러와서 mydata에 저장한 후 다음 물음에 답하시오.
# 
 mydata <- read.csv( "https://stats.idre.ucla.edu/stat/data/binary.csv" )
 head(mydata)
# (1) gre, gpa, rank를 이용해 합격 여부(admit)를 예측하는 로지스틱 모델을 만드시오(0: 불합격, 1:합격).
mydata.new<-mydata
mydata.new
str(mydata.new)
head(mydata.new) 

my_model<-glm(admit~.,data=mydata.new)
my_model

# (2) mydata에서 합격 여부(admit)를 제외한 데이터를 예측 대상 데이터로 하여 (1)에서 만든 모델에 입력하여 
# 합격 여부를 예측하고 실제값과 예측값을 나타내시오. 무슨말??
# 
#선생님
#예측값
pred <- predict( mydata_model, mydata[ , c( 'gre', 'gpa', 'rank' ) ] )
pred
# one-hot encoding
pred <- round( pred, 0 )
pred

# 예측값, 정답
result <- data.frame( predict = pred, answer = mydata$admit )
result

#(3) 만들어진 모델의 예측 정확도를 나타내시오.
acc <- mean( result$predict == result$answer )
acc

#예측 정확도 : 0.705


#내가 한것
myzero<-mydata.new%>%filter(admit==0)  
myzero

admit=(-0.1824127)+
    (0.0004424*myzero$gre)+
    (0.1510402*myzero$gpa)+
    ( -0.1095019*myzero$rank)
admit
pred<-predict(my_model,myzero)
pred
pred<-round(admit,0)
pred

#예측값이 admit, 실제값은 mydata$admit ??


# (3) 만들어진 모델의 예측 정확도를 나타내시오.  ??
       
answer<-mydata.new$admit
pred==answer
acc<-mean(pred==answer)
acc

#67%........................예측정확도일까요?.....


