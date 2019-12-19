#15일차  p.491~512
#다중선형 회귀분석(multiple linear regression analysis)
#lm 하고 바로 smmary 함수 써도 됌
#다중선형 회귀모델:여러개의 독립변수를 다루는 회귀모델

#회귀식
#y=B0+B1X1+B2X2+B3X3+.....+BnXn

#독립변수가 n개인 다중선형 회귀에서 주어진 자료를 이용해 
#B0,B1,B2,B3,....,Bn 의 값을 알아내는 회귀모델
library(tidyverse)
library(car)
library(dplyr)

str(Prestige)
head(Prestige)

newdata<-Prestige[,c(1:4)]
head(newdata)
plot(newdata,pch=16,col='blue',
     main = 'matrix scatterplot')

#확인했음. 모델을 만들어봄
model<-lm(income~education+prestige+women,
          data = newdata)
model

coef(model)

# Call:
#     lm(formula = income ~ education + prestige + women, data = newdata)
# 
# Coefficients:
#     (Intercept)    education     prestige        women  
#     -253.8         177.2        141.4          -50.9  
#      B0             B1           B2              B3



#회귀식
income=(-253.8)+
       ( 177.2*newdata$education)+
       (141.4*newdata$prestige)-
        (50.9*newdata$women)
income

fitted(model)    #회귀선에 넣었을때의 예측값
residuals(model) #회귀선과 실제값의 오차
deviance(model) #오차제곱의 총합
deviance(model)/length(newdata$education) #dev (오차의)제곱한 후 len (개수)의 나누기=평균 :평균제곱오차

summary(model)

# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -253.850   1086.157  -0.234    0.816    
# education    177.199    187.632   0.944         0.347    
# prestige     141.435     29.910   4.729         e-06 ***      #영향높음
# women        -50.896      8.556  -5.948     4.19e-08 ***     #영향높음

#Adjusted R-squared:  0.6323  0~1 , 1에 가까워 질수록 예측값이 실제값과 차이가 적어짐. 현실설명력이 높아진다. (약 63%) 
#education 은 필요가 상관도가 떨어진다는 결론. 모델을 만들때 필요가 없다는 결론
#변수 여러개가 반드시 영향을 미치는 건 아니다는 결론

Prestige
newdata2<-Prestige[,c(1:5)]
model2<-lm(income~.,data=newdata2)  
summary(model2)

library(MASS)
model3<-stepAIC(model2)   
summary(model3)

# Start:  AIC=1607.93
# income ~ education + women + prestige + census
# 
# Df Sum of Sq       RSS    AIC
# - census     1    639658 649654265 1606.0
# - education  1   5558323 654572930 1606.8
# <none>                   649014607 1607.9
# - prestige   1 143207106 792221712 1626.3
# - women      1 212639294 861653901 1634.8
# 
# Step:  AIC=1606.03
# income ~ education + women + prestige
# 
# Df Sum of Sq       RSS    AIC
# - education  1   5912400 655566665 1605.0
# <none>                   649654265 1606.0
# - prestige   1 148234959 797889223 1625.0
# - women      1 234562232 884216497 1635.5
# 
# Step:  AIC=1604.96
# income ~ women + prestige
# 
# Df Sum of Sq        RSS    AIC
# <none>                   655566665 1605.0
# - women     1 234647032  890213697 1634.2
# - prestige  1 811037947 1466604612 1685.1


# > summary(model3)

# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     431.574       807.630  0.534   0.594    
#     women        -48.385      8.128  -5.953   4.02e-08 ***
#     prestige     165.875     14.988  11.067  < 2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Multiple R-squared:   0.64,	Adjusted R-squared:  0.6327    #처음에 Adjusted R-squared:   0.6289 더 높아져서 신뢰도있는 데이터가 됨





#범주형: 결과값이 범주형으로 나와야 함 예)남, 여인가, 어떤 품종인가
#Linear Regession 연속형 데이터에 대한 예측
#Logistic Regession : (로지스틱 회귀분석) :one hot encoding     #예)스팸or No
#                   :회귀모델에서 종속변수의 값의 형태가 범주형인 경우의 에측모델
#                   :주어진 데이터로 부터 어떤 범주를 예측하는 분야를 회귀와 data로부터 어떤 범주를 예측하는 분야를 
#                   회귀와 구분하여 분류(classification) 이라고 한다. 

#                   로지스틱 회귀도 기본적으로 회귀기법이기 때문에 종속변수는 숫자로 표현되어야 한다. 
#                   예)yes 와 no 는 0과 1로 setosa, versicolor, virginica 는 1,2,3과 같은 숫자로 바꾼 후에 로지스틱 회귀적용
#
iris.new<-iris
iris.new$Species<-as.integer(iris.new$Species)
head(iris.new)

iris_model<-glm(Species~.,data = iris.new)  #glm 로지스틱 함수 ,y값을 speccies 로 범주형이어야 한다. ~.은 나머지 모두
iris_model
coef(iris_model)
summary(iris_model)

unknown<-data.frame(rbind(c(5.1,3.5,1.4,0.2)))
names(unknown)<-names(iris)[1:4]
unknown

pred<-predict(iris_model,unknown)  
pred

pred<-round(pred,0) #반올림 함수

levels(iris$Species)[pred]    

#setosa 가 나옴 #결국 어떤 꽃인지 알아내는 것

#test 
test<-iris[,1:4]       
pred<-predict(iris_model,test)    
pred<-round(pred,0)

answer<-as.integer(iris$Species)
pred==answer
acc<-mean(pred==answer)
acc


#예제문제
#문3) 
#UCLA 대학원의 입학 데이터를 불러와서 mydata에 저장한 후 다음 물음에 답하시오.

mydata <- read.csv( "https://stats.idre.ucla.edu/stat/data/binary.csv" )

#(1) gre, gpa, rank를 이용해 합격 여부(admit)를 예측하는 로지스틱 모델을 만드시오(0: 불합격, 1:합격).
str( mydata )
head( mydata )

mydata_model <- glm( admit~gre + gpa + rank, data = mydata )

coef( mydata_model )
summary( mydata_model )

#(2) mydata에서 합격 여부(admit)를 제외한 데이터를 예측 대상 데이터로 하여 (1)에서 만든 모델에 입력하여        (합격여부에 상관없이)
#합격 여부를 예측하고 실제값과 예측값을 나타내시오.

# 예측값
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
