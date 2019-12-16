#14일차 

#통계341~365/371~/489~
#단순선형 회귀분석(simple linear rearession analysis)
#
#
#예)특정정류장의 하차예측->정류장별 하차인원 데이터로 파악->이유찾기(예, 10%하차한다 왜?)->
#data model :예측,분류, 군집화 
#
#Modeling :Model 을 만들어내는 과정. 
#data model:현실세계를 표현하는 수식을 도출하는 과정(수식:통계적 관점으로 데이터 모델링)
#y=wx+b (b:특성, y:반응변수,x:독립변수/ w,b 는 매개변수(w와 b에 따라서 y를 예측 함) 



#model 과정
#1.model 선택 :수식 결정 y=wx+b  (weight :기울기 ,bias 경향) :회귀식: 연속형 데이터 !! 
#2.실제 데이터(x,y)를 이용하여 w,b(상수) 값을 결정 ->훈련한다. 훈련과정
#3.실제 데이터를 통한 예측
#4.model 평가


#단순 선형 회귀 분석:y=wx+b 예측에 사용 !! (산점도에서 자료가 분포 되었을 때, 오차가 작은 선을 그리는)  / 다중 선형회귀 분석
#modeling :현실 세계에서 일어나는 현상을 수학식으로 표현하는 행위
#데이터 과학에서 
#독립변수 x 를 설명변수(explanatory variable),특징(feature)
#종속변수 y 를 반응 변수 (response variable), 레이블(label)
                                     #(훈련 data)
#데이터 과학에서 modeling 이란 수집한 data를 이용하여 최적의 모델을 찾아내는 과정



#최적의 모델을 찾는 과정
#모델:y=wx+b->w, b 를 계수 (매개변수)

#1.모델 선택->선형 방적식 선택
#2.주어진 data(훈련data)를 적용하여 매개변수 결정
# ->lm()
#3.예측은 훈련 data에 없는 새로운 data로 모델이 레이블을 추정하는 과정
#  ->predict()
#완성된 모델에 대한 품질 평가 
#  ->summary() :결과 이해


#회귀분석(Regression analysis)
#관찰된 연속형 변수들에 대해 두 변수 사이의 모형을 구한뒤 적합도를 측정해 내는 분석 방법

#시간에 따라 변화하는 데이터나 어떤 영향, 가설적 실험,인과 관계의 모델링들의 예측에 이용될 수 있다., 

#단순 선형회귀분석 (simple linear regression analysis)
#독립변수와 종속변수 와의 관계가 선형으로 표현, 
#하나의 독립변수를 다루는 분석방법

#단순선형 회귀 모델의 회귀식: y=wx+b (w,b 는 상수)
#                    w,b 는 어떻게 찾을 수 있을까?
#                    x,y 로 구성된 data를 이용하여 w,b 를 찾아내는 모델


#단순선형 회귀분석:하나의 독립변수 
#                 :예측 model
#주행거리와 제동거리 사이의 회귀모델
str(cars)
head(cars)

#산점도를 통한 선형관계 확인
plot(dist~speed,data=cars)
plot(cars)

#회귀모델 구하기 :예)판매량 , 제주도 학력에 따른 취업률 예측
#종속(반응)변수~독립(설명)변수 순서로 "지정" : #x 축(speed) 기준으로 했을 때, y축 (dist) 

model<-lm(dist~speed,cars)  #첫번째 인수가 y 값, ~뒤에 x 값 
model

# Call:
# lm(formula = dist ~ speed, data = cars)
# 
# Coefficients: ->계수, 매개변수
# (Intercept)        speed  
#     -17.579        3.932  
#        b값          w값
#결과해석 y=3.932*x -17.579
abline(model)


#관련함수 

coef(model)      #매개변수(계수)-w,b 값 출력
cars
fitted(model)    #훈련 data에 있는 샘플에 대한 예측값 
residuals(model)  #잔차 : 회귀식으로 추정된 값과 실제 값과의 차이 (오차)

#잔차제곱합을 평균 제곱 오차(MES-mean squarde error)로 변환
deviance(model)/length(cars$model)


b<-coef(model)[1]
w<-coef(model)[2]
speed<-21.5
dist<-w*speed+b
dist

#확인인가?
df<-data.frame(speed=c(21.5,25.0,25.5,26.0,26.5,27.0,27.5,28.0))
predict(model,df)                              #lm 함수, 데이터프레임(스피드값 넣어준)
plot(df$speed,predict(model,df),col='red',     #데이터프레임(스피드값 넣어준)$구하려는 설명변수, predict(lm 함수, 데이터 프레임                                                 #(스피드값 넣어준 )
     cex=2,pch=20)
#pridict r에서 제공하는 예측수행함수->plot 에 넣어서 확인

abline(model)


speed<-cars[,1]
pred<-w*speed+b
pred

compare<-data.frame(pred,cars[,2],
                    pred-cars[,2])
compare

colnames(compare)<-c('예상','실제','오차')

head(fitted(model),3)   #예측
head(residuals(model),3) #추정된 값과의 차이
head(compare,3)


summary(model)

#평균은 클수록, 분산은 작을 수록, 데이터 크기가 클수록 믿음이 커진다.->t-통계량 (t-statistics)/t-값(t-value)
#t-값이 크면 대립가설에 대한 믿음이 강해진다. 
#t-값이 작으면 대립가설에 대한 믿음이 약해진다. 
#데이터를 통해 '대립가설이 통계학적으로 유의미하다'라는 것을 증명하고 확인하는 작업을 t-검정(t-test)라 한다.

#'귀무가설이 참이라고 가정했을 때, 표본으로 부터 얻어지는 통계치가 나타날 (관측될) 확률'을 계산하는 데 이때
#계산된 확률값을 p 값이라 한다. 
#p 값이 매우 낮으면, 이러한 표본 통계값을 우연히 나타나기 어려운 케이스이기 때문에, 우리는 귀무가설을 채택하지
#않고 (기각하고), 대안적인 가설 , 즉 대립가설을 채택한다. 


str(cars)
head(cars)
car_model<-lm(dist~speed, data=cars)
coef(car_model)
plot(car_model); abline(car_model,col='red')
summary(car_model)

# Coefficients:
#          Estimate Std.    Error   t value   Pr(>|t|)    
# (Intercept) -17.5791     6.7584  -2.601    0.0123 *       #0.05 기준으로,아래 데이터 보다 회귀선에서 퍼져있다.
#     speed    3.9324     0.4155   9.464     1.49e-12 ***   #별이 많을 수록 영향을 많이 미침(스피드가 종속변수(제동거리)에 영향을 미침)

#공학용 계산기 e*1.09 에 -12승. cars 의 p 값은 0.8333..
str(women)
head(women)
women_model<-lm(weight~height,data=women)
coef(women_model)
plot(women_model);
abline(women_model,col='red')
summary(women_model)

#공학용 계산기 e*1.09 에 -14승. cars 의 p 값은 0.07...
# Coefficients:
#          Estimate Std.     Error    t value        Pr(>|t|)    
# (Intercept) -87.51667    5.93694  -14.74         1.71e-09 ***   #데이터가 회귀선에 모여있다.
#     height        3.45000    0.09114   37.85     1.09e-14 ***