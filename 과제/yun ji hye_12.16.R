# 문1)
# state.x77 데이터셋에서 문맹률(Illiteracy)을 이용해 범죄율(Murder)을 예측
# 하는 단순선형 회귀모델을 만드시오. 그리고 문맹률이 0.5, 1.0, 1.5일 때 범
# 죄율을 예측하여 보시오.
# 
y=wx+b
str(state.x77)
st<-data.frame(state.x77)
plot(Murder~Illiteracy,data=st)
plot


state<-lm(Murder~Illiteracy,st)
state
coef(state)

y=w*x+b
y=4.257*Illiteracy+2.397

df<-data.frame(Illiteracy=c(0.5,1.0,1.5))
predict(state,df)
plot(df$Illiteracy,predict(state,df),col='red',    #pridict r에서 제공하는 예측수행함수->plot 에 넣어서 확인
     cex=2,pch=20)

abline(state)

summary(state)
# 문2)
# trees 데이터셋에서 나무둘레(Girth)로 나무의 볼륨(Volume)을 예측하는 단
# 선형 회귀모델을 만드시오. 그리고 나무 둘레가 8.5, 9.0, 9.5일 때, 나무의
# 볼륨(Volume)을 예측하여 보시오.
# 

trees
y=wx+b
str(trees)
plot(Volume~Girth,data=trees)
plot


tr1<-lm(Volume~Girth,trees);tr1
tr1

df<-data.frame(Girth=c(8.5,9.0,9.5))
predict(tr1,df)
plot(df$Girth,predict(tr1,df),col='red',    #pridict r에서 제공하는 예측수행함수->plot 에 넣어서 확인
     cex=2,pch=20)



# 문3) 
# pressure 데이터셋에서 온도(temperature)로 기압(pressure)을 예측하는 단
# 순선형 회귀모델을 만드시오. 그리고 온도가 65, 95, 155일 때 기압을 예측
# 하여 보시오.
pressure
str(pressure)
plot(pressure~temperature,pressure)

pr<-lm(pressure~temperature,pressure)
pr

y=1.512*x-147.899

temp<-data.frame(temperature=c(65,95,155));temp
predict(pr,temp)
plot(temp$temperature,predict(pr,temp),col='red',    #pridict r에서 제공하는 예측수행함수->plot 에 넣어서 확인
     cex=2,pch=20)
abline(press)