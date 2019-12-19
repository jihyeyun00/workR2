# 문1)
# R에서 제공하는 state.x77 데이터셋에 대해 k-평균 군집화를 실시하고 결과를 그래프로 출력하시오.
# 
# • 군집의 수는 5로 한다.
# • state.x77은 각 변수(열)의 값들의 단위의 차이가 많이 나기 때문에 0~1 표준화를 실시한 후 군집화를 실행한다.
# 
library(cluster)
head(state.x77)
str(state.x77)
my_st<-state.x77
my_st

#k평균실시
st_mean<-kmeans(x=my_st,center=5)
st_mean

#표준화

std<-function(x){
    return((x-min(x))/(max(x)-min(x)))
}

mydata<-apply(my_st,2,std)
st_mean<-kmeans(x=mydata,center=5)
st_mean

#표준화 실시후 군집화 시행

clusplot(mydata,        #군집대상
         st_mean$cluster,   #군집번호
         color=TRUE,    #원의 색
         shade=TRUE,    #원의 빗금표시 유무
         labels=2,      #관측값 출력 형태
         lines=1)       #중심선 연결 표시


# 문2)
# mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-평균 군집화를 실시하고 결과를 그래프로 출력하시오.
# 
# • 군집의 수는 2로 한다.
# • Sonar 데이터셋에서 마지막에 있는 Class 열은 제외하고 군집화를 실행한다.
# 
library( mlbench )
data( "Sonar" ) 			# 데이터셋 불러오기

head(Sonar)
str(Sonar)
son<-Sonar[,1:60]

#표준화 안했을 때
so_mean<-kmeans(x=so,center=2)
so_mean

clusplot(son,        
         so_mean$cluster,   
         color=TRUE,   
         shade=TRUE,    
         labels=2,      
         lines=1) 


#표준화 해도 같음
std<-function(x){
    return((x-min(x))/(max(x)-min(x)))
}
mysonar<-apply(Sonar[,1:60],2,std)
so_mean<-kmeans(x=mysonar,center=2)
so_mean

clusplot(mysonar,        
         so_mean$cluster,   
         color=TRUE,   
         shade=TRUE,    
         labels=2,      
         lines=1) 

# 문3) 
# mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-최근접 이웃 알고리즘을 이용하여 모델을 만들고 예측 정확도를 측정하시오.

library(class)
# 
# . Sonar 데이터셋에서 마지막에 있는 Class 열이 그룹 정보이다.
# . Sonar 데이터셋에서 홀수 번째 데이터(관측값)를 훈련용 데이터로 하고, 짝수번째 데이터(관측값)를 테스트용 데이터로 한다.
# . k-최근접 이웃에서 k를 3, 5, 7로 다르게 하여 예측 정확도를 비교한다.

tr.idx <- seq(1,nrow(Sonar),2)
tr.idx

trial_r<-Sonar[tr.idx,1:60]
trial_t<-Sonar[-tr.idx,1:60]
cl_r<-factor(Sonar[tr.idx,61])
cl_t<-factor(Sonar[-tr.idx,61])
pred_k<-knn(trial_r,trial_t,cl_r,k=3,prob = TRUE)
pred_k

acc<-mean(pred_k==cl_t)   
acc

table(pred_k,cl_t)

# 문4) 
# mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-최근접 이웃 알고리즘을 이용하여 모델을 만들고 예측 정확도를 측정하시오.
# 
# . Sonar 데이터셋에서 마지막에 있는 Class 열이 그룹 정보이다.
# . k-최근접 이웃에서 k는 3으로 한다.
# . 5-fold 교차 검증 방법으로 예측 정확도를 측정한다.

install.packages("cvTools")
library(cvTools)

k=3                           #1번만 하면 오버피팅나올 가능성있음 
folds<-cvFolds(nrow(iris),K=k) #폴드 생성 , 10번 훈련하겠다
acc<-c()                       #폴드별 예측 정확도 저장용 벡터
for(i in 1:k){
    ts.idx<-folds$which==i
    ds.tr<-iris[-ts.idx,1:4]
    ds.ts<-iris[ts.idx,1:4]
    cl.tr<-factor(iris[-ts.idx,5])
    cl.ts<-factor(iris[ts.idx,5])
    pred<-knn(ds.tr,ds.ts,cl.tr,k=5)
    acc[i]<-mean(pred==cl.ts)   #예측 정확도
}
acc  #폴드별 예측 정확도
mean(acc)   #폴드 평균 예측 정확도
