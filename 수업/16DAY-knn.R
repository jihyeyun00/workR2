#분류(classfication)
#지도학습모델=독립변수, 종속변수 둘다 주어짐 
#KNN (K-nearest neighbor) 최근접이웃,유유상종   cf. linear 연속형 / logistic 범주형 regression 도 지도학습임

library(class)
#훈련용/ 테스트용 데이터 준비

tr.idx<-c(1:25,51:75,101:125)  
ds.tr<-iris[tr.idx,1:4]        #훈련용
ds.ts<-iris[-tr.idx,1:4]       #테스트용
cl.tr<-factor(iris[tr.idx,5])     #훈련용 그룹정보
cl.ts<-factor(iris[-tr.idx,5])    #테스트 그룹정보
cl.ts
pred<-knn(ds.tr,ds.ts,cl.tr,k=3,prob = F)  #k=3 3개 체크 prob=테스트 데이터 별 분류 형성 비율 표시여부
pred
acc<-mean(pred==cl.ts)    #최근접이웃 모델에서 cl.ts랑 같은 것을 찾아서 전체평균을 acc에 넣는것 
acc
table(pred,cl.ts)


# cl.ts
# pred         setosa versicolor virginica
# setosa         25          0         0       
# versicolor      0         23         3      #그룹을 나눴는데 3개를 못나눴다는
# virginica       0          2        22


#교차검증방법 (k-fold cross validation) p.
install.packages("cvTools")
library(cvTools)

k=10                           #1번만 하면 오버피팅나올 가능성있음 
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





#예제) knn 이용하는 방법
#문3) 
#mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-최근접 이웃 알고리즘을 이용하여 모델을 만들고 예측 정확도를 측정하시오.
#
# . Sonar 데이터셋에서 마지막에 있는 Class 열이 그룹 정보이다.
# . Sonar 데이터셋에서 홀수 번째 데이터(관측값)를 훈련용 데이터로 하고, 짝수번째 데이터(관측값)를 테스트용 데이터로 한다.
# . k-최근접 이웃에서 k를 3, 5, 7로 다르게 하여 예측 정확도를 비교한다.

# 결측값 제거
ds.new <- Sonar[ complete.cases( Sonar ), ]

# 훈련, 학습 데이터 생성
idx <- seq( 1, nrow( ds.new ), 2 )
x.train <- ds.new[ idx, -61 ]
y.train <- ds.new$Class[ idx ]
x.test <- ds.new[ -idx, -61 ]
y.test <- ds.new$Class[ -idx ]

# 분류 모델
library( class )

pred3 <- knn( x.train, x.test, y.train, k = 3 )
acc3 <- mean( pred3 == y.test )
acc3

pred5 <- knn( x.train, x.test, y.train, k = 5 )
acc5 <- mean( pred5 == y.test )
acc5

pred7 <- knn( x.train, x.test, y.train, k = 7 )
acc7 <- mean( pred7 == y.test )
acc7

# 예측 정확도
#k=3 일 때 : 0.8269231
#k=5 일 때 : 0.75
#k=7 일 때 : 0.7115385



#문4) 
#mlbench 패키지에서 제공하는 Sonar 데이터셋에 대해 k-최근접 이웃 알고리즘을 이용하여 모델을 만들고 예측 정확도를 측정하시오.
#
# . Sonar 데이터셋에서 마지막에 있는 Class 열이 그룹 정보이다.
# . k-최근접 이웃에서 k는 3으로 한다.
# . 5-fold 교차 검증 방법으로 예측 정확도를 측정한다.

data( "Sonar" )                             # 데이터셋 불러오기

# 결측값 제거
ds.new <- Sonar[ complete.cases( Sonar ), ]

library( class )
library( cvTools )        

folds <- cvFolds( nrow( ds.new ), K = 5)    # 5 폴드 생성
acc <- c()                                  # 폴드별 예측정확도 저장용 벡터

for ( i in 1:5 ) {
  # 훈련, 학습 데이터 생성
  idx <- folds$which == i
  x.train <- ds.new[ -idx, -61 ]
  y.train <- ds.new$Class[ -idx ]
  x.test <- ds.new[ idx, -61 ]
  y.test <- ds.new$Class[ idx ]
  
  # 분류 모델
  pred <- knn( x.train, x.test, y.train, k = 3 )
  acc[i] <- mean( pred == y.test )
}

acc
mean( acc )
