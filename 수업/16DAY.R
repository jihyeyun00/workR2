#16일차 p.512~/537~/

#군집화 (clustering)/ 분류(classfication)

#군집화 :주어진 대상 데이터들을 유사성이 높은 것끼리 묶어주는 기술
#         (군집, 범주,그룹)
#k-means(평균) 군집화 알고리즘

mydata<-iris[,1:4]             #품종이 없다고 가정.
fit<-kmeans(x=mydata,center=3)
fit

#K-means clustering with 3 clusters of sizes 33, 21, 96->3개 군집에 속한 데이터개수
# Cluster means:   ->3개 군집의 중심점 좌표
#     Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1 군집번호    5.175758    3.624242     1.472727   0.2727273 #1번 군집의 중심점 좌표
# 2 군집번호    4.738095    2.904762     1.790476   0.3523810
# 3 군집번호    6.314583    2.895833     4.973958   1.7031250

# Clustering vector: 첫번째는 1번 군집, 두번째는 2번 군집
# [1] 1 2 2 2 1 1 1 1 2 2 1 1 2 2 1 1 1 1 1 1 1 1 1 1 2 2 1 1 1 2 2 1 1 1 2 1 1 1 2 1 1 2 2 1 1 2 1 2
# [49] 1 1 3 3 3 3 3 3 3 2 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3
# [97] 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# [145] 3 3 3 3 3 3


# Available components:
#     
#1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
# [7] "size"         "iter"         "ifault"      


fit$cluster     #벡터여서 되는 것 
fit$centers    #데이터 프레임이어서 안되는 것  

library(cluster)
clusplot(mydata,        #군집대상
         fit$cluster,   #군집번호
         color=TRUE,    #원의 색
         shade=TRUE,    #원의 빗금표시 유무
         labels=2,      #관측값 출력 형태
         lines=1)       #중심선 연결 표시
subset(mydata,fit$cluster==2)    #mydata에서 2번 군집에 있는 것을 뽑아라



#대상 데이터 표준화 후 군집화

#데이터와 데이터의 거리를 계산할때 발생하는 문제
#모든 변수가 거리 계산에 동등한 영향을 갖도록 하기 위해서 모든 변수의 자료 범위를 0~1사이로 
#표준화 한 후 거리 계산을 한다.

#(x-min(A))/(max(A))-min(A))
# x:변수 A의 임의의 관측값
# max(A),min(A)는 변수 A 관측값중 최대/최소값

std<-function(x){
    return((x-min(x))/(max(x)-min(x)))
}
mydata<-apply(iris[,1:4],2,std)
fit<-kmeans(x=mydata,center=3)
fit
#아래처럼 나옴
# K-means clustering with 3 clusters of sizes 50, 39, 61
# 
# Cluster means:
#     Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1    0.1961111   0.5950000   0.07830508  0.06083333
# 2    0.7072650   0.4508547   0.79704476  0.82478632
# 3    0.4412568   0.3073770   0.57571548  0.54918033
# 
# Clustering vector:
#[1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#[49] 1 1 2 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# [97] 3 3 3 3 2 3 2 2 2 2 3 2 2 2 2 2 2 3 2 2 2 2 2 3 2 3 2 3 2 2 3 3 2 2 2 2 2 3 3 2 2 2 3 2 2 2 3 2
# [145] 2 2 3 2 2 3
# 
# Within cluster sum of squares by cluster:
#     [1] 1.829062 2.073324 3.079830
# (between_SS / total_SS =  83.0 %)
# 
# Available components:
#     
# [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
# [7] "size"



#KNN (K-nearest neighbor) 최근접이웃

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

