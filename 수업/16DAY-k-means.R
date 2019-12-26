#16일차 p.512~/537~/

#군집화 (clustering)

#군집화 :주어진 대상 데이터들을 유사성이 높은 것끼리 묶어주는 기술
#         (군집, 범주,그룹)
#k-means(평균) 군집화 알고리즘

mydata<-iris[,1:4]             #품종이 없다고 가정.
fit<-kmeans(x=mydata,center=3)
fit

#K-means clustering with 3 clusters of sizes 33, 21, 96        #3개 군집에 속한 데이터개수
# Cluster means:                                               #3개 군집의 중심점 좌표
#     Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1 5.175758    3.624242     1.472727   0.2727273              #1번 군집의 중심점 좌표
# 2 4.738095    2.904762     1.790476   0.3523810              #2번 군집의 "
# 3 6.314583    2.895833     4.973958   1.7031250              #3번 "

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

#데이터와 데이터의 거리를 계산할때 발생하는 문제 예)키와 몸무게의 단위가 다를때 
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




#예제 k-means이용하는 방법
#문1)
#R에서 제공하는 state.x77 데이터셋에 대해 k-평균 군집화를 실시하고 결과를 그래프로 출력하시오.
#
#• 군집의 수는 5로 한다.
#• state.x77은 각 변수(열)의 값들의 단위의 차이가 많이 나기 때문에 0~1 표준화를 실시한 후 군집화를 실행한다.

# 0~1 표준화 함수
std <- function( x ) {
    result <- ( x - min( x ) ) / ( max( x ) - min( x ) )
    return( result )
}

# 데이터 표준화
ds.new <- std( state.x77 )
head( ds.new )

# 군집화
fit <- kmeans( x = ds.new, centers = 5 )
fit

# 차원 축소 후 군집 시각화
library( cluster )

clusplot( ds.new, fit$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0 )


