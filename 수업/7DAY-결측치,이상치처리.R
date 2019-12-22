#7일차

#데이터 가공 및 전처리(60%소요)-#수집한 데이터가 변수들을 추가적으로 포함시켜야 하는 작업이 필요할 수도 있다

#Data preprocessing
#원시 자료에 대하여 data 정제 /가공을 수행하여 분석에 적합한 형태로 만드는 과정


#1.data 정제
#-결측치에 대한 처리 
#(missing value)->NA:(산술연산이 안됌 데이터를 읽을 수 없다.)
#1)rational Approach :계산식으로 할 수 있느냐 ,예)남여, 를 구할때 주민번호가 있으면 그 것을 이용해서 남여를 구하는 것 
#2)Listwise direction:삭제,그 만큼 데이터 양이 줄어든다. 분석 결과에 영향을 미칠수 있다. 
# 3)prirwise direcntion:상관관계를 따져서 관측치에 임의로 넣는것.원래 데이터가 아니므로 분석에 영향을 미칠수 있다. 
#4)단순 대입법:평균이나 중앙값을 무조건 넣는것
#5)다중 대입법:데이터를 여러번 최소 5~10번 이상 결측치 대상 데이터를 여러개 만들어서 한개를 다시 만듦, 분석의 왜곡을 줄일 수 있음
#1,2,4많이 쓰지만 5번을 요즈음에 씀

#변수=특성(특징, feature)



#결측치 처리

#vector 의 결측치 처리-#벡터일떄, is.na , sum 함수 쓰면 된다.
z<-c(1,2,3,NA,5,NA,8)
sum(z)
is.na(z)    #제어문을 안써도 자동반복: R의 장점:Iterater 자동반복기능
sum(is.na(z)) #false=0 true=2 ,결측치 수를 구해줌
sum(z,na.rm=TRUE)  #na.rm=true z에서 na를 제거하라


#결측치 대체 및 제거
z1<-z
z2<-c(5,8,1,NA,3,NA,7)
z1[is.na(z1)]<-0          #단순대입법, 결측치가 포함된 (벡터에)인덱스에서 true만 찾아서 0으로 대체 시키는 것 

#z1[c('','') , c(1, 3)]  #매트릭스,데이터 프레임 으로 할때 요소가져올때,가로세로나옴  !!!
#z1[]라면 벡터.가로 한줄로 나올때 !!!

#Listwise derection #결측치 삭제
z3<-as.vector(na.omit(z2)) #as.vector벡터로 변환해라
z3




#Matrix/ data frame 결측치 처리:열별 결측치 확인
x<-iris
x[1,2]<-NA
x[1,3]<-NA
x[2,3]<-NA
x[3,4]<-NA
head(x)

#for문 이용
for(i in ncol(x)){
  this.na<-is.na(x[,i])
  cat(colnames(x)[i],
      "\t",sum(this.na),
      "\n")
}

#apply() 이용   apply(data.frame,행/열,함수) /
#               apply(x,2,function(y) sum(is.na(y)))
#                          익명함수
                            
col_na<-function(y){
  return(sum(is.na(y)))
}


na_count<-apply(x,2,col_na)
na_count

na_count<-apply(x,2,
        function(y) sum(is.na(y)))  #익명함수
na_count



barplot(na_count[na_count>0])

install.packages("VIM")
require(VIM)     #library 써도됌

#결측치 자료 조합 확인용 시각화 도구
aggr(x,prop=FALSE,numbers=TRUE)

#두개의 변수간의 결측치 관계 확인 시각화 도구
marginplot(x[c("Sepal.Width","Petal.Width")],pch=20,   
           col=c("darkgray","red","blue"))

marginplot(x[c("Sepal.Width","Sepal.Length")],pch=20,
           col=c("darkgray","red","blue"))



#Matrix/data frame 의 행(data)별 결측치 확인 (행은 col의 집합, 변수)
rowSums(is.na(x))
sum(rowSums(is.na(x))>0) #결측치가 포함되어 있는 행의 수

sum(is.na(x))  #행인지 열인지 구분은 안되지만 결측치는 4개가 있다.




#결측치를 제외한 새로운 데이터셋 생성
head(x)
x[!complete.cases(x),]   #NA가 포함된 행 출력 (결측치를 지우지, 않은 것을 가져온것)
y<-x[complete.cases(x),]  #NA 가 포함되지 않은 행 데이터를 Y에 넣어라
head(y)

summary(x)

#이상치(outier)
# 1.논리적으로 성립되지 않는 값
# 2.상식적으로 용인되지 않는 값-1)(summary 함수해보고) 상자수염시각화 도구


#특이값,이상치(outier)   :대부분 결측치로 변환시킴 
st<-data.frame(state.x77)
summary(st$Income)
boxplot(st$Income)
boxplot.stats(st$Income)$out   #$out :이상치 뽑아내는 함수

#참조(5DAY 에 있는것)
boxplot(dist,main='자동차제동거리')

boxplot.stats(dist)        #전체 (사분위수,관측치 개수,중앙값신뢰구간이상치 목록)뽑아오기
boxplot.stats(dist)$stats  #정상범위 사분위수,상태
boxplot.stats(dist)$n      #관측치 개수
boxplot.stats(dist)$conf   #중앙값 신뢰구간
boxplot.stats(dist)$out   #이상치(특이값)목록


#특이값처리
out.Val<-boxplot.stats(st$Income)$out  
st$Income[st$Income%in%out.Val]<-NA  #%in% 포함되어있다 는 함수
head(st)

newdata<-st[complete.cases(st),] 
head(newdata)
boxplot.stats(newdata$Income)
boxplot(newdata$Income)

#data 정제
# 결측치 처리
# 이상치 처리

