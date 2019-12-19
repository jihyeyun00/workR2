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
결측치 처리
이상치 처리

#data 가공
-정렬
-추가
-필터링
-집계
-병합




#data 가공
#데이터 정렬
#vector 정렬

v1<-c(1,7,6,8,4,2,3)
order(v1)   
v1<-sort(v1)
v1
v1<-sort(v1,decreasing = T)




#matrix/data frame 정렬
head(iris)
order(iris$Sepal.Length)    #첫번째행이 전체행의 14번째
iris[order(iris$Sepal.Length),]   #Ascending  오름차순으로 했을때 첫번째가 14번째가 된다
iris[order(iris$Sepal.Length,decreasing = T),]   #des
iris.new<-iris[order(iris$Sepal.Length),]
head(iris.new)
iris[order(iris$Sepal.Length,decreasing=T,   #정렬기준을 2개 설정
     iris$Sepal.Length),]



#데이터분리

sp<-split(iris,iris$Species)   #품종별로 나누는 함수,(list 로 )
sp
summary(sp)
summary(sp$setosa)
sp$setosa


#데이터 선택
subset(iris,Species=="setosa")
subset(iris,Sepal.Length>7.5)
subset(iris,Sepal.Length>5.1 & Sepal.Width >3.9) 


subset(iris,Sepal.Length>7.6,
       select = c(Petal.Length,Petal.Width))


#데이터 sampling
#숫자를 임의로 추출
#비복원 추출 :다음 샘플링때는 포함되지 않는것 ,R에서는 비복원추출
#복원 추출: 샘플링 포함해서 데이터 추출


x<-1:100
y<-sample(x,size = 10,replace=FALSE)    #sample(대상,임의의 숫자,방법) 비복원 추출 (벡터)
y


#행을 임의로 추출
idx<-sample(1:nrow(iris),size = 50,
            replace = FALSE)
iris.50<-iris[idx,]
dim(iris.50)
head(iris.50)

sample(1:20,size=5)
sample(1:20,size=5)
sample(1:20,size=5)

set.seed(100)      #set.seed(동일한숫자)함수는 샘플함수 전에 나왔기 때문에 동일한 값 나옴
sample(1:20,size=5)
set.seed(100)
sample(1:20,size=5)
set.seed(100)
sample(1:20,size=5)


#데이터 조합
combn(1:5,3)   #combn (무작위로 뽑아서 )조합하는 함수, 콤마뒤에 있는 개수대로 나옴

x=c("red","green","blue","black","white")
com<-combn(x,2)
com

combn(x,2)

for(i in 1:ncol(com)){
  cat(com[,i],"\n")
}

#데이터집계

agg<-aggregate(iris[,-5],              #aggregate 합계 , 총액
               by=list(iris$Species),
               FUN = mean)
agg

agg<-aggregate(iris[,-5],            
               by=list(iris$Species),
               FUN = sd)
agg

head(mtcars)     #??
agg<-aggregate(mtcars,            
               by=list(cyl=mtcars$cyl),
               vs=mtcars$vs,
               FUN = max)
agg

#데이터병합
x<-data.frame(name=c("a","b","c"),    #merge :병합하다
              mat=c(90,80,40))
y<-data.frame(name=c("a","b","d"),
              korean=c(75,60,90) )
z<-merge(x,y,by=c("name"))
z              


merge(x,y)   #변수만 추가                       
merge(x,y,all.x = T)  #변수추가
merge(x,y,all.y = T)  #변수추가
merge(x,y,all=T)      #변수,데이터 추가
#all.x,y는 기준을 정해준것 all은 걍 다

x<-data.frame(name=c("a","b","c"),    #merge :병합하다
              mat=c(90,80,40))
y<-data.frame(sname=c("a","b","d"),
              korean=c(75,60,90) )

merge(x,y,by.x = c("name"),
      by.y = c("sname"))


#dplyr package  -%>% pipe 연산자 (왼쪽 ctrl+ 왼쪽 shift + m)

install.packages("dplyr")       #dplyr 
library(dplyr)




df<-data.frame(var1=c(1,2,1),
               var2=c(2,3,2))
df

#rename () :이름변경
df<-rename(df,v1=var1,v2=var2)
df


#파생변수 추가
df$sum<-df$v1+df$v2
df

df[2,1]<-5
df

df <- data.frame(id = c(1, 2, 3, 4, 5, 6), class = c(1, 1, 1, 1, 2, 2),
                 math = c(50, 60, 45, 30, 25, 50), 
                 english = c(98, 98, 86, 98, 80, 89), 
                 science = c(50, 60, 78, 58, 65, 98))
df


#filter():행추출     df를 class==1 조건에 만족하는 것으로 
df%>%filter(class==1)
df%>%filter(class==2)    
df%>%filter(class!=1)
df%>%filter(class!=2)


df%>%filter(science>70)
df%>%filter(math<50)

df%>%filter(class==1 & math>=50)
df%>%filter(math>=50 | english>=90)
df%>%filter(class==1 %in% c(1,2,5))

class1<-df %>% filter(class==1)    #split 함수는 list 형식으로 추출,파이프는 따로 종류도 
class2<-df %>% filter(class==2)    
class1
class2


#select():변수추출
df %>% select(math)
df %>% select(science)

df %>% select(class,math,science)
df %>% select(-math)


#dplyr 함수 조합
df%>%filter(class==1)%>%select(science)

df%>%select(id,science)%>%head

df%>%select(id,science)%>%sum
df%>%select(science)%>%sum
  
df%>%select(id,science)%>%max

#arrange():정렬
df%>%arrange(science)
df%>%arrange(desc(science))


#mutate():파생변수추가
df%>%
  mutate(total=math+english+science)%>%head
  
           
df%>%
  mutate(total=math+english+science,average=(math+english+science)/3)%>%head

df%>%
  mutate(grade=ifelse(science>=60,'pass','fail'))%>%head

df%>%
  mutate(total=math+english+science,
         average=(math+english+science)/3 )%>%
  mutate(grade=ifelse(science>60,'pass',
         ifesle(average<60, 'fail',
                'normal')))%>%head  

df%>%
  mutate(total=math+english+science,
         average=(math+english+science)/3 )%>%arrange(desc(average))%>%head

df.sort<-df%>%          #필요하다면 변수에 담고
  mutate(total=math+english+science,
         average=(math+english+science)/3 )%>%arrange(desc(average))%>%head
df.sort




#summary():집단별 요약
#group_by:집단별 나누기
df%>%summarise(mean_math=mean(math))

df%>%group_by(class)%>%
  summarise(mean_math=mean(math),
            mean_english=mean(english),
            mean_science=mean(science),
            n=n())                       #n()은 빈도수를 계산해 table이랑 비슷, tibble은 data.frame이랑 비슷
                                            #db= double, 숫자


install.packages("ggplot2")

str(ggplot2::mpg)     # :: 의 의미ggplot2 라는 애가 가지고 있는 mpg 
mpg<-data.frame(ggplot2::mpg)
dim(mpg)
str(mpg)
head(mpg)
View(mpg)


mpg%>%
  group_by(manufacturer,drv)%>%
  summarise(mean_cty=mean(cty))%>%
  head(10)

mpg%>%
  group_by(manufacturer,drv)%>%
  filter(class=='suv')%>%
  mutate(tot=(cty+hwy)/2)%>%
  summarise(mean_tot=mean(tot))%>%
  arrange(desc(mean_tot))%>%
  head(5)


#data 합치기 -공통변수가 있다는 가정하에   ##전체질문
# left_join(): 가로로합치기(변수추가)
# inner_join(): 가로로합치기(변수추가)
# full_join(): 가로로합치기(변수추가)
# bind_rows():세로로 합치기(데이터추가)

df1<-data.frame(id=c(1,2,3,4,5),         #??
                midterm=c(60,80,70,90,85))

df1
df2<-data.frame(id=c(1,2,3,4,5),
                final=c(60,80,70,90,85))
df2
total<-left_join(df1,df2,by="id")
total

df1<-data.frame(id=c(1,2,3),
                address=c("서울","부산","제주"),
                stirngsFactors=F)   #팩터로 하지마
df2<-data.frame(id=c(1,2,4),
               gender=c("남","여","남"))


df_left<-left_join(df1,df2,by='id')
df_left
df_inner<-inner_join(df1,df2,by='id')
df_inner
df_full<-full_join(df1,df2,by='id')
df_full
df_bind<-bind_rows(df1,df2)
df_bind





df1<-data.frame(id=c(1,2,3,4,5),
                test=c(60,80,70,90,85))
df2<-data.frame(id=c(1,2,3,4,5),
                test=c(60,80,70,90,85))

df_all<-bind_rows(df1,df2)
df_all


install.packages("psych")
library(psych)

summary(mtcars)         #요약할때 쓰는 함수
describe(mtcars)

install.packages("descr")
require(descr)

df<-data.frame(id=c(1,2,4),
                gender=c("남","여","남"))
table(df$gender)

freq(df$gender)   #그래프를 자동으로  
freq(df$gender,plot=F)