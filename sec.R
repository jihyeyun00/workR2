
# break/next
# 
#sum <- 0
for (i in 1:10) {
  sum <- sum + i 
  if (i <- 5) {
    break  #멈춤
  }
}
sum


sum <- 0
for (i in 1:10) {
  if (i %% 2 == 0){
    next    #걸러냄
}
 sum <- sum + i 
}
 sum


#산술내장함수
 
log(10)+5           #로그함수
log(10, base = 2)    
sqrt(25)            #제곱근
max(5,3,2)           #가장 큰 수           
min(3,9,5)            #가장 작은 수
abs(-10)             #절대값
factorial(5)          #팩토리얼
sin(pi/2)             #삼각함수


#사용자정의함수
#최대값구하는 함수
mymax <- function(x,y) {      #함수정의
  num.max <- x
  if(y>num.max){
    num.max <- y
  }
  return(num.max)
} 

mymax(10,15)                  #함수 호출
a <- 10
b <- 5
c <- 8
max <- mymax(a,b)
max <- mymax(max,c)
max

#사용자 정의함수 매개변수 초기값 설정
#나눗셈 함수
mydiv <- function(x,y=2){   #디폴트 값 y=2, y값이 없을 때 2를 쓰겠다.
  result <- x/y             #y값먼저 오른쪽에서 
  
  return(result)
}

mydiv(x=10, y=3)  #인수 값을 넣을 수도 있고 아닐수도 있고
mydiv(10,3)
mydiv(10)
  
  
  


score <- scan()
result <- "노력"
if(score >=85){
  result <- "우수"
}
cat("당신 ", result, score)





#외부 파일에 있는 함수 호출
setwd("D:/WorkR")         #경로지정
source("mylib.R")         #lib 파일 지정
 
#함수 호출

my_max(10,5)
my_div(8,2)


#vector 도입

a <- 10
b <- 5
c <- 8
max <- a
if(b>max) {max <- b}
if(c>max) {max <- c}
max

v <- c(10,5,8)
max <- v[1]
for(i in 2: length(v)){
  if(v[i]>max){
    max <- v[i]
 }
}
max

#10개일때

v <- c(10,5,8,21,15,7,31,9,100,16)    #무슨말: max 값이 비교했을때 큰 값으로바뀜
max <- v[1]
for(i in 2: length(v)){
  if(v[i]>max){
    max <- v[i]
  }
}
max


#vector 생성
x <- c(1,2,3)
y <- c("a","b","c")
z <- c(TRUE,TRUE,FALSE,TRUE)
x;y;z


class(x);class(y);class(z)


w <- c(1,2,3,"a","b","c")   #문자형으로 맞춤-동일자료형집합,원래는 이렇게 쓰면 안됌
w
class(w)              


v1 <- 50:90; v1             
v2 <- c(1,2,3,50:90);v2
class(v1); class(v2)



v3 <- seq(1,101,3);        #seq (어디부터, 어디까지, 몇씩) 
v4 <- seq(0.1, 1.0, 0.1);

v5 <- rep(1,times = 5);     #rep 반복
v6 <- rep(1:5, times = 3);
v7 <- rep(c(1,5,9),times = 3);


#벡터 원소값에 이름지정
score <- c(90,85,70); #names 벡터(원소)에다 이름을 지정하는 함수,숫자가 일치해야
names(score)
names(score) <- c("hong","kim","lee")
names(score)
score


#벡터 원소접근              

score[1]
score[2]
score[3]
score["hong"]
score["kim"]
score["lee"]

d <- c(1,4,3,7,8)
d[1]; d[2]; d[3]; d[4]; d[5]; d[6]   #[]벡터에서 쓰는 괄호



for (i in 1:length(score)) {
  print(score[i])
    }

score_names <- c("hong", "kim", "lee")   
for (i in 1:length( score ) ) {
  print(score[score_names[i]])
}


score[c("Hong","kim","Lee")]  #?


#벡터에서 여러개의 값을 한번에 추출(기억해야)
d <- c(1,4,3,7,8)       
d[c(1,3,5)]             #첫번째, 세번째 다섯번째
d[1:3]                  #첫번째부터 세번째
d[seq(1,5,2)]           #첫번째 부터 다섯번째 까지 다 보는데 두칸씩 봐서 d[1],d[3],d[5]  
d[-2]                   #두번째 인덱스를 제외함
d[-c(3:5)]              #세번째~다섯번째 인덱스 제외

벡터에 이름 쓰는 방법

GNP <- c(2090,2450,960);
names(GNP) <- c("Korea","Japan","Nepal"); #열의 이름을 정해주는 것 2090열의 이름은 korea
GNP[1]
GNP["Korea"]
GNP[c("Korea","Nepal")]


#벡터요소값 변경
v1 <- c(1,5,7,8,9);       v1 #c함수를 잘 활용하면 
v1[2] <- 3;       v1
v1[c(1,5)] <- c(10,20);     v1



#벡터간 연산           # 두 인덱스 길이가 같아야(개수가 같아야)
x <- c(1,2,3)
y <- c(4,5,6)
x+y
x*y
z <- x+y
z


#벡터에 적용 가능한 함수
d <- c(1,2,3,4,5,6,7,8,9,10)
sum(d)  #합계
sum(2*d)
length(d) #벡터의 요소 개수(길이)
mean(d[1:5]) #평균
mean(d) 
median(d[1:5]) #중앙값
median(d) 
max(d) #최대값
min(d) #최소값
sort(d) #정렬
sort(d,decreasing = FALSE) 
sort(d,decreasing = TRUE)
range(d) #값의 범위(최소값~최대값)
var(d) #분산
sd(d) #표준편차




v <- sum(d)/length(d); v 


#벡터에 논리연산 적용 유용함 !!!!
d >= 5
d[ d > 5 ]
sum( d > 5)  #조건에 만족하는 인덱스 개수를 구함
sum(d[d>5])   #조건에 만족하는 인덱스 내용의 합계,굉장히 유용함
d==5      #5와 같은 것

cond <- d>5 & d<8;cond
d[cond] 

all(d>5)  #모두 5보다 크냐
any(d>5)  #어떤것이 5보다 크냐

head(d) #아무것도 지정 안될때는 처음부터 6개
tail(d) #아무것도 지정 안될때, 끝에서 6개
head(d,3) #앞에서 3개
tail(d,3) #끝에서 3개

x <- c(1,2,3)
y <- c(3,4,5)
z <- c(3,1,2)

w <- c(x,y);   w
union(x,y) #합집합
intersect(x,y) #교집합
setdiff(x,y) #차집합
setequal(x,y) #x,y 에 동일한 요소가 있는지 확인(모두)
setequal(x,z)





##List

ds <- c(90,85,70,84)
my.info <- list(name='Hong', age=30, status= TRUE, score=ds)
                 key, value                             벡터
my.info
my.info[1]     #실수하기 쉬운것
my.info[[1]]   #key 보다는 value 가 보통 필요할것
my.info$name     
my.info[[4]]     #ds 내용 전체

my.info[[4]][1]   #벡터의 첫번째




