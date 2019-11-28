
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
max <- v{1}
for(i in 2: length(v)){
  if(v[i]>max){
    max <- v[i]
 }
}
max
#10개일때

v <- c(10,5,8,21,15,7,31,9,100,16)
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
