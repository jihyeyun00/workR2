#
#T 프로그래밍 1일차
#
#first.R
#
#작성자:윤지혜
#최초작성일:2019.11.26
#
#
print("HELLO,WORLD!")

number <- 10
number2 <- 100

number3 <- number 
number4 <- number2
number2 <- number

numberValue <- 1
str_value <- "R Lunguage"
booleanvalue <- TRUE
numberValue <- 1
numberValue
print(numberValue)
print(str_value)
print(booleanvalue)

cat("numeric number:",numberValue,"\n")
cat("String :",str_value, "\n")
cat("booleanvalue:",booleanvalue,"\n")

numberValue <- scan()
cat("Numeric number:",numberValue,"\n")

number1 <- 10
number2 <- 20
resultAdd <- number1 <- number2
resultSub <- number1 <- number2
resultMul <- number1 <- number2
resultDiv <- number1 / number2
resultRem <- number2 %% number1
resultSec<- number2 ^ 5

print(resultAdd)
print(resultSub)
print(resultMul)
print(resultDiv)
print(resulRem)
print(resultSec)

number1 <- 0
number1 <- number1+10
number1
number1 <- number1+10
number1
number1 <- number1+10
number1

number2 <- 100
number1 <- number2+10
number1
number2

print((number1+10)*number2/2)

number301 <- number300+2
number301

number1 <- 10.5
number2 <- 10
print(number1>number2)
print(number1>=number2)
print(number1<number2)
print(number1<=number2)
print(number1!=number2)
      
print(number1> 10& number2>10)
print(number1> 10 |number2>10)
print(!(number1 >10))
      

number <- "100"
str <- "R languge"
result=number +str
print(result)

number <- "100"
str <- "R languge"
result=number +str
print(result)

제어문 -선택구조

job.type <- 'a'
if(job.type=='b'){
 bonus <- 200
} else {
  bonus <- 100}
cat("joy type :",job.type,"\t\tbounus:",bonus)

job.type <- 'b'

if(job.type=='a'){bonus <- 200}
cat("joy type :",job.type,"\t\tbonus:",bonus)

score <- 85

if(score >=90)
  {grade <- 'a'
} else if(score>=80){grade <-'b'
} else if(score>=70){grade <- 'c'
} else if(score>=60) {grade <- 'd'
} else {grade <- 'f'}
cat("score :",score,"\t\tgrade:",grade)


num <- 20
rema <- num %% 2
#if (remainder ==0){
if(num%% 2 ==0){
  oddeven <- '짝수'
}else{
  oddeven <- '홀수'
}
     
cat("num:",num,"는",oddeven,"이다")

a <- 4
b <- 20

if(a>5 & b>5) {
  cat(a,">5 and",b,">5")
} else { 
  cat(a,"<=5 or ",b,"<=5")
}

a <- 10
b <- 20

if(a>b){
c <- a
} else {
 
 
}

c <- ifelse(a>b,a,b)
cat("a =",a, "\tb=", b, "\tb=",c)

a <-10
b <- 5
c <- 8 

if(a>b and a>c and  )
  
a <- 10      #ma 에 a를 저장해서 최대값을 구하는 방법
b <- 5
c <- 8
ma <- a

if(b>ma){
  ma=b
}
if(c>ma){
  ma=c
}
cat("a=",a,"b=",b,"c=",c,"ma=",ma)


#반복구조:
#for문

for(i in 1:10) {
  print('*')
}

multiple=2
for(i in 2:9){
  cat(multiple,'x',i,'=',multiple*i,'\n')
}

#while 문
i <- 1

while(i <= 10) {
  print(i)
  i <- i+1
}

multiple <- 2
i <- 2
while(i<=9) {
  cat(multiple,'x',i,'=',multiple*i,'\n')
  i <- i+1
}

for(i in 1:100) {
  i <- i+1
  cat(i="i+1",i,"\n")
}

1부터 100까지 한줄에 10개씩 출력
#1
linecount <- 1
for (i in 1:100) {
  cat(i,'')
  linecount <- linecount+1
  if(linecount>10){
    print('\n')
    linecount <- 1
  }
}

#2
for (i in 1:100){
  cat(i,'')
  if(i %% 10==0){
    print('\n')
  }
}

