#4일차


#함수 반환 값(return 값)이 여러개일때의 처리
myfunc <- function(x,y)
  val.sum <- ｘ ＋ ｙ
  val.mul <- x*y
  
 return(list(sum=val.sum, mul=val.mul)))
  
  result <- myfucn(5,8)
  s <- result$sum
  m <- result$mul
  cat('5+8=', s, '\n')
  cat('5+8=', m, '\n')
  
  
  
  
#Matrix 생성
z <- matrix(1:20, nrow = 4)  #열 우선 방식
z
z <- matrix(1:20, ncol = 4)  #
z
z <- matrix(1:20, nrow = 4, ncol=5)
z

z <- matrix(1:20, nrow =4, ncol = 5,byrow = T)
z


x <- 1:4
x
y <- 5:8
y
z <- matrix(1:20,nrow = 4,ncol = 5)
z

m1 <- cbind(x, y)
m1
m2 <- rbind(x, y)
m2
m3 <- rbind(m2, x)
m3
m4 <- cbind(z, x)
m4


#Matrix 에서 cell 값 추출

z[2,3]
z[1,4]
z[2, ]
z[ ,4 ]

z[2, 1:3]
z[1,c(1,2,4)]
z[1:2,]
z[ , c(1,4)]


#Matrix 에서 행/열에 이름지정

score <- matrix(c(90,85,69,78,
                  85,96,49,95,
                  91,82,70,70),
                nrow = 4, ncol=3)

score
rownames(score) <- c("Hong","Kim","Lee","yoo")
colnames(score) <- c("English","Math","Science")
score

score['Hong','Math']
score['Kim',c('Math','Science')]
score['Lee']
score[ , 'English']
rownames(score)
colnames(score)
colnames(score)[2]


#Data Frame 생성 -변수내용은 동일 해야 셀 단위로 다르면 안됨
city <- c("seolul","Tokyo","Washington")
rank <- c(1,2,3)
city.info <- data.frame(city,rank)
city.info

name <- c("Hong","Kim","Lee")
age <- c(22,20,25)
gender=factor(c("M","F","M"))
blood.type=factor(c("A","B","O"))
person.info <- data.frame(name,age,gender,blood.type)
person.info

person2.info <- data.frame(name=c("Hong","Kim","Lee"),
                           age=c(22,20,25),
                           gender=factor(c("M","F","M")),
                           blood.type=factor(c("A","O","B")))
person2.info


#요소접근 방법
city.info[1,1]
city.info[1, ]
city.info[ ,1]
city.info[city.info$city, ]
city.info[,"rank"]

person.info$name
person.info[person.info$name=="Hong",]
person.info[person.info$name=="Hong",c("name","age")]

data()

iris
iris[ ,c(1:2)]
iris[,c(1,3,5)]
iris[,c("Sepal.Length","Sepcies")]
iris[1:5, ]; iris[1:5,]


#matrix 와 dara frame에서 사용하는 함수
dim(person.info) #관측치의 수와 변수의 수: 
dim(iris)
nrow(person.info) #행의 개수를 셀때도 사용함
nrow(m3)
ncol(person.info) #열의 개수를 셀때도 사용함
head(iris)           #실제 데이터 확인
tail(iris)           #"
str(iris)            #요약정보확인,팩터자료형도 확인하기
iris[ ,5]
unique(iris[ ,5])   #중복된 데이터중에서 하나씩만 뽑아서 나열하라
table(person.info[,"blood.type"])#어떤 종류로 구성되는지 알수 있다
table(iris,"Species") 

str(iris)
str(city.info)
str(person.info)
table(person.info[,"blood.type"])#어떤 종류로 구성되는지 알수 있다.




#Matrix/data frame사용함수
#행렬/열별 합계와 평균계산 (apply 함수이용한 경우)
colSums(iris[,-5]);  apply(iris[,1:4],2,sum) #팩터타입은 제외 마지막 열제외(팩터타입으로 나왔움) 1에서 4변수 까지 열 나오고 행은 다 더한거
colMeans(iris[,-5]);apply(iris[,1:4],2,mean) #숫자가 2가 열row
rowSums(iris[,-5]);  apply(iris[,-5],1,sum)
rowMeans(iris[,-5]); apply(iris[,-5],1,mean)
apply(iris[,-5],2,median)



#행/열 방향전환
z <- matrix(1:20, nrow=4, ncol=5); z
t(z)

#조건에 맞는 행과 열의 값 추출(data frame만 가능,매트릭스에서는 못씀)
IR.1 <- subset(iris, Species=="setosa");   IR.1 #subset은 부분만 추출하고자 할때
IR.2 <- subset(iris, Sepal.Length>5.0 & Sepal.Width>4.0); IR.2
IR.2[ , c(2,4)]



#Matrix/data frame 산술연산
a <- matrix(1:20,4,5)
b <- matrix(21:40,4,5)

2*a
b-5
2*a+3*b

a+b
b-a
b/a
a*b

#Matrix/data frame 자료구조확인 /변환
class(iris);      str(iris)
class(state.x77); str(state.x77)
is.matrix(iris)
is.data.frame(iris)
is.matrix(state.x77)
is.data.frame(state.x77)

st <- data.frame(state.x77)
str(st)
head(st)
class(st)
dim(st)

iris.m <- as.matrix(iris[,1:4])
head(iris.m)
class(iris.m)
str(st)

head(st)
Population
attach(st)    
Population
detach(st)
Population



#csv file 내용읽기
setwd("D:/WorkR2")  #/써야한다               #먼저 컴퓨터에 다운 받고
air <- read.csv("airquality.csv",header=T)  #file 안써도 돼, 인코딩 안써도 돼(한글깨질때 사용),sheetIndex=1 도 안해도 돼, 헤더가 있으면 T(엑셀파일에서 오존,등등)
air
View(air)


                                       #없는 경우는 F
class(air)
dim(air)
str(air)
head(air)
tail(air)


setwd("D:/WorkR")  #/써야한다
write.csv(person.info,"person_info.csv",row.names = F)



