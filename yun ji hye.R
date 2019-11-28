#윤지혜 11.28

d1 <- 1:50;  d1   #1-1
d2 <- 51:100; d2  


x <- c(d1)
y <- c(d2)

x+y  #3
x-y
x*y
y/x

sum(x) #4
sum(y)
sum(x+y) #5

max(y) #6
min(y)

mean(x) #7
mean(y)
mean(y-x)
sort(x,decreasing = TRUE) #8
sort(x,decreasing = FALSE) #9
sort(y,decreasing = FALSE)

head(x,10)
head(y,10)
d3 <- {head(x,10) & head(y,10)}

v1 <- 51:90 ; v1
d <- v1
d[v1<60]  #2-1
length(d>70)  #2
sum(d)        #3
d[60<v1 & 73>v1]#4
d[65>v1 | 80<v1]#5
all(d/7)#6
#7
sum(d/2 == 0)
