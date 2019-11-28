# À±ÁöÇý 11.28
#1-1
d1 <- 1:50;  d1   
d2 <- 51:100; d2  


x <- c(d1)
y <- c(d2)


#1-2
length(y)
#1-3
x+y 
x-y
x*y
y/x

#1-4

sum(x) 
sum(y)

#1-5
sum(x)+sum(y)

#1-6
max(y)
min(y)

#1-7
mean(x)
mean(y)
abs(mean(x-y))

#1-8
sort(x,decreasing = TRUE) 
#1-9
sort(x,decreasing = FALSE) 
sort(y,decreasing = FALSE)

#1-9?
d3 <- head((sort(x,decreasing = FALSE),10) & head(sort(y,decreasing = FALSE),10))





#2
v1 <- 51:90 ; v1

v2<- v1

#2-1
seq(51,60) 

#2-2
length(v1<70)

#2-3
v2[sum(v1>65)]   

#2-4
v2[v1>60 & v1<73]

#2-5
v2[v1<65 & v1>80]

#2-6
for (i in v1) {    
  if(i %% 7==3)
    print(i)
}

#2-7?
for (i in v1){   
  if(i * 7)}
v2[v1(i*7)] <- 0 

#2-8?
for(i in v1) {  
  if(i %% 2==0){
    sum(v1(i))
  }
}  

#2-9?
for (i in v1){
  if(i %% 2==1)
    | for (i > 80)
      print(i)
}

#2-10?
for (i in v1) {   
  if (i %% 3==0)
    if (i %% 5==0)
      intersect(i,i)
}

#2-11?
for (i in v1) {     
  if(i %% 2==0)
    i*2
}

#2-12?





