#윤지혜

>score
m  f
[1,] 10 21
[2,] 40 60
[3,] 60 70
[4,] 20 30
 
#문1-1
score <- matrix(c(10,21,40,60,60,70,20,30),4,2)
score

colnames(score) <- c("male","female")
score

score[2,]

score[,2]

score[3,2]

#문2
class(state.x77)
st <- data.frame(state.x77)

st

3.
names(st)

4.?

  
5.
dim(st)
  
6.
summary(st)

7.
rowSums(st)
rowMeans(st)

8.
colSums(st)
colMeans(st)

9.?
st.1 <- subset(st,Species=="Florida"); st.1
st
st["Florida",]

10.