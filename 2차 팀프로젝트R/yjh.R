library(dplyr)
library(ggplot2)


setwd("C:/TeamProject-R/teamproject1차/data")
abc.1<-read.csv('외국인설문조사불만족.csv')
View(abc.1)


abc.1 <- as.vector(abc.1[5,4:18])

names(abc.1) <- c("쇼핑","강요","언어","기타","없음","음식","물가","대중교통불편","택시기사서비스불편","관광가이드불편","여행정보 얻기어려움","관광정보 불명확","관광안내원 불친절","안내표지판","식당음식불결")

abc.1 <- t(abc.1)

dim(abc.1)
class(abc.1)
str(abc.1)
head(abc.1)

abc.1 <- as.numeric(abc.1)
names(abc.1) <- c("쇼핑","강요","언어","기타","없음","음식","물가","대중교통불편","택시기사서비스불편","관광가이드불편","여행정보 얻기어려움","관광정보 불명확","관광안내원 불친절","안내표지판","식당음식불결")
names <- c("쇼핑","강요","언어","기타","없음","음식","물가","대중교통불편","택시기사서비스불편","관광가이드불편","여행정보 얻기어려움","관광정보 불명확","관광안내원 불친절","안내표지판","식당음식불결")
as.data.frame(abc.1)
# abc.1 <- t(abc.1)
abc.1 <- data.frame(abc.1, names)

abc.1 <- abc.1[-5,]

score <- c(24.9, 6.7, 4.6, 3.6, 2.9)
label <- c('언어', '여행정보 얻기 어려움', '관광정보 불명확', '안내표지판', '기타')
# pct <- round(score / sum(score) * 100)
label <- paste(label)
pie(score, clockwise = T, labels = label, main = '외국인 관광객 불만족도')

library(ggplot2)

ggplot(abc.1, aes(x=names,y=abc.1))+           
  geom_bar(stat = "identity",              
           width = 0.7,                    
           fill="steelblue")+
  ggtitle("외국인 관광객 불만사항")+
  theme(plot.title = element_text(size = 25,   
                                  face = "bold",
                                  colour="steelblue")) +
    labs(x = '(%)', y = '항목', size = 20)

pie(abc.1[,1])

###########



# Create Data
data <- data.frame(
    group=LETTERS[1:5],
    value=c(13,7,9,21,2)
)


data <- data.frame(label, score)
data
# Basic piechart
ggplot(data, aes(x="", y=score, fill=label)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0)


