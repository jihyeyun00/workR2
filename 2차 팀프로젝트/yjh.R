library(dplyr)
library(ggplot2)


setwd("D:\workR2")
abc.1<-read.csv('외국인설문조사불만족.csv')
View(abc.1)

abc.1 <- abc.1[5,];abc.1
abc.1 <- abc.1[,c(4:18)];abc.1
colnames(abc.1) <- c("쇼핑","강요","언어","기타","없음","음식","물가","대중교통불편","택시기사서비스불편","관광가이드불편","여행정보 얻기어려움","관광정보 불명확","관광안내원 불친절","안내표지판","식당음식불결")
abc.1 <- t(abc.1)
abc.1 <- data.frame(reason=c("쇼핑","강요","언어","기타","없음","음식","물가","대중교통불편","택시기사서비스불편","관광가이드불편","여행정보 얻기어려움","관광정보 불명확","관광안내원 불친절","안내표지판","식당음식불결"),abc.1)
abc.1


library(ggplot2)

ggplot(abc.1,aes(x=reason,y=X5))+           
  geom_bar(stat = "identity",              
           width = 0.7,                    
           fill="steelblue")+
  ggtitle("외국인 관광객 불만사항")+
  theme(plot.title = element_text(size = 25,   
                                  face = "bold",
                                  colour="steelblue"))

###########




