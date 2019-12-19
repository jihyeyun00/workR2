library(rJava)
library(xlsx)
library(dplyr)
library(tidyverse)

setwd('C:/TeamProject-R/teamproject2차/data/똥현이')
global.texi <- read.xlsx(file = '글로벌택시.xlsx', sheetIndex = 1, encoding = 'UTF-8')
total.texi <- read.xlsx(file = '18년 교통관광산업 현황.xlsx', sheetIndex = 1, encoding = 'UTF-8')
happy.texi <- read.xlsx(file = '행복택시.xlsx', sheetIndex = 1, encoding = 'UTF-8')
View(global.texi)
View(total.texi)
View(happy.texi)

str(global.texi)
str(total.texi)
str(happy.texi)

# 제주 전체택시
total.texi <- total.texi[4:5, 10]
total.texi <- as.vector(total.texi) 
total.texi <- as.integer(total.texi)
total.texi <- as.data.frame(total.texi)
names(total.texi) <- '택시'

total.texi
str(total.texi)
sum(happy.texi)

texi <- data.frame(global.texi, 총택시 = sum(total.texi))
texi <- data.frame(texi, 행복택시 = sum(happy.texi))
texi <- texi %>% mutate(글로벌택시 = 영어 + 중국어 + 일본어)
texi




# Create Data
data <- data.frame(
    group=names(texi)[4:6],
    value=c(texi[,4], texi[,5], texi[,6]),
    stringsAsFactors = FALSE
)
data
# Compute the position of labels
data <- data %>% 
    arrange(desc(group)) %>%
    mutate(prop = value / sum(data$value) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )

str(data)
label <- paste(round(data$prop,0))
label <- paste(label,'%',sep = '')
data
# Basic piechart
ggplot(data, aes(x="", y=prop, fill=c('steelblue', 'red', 'green'))) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.position="none") +
    
    geom_text(aes(y = ypos, label = label), color = "white", size=3) +
    scale_fill_brewer(palette="Set1")


ggplot(data, aes(x="", y=prop, fill=group)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0)+
    theme_void()+
    theme(legend.position='right')+
    theme(
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15)
    )+
    geom_text(aes(x = c(1.15,1, 1),y = ypos, label = label), color = "white", size=10) +
    scale_fill_brewer(palette="Set1")

