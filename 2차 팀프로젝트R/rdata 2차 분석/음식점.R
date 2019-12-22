library(xlsx)
library(rJava)
library(tidyverse)

setwd('C:/TeamProject-R/teamproject2차/data/똥현이')
jeju <- read.csv('제주시_일반음식점현황.csv')
seo <- read.csv('서귀포시_일반음식점현황.csv')
menu <- read.xlsx(file = '메뉴판 다국어.xlsx', sheetIndex = 1, encoding = 'UTF-8')

View(jeju)
View(seo)
View(menu)

head(jeju)
head(seo)
head(menu.ch)

tail(jeju)
tail(seo)
tail(menu)

menu.ch <- menu[-1,]

count.food <- c(nrow(jeju) + nrow(seo), nrow(menu.ch))

names(count.food) <- c('제주도 음식점', '다국어')
count.food    


# Create Data
data <- data.frame(
    group=names(count.food)[1:2],
    value=c(count.food[1], count.food[2]),
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

ggplot(data, aes(x="", y=prop, fill=group)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0)+
    theme_void()+
    theme(legend.position='right')+
    theme(
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15)
    )+
    geom_text(aes(y = ypos, label = label), color = "white", size=10) +
    scale_fill_brewer(palette="Set1")
