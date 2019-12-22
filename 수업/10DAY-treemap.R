#트리맵
#treemap
install.packages("treemap")
library(treemap)


data(GNI2014)
dim(GNI2014)
str(GNI2014)
head(GNI2014)
View(GNI2014)

treemap(GNI2014,
        index = c("continent","iso3"),   #계층구조 콤마로 여러개해도 돼?
        vSize = "population",            #타일크기
        vColor="GNI",                     #타일컬러
        type="value",                   #타일컬러링 방법(값이 높은것은 진하게)
        bg.labels="yellow",             #레이블배경색
        title="World's GNI")            #제목


st<-data.frame(state.x77)
st<-data.frame(st,stname=rownames(st)) #data.frame(컬럼1=자료, 컬럼2=자료...)
View(st)
treemap(st,
        index=c("stname"),
        vSize = "Area",
        vColor = "Income",
        type="value",
        title="미국주별수입")


#산점도에 Bubble 추가 (bubble chart) 1)버블차트
symbols(st$Illiteracy, st$Murder,    #원의 x, y 좌표
        circles = st$Population,     #원의 반지를
        inches = 0.3,               #원 크기 조절값
        fg="white",                     #원 테두리 색
        bg="lightgray",            #원 바탕색
        lwd=1.5,                    #원 테두리선 두께
        xlab='rate of Illiteracy',
        ylab = "crime(murder) rate",
        main= "Illiteracy and Crime")
text(st$Illiteracy, st$Murder,      #텍스트 출력 x,y 좌표
     rownames(st),                  #출력할 text
     cex=0.6,                        #폰트크기
     col ="brown")                    #폰트컬러


#ggplot 이용 2)버블차트 The R Graph Gallery :http://r-graph-gallery.com/index.html
# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Interactive version
p <- data %>%
  mutate(gdpPercap=round(gdpPercap,0)) %>%
  mutate(pop=round(pop/1000000,2)) %>%
  mutate(lifeExp=round(lifeExp,1)) %>%
  
  # Reorder countries to having big bubbles on top
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  
  # prepare text for tooltip
  mutate(text = paste("Country: ", country, "\nPopulation (M): ", pop, "\nLife Expectancy: ", lifeExp, "\nGdp per capita: ", gdpPercap, sep="")) %>%
  
  # Classic ggplot
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none")

# turn ggplot interactive with plotly
pp <- ggplotly(p, tooltip="text")
pp