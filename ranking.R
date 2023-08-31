#=============条形图===============================

library(ggplot2)

# 生成一个简单的data
data <- data.frame(
  name=c("A","B","C","D","E") ,  
  value=c(3,12,5,18,45)
)

data
# 绘制最基本的条形图
ggplot(data, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")

# 填充不同的颜色
ggplot(data, aes(x=name, y=value,fill=name)) +
  geom_bar(stat = "identity",color="blue", fill=rgb(0.1,0.4,0.5,0.7) )

ggplot(data, aes(x=name, y=value,fill=name)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1")


#  引入一个较原始的数据集
mtcars

ggplot(mtcars, aes(x=as.factor(cyl),fill=as.factor(cyl))) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) ) 

ggplot(mtcars, aes(x=as.factor(cyl),fill=as.factor(cyl))) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1")

ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +  
  geom_bar( ) +
  scale_fill_manual(values = c("red", "green", "blue") )

#  由竖直变水平
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +  
  geom_bar( ) +
  scale_fill_manual(values = c("red", "green", "blue") )+
  coord_flip()

# 调整bar的宽度
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +  
  geom_bar( width = 0.8) +
  scale_fill_manual(values = c("red", "green", "blue") )+
  coord_flip()

# 如何在条形图上画errorbar?
# 生成一个数据
data <- data.frame(
  name=letters[1:5],
  value=sample(seq(4,15),5),
  sd=c(1,0.2,3,2,4)
)
data

ggplot(data) +
  geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

#=================雷达图======================
# 最基本的雷达图
library(fmsb)

# 生成data：
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )
data <- rbind(rep(20,10) , rep(0,10) , data)
data

radarchart(data)

# 填充颜色加数值刻度
radarchart( data  , 
            
            #多边形颜色设置
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=8 , 
            
            #网格线颜色设置
            cglcol="grey", cglty=1, cglwd=8,
            
            #label设置
            vlcex=1
)

# 多组雷达图
set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")
data

data <- rbind(rep(20,5) , rep(0,5) , data)
data

radarchart(data)

# 多组颜色填充

radarchart( data  , axistype=1 , 
            #调整多边形颜色
            pcol=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) ) ,
            pfcol=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) ) , 
            plwd=4 , plty=1,
            #调整网格线
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #调整标签
            vlcex=0.8 
)
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=10 , 
       col=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) ) ,
       text.col = c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) ), cex=1.2, pt.cex=3)


# 词云图--------------------------
# 载入函数库
library(wordcloud2) 
# wordcloud2 自带函数
head(demoFreq)

# 基本词云图
wordcloud2(data=demoFreq, size=1.6)

# 调整词云图色系色
wordcloud2(demoFreq, size=1.6, color='random-dark')

# 词条只能是蓝色或者绿色
wordcloud2(demoFreq, size=1.6, color=rep_len( c("green","blue"), nrow(demoFreq) ) )

# 背景色调整成黑色
wordcloud2(demoFreq, size=1.6, color='random-light', backgroundColor="black")

# 字体调成斜体
wordcloud2(demoFreq, size = 2.3, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 0.5)

#  形状改成星星状
wordcloud2(demoFreq, size = 0.7, shape = 'star')



#平行坐标系图---------------------------------
library(hrbrthemes)
library(GGally)
library(viridis)
library(dplyr)
# 使用鸢尾花数据集
data <- iris

head(iris)

ggparcoord(data,
           columns = 1:4, groupColumn = 5, order = "anyClass",
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )

# 高亮多组中的某一组
data %>%
  arrange(desc(Species)) %>%
  ggparcoord(
    columns = 1:4, groupColumn = 5, order = "anyClass",
    showPoints = TRUE, 
    title = "Original",
    alphaLines = 1
  ) + 
  scale_color_manual(values=c( "#69b3a2", "#E8E8E8", "#E8E8E8") ) +
  theme_ipsum()+
  theme(
    legend.position="Default",
    plot.title = element_text(size=10)
  ) +
  xlab("")
