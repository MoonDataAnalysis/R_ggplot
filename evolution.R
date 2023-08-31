# 线图呈现变化过程==============================

# 载入函数库
library(ggplot2)
# 生成数据
xValue <- 1:10
yValue <- cumsum(rnorm(10))
data <- data.frame(xValue,yValue)
data

# 基本线图
ggplot(data, aes(x=xValue,y=yValue)) +
  geom_line()

# 设置线的颜色、大小、透明度、线型
library(hrbrthemes)
ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line( color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  theme_ipsum() +
  ggtitle("Evolution of something")

# 多组的line chart---------
# 生成数据
library(babynames)
library(viridis)
library(tidyverse)
head(babynames)
data <- babynames %>% 
  filter(name %in% c("Amanda", "Jessica",    "Patricia", "Deborah",   "Dorothy",  "Helen")) %>%
  filter(sex=="F")
head(data)

data %>%
  ggplot( aes(x=year, y=n, group=name, color=name)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum()
# area chart------------------
# 载入数据库
library(ggplot2)

# 生成数据
xValue <- 1:50
yValue <- cumsum(rnorm(50))
data <- data.frame(xValue,yValue)
head(data)

# 绘图
ggplot(data, aes(x=xValue, y=yValue)) +
  geom_area()

# 完善一点
library(hrbrthemes)
ggplot(data, aes(x=xValue, y=yValue)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="#69b3a2") +
  theme_ipsum() +
  ggtitle("Evolution of something")


# 分组的area chart-------

# 生成数据
time <- as.numeric(rep(seq(1,7),each=7))  # x Axis
value <- runif(49, 10, 100)               # y Axis
group <- rep(LETTERS[1:7],times=7)        # group, one shape per group
data <- data.frame(time, value, group)
head(data)
# 分组的area chart
ggplot(data, aes(x=time, y=value, fill=group)) + 
  geom_area()


# 设置分组顺序
data$group <- factor(data$group , levels=c("B", "A", "D", "E", "G", "F", "C") )
# 绘图
ggplot(data, aes(x=time, y=value, fill=group)) + 
  geom_area()

#如果我们需要让y轴是占比的话
library(dplyr)
data
data <- data  %>%
  group_by(time, group) %>%
  summarise(n = sum(value)) %>%
  mutate(percentage = n / sum(n))
head(data)

# 绘图
ggplot(data, aes(x=time, y=percentage, fill=group)) + 
  geom_area(alpha=0.6 , size=1, colour="black")


# 绘制交互性的时间序列图（area chart）
# 载入函数库
library(ggplot2)
library(dplyr)
library(babynames)
library(viridis)
library(hrbrthemes)
library(plotly)

# 使用babynames中的数据集
data <- babynames %>% 
  filter(name %in% c("Ashley", "Amanda", "Jessica",    "Patricia", "Linda", "Deborah",   "Dorothy", "Betty", "Helen")) %>%
  filter(sex=="F")

# 非交互性的堆叠area chart
p <- data %>% 
  ggplot( aes(x=year, y=n, fill=name, text=name)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  theme(legend.position="none") 
p
# 将其转化为交互性图
p <- ggplotly(p, tooltip="text")
p


#流图（streamgraph）----------------
# 载入函数库
library(ggstream)
library(ggplot2)
# ggstream自带数据
blockbusters

# 基本流图
ggplot(blockbusters, aes(x = year, y = box_office, fill = genre)) +
  geom_stream()

# 添加标签
ggplot(blockbusters, aes(x = year, y = box_office, fill = genre)) +
  geom_stream() +
  geom_stream_label(aes(label = genre))


