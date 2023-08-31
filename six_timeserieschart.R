"
本节课旨在梳理6种时间序列可视化图（使用dygraphs实现）
"
#1、线图-------------------
library(dygraphs)
library(xts) 

#生成数据
data <- data.frame(
  time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), 
  value=runif(41)
)
head(data)

# 转换成xts格式（https://rpubs.com/mpfoley73/504487）
data <- xts(x = data$value, order.by = data$time)
head(data)

# 画图加点
p <- dygraph(data) %>%
  dyOptions( drawPoints = TRUE, pointSize = 4 )
p

# area chart----------
p <- dygraph(data) %>%
  dyOptions( fillGraph=TRUE )
p

# 直方图形式-------
p <- dygraph(data) %>%
  dyOptions( stepPlot=TRUE, fillGraph=TRUE)
p

# 棒棒糖图形式-------
p <- dygraph(data) %>%
  dyOptions( stemPlot=TRUE)
p

# 箱型图形式---------
# 生成数据
trend <- sin(seq(1,41))+runif(41)
data <- data.frame(
  time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), 
  value1=trend, 
  value2=trend+rnorm(41), 
  value3=trend+rnorm(41), 
  value4=trend+rnorm(41) 
)
head(data)
# 转化为xts形式
data <- xts(x = data[,-1], order.by = data$time)

# 绘图
p <- dygraph(data) %>%
  dyCandlestick()
p

# 线图加置信区间形式------------
# 生成数据
trend <- sin(seq(1,41))+runif(41)
data <- data.frame(
  time=seq(from=Sys.Date()-40, to=Sys.Date(), by=1 ), 
  trend=trend, 
  max=trend+abs(rnorm(41)), 
  min=trend-abs(rnorm(41, sd=1))
)
head(data)

# 转化为xts形式
data <- xts(x = data[,-1], order.by = data$time)

# 绘图
p <- dygraph(data) %>%
  dySeries(c("min", "trend", "max"))
p
