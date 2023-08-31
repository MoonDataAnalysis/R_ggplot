#散点图----------------------------
#绘制基本的点图
library(ggplot2)

#使用鸢尾花数据集
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point()

#设置颜色、形状、透明度
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point(
    color="orange",
    fill="#69b3a2",
    shape=21,
    alpha=0.5,
    size=6,
    stroke = 8
  )

#分组绘制
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + 
  geom_point(size=6) +
  theme_ipsum()

#透明度
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, alpha=Species)) + 
  geom_point(size=6, color="#69b3a2") +
  theme_ipsum()

# 形状分组
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, shape=Species)) + 
  geom_point(size=6) +
  theme_ipsum()

#形状+颜色分组
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, shape=Species, alpha=Species, size=Species, color=Species)) + 
  geom_point() +
  theme_ipsum()

#点 加文本marker
data=head(mtcars, 30)
head(data)

ggplot(data, aes(x=wt, y=mpg)) +
  geom_point() + 
  geom_text(
    label=rownames(data), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  )
#点加label
ggplot(data, aes(x=wt, y=mpg)) +
  geom_point() + 
  geom_label(
    label=rownames(data), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  )

# 标记某一个点（简单文本）
ggplot(data, aes(x=wt, y=mpg)) +
  geom_point() + 
  geom_label(
    label="Look at this!", 
    x=4.1,
    y=20,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="#69b3a2"
  )


#地毯散点图
head(iris)

# plot
ggplot(data=iris, aes(x=Sepal.Length, Petal.Length)) +
  geom_point() +
  geom_rug(col="steelblue",alpha=0.1, size=1.5)

#线性相关可视化，加置信度
data <- data.frame(
  cond = rep(c("condition_1", "condition_2"), each=10), 
  my_x = 1:100 + rnorm(100,sd=9), 
  my_y = 1:100 + rnorm(100,sd=16) 
)
head(data)

# Basic scatter plot.
p1 <- ggplot(data, aes(x=my_x, y=my_y)) + 
  geom_point( color="#69b3a2") +
  theme_ipsum()
p1
# with linear trend
p2 <- ggplot(data, aes(x=my_x, y=my_y)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()
p2

# linear trend + confidence interval
p3 <- ggplot(data, aes(x=my_x, y=my_y)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

p3
# 热力图-------------------------------
#最基本的热力图
"每一列是一个变量，每一行是一个observation，每个方形是值
越红，值越大"
data <- as.matrix(mtcars)#heatmap只适用于matrix
head(data)
heatmap(data)
heatmap(t(data))

#标准化后
heatmap(data, scale="column")


# 去掉树突
heatmap(data, Colv = NA, Rowv = NA, scale="column")

# palette调色------
"
R 自带的调色盘有terrain.color(), rainbow(), heat.colors(), topo.colors() or cm.colors()
"
heatmap(data, scale="column", col = cm.colors(256))
heatmap(data, scale="column", col = terrain.colors(256))

# Rcolorbrewer palette调色---------

library(RColorBrewer)#https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(data, scale="column", col = coul)

#增加label和主题
heatmap(data, Colv = NA, Rowv = NA, scale="column", col = coul, xlab="variable", ylab="car", main="heatmap")
#加样本名称
heatmap(data, scale="column", cexRow=1.5, labRow=paste("new_", rownames(data),sep=""), col= colorRampPalette(brewer.pal(8, "Blues"))(25))

#有的时候heatmap希望比较实际数据与期望数据的差异
my_group <- as.numeric(as.factor(substr(rownames(data), 1 , 1)))
colSide <- brewer.pal(9, "Set1")[my_group]
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(data, Colv = NA, Rowv = NA, scale="column" , RowSideColors=colSide, col=colMain   )


#correlation map------------------------------------------
"
使用GGally生成pairs plot
"
library(GGally)

# 随便编一个数据
data <- data.frame( var1 = 1:100 + rnorm(100,sd=20), v2 = 1:100 + rnorm(100,sd=27), v3 = rep(1, 100) + rnorm(100, sd = 1)) 
data$v4 = data$var1 ** 2 
data$v5 = -(data$var1 ** 2) 

data
# pairs plots 呈现变量关系 
# 散点图加密度图
ggpairs(data, title="correlogram with ggpairs()") 
#实现分组
data(flea)
head(flea)
ggpairs(flea, columns = 2:4, ggplot2::aes(colour=species))

# 灵活分配ggpairs各子图关系
data(tips, package = "reshape")
head(tips)
ggpairs(
  tips[, c(1, 3, 4, 2)],
  upper = list(continuous = "density", combo = "box_no_facet"),
  lower = list(continuous = "points", combo = "dot_no_facet")
)
#使用CORRGRAM绘图---------
"panel.ellipse to display ellipses
panel.shade for coloured squares
panel.pie for pie charts
panel.pts for scatterplots"
library(corrgram)
# heat 加 pie
corrgram(mtcars, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="Car Milage Data in PC2/PC1 Order") 
corrgram(mtcars, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt, main="Car Milage Data (unsorted)")
#椭圆 加 点
corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse, upper.panel=panel.pts, text.panel=panel.txt, diag.panel=panel.minmax, main="Car Milage Data in PC2/PC1 Order") 

#泡泡图---------------------
"
泡泡图是散点图的一种，只不过加入了第三个维度。
第三个维度根据泡泡的大小来呈现
"
library(gapminder)
head(gapminder)
data <- gapminder %>% 
  filter(year=="2007") %>% 
  dplyr::select(-year)

# 最基本的泡泡图
ggplot(data, aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.7)

#  根据洲调整颜色，根据人口调整大小 
data %>%
arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Population (M)")


#  两维密度图-------------------------
library(tidyverse)

a <- data.frame(x=rnorm(2000,10,1.9),y=rnorm(2000,10,1.2))
b <- data.frame( x=rnorm(2000, 14.5, 1.9), y=rnorm(2000, 14.5, 1.9) )
c <- data.frame( x=rnorm(2000, 9.5, 1.9), y=rnorm(2000, 15.5, 1.9) )
data <- rbind(a,b,c)
data
# 用点图表示
ggplot(data, aes(x=x, y=y) ) +
  geom_point()

# 基本的二维密度图
ggplot(data, aes(x=x, y=y) ) +
  geom_bin2d() +
  theme_bw()

# 设置bins的大小,填充颜色
ggplot(data, aes(x=x, y=y) ) +
  geom_bin2d(bins = 10) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

# 另一个相似功能的函数geom_hex()
ggplot(data, aes(x=x, y=y) ) +
  geom_hex(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

# 如果想呈现登高线的密度图
ggplot(data, aes(x=x, y=y) ) +
  geom_density_2d()

# 登高线内填充颜色
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")

# 等高线内填充颜色，线则显示白色
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")

#设置成光栅式
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous() +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

# 使用scale_fill_distiller调整颜色
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette=4, direction=-1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )

ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_distiller(palette= "Spectral", direction=1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 
