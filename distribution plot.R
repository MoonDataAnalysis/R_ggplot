rm(list=ls())
# Library
library(ggplot2)

#小提琴图----------------------------------------------------
"A,B,C,D四组，都是连续值，4组分布是？
"
# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

head(data)
# 最基本的小提琴图
p <- ggplot(data, aes(x=name, y=value, fill=name)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()

p

#控制顺序
data$name=factor(data$name,levels = c("D","C","B","A")) 

ggplot(data, aes(x=name, y=value, fill=name)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()

#由垂直变水平
ggplot(data, aes(x=name, y=value, fill=name)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()+
  coord_flip()

#换个主题
ggplot(data, aes(x=name, y=value, fill=name)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()+
  coord_flip()+
  theme_dark()#更多themes见https://ggplot2.tidyverse.org/reference/ggtheme.html


#小提琴图加箱图

# sample size
library(dplyr) #group_by() ; summarize(); n()
library(viridis) #scale_fill_viridis

sample_size = data %>% group_by(name) %>% summarize(num=n())
sample_size

# Plot
data = data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(name, "\n", "n=", num)) 

  ggplot(data, aes(x=myaxis, y=value, fill=name)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("")

#分组小提琴图
"男女客户一周就餐给小费差异"
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/10_OneNumSevCatSubgroupsSevObs.csv", header=T, sep=",") %>%
  mutate(tip = round(tip/total_bill*100, 1))

head(data)
# 以性别分组
library(forcats)
data %>%
  mutate(day = factor(day, levels=c("Thur", "Fri", "Sat", "Sun"))) %>%
  ggplot(aes(fill=sex, y=tip, x=day)) + 
  geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
  scale_fill_viridis(discrete=T) +
  theme_ipsum()  +
  xlab("") +
  ylab("Tip (%)") +
  ylim(0,40)

#统计数据 + 画小提琴图
library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)

data("penguins", package = "palmerpenguins")

penguins <- tidyr::drop_na(penguins)#删掉缺失值

"三种企鹅（Adelie, Chinstrap, and Gentoo）的分布广度是否有显著差异？
方差分析 + 小提琴图， 要求在图上展示统计结果
"
plt <- ggbetweenstats(
  data = penguins,
  x = species,
  y = bill_length_mm
)

plt

ggsave(
  filename = "C:/Users/77387/Desktop/Data_analysis_courses/直播课/R语言与可视化/描述图/p1.png",
  plot = plt,
  width = 8,
  height = 8,
  device = "png"
)

# density plot--------------------------
"density plot 适合呈现连续数据分布态势"

data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)

head(data)

#最基本的density plot
library(hrbrthemes)
data %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  geom_density(fill="#69b3a2", color="#e9ecef") +
  theme_ipsum()

#两组连续变量，比较density
data <- data.frame(
  male = rnorm(1000),
  female = rnorm(1000, mean=2)
)

head(data)

ggplot(data, aes(x=x) ) +
  # Top
  geom_density( aes(x = male, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=4.5, y=0.25, label="male"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = female, y = -..density..), fill= "#404080") +
  geom_label( aes(x=4.5, y=-0.25, label="female"), color="#404080") +
  theme_ipsum() +
  xlab("value of x")

#多组连续变量比较
"不同级别钻石的价格"
head(diamonds)

ggplot(data=diamonds, aes(x=price, group=cut, fill=cut)) +
  geom_density(adjust=1.5) +
  theme_ipsum()


# 调透明
ggplot(data=diamonds, aes(x=price, group=cut, fill=cut)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

# 多组连续数据，分开画density plot
ggplot(data=diamonds, aes(x=price, group=cut, fill=cut)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~cut) + #分割
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines")#子图间隔
  )

# 堆叠的density plot（堆叠图）
ggplot(data=diamonds, aes(x=price, group=cut, fill=cut)) +
  geom_density(adjust=1.5, position="fill") +
  theme_ipsum()

# 边际分布
"两个连续变量，可能存在某种关联"
library(ggExtra)

# The mtcars dataset is proposed in R
head(mtcars)
"车的重量（wt）和每公里耗油（mpg）"
# 生成点图
p <- ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, size=cyl)) +
  geom_point() +
  theme(legend.position="none")
p

# 在点图外呈现density
ggMarginal(p, type="density")

#直方图--------------------------------------
"直方图和density plotshi基本是一样的，主要呈现连续/离散数据分布"


data=data.frame(value=rnorm(100))

head(data)
# 基本直方图
ggplot(data, aes(x=value)) + 
  geom_histogram()

#加个颜色，调一下宽度
data %>%
  ggplot( aes(x=value)) +
  geom_histogram( binwidth=0.5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  theme_ipsum() 

#两组连续变量分布

data <- data.frame(
  male = rnorm(100),
  female = rnorm(100, mean=2)
)

ggplot(data, aes(x=x) ) +
  # Top
  geom_histogram( aes(x = male, y = ..density..), fill="#69b3a2" ) +
  geom_label( aes(x=4.5, y=0.25, label="male"), color="#69b3a2") +
  # Bottom
  geom_histogram( aes(x = female, y = -..density..), fill= "#404080") +
  geom_label( aes(x=4.5, y=-0.25, label="female"), color="#404080") +
  theme_ipsum() +
  xlab("value of x")

#多组直方图
ggplot(data=diamonds, aes(x=price, group=cut, fill=cut)) +
  geom_histogram(adjust=1.5, alpha=.6) +
  theme_ipsum()

#多组直方图分子图展示
ggplot(data=diamonds, aes(x=price, group=cut, fill=cut)) +
  geom_histogram(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~cut) + #分割
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines")#子图间隔
  )

# 边际分布
# 生成点图
head(mtcars)
p <- ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, size=cyl)) +
  geom_point() +
  theme(legend.position="none")
p

# 在点图外呈现直方图
ggMarginal(p, type="histogram")

#箱型图---------------------------------
"和小提琴图作用基本一样，不同在于，箱型图显示中值和第一个、第三个四分位数"

#基本箱型图
head(mtcars)
"气缸数和每公里耗油"
ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")


ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + 
  geom_boxplot(
    
    # 箱子刑场
    color="blue",
    fill="blue",
    alpha=0.2,
    
    # notch
    notch=T,
    notchwidth = 0.8,
    
    # 异常值
    outlier.colour="red",
    outlier.fill="red"
    
  )

#强调某一个箱
mtcars$cyl = factor(mtcars$cyl)
mtcars %>%
  mutate(cyl=factor(cyl))%>%
  mutate( type=ifelse(cyl==6,"Highlighted","Normal")) %>%
  ggplot( aes(x=cyl, y=mpg, fill=type, alpha=type)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#69b3a2", "grey")) +
  scale_alpha_manual(values=c(1,0.1)) +
  theme_ipsum() +
  theme(legend.position = "none") +
  xlab("") 

#分组画箱型图
"假设有下面数据，病情类别，治疗和医嘱数量"
data=data.frame(variety=rep(LETTERS[1:7], each=40),
                treatment=rep(c("high","low"),each=20) ,
                note=seq(1:280)+sample(1:150, 280, replace=T))
head(data)

ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot()


#山脊图----------------------------
"把一堆density plot、直方图整合到一起，
看多组连续变量的差异"
# library
library(ggridges)
library(ggplot2)

"钻石类别和价格的关系"
head(diamonds)

#最基本的山脊图（组合density plot）
ggplot(diamonds, aes(x = price, y = cut, fill = cut)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

#组合直方图形成山脊图
ggplot(diamonds, aes(x = price, y = cut, fill = cut)) +
  geom_density_ridges(stat="binline", bins=20) +
  theme_ridges() + 
  theme(legend.position = "none")

#热力山脊图
library(viridis)
"月份与温度"
head(lincoln_weather)

ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
