# 分组和堆叠的 barplot（）=======================
library(ggplot2)

# 生成数据
specie <- c(rep("pandas",3),rep("cat",3),rep("dog",3),rep("tiger",3))
condition <- rep(c("height" , "weight" , "power") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)
head(data)
# 分组bar图
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")

# 堆叠bar图
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity")

# 加个主题
library(viridis)
library(hrbrthemes)
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T)+
  theme_ipsum()+
  xlab("")


# 树图（Treemap）==========================
library(treemap)
# 生成数据
group <- c("group-1","group-2","group-3")
value <- c(13,5,22)
data <- data.frame(group,value)
head(data)
# 绘制树图
treemap(data,
        index="group",
        vSize="value"
)

# 绘制多层树图-----
#生成一个数据
group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
subgroup
value <- c(13,5,22,12,11,7,3,1,23)
data <- data.frame(group,subgroup,value)
head(data)
#绘制多层树图
treemap(data,
        index=c("group","subgroup"),
        vSize="value"
) 
# 调整标签
treemap(data, index=c("group","subgroup"),     
        vSize="value", 
        
        fontsize.labels=c(15,12),                # 调整标签字体大小.一级标签大小、二级标签大小.....
        fontcolor.labels=c("white","orange"),    # 标签的颜色，一级标签字体颜色，二级标签字体颜色
        fontface.labels=c(2,1),                  # 标签字体形状设置: 1,2,3,4 代表 标准, 粗体, 斜体, 粗体加斜体...
        bg.labels=c("transparent"),              # 背景颜色
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # 标签放置位置
        inflate.labels=F,                        # 自动调整标签，长方形越大标签字越大
        
)

# 设置背景颜色
treemap(data, index=c("group","subgroup"), vSize="value", 
        palette = "Set1",                        
        title="My Treemap",                     
        fontsize.title=12,                       
        
) 


# 环形图=================================
library(ggplot2)

# 生成数据
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)
data
# 计算百分比
data$fraction = data$count / sum(data$count)

data
# 计算累积百分比
data$ymax = cumsum(data$fraction)
data
# 计算每个长方形的底
data$ymin = c(0, head(data$ymax, n=-1))
data
# 绘图
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") + # 把堆积bar图掰成环形
  xlim(c(2, 4)) # 呈现整个圆，删掉变成pie图


#环形图添加标签------
#定义每个标签的位置
data$labelPosition <- (data$ymax + data$ymin) / 2
data
#拼接标签内容
data$label <- paste0(data$category,"\n value: ", data$count)
data
#绘制图
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


# 调整环形的宽度
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  xlim(c(-2, 4)) + #反复调整xlim到你觉得最美观的程度
  theme_void() +
  theme(legend.position = "none")



# 饼图的绘制================================
# 环形图和饼图只有一步之遥
# 载入ggplot2
library(ggplot2)

# 生成一个数据
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)
data
# 最基本的饼图
ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)#添加start=0，从12点起始
  #不设置xlim就可以得到饼图

# 加标签
library(dplyr)
data <- data %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
data

#添加标签
ggplot(data, aes(x="", y=prop, fill=group)) + 
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = group), color = "white", size=6) + #根据占比填充label
  scale_fill_brewer(palette="Set1")

#系统树图==========================


library(ggraph)
library(igraph)
library(tidyverse)

# 生成边数据 
d1 <- data.frame(from="origin", to=paste("group", seq(1,5), sep=""))
d1
d2 <- data.frame(from=rep(d1$to, each=5), to=paste("subgroup", seq(1,25), sep="_"))
d2
edges <- rbind(d1, d2)
edges

# 生成系统结构
mygraph <- graph_from_data_frame(edges)

# 画出树图
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point() +
  theme_void()

# 变成环形
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal()

# 添加标签和节点
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal() +
  geom_node_text(aes( label=name, filter=leaf) , angle=90 , hjust=1, nudge_y = -0.01) +
  ylim(-.4, NA)

# 绘制漂亮的环形系统树图---------------
# 需要的包
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer) 
# 生成数据
d1=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d1
d2=data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
d2
edges=rbind(d1, d2)
edges
# 
vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) , 
  value = runif(111)
) 
vertices

# 在vertices基础上加上group
vertices$group = edges$from[ match( vertices$name, edges$to ) ]
vertices

#要添加label
vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
head(vertices,20)
vertices$angle= 90 - 360 * vertices$id / nleaves
vertices
# 是否hjust 
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)
vertices
# 如果角度为-90，就跳180度
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
vertices
# 生成图结构
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# Make the plot
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2.7, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,10) ) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))

# 使用dendextend包可以很好地汇总方形树结构
library(dendextend)
library(tidyverse)

head(mtcars)

# 根据mpg, cyl, disp分簇
dend <- mtcars %>% 
  select(mpg, cyl, disp) %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() 

dend
# 绘图
par(mar=c(7,3,1,1))  # Increase bottom margin to have the complete label
plot(dend)

# 高亮某一个簇

# 需要高亮的簇用黄色蓝色高亮
par(mar=c(1,1,1,7))
dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  plot(horiz=TRUE, axes=FALSE)
abline(v = 350, lty = 2)
# 需要高亮的簇用方块高亮
par(mar=c(9,1,1,1))
dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  plot(axes=FALSE)
rect.dendrogram( dend, k=3, lty = 5, lwd = 0, x=1, col=rgb(0.1, 0.2, 0.4, 0.1) ) 


