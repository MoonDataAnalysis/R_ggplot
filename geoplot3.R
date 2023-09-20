rm(list = ls())
# Libraries
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(mapdata)

#------------------- 全世界内 tweeter 使用密集程度------------------
# 8个月内tweeter使用数据
data <- read.table("C:/Users/77387/Desktop/Data_analysis_courses/data/17_ListGPSCoordinates.txt", sep=",", header=T)

# 获取地图数据
world <- map_data("world")

# 绘图
# plot
ggplot(data, aes(x=homelon, y=homelat)) + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_bin2d(bins=100) +
  annotate("text", x = 175, y = 80, label="Where people tweet about ", 
                    colour = "black", size=4, alpha=1, hjust=1) + # 添加注释
  annotate("segment", x = 100, xend = 175, y = 73, yend = 73, colour = "black",size=0.2, alpha=1) + # 添加注释
  theme_void() +
  ylim(-70, 80) +
  scale_fill_viridis(
    trans = "log", 
    breaks = c(1,7,54,403,3000),
    name="Tweet # recorded in 8 months", 
    guide = guide_legend( keyheight = unit(2.5, units = "mm"), keywidth=unit(10, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  )  +
  ggtitle( "" ) +
  theme(
    legend.position = c(0.8, 0.09),
    legend.title=element_text(color="black", size=8),
    text = element_text(color = "#22211d"),
    plot.title = element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) 

#----------可以使用6边型的bins---------
data %>%
  filter(homecontinent=='Europe') %>%
  ggplot( aes(x=homelon, y=homelat)) + 
  geom_hex(bins=59) +
  annotate("text", x = -27, y = 72, label="Where people tweet about #Surf", colour = "black", size=5, alpha=1, hjust=0) +
  annotate("segment", x = -27, xend = 10, y = 70, yend = 70, colour = "black", size=0.2, alpha=1) +
  theme_void() +
  xlim(-30, 70) +
  ylim(24, 72) +
  scale_fill_viridis(
    option="B",
    trans = "log", 
    breaks = c(1,7,54,403,3000),
    name="Tweet # recorded in 8 months", 
    guide = guide_legend( keyheight = unit(2.5, units = "mm"), keywidth=unit(10, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  )  +
  ggtitle( "" ) +
  theme(
    legend.position = c(0.8, 0.09),
    legend.title=element_text(color="black", size=8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) 

#--------------当地理单位过大时-----------------
library(maptools)
data(wrld_simpl)
afr=wrld_simpl[wrld_simpl$REGION==2,]
plot(afr)


library(tidyverse)
library(broom)
afr_fortified <- tidy(afr)
afr_fortified = afr_fortified %>% left_join(. , afr@data, by=c("id"="ISO3")) 

ggplot() +
  geom_polygon(data = afr_fortified, aes(fill = POP2005, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  coord_map() +
  theme_void()
