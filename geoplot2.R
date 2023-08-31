rm(list = ls())

# Geospatial data available at the geojson format
library(geojsonio)
spdf <- geojson_read("C:/Users/77387/Desktop/Data_analysis_courses/data/communes.geojson",  what = "sp")

spdf@data
# 由于数据量太大，只挑选了其中部分
spdf <- spdf[ substr(spdf@data$code,1,2)  %in% c("06", "83", "13", "30", "34", "11", "66") , ]

library(broom)
library(ggplot2)
# 使用tidy将数据转化为ggplot2 接受的格式
spdf_fortified <- tidy(spdf, region = "code")
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), 
               fill="white", color="grey") +
  theme_void() +
  coord_map()

# 读取数据变量
data <- read.table("C:/Users/77387/Desktop/Data_analysis_courses/data/data_on_french_states.txt", header=T, sep=";")
head(data)

# 餐馆数量分布？
library(dplyr)
data %>%
  ggplot( aes(x=nb_equip)) +
  geom_histogram(bins=20, fill='skyblue', color='#69b3a2') + scale_x_log10()

# 空间数据和数值数据合并
library(dplyr)
spdf_fortified = spdf_fortified %>%
  left_join(. , data, by=c("id"="depcom"))

head(spdf_fortified)

ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = nb_equip, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()

# 画的更精美一些
library(viridis)
p <- ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = nb_equip, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(trans = "log", breaks=c(1,5,10,20,50,100), name="Number of restaurant", 
                     guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                           keywidth=unit(12, units = "mm"), 
                                           label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "South of France Restaurant concentration",
    subtitle = "Number of restaurant per city district"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.title = element_text(size= 22,  color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    
    legend.position = c(0.7, 0.09)
  ) +
  coord_map()
p
#--------------------读取本地的shp文件------------------------------------
# 使用 rgdal 读取 shapefile-----------------
library(rgdal)
my_spdf <- readOGR( 
  dsn= "C:/Users/77387/Desktop/Data_analysis_courses/data/world_shape_file/" , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

# 取非洲数据
# Select Africa only
africa <- my_spdf[my_spdf@data$REGION==2 , ]
# Plot
plot(africa , xlim=c(-20,60) , ylim=c(-40,40))


# 2005各国人口量？
africa@data$POP2005
# 转化为数值
africa@data$POP2005 <- as.numeric( africa@data$POP2005 )

library(dplyr)
africa@data %>% 
  ggplot( aes(x=as.numeric(POP2005))) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white')

# 填充到地图中
library(RColorBrewer)
my_colors <- brewer.pal(9, "Reds") 
my_colors <- colorRampPalette(my_colors)(30)

class_of_country <- cut(africa@data$POP2005, 30) #转化成因子，有一定的等级顺序
my_colors <- my_colors[as.numeric(class_of_country)]

# 画图
# Make the plot
plot(africa , xlim=c(-20,60) , ylim=c(-40,40), col=my_colors ,  bg = "#A6CAE0")


#------如何使用leaflet中自带的地图？------------------
my_spdf <- readOGR( 
  dsn= "C:/Users/77387/Desktop/Data_analysis_courses/data/world_shape_file/" , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

my_spdf@data$POP2005[which(my_spdf@data$POP2005==0)]=NA

my_spdf@data$POP2005 <- as.numeric(my_spdf@data$POP2005) / 1000000 %>% round(2)

library(leaflet)
library(ggplot2)
mypalette <- colorNumeric( palette="viridis", domain=my_spdf@data$POP2005, na.color="transparent")
mypalette(c(45,43))

m <- leaflet(my_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( fillColor = ~mypalette(POP2005), stroke=FALSE )

m


# 各国家人口分布
my_spdf@data %>% 
  ggplot( aes(x=as.numeric(POP2005))) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white') +
  xlab("Population (M)") + 
  theme_bw()

# 四分位取颜色
m <- leaflet(my_spdf)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", POP2005)(POP2005) )
m

# Bin
m <- leaflet(my_spdf)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorBin("YlOrRd", POP2005)(POP2005) )
m

# 手动设置bin
library(RColorBrewer)
mybins <- c(0,10,20,50,100,500,Inf)
mypalette <- colorBin( palette="YlOrBr", 
                       domain=my_spdf@data$POP2005, na.color="transparent", bins=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", my_spdf@data$NAME,"<br/>", 
  "Area: ", my_spdf@data$AREA, "<br/>", 
  "Population: ", round(my_spdf@data$POP2005, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

# 最终的地图
leaflet(my_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(POP2005), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~POP2005, opacity=0.9, title = "Population (M)", position = "bottomleft" )

  

