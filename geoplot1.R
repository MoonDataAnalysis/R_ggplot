rm(list = ls())
# 使用 rgdal 读取 shapefile-----------------
library(rgdal)
my_spdf <- readOGR( 
        dsn= "C:/Users/77387/Desktop/Data_analysis_courses/data/world_shape_file/" , 
        layer="TM_WORLD_BORDERS_SIMPL-0.3",
        verbose=FALSE
)
my_spdf@data
# 使用plot()函数绘制---------------
par(mar=c(0,0,0,0))
plot(my_spdf, col="#f2f2f2", 
     bg="skyblue", lwd=0.25, border=0 )

# 从中选取一个地区
africa <- my_spdf[my_spdf@data$REGION==2 , ]
africa #非洲

# 绘制非洲
plot(africa , xlim=c(-20,60) , ylim=c(-40,35), col="steelblue", lwd=0.5 )

# 各地理单位填充缩写
library(rgeos)

# 仅选择大国
africaBig <- africa[which(africa@data$AREA>75000), ]

# 合并数据集，添加id
centers <- cbind.data.frame(data.frame(gCentroid(africaBig, byid=TRUE), id=africaBig@data$FIPS))
centers
# 在地图上显示

plot(africa , xlim=c(-20,60) , ylim=c(-40,35), lwd=0.5 )
text(centers$x, centers$y, centers$id, cex=.9, col="#69b3a2")

# 读取.geoJson数据----------------
library(geojsonio)
spdf <- geojson_read("C:/Users/77387/Desktop/Data_analysis_courses/data/communes.geojson",  what = "sp")

plot(spdf ,  col="grey")


# 使用leaflet- 创建交互性map-----------------------
library(leaflet)
#初始化 leaflet map
m <- leaflet()
m
# Then we Add default OpenStreetMap map tiles
m <- addTiles(m)
m
#或者
library(dplyr)
m <- leaflet() %>%
         addTiles()
 
# 背景1：NASA卫星俯瞰
leaflet() %>%
    addTiles() %>%
    setView(lng = 2.34, lat = 48.85, zoom = 5)

leaflet() %>%
        addTiles() %>%
        setView(lng = 2.34, lat = 48.85, zoom = 5) %>% #zoom dao 经度2.34，维度48.85
        addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")


# 背景2：自然地形
leaflet() %>% 
        addTiles() %>% 
        setView( lng = 2.34, lat = 48.85, zoom = 3 ) %>% 
        addProviderTiles("Esri.WorldImagery")
m

# 有非常多的tiles，请见：http://leaflet-extras.github.io/leaflet-providers/preview/index.html


# 只有boundaries的地图--------------------------
# 获取世界地区
library(maps)
# maps函数库的世界地图boundaries
map('world',col="grey", 
    fill=TRUE, bg="white", 
    lwd=0.05, mar=rep(0,4),
    border=0, ylim=c(-80,80) )

# mapdata函数库有更多地区的地图
library(mapdata)

# 日本、中国
map('japan',col="black", lwd=1, mar=rep(0,4) )
map('china',col="black", lwd=1, mar=rep(0,4) )

# oz函数库中有一些针对澳大利亚的边界地图
library(oz)
par(mar=rep(0,4))
oz( states=TRUE, col="#69b3a2")
# 使用MAPS，MAPDATA和oz获取最基本的边界（boundaries）
