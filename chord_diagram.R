# chord diagram 和弦图
# library
library(circlize)

# Create data
set.seed(123)
data = data.frame(
  factor = sample(letters[1:8], 1000, replace = TRUE),
  x = rnorm(1000), 
  y = runif(1000)
)

head(data)
# 初始化一个图
par(mar = c(1, 1, 1, 1) ) 
circos.initialize(factors = data$factor, x = data$x )

# 根据因子生成基本的组
circos.trackPlotRegion(factors = data$factor, y=data$y , bg.col = rgb(0.1,0.1,seq(0,1,0.1),0.4) , bg.border = NA)

# 添加链接
circos.link("a", 0, "b", 0, h = 0.4)

# Add a link between a point and a zone
circos.link("e", 0, "g", c(-1,1), col = "green", lwd = 2, lty = 2, border="black" )

# Add a link between a zone and another
circos.link("c", c(-0.5, 0.5), "d", c(-0.5,0.5), col = "red", border = "blue", h = 0.2)


# Create an adjacency matrix: 
# a list of connections between 20 origin nodes, and 5 destination nodes:
numbers <- sample(c(1:1000), 100, replace = T)
data <- matrix( numbers, ncol=5)
rownames(data) <- paste0("orig-", seq(1,20))
colnames(data) <- paste0("dest-", seq(1,5))
head(data)
# Load the circlize library
library(circlize)

# Make the circular plot
chordDiagram(data, transparency = 0.5)

# 调整一下组名的位置--------------------
# Create an edge list: a list of connections between 10 origin nodes, and 10 destination nodes:
origin <- paste0("orig ", sample(c(1:10), 20, replace = T))
destination <- paste0("dest ", sample(c(1:10), 20, replace = T))
data <- data.frame(origin, destination)

# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(origin, destination))

# Charge the circlize library
library(circlize)

# Make the circular plot
chordDiagram(adjacencyData, transparency = 0.5)


#======来一个实际例子================
"1960年至2015年间全球双边移民流量估算"

# Load dataset from github
data <- read.table("C:/Users/77387/Desktop/Data_analysis_courses/data/13_AdjacencyDirectedWeighted.txt", header=TRUE)
head(data)
# short names
colnames(data) <- c("Africa", "East Asia", "Europe", "Latin Ame.",   "North Ame.",   "Oceania", "South Asia", "South East Asia", "Soviet Union", "West.Asia")
rownames(data) <- colnames(data)
chordDiagram(data, transparency = 0.5)
