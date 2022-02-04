library(ggplot2)
library(GGally)
library(dplyr)
library(caret)
source("~/Downloads/online-retail-segmentation/utils.r")

data = read.csv2("Downloads/online-retail-segmentation/prodCluster/prodcluster.csv",
                 col.names = c("stockcode", "cust_count", "revenue", "num_sales", "avg_revenue"),
                 stringsAsFactors = FALSE)
head(data, 2)

# checking for missing values
sum(is.na(data))

# convert columns to numeric
sapply(data, class)
cols.num = c("revenue", "avg_revenue")
data[cols.num] = sapply(data[cols.num],as.numeric)
sapply(data, class)

dim(data)

data = data[!(data$stockcode %in% c('POST', 'D', 'C2', 'DOT', 'M', 'S', 'm', 'PADS', 'B', 'CRUK')),]

dim(data)

ggpairs(data[which(names(data) != "stockcode")], 
        upper = list(continuous = ggally_points), 
        lower = list(continuous = "points"), 
        title = "Products before outlier removal")

dim(data)
data = data[data$avg_revenue > 0,]
dim(data)

# add transformation here
data.transformed = data.frame(data)
dim(data.transformed)

# centering data
pp = preProcess(data.transformed[-1], method = c("range"))
data.scale = predict(pp, as.data.frame(data.transformed[-1]))

summary(data.scale)


# plot elbow
err = multiKmeans(data.scale, 1, 15, 1000)
plot(err, pch=19, type='b')

# run kmeans with optimal k
set.seed(42)
k4 = kmeans(data.scale, centers = 4, iter.max = 1000)
k4$size

data[-1] %>%
  mutate(Cluster = k4$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("median")

# fviz_cluster(k4, data = data.scale)