library(ggplot2) 
library(GGally)
library(dplyr)

set.seed(42)

data = read.csv2("Downloads/online-retail-clustering/custCluster/custcluster.csv", col.names = c("customerID", "prod_purchased", "basket", "revenue", "num_visits", "avg_spend"))
head(data)

# checking for missing values
sum(is.na(data))

sapply(data, class)
cols.num = c("revenue", "avg_spend")
data[cols.num] <- sapply(data[cols.num],as.numeric)

sapply(data, class)

ggpairs(data[which(names(data) != "customerID")], upper = list(continuous = ggally_points), lower = list(continuous = "points"), title = "Products before outlier removal")
summary(data)

sum(data[data$revenue > 0, ]$revenue)

data.scale = scale(data[-1], scale=FALSE)

data.scale

source("~/Downloads/online-retail-clustering/custCluster/multikmeans.r")
err = multiKmeans(data.scale, 1, 10, 1000)
plot(err)

k4 = kmeans(data, centers = 4, iter.max = 1000)
k4$cluster


data %>%
 mutate(Cluster = k4$cluster) %>%
 group_by(Cluster) %>%
 summarise_all("mean")

# 1 - Frequent
# 2 - Occasional
# 3 - Sale
# 4 - SME

k4$cluster
data
data$cluster = k4$cluster
data[data$cluster == 1, ]

k4$size
