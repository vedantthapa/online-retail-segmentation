library(ggplot2) 
library(GGally)
library(dplyr)
library(caret)


data = read.csv2("~/Downloads/online-retail-segmentation/custCluster/custcluster.csv", 
                 col.names = c("customerID", "prod_purchased", "basket", "revenue", "num_visits", "avg_spend"),
                 stringsAsFactors = FALSE)
head(data)

# checking for missing values
sum(is.na(data))

# convert columns to numeric
sapply(data, class)
cols.num = c("revenue", "avg_spend")
data[cols.num] = sapply(data[cols.num],as.numeric)
sapply(data, class)

head(data)

ggpairs(data[which(names(data) != "customerID")], 
        upper = list(continuous = ggally_points), 
        lower = list(continuous = "points"), 
        title = "Products before outlier removal")

pp = preProcess(data[-1], method = "range")
data.scale = predict(pp, as.data.frame(data[-1]))

head(data.scale)
summary(data.scale)

cor(data.scale, method = "pearson")


source("~/Downloads/online-retail-segmentation/multikmeans.r")
err = multiKmeans(data.scale, 1, 15, 1000)
plot(err, pch=19, type='b')

set.seed(42)
k4 = kmeans(data.scale, centers = 4, iter.max = 1000)

data %>%
 mutate(Cluster = k4$cluster) %>%
 group_by(Cluster) %>%
 summarise_all("median")

# 1 - Frequent
# 2 - Occasional
# 3 - Sale
# 4 - SME

# data$cluster = k4$cluster
# data[data$cluster == 1, ]




