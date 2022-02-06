# imports
library(ggplot2)
library(GGally)
library(dplyr)
library(caret)
library(factoextra)
source("~/Downloads/online-retail-segmentation/utils.r")

# read data
data = read.csv2("Downloads/online-retail-segmentation/prodCluster/prodcluster.csv",
                 col.names = c("stockcode", "cust_count", "revenue", 
                               "popularity", "avg_revenue"),
                 stringsAsFactors = FALSE)


head(data, 2)
dim(data)

# convert columns to numeric
sapply(data, class)
cols.num = c("revenue", "avg_revenue")
data[cols.num] = sapply(data[cols.num],as.numeric)
sapply(data, class)


# checking for missing values
vis_miss(data)

# filter rows
data = data[!(data$stockcode %in% c('POST', 'D', 'C2', 'DOT', 'M', 'S', 'm', 'PADS', 'B', 'CRUK')),]
dim(data)

data = data[data$avg_revenue > 0, ]
dim(data)

ggpairs(data[which(names(data) != "stockcode")], 
        upper = list(continuous = ggally_points), 
        lower = list(continuous = "points"), 
        title = "Pairplot of the Product data before Transformation & Scaling")+
        theme(axis.text.x = element_text(angle = 50, hjust = 1))


# apply transformation
data.transformed = log(1 + data[-1])


# centering data
pp = preProcess(data.transformed, method = c("center"))
data.scale = predict(pp, as.data.frame(data.transformed))

summary(data.scale)

# plot elbow
err = multiKmeans(data.scale, 2, 15, 1000)
err = as.data.frame(err)
err$k = seq(2:15)

ggplot(err, aes(x = k, y = err)) + 
  geom_line() + 
  geom_point(size = 4) + 
  geom_label_repel(aes(label = "Optimal 'k' value"), 
                   data = subset(err, k == 4),
                   box.padding = 6,
                   position = position_dodge(width=0.9),
                   size = 6) +
  geom_point(data = subset(err, k == 4), color = "yellow") +
  ggtitle("Elbow plot for Products") +
  ylab("WCSS (Within Cluster Sum of Squares)") +
  xlab("k (Number of Clusters)") +
  xlim(1, 15)

# train kmeans with optimal k
set.seed(42)
k4 = kmeans(data.scale, centers = 4, iter.max = 1000)
k4$size

# view median for each feature
x = data[-1] %>%
  mutate(Cluster = k4$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("median")
x
write.csv(x, "~/Downloads/online-retail-segmentation/product.csv", row.names = FALSE)

# visualize clusters
fviz_cluster(k4, data = data.scale)
plot(data.scale, col=k4$cluster)

# save the results
data$cluster = k4$cluster
write.csv(data, "~/Downloads/online-retail-segmentation/prodCluster/Cluster.csv", 
          row.names = FALSE)