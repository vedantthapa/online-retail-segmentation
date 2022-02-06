# imports
library(ggplot2) 
library(GGally)
library(dplyr)
library(caret)
library(naniar)
library(ggrepel)
source("~/Downloads/online-retail-segmentation/utils.r")

# read data
data = read.csv2("~/Downloads/online-retail-segmentation/custCluster/custcluster.csv", 
                 col.names = c("customerID", "prod_purchased", "basket", 
                               "revenue", "num_visits", "avg_spend", "recency"),
                 stringsAsFactors = FALSE)
head(data, 2)
dim(data)

# convert columns to numeric
sapply(data, class)
cols.num = c("revenue", "avg_spend")
data[cols.num] = sapply(data[cols.num],as.numeric)
sapply(data, class)

# checking for missing values
vis_miss(data)

# keep only revenue > 0
data = data[data$revenue >= 0, ]
dim(data)

ggpairs(data[which(names(data) != "customerID")], 
        upper = list(continuous = ggally_points), 
        lower = list(continuous = "points"), 
        title = "Pairplot of the Customer data before transformation")+
        theme(axis.text.x = element_text(angle = 50, hjust = 1))

# Apply transformation
data.transformed = log(1 + data)

# centering data
pp = preProcess(data.transformed[-1], method = c("center"))
data.scale = predict(pp, as.data.frame(data.transformed[-1]))

summary(data.scale)

# plot elbow
err = multiKmeans(data.scale, 1, 15, 1000)
err = as.data.frame(err)
err$k = seq(1:15)

ggplot(err, aes(x = k, y = err)) + 
  geom_line() + 
  geom_point(size = 4) + 
  geom_label_repel(aes(label = "Optimal 'k' value"), 
                   data = subset(err, k == 4),
                   box.padding = 6,
                   position = position_dodge(width=0.9),
                   size = 6) +
  geom_point(data = subset(err, k == 4), color = "yellow") +
  ggtitle("Elbow plot for Customers") +
  ylab("WCSS (Within Cluster Sum of Squares)") +
  xlab("k (Number of Clusters)")

# run kmeans with optimal k
set.seed(42)
k4 = kmeans(data.scale, centers = 4, iter.max = 1000)
k4$size

# view median for each feature
x = data[-1] %>%
 mutate(Cluster = k4$cluster) %>%
 group_by(Cluster) %>%
 summarise_all("median")
x
write.csv(x, "~/Downloads/online-retail-segmentation/customer.csv", row.names = FALSE)

# visualize clusters
fviz_cluster(k4, data = data.scale)
plot(data.scale, col = k4$cluster)

# save the results
data$cluster = k4$cluster
write.csv(data, "~/Downloads/online-retail-segmentation/custCluster/Cluster.csv", 
          row.names = FALSE)