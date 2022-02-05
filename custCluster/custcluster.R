library(ggplot2) 
library(GGally)
library(dplyr)
library(caret)
library(naniar)
library('ggrepel')

source("~/Downloads/online-retail-segmentation/utils.r")

data = read.csv2("~/Downloads/online-retail-segmentation/custCluster/custcluster.csv", 
                 col.names = c("customerID", "prod_purchased", "basket", "revenue", "num_visits", "avg_spend", "recency"),
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
ggpairs(data.scale, 
        upper = list(continuous = ggally_points), 
        lower = list(continuous = "points"),
        title = "Pairplot of the Customer data after transformation")

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
  geom_point(data = subset(err, k == 4), color = "blue") +
  ggtitle("Elbow plot for Customers") +
  ylab("WCSS (Within Cluster Sum of Squares)") +
  xlab("k (Number of Clusters)")

# run kmeans with optimal k
set.seed(42)
k4 = kmeans(data.scale, centers = 4, iter.max = 1000)
k4$size

data %>%
 mutate(Cluster = k4$cluster) %>%
 group_by(Cluster) %>%
 summarise_all("median")

fviz_cluster(k4, data = data.scale)
plot(data.scale, col = k4$cluster)

head(data)
data$cluster = k4$cluster
write.csv(data, "~/Downloads/online-retail-segmentation/custCluster/Cluster.csv")

# outlier points
# customerID prod_purchased basket  revenue num_visits avg_spend cluster
# 3340      17450          69029    127 189147.0         47  4024.405       1
# 3811      18102          64122    151 251594.3         61  4124.497       1

# FINAL
# [1]  773 1342 1324  473
# # A tibble: 4 × 8
# Cluster customerID prod_purchased basket revenue num_visits avg_spend recency
# <int>      <dbl>          <dbl>  <dbl>   <dbl>      <dbl>     <dbl>   <dbl>
#   1       1     15410           2005     122   3266.          9     368.       32 - high priority 
# 2       2     15606.           180.     20    318.          1     199.      114 - occasional
# 3       3     15600.           577      52    960.          4     275.       55 - Frequent
# 4       4     15565             50       6    114.          1      95.6     204 - rare


