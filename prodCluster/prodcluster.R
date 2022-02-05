library(ggplot2)
library(GGally)
library(dplyr)
library(caret)
library(factoextra)
source("~/Downloads/online-retail-segmentation/utils.r")

data = read.csv2("Downloads/online-retail-segmentation/prodCluster/prodcluster.csv",
                 col.names = c("stockcode", "cust_count", "revenue", "popularity", "avg_revenue"),
                 stringsAsFactors = FALSE)

dim(data)
head(data, 2)

# convert columns to numeric
sapply(data, class)
cols.num = c("revenue", "avg_revenue")
data[cols.num] = sapply(data[cols.num],as.numeric)
sapply(data, class)


# checking for missing values
vis_miss(data)

data = data[!(data$stockcode %in% c('POST', 'D', 'C2', 'DOT', 'M', 'S', 'm', 'PADS', 'B', 'CRUK')),]
dim(data)

data = data[data$avg_revenue > 0, ]
dim(data)

ggpairs(data[which(names(data) != "stockcode")], 
        upper = list(continuous = ggally_points), 
        lower = list(continuous = "points"), 
        title = "Pairplot of the Product data before Transformation & Scaling")+
        theme(axis.text.x = element_text(angle = 50, hjust = 1))


# add transformation here
data.transformed = log(1 + data[-1])


# centering data
pp = preProcess(data.transformed, method = c("center"))
data.scale = predict(pp, as.data.frame(data.transformed))

summary(data.scale)

ggpairs(data.scale, 
        upper = list(continuous = ggally_points), 
        lower = list(continuous = "points"), 
        title = "Pairplot of the Product data after Transformation and Scaling")


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
  geom_point(data = subset(err, k == 4), color = "blue") +
  ggtitle("Elbow plot for Products") +
  ylab("WCSS (Within Cluster Sum of Squares)") +
  xlab("k (Number of Clusters)") +
  xlim(1, 15)

# train kmeans with optimal k
set.seed(42)
k4 = kmeans(data.scale, centers = 4, iter.max = 1000)
k4$size

data[-1] %>%
  mutate(Cluster = k4$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("median")


fviz_cluster(k4, data = data.scale)
plot(data.scale, col=k4$cluster)

# fviz_cluster(k4, data = data.scale)

# normalization
#[1] 2313  903   75  338
# # A tibble: 4 × 5
# Cluster cust_count revenue num_sales avg_revenue
# <int>      <dbl>   <dbl>     <dbl>       <dbl>
#   1       1         15    173.        18        9.89
# 2       2         93   1635.       126       12.7 
# 3       3        385  18934.       703       27.5 
# 4       4        200   5991.       304       19.3 

# log transform
# [1]  582  964  818 1265
# A tibble: 4 × 5
# Cluster cust_count revenue num_sales avg_revenue
# <int>      <dbl>   <dbl>     <dbl>       <dbl>
#   1       1          2     17          2        5.98
# 2       2         13    145.        16        9.68
# 3       3        157   4041.       225       19.1 
# 4       4         53    755.        67       11.2 


# log transform - standardization
# [1]  667  959  784 1219
# # A tibble: 4 × 5
# Cluster cust_count revenue num_sales avg_revenue
# <int>      <dbl>   <dbl>     <dbl>       <dbl>
#   1       1          3    20.0         3        7.5 
# 2       2         15   171.         18        9.92
# 3       3        160  4065.        230       18.1 
# 4       4         56   799.         70       11.3 


# final
# A tibble: 4 × 5
# Cluster cust_count revenue popularity avg_revenue
# <int>      <dbl>   <dbl>      <dbl>       <dbl>
#   1       1         15    170.         18        9.79 - not kadak
# 2       2         93   1634.        126       12.7  - potentially kadak
# 3       3        385  18934.        703       27.5  - super kadak
# 4       4        200   5991.        304       19.3  - semi kadak
