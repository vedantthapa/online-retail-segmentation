library(ggplot2) 
library(GGally)
library(dplyr)
library(caret)
library(naniar)
library(gridExtra)
library(patchwork)
source("~/Downloads/online-retail-segmentation/utils.r")

data = read.csv2("~/Downloads/online-retail-segmentation/custCluster/custCusterRecency.csv", 
                 col.names = c("customerID", "prod_purchased", "basket", "revenue", "num_visits", "avg_spend", "recency"),
                 stringsAsFactors = FALSE)
head(data, 2)
dim(data)

# checking for missing values
vis_miss(data)

# convert columns to numeric
sapply(data, class)
cols.num = c("revenue", "avg_spend")
data[cols.num] = sapply(data[cols.num],as.numeric)
sapply(data, class)

# keep only revenue > 0
data = data[data$revenue >= 0, ]

summary(data[-1])

head(data)

ggpairs(data[which(names(data) != "customerID")], 
        upper = list(continuous = ggally_points), 
        lower = list(continuous = "points"), 
        title = "Pairplot of the data before transformation")+
        theme(axis.text.x = element_text(angle = 50, hjust = 1))

dim(data)

data.transformed = log(1 + data)
data.transformed
ggpairs(data.transformed[which(names(data.transformed) != "customerID")], 
        upper = list(continuous = ggally_points), 
        title = "Pairplot of the data after transformation")

# centering data
pp = preProcess(data.transformed[-1], method = c("center"))
data.scale = predict(pp, as.data.frame(data.transformed[-1]))

summary(data.scale)

ggpairs(data.scale, 
        upper = list(continuous = ggally_points), 
        lower = list(continuous = "points"),
        diag = list(continuous = "barDiag"),
        title = "Pairplot of the data after transformation")

grid.arrange(ggp1, ggp2, ncol = 2) 

# plot elbow
err = multiKmeans(data.scale, 1, 15, 1000)
plot(err, pch=19, type='b')

# run kmeans with optimal k
set.seed(42)
k4 = kmeans(data.scale, centers = 4, iter.max = 1000)
k4$size

x = data %>%
 mutate(Cluster = k4$cluster) %>%
 group_by(Cluster) %>%
 summarise_all("median")
x

fviz_cluster(k4, data = data.scale)

# 1 - Frequent
# 2 - Occasional
# 3 - Sale
# 4 - SME

dummy = data.frame(data)
dummy$cluster = k4$cluster
dummy[dummy$cluster == 1, ]

dummy[dummy$customerID %in% c(17450, 18102), ]

# outlier points
# customerID prod_purchased basket  revenue num_visits avg_spend cluster
# 3340      17450          69029    127 189147.0         47  4024.405       1
# 3811      18102          64122    151 251594.3         61  4124.497       1


# # A tibble: 4 × 7
# Cluster customerID prod_purchased basket revenue num_visits avg_spend
# <int>      <dbl>          <dbl>  <dbl>   <dbl>      <dbl>     <dbl>
#   1       1      15306           609      49   1008.          4      281.
# 2       2      15689            80      10    152.          1      114.
# 3       3      15661           201      22    356.          1      243.
# 4       4      15557           376.     35    649.          3      232.
# 
# 
# # A tibble: 4 × 7
# # 3949
# Cluster customerID prod_purchased basket revenue num_visits avg_spend
# <int>      <dbl>          <dbl>  <dbl>   <dbl>      <dbl>     <dbl>
#   1       1     17776          66576.   139  220371.         54     4074.
# 2       2     15311          25646    120   38995.         45     1096.
# 3       3     15296.          3931    148.   6461.         15      439.
# 4       4     15586            318     32     556.          2      225.
# 

# # A tibble: 4 × 7
# Cluster customerID prod_purchased basket revenue num_visits avg_spend
# <int>      <dbl>          <dbl>  <dbl>   <dbl>      <dbl>     <dbl>
#   1       1      15540          34934  244.   57395.       47.5     1135. - SME
# 2       2      15099          16015   78.5  27706.       29.5      812.   - Frequent
# 3       3      15416           3366  142     5535.       14        430.   - Occasional
# 4       4      15588            310   31      538.        2        221.   - Rare
# > 
# [1]  264  909  527 1242

# [1]    2   25  236 3686
# [1]   10   24  295 3618

# log transformation
# [1]  683 1399 1336  494
# # A tibble: 4 × 7
# Cluster customerID prod_purchased basket revenue num_visits avg_spend
# <int>      <dbl>          <dbl>  <dbl>   <dbl>      <dbl>     <dbl>
#   1       1     15382           2141     127   3548.         10     385. 
# 2       2     15607            186      21    329.          2     199. 
# 3       3     15583            622.     56   1033.          4     284. 
# 4       4     15586.            52       6    115.          1      96.4


# FINAL
# [1]  773 1342 1324  473
# # A tibble: 4 × 8
# Cluster customerID prod_purchased basket revenue num_visits avg_spend recency
# <int>      <dbl>          <dbl>  <dbl>   <dbl>      <dbl>     <dbl>   <dbl>
#   1       1     15410           2005     122   3266.          9     368.       32 - high priority 
# 2       2     15606.           180.     20    318.          1     199.      114 - occasional
# 3       3     15600.           577      52    960.          4     275.       55 - Frequent
# 4       4     15565             50       6    114.          1      95.6     204 - rare

