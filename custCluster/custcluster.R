library(ggplot2) 
library(GGally)
library(dplyr)
library(caret)
source("~/Downloads/online-retail-segmentation/utils.r")

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

dim(data)
# data = data[!data$customerID %in% c(17450, 18102), ]
# dim(data)

ggpairs(data[which(names(data) != "customerID")], 
        upper = list(continuous = ggally_points), 
        lower = list(continuous = "points"), 
        title = "Products after outlier removal")

# centering data
pp = preProcess(data[-1], method = c("center"))
data.scale = predict(pp, as.data.frame(data[-1]))

summary(data.scale)

# check correlation
cor(data.scale, method = "pearson")

# plot elbow
err = multiKmeans(data.scale, 1, 15, 1000)
plot(err, pch=19, type='b')

# run kmeans with optimal k
set.seed(42)
k4 = kmeans(data.scale, centers = 5, iter.max = 1000)
k4$size

data %>%
 mutate(Cluster = k4$cluster) %>%
 group_by(Cluster) %>%
 summarise_all("median")



# 1 - Frequent
# 2 - Occasional
# 3 - Sale
# 4 - SME

data$cluster = k4$cluster
data[data$cluster == 1, ]

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

