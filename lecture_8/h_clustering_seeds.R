library(datasets) # contains iris dataset
library(cluster)  # clustering algorithms
library(factoextra) # visualization
library(purrr) # to use map_dbl() function

set.seed(786)
file_loc <- 'seeds_dataset.txt'
seeds_df <- read.csv(file_loc,sep = '\t',header = FALSE)

feature_name <- c('area','perimeter','compactness','length.of.kernel','width.of.kernal','asymmetry.coefficient','length.of.kernel.groove','type.of.seed')
colnames(seeds_df) <- feature_name

str(seeds_df)
summary(seeds_df)
any(is.na(seeds_df))

seeds_label <- seeds_df$type.of.seed
seeds_df$type.of.seed <- NULL
str(seeds_df)

seeds_df <- na.omit(seeds_df)

seeds_df_sc <- as.data.frame(scale(seeds_df))
summary(seeds_df_sc)

dist_mat <- dist(seeds_df_sc, method = 'euclidean')

hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 3)

plot(hclust_avg)
rect.hclust(hclust_avg , k = 3, border = 2:6)
abline(h = 3, col = 'red')

suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)

suppressPackageStartupMessages(library(dplyr))
seeds_df_cl <- mutate(seeds_df, cluster = cut_avg)
count(seeds_df_cl,cluster)

suppressPackageStartupMessages(library(ggplot2))
ggplot(seeds_df_cl, aes(x=area, y = perimeter, color = factor(cluster))) + geom_point()

table(seeds_df_cl$cluster,seeds_label)