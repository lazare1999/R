# Load required packages
library(datasets) # contains iris dataset
library(cluster)  # clustering algorithms
library(factoextra) # visualization
library(purrr) # to use map_dbl() function
df
# Load and preprocess the dataset
df <- iris[, 1:4]
df <- na.omit(df)
df <- scale(df)
df
# Dissimilarity matrix
d <- dist(df, method = "euclidean")
d
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

rect.hclust(hc1 , k = 3, border = 2:6)

# Phylogenic
Phylo <- fviz_dend(hc1, cex = 0.8, lwd = 0.8, k = 3,
                   rect = TRUE,
                   k_colors = "jco",
                   rect_border = "jco",
                   rect_fill = TRUE,
                   type = "phylogenic")
Phylo

# Circular
Circ <- fviz_dend(hc1, cex = 0.8, lwd = 0.8, k = 3,
                  rect = TRUE,
                  k_colors = "jco",
                  rect_border = "jco",
                  rect_fill = TRUE,
                  type = "circular")
Circ
