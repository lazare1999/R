library(devtools)
library(ggbiplot)

iris.pca <- prcomp(iris[, 1:4], center = TRUE, scale = TRUE)

ggbiplot(iris.pca)
ggbiplot(iris.pca, labels=rownames(iris))
ggbiplot(iris.pca, labels=rownames(iris), ellipse=TRUE, groups=iris$Species)
ggbiplot(iris.pca, labels=rownames(iris), ellipse=TRUE, groups=iris$Species, choices=c(3,4))
ggbiplot(iris.pca, labels=rownames(iris), ellipse=TRUE, groups=iris$Species, circle=TRUE)
ggbiplot(iris.pca, labels=rownames(iris), ellipse=TRUE, groups=iris$Species, obs.scale = 1, var.scale = 1)
ggbiplot(iris.pca, labels=rownames(iris), ellipse=TRUE, groups=iris$Species, obs.scale = 1, var.scale = 1, var.axes=FALSE)
ggbiplot(iris.pca, labels=rownames(iris), ellipse=TRUE, groups=iris$Species, obs.scale = 1, var.scale = 1) +
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "dark blue"))+
  ggtitle("PCA of iris dataset")+
  theme_minimal()+
  theme(legend.position = "bottom")