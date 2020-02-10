library(keras)
library(magick)  # for preprocessing images
library(tidyverse)
library(imager)
library(here)
library(Rtsne)

model <- application_vgg16(weights = "imagenet", 
                           include_top = FALSE)
model

image_prep <- function(x) {
  arrays <- lapply(x, function(path) {
    img <- image_load(path, target_size = c(224, 224), grayscale = FALSE)
    x <- image_to_array(img)
    x <- array_reshape(x, c(1, dim(x)))
    x <- imagenet_preprocess_input(x, mode="tf")
  })
  do.call(abind::abind, c(arrays, list(along = 1)))
}

image_files_path <- here::here("LilyAI/old_patterns_data/patterns")

file_list <- list.files(image_files_path, full.names = TRUE, recursive = TRUE)
head(file_list)

vgg16_feature_list <- data.frame()

for (image in file_list) {
  
  print(image)
  cat("Image", which(file_list == image), "from", length(file_list))
  
  vgg16_feature <- predict(model, image_prep(image))
  
  flatten <- as.data.frame.table(vgg16_feature, responseName = "value") %>%
    select(value)
  flatten <- cbind(image, as.data.frame(t(flatten)))
  
  vgg16_feature_list <- rbind(vgg16_feature_list, flatten)
}

save(vgg16_feature_list, file = here::here("vgg16_feature_list.RData"))

load(here::here("vgg16_feature_list.RData"))


pca <- prcomp(vgg16_feature_list[, -1],
              center = TRUE,
              scale = FALSE)

save(pca, file = here::here("pca.RData"))
load(here::here("pca.RData"))

tsne <- Rtsne(as.matrix(vgg16_feature_list[, -1]))

data.frame(PC1 = pca$x[, 1], 
           PC2 = pca$x[, 2]) %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point()

data.frame(x = tsne$Y[,1], 
           y = tsne$Y[,2]) %>% 
  ggplot(aes(x=x, y=y)) +
  geom_point()

set.seed(50)
cluster_pca <- kmeans(pca$x[, 1:10], 6)
cluster_feature <- kmeans(vgg16_feature_list[, -1], 6)

cluster_list <- data.frame(cluster_pca = cluster_pca$cluster, 
                           cluster_feature = cluster_feature$cluster,
                           vgg16_feature_list) %>%
  select(cluster_pca, cluster_feature, image)

head(cluster_list)

cluster_list %>%
  count(cluster_pca)

cluster_list %>%
  count(cluster_feature)

cluster_list %>%
  mutate(PC1 = pca$x[, 1],
         PC2 = pca$x[, 2]) %>%
  ggplot(aes(x = PC1, y = PC2, color = factor(cluster_feature))) +
  geom_point()

cluster_list %>%
  mutate(tsne1 = tsne$Y[, 1],
         tsne2 = tsne$Y[, 2]) %>%
  ggplot(aes(x = tsne1, y = tsne2, color = factor(cluster_feature))) +
  geom_point()

plot_random_images <- function(n_img = 16,
                               cluster = 1,
                               rows = 4,
                               cols = 4) {
  cluster_list_random_cl_1 <- cluster_list %>%
    filter(cluster_feature == cluster) %>%
    sample_n(n_img)
  
  graphics::layout(matrix(c(1:n_img), rows, cols, byrow = TRUE))
  for (i in 1:n_img) {
    path <- as.character(cluster_list_random_cl_1$image[i])
    img <- load.image(path)
    plot(img, axes = FALSE)
    title(main = paste("Cluster PCA", cluster))
  }
}

sapply(c(1:6), function(x) plot_random_images(cluster = x))

dim(vgg16_feature_list)

