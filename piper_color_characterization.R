library(patternize)
library(recolorize)
library(ggplot2)

# set directory to location of the image files
setwd("C:/Users/anush/OneDrive/Anusha's Desktop/College/Applications-Internships-Research/Piper Color Sorting/Color Files/cropped_ripe/PNG files")

# make a list of image file paths
imgs <- list.files("C:/Users/anush/OneDrive/Anusha's Desktop/College/Applications-Internships-Research/Piper Color Sorting/Color Files/cropped_ripe/PNG files",full.names = TRUE, recursive = TRUE)

# set the bin size to the desired number of clusters
bin_size <- 20

# make an empty list for storing the recolorize objects
rc_list <- vector("list", length(imgs))
names(rc_list) <- names(imgs)

# for every image, run the same recolorize2 function to fit a recolorize object
for (i in 1:length(imgs)) {
  rc_list[[i]] <- recolorize2(imgs[[i]], bins = bin_size,
                              cutoff = 35, plotting = FALSE)
}

# get a dataframe of all colors:
all_palettes <- do.call(rbind, lapply(rc_list, function(i) i$centers))              

# and for cluster sizes (as a proportion of their original image):
all_sizes <- do.call(c, lapply(rc_list, function(i) i$sizes))

# plot colors using hclust and return grouping list:
# par(mar = rep(1, 1))
cluster_list <- hclust_color(all_palettes, n_final = bin_size)

# make an empty matrix for storing the new palette
piper_palette <- matrix(NA, ncol = 3, nrow = length(cluster_list))

# for every color in cluster_list...
for (i in 1:length(cluster_list)) {
  
  # get the center indices
  idx <- cluster_list[[i]]
  
  # get the average value for each channel, using cluster size to get a weighted average
  ctr <- apply(all_palettes, 2, 
               function(j) weighted.mean(j[idx], 
                                         w = all_sizes[idx]))
  
  # store in the palette matrix
  piper_palette[i, ] <- ctr
}

# check that our colors seem reasonable
#par(mar = rep(0, 4))
plotColorPalette(piper_palette)

# map every image to the same set of clustered colors
impose_list <- lapply(imgs, function(i) imposeColors(i, piper_palette, 
                                                     adjust_centers = FALSE, 
                                                     plotting = FALSE))

# make a matrix that will contains the percentage of mapping to each color cluster
every_size <- matrix(0,228,bin_size)
j <- 0

# add those percentages to the matrix
for (i in impose_list) {
  percent <- as.matrix((i$sizes)/sum(i$sizes))
  j <- j+1
  every_size[j,1:bin_size] <- percent
}

# list of image file names
file_names <- list.files(path="C:/Users/anush/OneDrive/Anusha's Desktop/College/Applications-Internships-Research/Piper Color Sorting/Color Files/cropped_ripe/PNG files", pattern=".png", all.files=TRUE,
                         full.names=FALSE)

names <- as.matrix(file_names)

# vector of species names
species <- c('aduncum','aequale','augustum','auritifolium','auritum','colonense','concepcionis','cyanophyllum',
               'darianense','dryadum','garag','generalense','holdrigeanum','imperiale','melanocladum',
               'multiplinervium','nudifolium','paulownifolium','peltatum','peracuminatum','reticulatum',
               'sanctifelicis','silvivagum','spD','sublineatum','tonduzii','umbricola','urostachyum','xanthostachyum')

# make an empty matrix to contain the averages (by species) of the percents
averages = matrix(, nrow = length(species), ncol = bin_size)

# add the average values into that matrix
for (spec in species) {
  species_number <- which(species == spec)
  for (k in 1:bin_size) {
    averages[species_number,k] <- mean(every_size[,k][which(grepl(spec,names)== TRUE)])
  }
}

# get PC values of averaged data
pca <- prcomp(averages, scale.=TRUE)
scores = as.data.frame(pca$x)

# make a PCA plot (PC1 vs. PC2) with species names
ggplot(data = scores, aes(x = PC1, y = PC2, label = species)) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "tomato", alpha = 0.8, size = 4) +
  ggtitle("PCA plot with 20 bins")

# make a PCA plot (PC1 vs. PC2) with dots (no names)
plot(scores$PC1, scores$PC2)

# make a PCA plot (PC2 vs. PC3) with species names
ggplot(data = scores, aes(x = PC2, y = PC3, label = species)) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "tomato", alpha = 0.8, size = 4) +
  ggtitle("PCA plot with 20 bins")

# make a PCA plot (PC2 vs. PC3) with dots (no names)
plot(scores$PC2, scores$PC3)

# save RData of the PCA averages
save(pca, file = "pca_avg_20.RData")
