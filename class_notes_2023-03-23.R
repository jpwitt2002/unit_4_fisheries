# 2023-03-23
# JPW

library(tidyverse)
library(palmerpenguins)

head(penguins)

pen_drop_na = penguins %>%
  drop_na()

pen_num = pen_drop_na %>%
  select(ends_with("mm"), body_mass_g)

head(pen_num)

pen_meta = pen_drop_na %>%
  select(species, sex, island, year)

head(pen_meta)

# make sure you remove na before you separate

# run the PCA
pen_pca = prcomp(pen_num, scale. = T, center = T)
summary(pen_pca)

str(pen_pca) # structure
pen_pca$sdev


dim(pen_num)
dim(pen_pca$x)


str(summary(pen_pca))

summary(pen_pca)$importance[2, ]

#calculate proportion of variance from sdevs (literally what we just did but diy)
(pen_pca$sdev)^2/sum(pen_pca$sdev^2)

# scree plot
plot(pen_pca)
#this one looks silly so no

pca_scree = data.frame(pc=c(1:4), 
                       var = summary(pen_pca)$importance[2, ])
ggplot(data=pca_scree, aes(x=pc, y=var)) +
  geom_col() + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  ggtitle("Scree Plot") + 
  ylab("Proportion of Variance Explained")

# Create biplot
head(pen_pca$x)
pen_pca_meta = cbind(pen_pca$x, pen_meta)
head(pen_pca_meta)

ggplot() + 
  geom_point(aes(x=PC1, y=PC2, color=species, shape=sex), data=pen_pca_meta) +
  coord_fixed(ratio=1)
#clear variation between sexes and species


# other, nicer, fancier things
# library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)

biplot(pen_pca)


ggbiplot(pen_pca, scale=1, groups=pen_meta$species, ellipse=T, alpha=0) + 
  geom_point(aes(color=pen_meta$species, shape=pen_meta$sex)) + 
  xlim(-3,3) + 
  theme_bw()


# look at PC3 and PC4
ggbiplot(pen_pca, scale=1, group=pen_meta$species, ellipse=T, choices=c(3,4))
# if PC1 and 2 look like this, literally just start over

