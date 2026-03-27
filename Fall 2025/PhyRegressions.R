# Run PGLS Function 

library(phytools)
library(geiger)
library(caper)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(rr2)

##read in the csv sheet that has your cancer data and your predictor variables
data <- read.csv("Spring 2026/final_clean_data_w_multivariate.csv")

data <- data %>%
  mutate(abs_shape = abs(shape_value))

##NOTE our PGLs model weighs the regression by Standard error, which we call "SE_simple"
## simple standard error is 1/sqrt(samplesize) which in this case is "n" number of necropsies 
data <- data %>%
  mutate(SE_simple = 1/sqrt(data$n))

##read phylogenetic tree in
#"min20Fixed516.nwk is a large phylogeny that includes all our vertebrates species
tree <- read.tree("min20Fixed516.nwk")

#the following steps prepare the data to be match to the phylogeny 
#once matched it prunes the phylogeny and the data to be symetrical
data$Species <- gsub(" ", "_", data$Species) 
includedSpecies<-data$Species
pruned.tree<-drop.tip(
  tree, setdiff(
    tree$tip.label, includedSpecies))
pruned.tree <- keep.tip(pruned.tree,pruned.tree$tip.label)
data$Keep <- data$Species %in% pruned.tree$tip.label
data <- data[!(data$Keep==FALSE),]
rownames(data)<-data$Species
SE<-setNames(data$SE_simple,data$Species)[rownames(data)]


abs_cancer<-pglsSEyPagel(cancer_prevalence~abs_shape,data=data,tree=pruned.tree,se=SE,method = "ML")

summary(abs_cancer)

#grab r squared, p value, and lambda from summary so we can plot it 

r.v.abs_cancer <- R2(phy = pruned.tree,abs_cancer)
r.v.abs_cancer <- format(r.v.abs_cancer[3])
r.v.abs_cancer <-signif(as.numeric(r.v.abs_cancer), digits= 2)
ld.v.abs_cancer <- summary(abs_cancer)$modelStruct$corStruct
ld.v.abs_cancer <- signif(ld.v.abs_cancer[1], digits = 2)
p.v.abs_cancer <-summary(abs_cancer)$tTable
p.v.abs_cancer <-signif(p.v.abs_cancer[2,4], digits = 2)


ggplot(data, aes(x = abs_shape, y = cancer_prevalence, color = Class, size = n)) +
  geom_point(alpha = 1) +
  scale_size_continuous(range = c(1, 5), guide = "none") +  # hide size legend
  geom_abline(intercept = coef(abs_cancer)[1], 
              slope = coef(abs_cancer)[2],
              color = 'grey', size = 1.2) +
  theme_minimal() +
  labs(
    x = "Distance from Type II Survivorship",
    y = "Cancer Prevalence (%)",
    color = "Class"
  ) + 
  labs(
    title = "Cancer Prevalence vs. Distance from Type II Survivorship", 
    subtitle = bquote(p-value:.(p.v.abs_cancer)~R^2:.(r.v.abs_cancer)~Lambda:.(ld.v.abs_cancer))
  ) +
  theme_cowplot(12)


##you can run this to save the plot to your working directory 
ggsave(filename='abs_cancer.png', width=10, height=8, limitsize=FALSE,bg="white")