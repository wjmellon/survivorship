# Run PGLS Function 

library(phytools)
library(geiger)
library(caper)
library(tidyverse)
library(cowplot)
library(cowplot)

##read in the csv sheet that has your cancer data and your predictor variables
data <- read.csv("Fall 2025/Fall 2025 Clean Data/final_clean_mortality_data.csv")

data <- data %>%
  mutate(abs_shape = abs(shape_value)) %>%
  filter(!is.na(ZIMS_Cancer_Mortality))

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


abs_mortality<-pglsSEyPagel(ZIMS_Cancer_Mortality~abs_shape,data=data,tree=pruned.tree,se=SE,method = "ML")

summary(abs_mortality)

#grab r squared, p value, and lambda from summary so we can plot it 

r.v.abs_mortality <- R2(phy = pruned.tree,abs_mortality)
r.v.abs_mortality <- format(r.v.abs_mortality[3])
r.v.abs_mortality <-signif(as.numeric(r.v.abs_mortality), digits= 2)
ld.v.abs_mortality <- summary(abs_mortality)$modelStruct$corStruct
ld.v.abs_mortality <- signif(ld.v.abs_mortality[1], digits = 2)
p.v.abs_mortality <-summary(abs_mortality)$tTable
p.v.abs_mortality <-signif(p.v.abs_mortality[2,4], digits = 2)


ggplot(data, aes(x = abs_shape, y = ZIMS_Cancer_Mortality, color = Class, size = n)) +
  geom_point(alpha = 1) +
  scale_size_continuous(range = c(1, 5), guide = "none") +  # hide size legend
  geom_abline(intercept = coef(abs_mortality)[1], 
              slope = coef(abs_mortality)[2],
              color = 'grey', size = 1.2) +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Cancer Mortality (%)",
    color = "Class"
  ) + 
  labs(
    title = "Cancer Mortality vs. Absolute Value of Survivorship", 
    subtitle = bquote(p-value:.(p.v.abs_mortality)~R^2:.(r.v.abs_mortality)~Lambda:.(ld.v.abs_mortality))
  ) +
  theme_cowplot(12)


##you can run this to save the plot to your working directory 
ggsave(filename='abs_mortality.png', width=10, height=8, limitsize=FALSE,bg="white")