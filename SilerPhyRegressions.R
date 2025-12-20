# Run PGLS Function 

library(phytools)
library(geiger)
library(caper)
library(tidyverse)
library(cowplot)
library(cowplot)
library(ggplot2)

##read in the csv sheet that has your cancer data and your predictor variables
data <- read.csv("Fall 2025/Siler/siler_parameters_all_min50species.csv")

data <- data %>%
  filter(FLAG == "0")

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


siler_a2_cancer<-pglsSEyPagel(cancer_prevalence~a2,data=data,tree=pruned.tree,se=SE,method = "ML")

summary(siler_a2_cancer)

#grab r squared, p value, and lambda from summary so we can plot it 

r.v.siler_a2_cancer <- R2(phy = pruned.tree,siler_a2_cancer)
r.v.siler_a2_cancer <- format(r.v.siler_a2_cancer[3])
r.v.siler_a2_cancer <-signif(as.numeric(r.v.siler_a2_cancer), digits= 2)
ld.v.siler_a2_cancer <- summary(siler_a2_cancer)$modelStruct$corStruct
ld.v.siler_a2_cancer <- signif(ld.v.siler_a2_cancer[1], digits = 2)
p.v.siler_a2_cancer <-summary(siler_a2_cancer)$tTable
p.v.siler_a2_cancer <-signif(p.v.siler_a2_cancer[2,4], digits = 2)


ggplot(data, aes(x = a2, y = cancer_prevalence, color = Class, size = n)) +
  geom_point(alpha = 1) +
  scale_size_continuous(range = c(1, 5), guide = "none") +  # hide size legend
  geom_abline(intercept = coef(siler_a2_cancer)[1], 
              slope = coef(siler_a2_cancer)[2],
              color = 'grey', linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "Mortality Risk in the Prime-Age Stage (a2)",
    y = "Cancer Prevalence (%)",
    color = "Class"
  ) + 
  labs(
    title = "Cancer Prevalence vs. Mortality Risk in the Prime-Age Stage (a2)", 
    subtitle = bquote(p-value:.(p.v.siler_a2_cancer)~R^2:.(r.v.siler_a2_cancer)~Lambda:.(ld.v.siler_a2_cancer))
  ) +
  theme_cowplot(12)


##you can run this to save the plot to your working directory 
ggsave(filename='siler_a2_cancer.png', width=10, height=8, limitsize=FALSE,bg="white")