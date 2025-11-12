library(phytools)
library(geiger)
library(caper)
library(tidyverse)
library(cowplot)
library(cowplot)

##read in the csv sheet that has your cancer data and your predictor variables
data <- read.csv("Fall 2025/Fall 2025 Clean Data/final_clean_mortality_data.csv")

##This step is speciifc to Olivia's stuff
##adds columns for standard error selects which class if there is a specific one
data <- data %>%
  mutate(abs_shape = abs(shape_value)) %>%
  filter(Class == "Aves")

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

##PGLs model and format is y~x or response_varaible~predictor_variable
abs_cancer_aves<-pglsSEyPagel(cancer_prevalence~abs_shape,data=data,tree=pruned.tree,se=SE,method = "ML")

##NOTE if you are running a bunch of different models, you can change the name
## THEN you can command+find and replace all of the names for the steps below
##MUCH FASTER, msg me if unclear 
##gives summary stats 
summary(abs_cancer_aves) 

#grab r squared, p value, and lambda from summary so we can plot it 

r.v.abs_cancer_aves <- R2(phy = pruned.tree,abs_cancer_aves)
r.v.abs_cancer_aves <- format(r.v.abs_cancer_aves[3])
r.v.abs_cancer_aves <-signif(as.numeric(r.v.abs_cancer_aves), digits= 2)
ld.v.abs_cancer_aves<- summary(abs_cancer_aves)$modelStruct$corStruct
ld.v.abs_cancer_aves <- signif(ld.v.abs_cancer_aves[1], digits = 2)
p.v.abs_cancer_aves<-summary(abs_cancer_aves)$tTable
p.v.abs_cancer_aves<-signif(p.v.abs_cancer_aves[2,4], digits = 2)

#plot
ggplot(data, aes(x = abs_shape, y = neoplasia_prevalence, color = Class)) +
  geom_point(alpha = 1, size = 1) +
  geom_abline(intercept = coef(abs_cancer_aves)[1], slope =  coef(abs_cancer_aves)[2],
              color = 'grey',linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Neoplasia Prevalence (%)",
    color = "Class") + 
  labs(
    title = "Neoplasia Prevalence vs. Absolute Value of Survivorship", 
    subtitle = bquote(p-value:.(p.v.abs_cancer_aves)~R^2:.(r.v.abs_cancer_aves)~Lambda:.(ld.v.abs_cancer_aves)))+
  ## theme_cowplot(12) cleans up graph and removes background grid. 
  theme_cowplot(12)

##you can run this to save the plot to your working directory 
ggsave(filename='aves_cancer.png', width=13, height=10, limitsize=FALSE,bg="white")