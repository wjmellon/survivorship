library(dplyr)
library(ggplot2)

# Load the dataset
data <- read.csv("records.csv")

# --- Define Species by Survivorship Type (using Scientific Names) ---
type1_species <- c(
  "Ailurus fulgens", "Didelphis marsupialis", "Allochrocebus lhoesti", "Bison bonasus",
  "Capromys pilorides", "Cercopithecus diana", "Chrysocyon brachyurus", "Erythrocebus patas",
  "Felis nigripes", "Naemorhedus griseus", "Oryx leucoryx", "Proteles cristata",
  "Tenrec ecaudatus", "Trachypithecus delacouri", "Panthera uncia"
)

trending_type1_species <- c(
  "Aepyprymnus rufescens", "Allenopithecus nigroviridis", "Alouatta caraya",
  "Aonyx capensis", "Aonyx cinerea", "Artibeus jamaicensis", "Ateles geoffroyi",
  "Ateles paniscus", "Brachylagus idahoensis", "Capra falconeri", "Cavia porcellus",
  "Cebuella pygmaea", "Cephalophus silvicultor", "Cercopithecus neglectus", "Cervus nippon",
  "Chinchilla brevicaudata", "Colobus guereza", "Connochaetes taurinus", "Cricetulus longicaudatus",
  "Damaliscus korrigum", "Dasyprocta punctata", "Dasyuroides byrnei", "Dendrolagus goodfellowi",
  "Elephantulus brachyrhynchus", "Erethizon dorsatum", "Felis silvestris", "Gazella spekei",
  "Gulo gulo", "Halichoerus grypus", "Hexaprotodon liberiensis", "Hylobates lar",
  "Leontopithecus chrysomelas", "Lophocebus aterrimus", "Macropus eugenii", "Madoqua guentheri",
  "Muntiacus muntjak", "Mustela putorius", "Neofelis nebulosa", "Orycteropus afer",
  "Otolemur garnettii", "Panthera leo", "Papio anubis", "Pteropus poliocephalus",
  "Rattus norvegicus", "Rousettus aegyptiacus", "Saguinus imperator", "Saimiri sciureus",
  "Semnopithecus entellus", "Sotalia fluviatilis", "Speothos venaticus", "Symphalangus syndactylus",
  "Tapirus indicus", "Tapirus terrestris", "Trachypithecus francoisi", "Tragelaphus eurycerus",
  "Tragulus javanicus"
)

type2_species <- c(
  "Acomys cahirinus", "Antilocapra americana", "Bos taurus",
  "Callosciurus prevostii", "Carollia perspicillata", "Cephalophus monticola", "Cephalophus niger",
  "Cercopithecus ascanius", "Chinchilla lanigera", "Choloepus didactylus",
  "Coendou rothschildi", "Helogale hirtula", "Lagothrix lagotricha",
  "Notamacropus rufogriseus", "Oreamnos americanus", "Pteropus hypomelanus",
  "Rhinoceros unicornis", "Semnopithecus hypoleucos", "Sylvilagus audubonii",
  "Tapirus bairdii", "Tursiops truncatus", "Urocyon littoralis", "Wallabia bicolor", "Sus barbatus"
)

trending_type3_species <- c(
  "Crateromys heaneyi", "Miopithecus talapoin", "Mus musculus", "Tolypeutes tricinctus"
)

type3_species <- c(
  "Catagonus wagneri", "Mesocricetus auratus"
)

# --- Create Data Frames ---
type1_df <- data.frame(Species = type1_species, Survivorship_Type = "Type I")
trending_type1_df <- data.frame(Species = trending_type1_species, Survivorship_Type = "Trending toward Type I")
type2_df <- data.frame(Species = type2_species, Survivorship_Type = "Type II")
trending_type3_df <- data.frame(Species = trending_type3_species, Survivorship_Type = "Trending toward Type III")
type3_df <- data.frame(Species = type3_species, Survivorship_Type = "Type III")

# --- Combine All Type Data Frames ---
all_types_df <- bind_rows(type1_df, trending_type1_df, type2_df, trending_type3_df, type3_df)

# --- Merge with Main Data to Get Body Mass and Normalize ---
merged_data <- data %>%
  select(Species = 28, Body_Mass = 46) %>% # Select Scientific Species Name and Body Mass
  inner_join(all_types_df, by = "Species") %>% # Join by scientific species name
  na.omit() %>% # Remove rows with missing body mass
  group_by(Survivorship_Type) %>%
  mutate(Normalized_Body_Mass = (Body_Mass - mean(Body_Mass)) / sd(Body_Mass)) %>%
  ungroup()

# --- Check if any species were not found ---
missing_species <- setdiff(all_types_df$Species, merged_data$Species)
if (length(missing_species) > 0) {
  print("WARNING: Some species were not found in the dataset:")
  print(missing_species)
}

# --- Plotting (Scatter Plot with Normalized Body Mass) ---
ggplot(merged_data, aes(x = Normalized_Body_Mass, y = Survivorship_Type, color = Survivorship_Type)) +
  geom_point(size = 3) +
  labs(
    title = "Normalized Body Mass by Survivorship Type",
    x = "Normalized Body Mass (within Survivorship Type)",
    y = "Survivorship Type",
    color = "Survivorship Type"
  ) +
  theme_minimal()