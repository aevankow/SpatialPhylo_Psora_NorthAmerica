
#@aevankow created this R script following the tutorial by @joelnitta 
"Obtaining occurrence and phylogeny data in R"
#https://github.com/joelnitta/spatial-phy-workshop/blob/main/tutorials/occ_phy.md
#Please see this tutorial for setup and other detailed information.

#AI including the University of Oslo's privacy friendly version of ChatGPT (GPT-5.1)
# and Claude (Version 2.19675.1) were used occasionally to refine code.
#AI was not used to run the scripts. AI was not used to write the manuscript.

#Coordinate Sources for the Psora manuscript:
#GBIF.org, Specimens Examined (SX from table), Sequenced specimens (from table)

#Import Sequence Table
SEQ_Psora_raw <-read.csv("Current_Sequence_Table_NA.csv", header = TRUE)

library(dplyr)
#Remove records without Lat / Long

SEQ_Psora_coords <- SEQ_Psora_raw %>%
  mutate(
    Longitude = as.numeric(na_if(trimws(Longitude), "")),
    Latitude  = as.numeric(na_if(trimws(Latitude), ""))
  ) %>%
  filter(!is.na(Longitude), !is.na(Latitude))

#Remove records from outside of North America
SEQ_Psora <- SEQ_Psora_coords %>%
  filter(Country %in% c("USA", "Canada", "Mexico", "Greenland"))

SEQ_Psora %>%
  group_by(Species) %>%
  summarize(n_records = n(), n_countries = n_distinct(Country)) %>%
  print(n = Inf)

write.csv(SEQ_Psora, "SEQ_Psora.csv", row.names = FALSE)

#Import Specimens Examined (SX)
SX_Psora_raw <- read.csv("SX_NA_updated.csv", header = TRUE)

SX_Psora_coords <- SX_Psora_raw %>%
  mutate(
    Longitude = as.numeric(na_if(trimws(Longitude), "")),
    Latitude  = as.numeric(na_if(trimws(Latitude), ""))
  ) %>%
  filter(!is.na(Longitude), !is.na(Latitude))

SX_Psora_coords%>%
  group_by(Species) %>%
  summarize(n_records = n(), n_countries = n_distinct(Country)) %>%
  print(n = Inf)

SX_Psora_coords%>%
  group_by(Country) %>%
  summarize(n_records = n(), n_species = n_distinct(Species)) %>%
  print(n = Inf)

target_species_sx <- c(
  "Psora brunneocarpa", "Psora californica", "Psora crenata",
  "Psora cerebriformis", "Psora himalayana", "Psora decipiens", "Psora hyporubescens",
  "Psora icterica", "Psora indigirkae", "Psora montana",
  "Psora nipponica",  "Psora nitida", "Psora pacifica", "Psora pruinosa", 
  "Psora pseudorussellii", "Psora rubiformis", "Psora russellii", 
  "Psora tenuifolia", "Psora tuckermanii", "Psora vallesiaca"
) #remove P. peninsularis, P. globifera, and P. luridella 

SX_Psora_target <- SX_Psora_coords %>%
  filter(Species %in% target_species_sx)

SX_Psora_target%>%
  group_by(Species) %>%
  summarize(n_records = n(), n_countries = n_distinct(Country)) %>%
  print(n = Inf)

#update names
library(stringr)

SX_Psora_updated <- SX_Psora_target %>%
  mutate(across(where(is.character), ~ str_replace_all(.x, c(
    "Psora crenata"    = "Psora chonion",
    "Psora himalayana" = "Psora elenkinii"
  ))))

SX_Psora_updated%>%
  group_by(Species) %>%
  summarize(n_records = n(), n_countries = n_distinct(Country)) %>%
  print(n = Inf)

SX_Psora <- SX_Psora_updated

write.csv(SX_Psora_coords, "SX_Psora.csv", row.names = FALSE)

library(rgbif)
#Import GBIF records
GBIF_Psora_raw <- occ_download_get('0025991-260519110011954') %>%
  occ_download_import()

GBIF_Psora_raw %>%
  group_by(countryCode) %>%
  summarize(n_species = n_distinct(species, na.rm = TRUE)) %>%
  print(n = Inf)

GBIF_Psora_nomissing <-
  GBIF_Psora_raw %>%
  mutate(species = na_if(species, "")) %>%
  filter(
    !is.na(species),
    !is.na(decimalLongitude),
    !is.na(decimalLatitude)
  )

# Define study area extent
xmin <- -180  # Change to your study area min longitude
xmax <- -15   # Change to your study area max longitude
ymin <- 15   # Change to your study area min latitude
ymax <- 90    # Change to your study area max latitude

GBIF_Psora_clean <-
  GBIF_Psora_nomissing %>%
  filter(decimalLongitude <= xmax)

GBIF_Psora_clean <-
  GBIF_Psora_clean %>%
  filter(decimalLatitude >= ymin)

GBIF_Psora_clean %>%
  group_by(countryCode) %>%
  summarize(n_species = n_distinct(species, na.rm = TRUE)) %>%
  print(n = Inf)

#Remove Iceland and Spain
GBIF_Psora_clean <-
  GBIF_Psora_clean %>%
  filter(!countryCode %in% c("IS", "ES"))
  
GBIF_Psora_clean %>%
  group_by(species) %>%
  summarize(n_countries = n_distinct(countryCode, na.rm = TRUE)) %>%
  print(n = Inf)

#my sequenced samples appear as "MATERIAL_SAMPLE" in basisOfRecord
GBIF_Psora_nomaterialsamples <-
  GBIF_Psora_clean %>%
  filter(!basisOfRecord %in% "MATERIAL_SAMPLE")

GBIF_Psora_nomaterialsamples %>%
  group_by(species) %>%
  summarize(n_records = n_distinct(basisOfRecord, na.rm = TRUE)) %>%
  print(n = Inf)

target_species <- c(
  "Psora californica", "Psora cerebriformis", "Psora crenata", "Psora decipiens", 
  "Psora hyporubescens", "Psora icterica", "Psora indigirkae","Psora montana", 
  "Psora nipponica", "Psora nitida", "Psora pacifica", "Psora peninsularis",
  "Psora pseudorussellii", "Psora rubiformis", "Psora russellii", "Psora tenuifolia",
  "Psora vallesiaca", "Psora elenkinii", "Psora himalayana", "Psora tuckermanii"
) 
#remove P. brunneocarpa (overlaps with other samples), remove P. globifera, 
#remove P. pruinosa, #remove P. tuckermanii

GBIF_Psora_target <- GBIF_Psora_nomaterialsamples %>%
  filter(species %in% target_species)

GBIF_Psora_target %>%
  group_by(species) %>%
  summarize(n_country = n_distinct(countryCode, na.rm = TRUE)) %>%
  print(n = Inf)

#update names
library(stringr)

GBIF_Psora_updated <- GBIF_Psora_target %>%
  mutate(across(where(is.character), ~ str_replace_all(.x, c(
    "Psora crenata"    = "Psora chonion",
    "Psora himalayana" = "Psora elenkinii"
  ))))

GBIF_Psora <- GBIF_Psora_updated

write.csv(GBIF_Psora, "GBIF_Psora.csv", row.names = FALSE)

library(ggplot2)
#I mapped each dataset to check if the points are in the correct grid
world_map <- ne_countries(returnclass = "sf")

ggplot()+
  geom_sf(data = world_map) +
  geom_point(
    data = GBIF_Psora,
    aes(x = decimalLongitude, y = decimalLatitude, color = species),
    size = 0.5) +
  theme(legend.position = "none")

#okay now with a little bit different style and dimensions
map_world = borders(database = "world", colour = "gray50", fill = "#383838")

ggplot() +
  map_world +
  geom_point(data = GBIF_Psora,
             aes(x = decimalLongitude,
                 y = decimalLatitude,
                 colour = species),
             alpha = 1.0, size = 2) + coord_cartesian(xlim = c((xmin),(xmax)), ylim = c((ymin),(ymax))) +
  theme(legend.position = "bottom") 

ggplot() +
  map_world +
  geom_point(data = SX_Psora,
             aes(x = Longitude,
                 y = Latitude,
                 colour = Species),
             alpha = 1.0, size = 2) + coord_cartesian(xlim = c((xmin),(xmax)), ylim = c((ymin),(ymax))) +
  theme(legend.position = "bottom") 

#All coordinates are now loaded. Time to clean. And map.
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(phyloregion)
library(sp)
library(raster)

#Merging...

merged_all <- bind_rows(
  SEQ_Psora %>%
    dplyr::select(species = Species, latitude = Latitude, longitude = Longitude),
  GBIF_Psora %>%
    dplyr::select(species, latitude = decimalLatitude, longitude = decimalLongitude),
  SX_Psora %>%
    dplyr::select(species = Species, latitude = Latitude, longitude = Longitude)
)

ggplot() +
  map_world +
  geom_point(data = merged_all,
             aes(x = longitude,
                 y = latitude,
                 colour = species),
             alpha = 1.0, size = 2) + coord_cartesian(xlim = c((xmin),(xmax)), ylim = c((ymin),(ymax))) +
  theme(legend.position = "bottom") 

#Merged with only North American taxa
merged_all_NA <- merged_all %>%
  filter(species %in% target_taxa)

target_taxa <- c(
  "Psora andersonii sp. nov.", "Psora brattiae sp. nov.","Psora brunneocarpa", 
   "Psora californica", "Psora cerebriformis", "Psora chonion", "Psora desertorum sp. nov.",
  "Psora hyporubescens", "Psora intermedia sp. nov.", "Psora luridella",
  "Psora montana", "Psora nitida", "Psora pacifica", "Psora peninsularis",
  "Psora pseudorussellii", "Psora pruinosa",
  "Psora russellii", "Psora tuckermanii", "Psora xerophila sp. nov. "
)

#Mapping...
laea_proj <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#cell_size <- 200000   # cell width in metres (200 km)
cell_size <- 100000 #100 km
#cell_size <- 50000 #50 km
hexagons  <- FALSE    # TRUE for hexagons (cell_size = flat-to-flat width)
  
# canaper randomization settings (as in the original script)
n_reps       <- 1000
n_iterations <- 1000000

north_america      <- ne_countries(scale = "medium", continent = "North America",
                                   returnclass = "sf")
north_america_laea <- st_transform(north_america, laea_proj)

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "gray90"),
  plot.title = element_text(hjust = 0.5)
)

# Base map + one grid layer; used for every map below
grid_map <- function(data, fill_var, title) {
  ggplot() +
    geom_sf(data = north_america_laea, colour = "black", fill = "white") +
    geom_sf(data = data, aes(fill = .data[[fill_var]]), colour = NA, alpha = 0.8) +
    coord_sf(crs = st_crs(laea_proj)) +
    labs(title = title) +
    plain
}

#Tree
library(ape)
#tree <- read.nexus("tree")
tree <- read.nexus("tree_new")
plot(tree)

# canaper rejects NaN / negative branch lengths
tree$edge.length[is.nan(tree$edge.length)] <- 0
stopifnot(all(tree$edge.length >= 0))

# Keep only species in the tree (tree tips use underscores)
psora_in_tree <- merged_all %>%
  #merged_all_NA is the alternative
  mutate(species = str_replace_all(species, " ", "_")) %>%
  filter(species %in% tree$tip.label)

n_distinct(psora_in_tree$species)   #28!! 19!!NA #27 (#24) #18 (#16) for North America
setdiff(str_replace_all(unique(merged_all$species), " ", "_"), tree$tip.label)  # dropped
#missing Psora nitida and "Psora_xerophila_sp._nov._"
#"Psora_nitida"        "Psora_crystallifera"

library(sf)
psora_pts <- psora_in_tree %>%
  mutate(rec_id = row_number()) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(laea_proj)

library(phyloregion)
grid <- st_make_grid(psora_pts, cellsize = cell_size, square = !hexagons) %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(grids = paste0("v", row_number()))

summary(as.numeric(st_area(grid)))   # all equal: 4e10 m^2 for 200 km squares

# Assign each record to one cell (a point on a shared edge goes to the first)
pts_in_grid <- st_join(psora_pts, grid, join = st_intersects) %>%
  st_drop_geometry() %>%
  distinct(rec_id, .keep_all = TRUE) %>%
  filter(!is.na(grids))

#Community matrix, richness, abundance
comm <- long2sparse(pts_in_grid, grids = "grids", species = "species")
comm[comm > 1] <- 1   # presence/absence

cell_stats <- pts_in_grid %>%
  group_by(grids) %>%
  summarize(
    richness  = n_distinct(species),
    abundance = n()
  ) %>%
  mutate(redundancy = 1 - richness / abundance)

ggplot(cell_stats, aes(x = redundancy)) + geom_histogram()

#Phylogenetic diversity
pd_psora <- PD(comm, tree)

poly_shp <- grid %>%
  inner_join(cell_stats, by = "grids") %>%
  left_join(tibble(grids = names(pd_psora), pd = pd_psora), by = "grids")

grid_map(poly_shp, "richness", "Species richness") +
  scale_fill_gradient(low = "yellow", high = "red")

grid_map(poly_shp, "abundance", "Number of records") +
  scale_fill_gradient(low = "yellow", high = "red")

grid_map(poly_shp, "pd", "Phylogenetic diversity") +
  scale_fill_gradient(low = "yellow", high = "red")

ggplot(poly_shp, aes(x = richness, y = pd)) + geom_point()

#CANAPE
dense_matrix <- as.matrix(comm)

library(canaper)
# Check how many iterations the curveball null model needs
iter_sim <- cpr_iter_sim(
  comm = dense_matrix,
  null_model = "curveball",
  n_iterations = 100000,
  thin = 1000
)

ggplot(iter_sim, aes(x = iteration, y = similarity)) +
  geom_line() +
  labs(x = "Num. iterations", y = "% Similarity")

rand_res <- cpr_rand_test(
  dense_matrix, tree,
  null_model = "curveball",
  n_reps = n_reps, n_iterations = n_iterations,
  tbl_out = TRUE
)

psora_canape <- rand_res %>%
  cpr_classify_endem() %>%
  cpr_classify_signif("pd") %>%
  cpr_classify_signif("rpd") %>%
  cpr_classify_signif("pe") %>%
  cpr_classify_signif("rpe")

count(psora_canape, endem_type)

write.csv(psora_canape, "psora_canape_target.csv", row.names = FALSE)

poly_shp_end <- poly_shp %>%
  left_join(
    psora_canape %>%
      select(grids = site, endemism = endem_type,
             pd_signif, rpd_signif, pe_signif, rpe_signif),
    by = "grids"
  )

#CANAPE maps
grid_map(poly_shp_end, "endemism", "CANAPE endemism type") +
  scale_fill_manual(values = mishler_endem_cols)   # as before; cpr_endem_cols in newer canaper

grid_map(poly_shp_end, "pd_signif", "Phylogenetic Diversity") +
  scale_fill_manual(values = cpr_signif_cols)

grid_map(poly_shp_end, "rpd_signif", "Relative Phylogenetic Diversity") +
  scale_fill_manual(values = cpr_signif_cols)

grid_map(poly_shp_end, "pe_signif", "Phylogenetic Endemism") +
  scale_fill_manual(values = cpr_signif_cols)

grid_map(poly_shp_end, "rpe_signif", "Relative Phylogenetic Endemism") +
  scale_fill_manual(values = cpr_signif_cols)

