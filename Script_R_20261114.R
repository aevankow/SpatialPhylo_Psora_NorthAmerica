
# @aevankow created this R script following the tutorial by @joelnitta 
"Obtaining occurrence and phylogeny data in R"
# https://github.com/joelnitta/spatial-phy-workshop/blob/main/tutorials/occ_phy.md
# Please see this tutorial for setup and other detailed information.

# AI including the University of Oslo's privacy friendly version of ChatGPT (GPT-5.1)
# and Claude (Version 2.19675.1) were used occasionally to refine code.
# AI was not used to run the scripts. AI was not used to write the manuscript.

# Coordinate Sources for the manuscript:
# 1) Sequenced specimens, "SEQ_Psora_raw" (Table S2, "Current_Sequence_Table_NA.csv")
# 2) Specimens Examined, "SX_Psora_raw" (Table S1, "SX_NA_updated.csv") 
# 3) GBIF.org, "GBIF_Psora_raw" (occurrence download, "0025991-260519110011954")

# Import Sequence Table S2
SEQ_Psora_raw <-read.csv("Current_Sequence_Table_NA.csv", header = TRUE)

library(dplyr)
# Remove records without Lat / Long

SEQ_Psora_coords <- SEQ_Psora_raw %>%
  mutate(
    Longitude = as.numeric(na_if(trimws(Longitude), "")),
    Latitude  = as.numeric(na_if(trimws(Latitude), ""))
  ) %>%
  filter(!is.na(Longitude), !is.na(Latitude))

# Remove records from outside of North America
SEQ_Psora <- SEQ_Psora_coords %>%
  filter(Country %in% c("USA", "Canada", "Mexico", "Greenland"))

# Check list of remaining species with number of records and number of countries
SEQ_Psora %>%
  group_by(Species) %>%
  summarize(n_records = n(), n_countries = n_distinct(Country)) %>%
  print(n = Inf)

# Save the filtered data
write.csv(SEQ_Psora, "SEQ_Psora.csv", row.names = FALSE)

# Import Specimens Examined (SX), Table S1
SX_Psora_raw <- read.csv("SX_NA_updated.csv", header = TRUE)

# Remove records without Lat / Long
SX_Psora_coords <- SX_Psora_raw %>%
  mutate(
    Longitude = as.numeric(na_if(trimws(Longitude), "")),
    Latitude  = as.numeric(na_if(trimws(Latitude), ""))
  ) %>%
  filter(!is.na(Longitude), !is.na(Latitude))

# Check list of remaining species
SX_Psora_coords%>%
  group_by(Species) %>%
  summarize(n_records = n(), n_countries = n_distinct(Country)) %>%
  print(n = Inf)

# Check list of remaining countries
SX_Psora_coords%>%
  group_by(Country) %>%
  summarize(n_records = n(), n_species = n_distinct(Species)) %>%
  print(n = Inf)

# Keep species with relatively reliable identifications 
# Remove P. peninsularis, P. globifera, and P. luridella 
target_species_sx <- c(
  "Psora brunneocarpa", "Psora californica", "Psora crenata",
  "Psora cerebriformis", "Psora himalayana", "Psora decipiens", "Psora hyporubescens",
  "Psora icterica", "Psora indigirkae", "Psora montana",
  "Psora nipponica",  "Psora nitida", "Psora pacifica", "Psora pruinosa", 
  "Psora pseudorussellii", "Psora rubiformis", "Psora russellii", 
  "Psora tenuifolia", "Psora tuckermanii", "Psora vallesiaca"
) 

SX_Psora_target <- SX_Psora_coords %>%
  filter(Species %in% target_species_sx)

# Check remaining species
SX_Psora_target%>%
  group_by(Species) %>%
  summarize(n_records = n(), n_countries = n_distinct(Country)) %>%
  print(n = Inf)

# Update names for P. chonion and P. elenkinii
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

# Save the filtered data
write.csv(SX_Psora_coords, "SX_Psora.csv", row.names = FALSE)

library(rgbif)
# Import GBIF records
GBIF_Psora_raw <- occ_download_get('0025991-260519110011954') %>%
  occ_download_import()

# List number of countries with Psora records
GBIF_Psora_raw %>%
  group_by(countryCode) %>%
  summarize(n_species = n_distinct(species, na.rm = TRUE)) %>%
  print(n = Inf)

# Remove records without species-level id, Lat and Long
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

# Remove records with Longitude greater than -15
GBIF_Psora_clean <-
  GBIF_Psora_nomissing %>%
  filter(decimalLongitude <= xmax)

# Remove records with Latitude less than 15
GBIF_Psora_clean <-
  GBIF_Psora_clean %>%
  filter(decimalLatitude >= ymin)

# List remaining countries
GBIF_Psora_clean %>%
  group_by(countryCode) %>%
  summarize(n_species = n_distinct(species, na.rm = TRUE)) %>%
  print(n = Inf)

# Remove Iceland and Spain
GBIF_Psora_clean <-
  GBIF_Psora_clean %>%
  filter(!countryCode %in% c("IS", "ES"))
 
# List species with number of countries 
GBIF_Psora_clean %>%
  group_by(species) %>%
  summarize(n_countries = n_distinct(countryCode, na.rm = TRUE)) %>%
  print(n = Inf)

# Some sequenced samples appear as "MATERIAL_SAMPLE" in basisOfRecord
# To remove the overlap in data, filter out "MATERIAL_SAMPLE"
GBIF_Psora_nomaterialsamples <-
  GBIF_Psora_clean %>%
  filter(!basisOfRecord %in% "MATERIAL_SAMPLE")

GBIF_Psora_nomaterialsamples %>%
  group_by(species) %>%
  summarize(n_records = n_distinct(basisOfRecord, na.rm = TRUE)) %>%
  print(n = Inf)

# Keep species with reliable coordinates
# Remove P. brunneocarpa (overlaps with other samples), remove P. globifera, 
# Remove P. pruinosa
target_species <- c(
  "Psora californica", "Psora cerebriformis", "Psora crenata", "Psora decipiens", 
  "Psora hyporubescens", "Psora icterica", "Psora indigirkae","Psora montana", 
  "Psora nipponica", "Psora nitida", "Psora pacifica", "Psora peninsularis",
  "Psora pseudorussellii", "Psora rubiformis", "Psora russellii", "Psora tenuifolia",
  "Psora vallesiaca", "Psora elenkinii", "Psora himalayana", "Psora tuckermanii"
) 

GBIF_Psora_target <- GBIF_Psora_nomaterialsamples %>%
  filter(species %in% target_species)

GBIF_Psora_target %>%
  group_by(species) %>%
  summarize(n_country = n_distinct(countryCode, na.rm = TRUE)) %>%
  print(n = Inf)

# Update names
library(stringr)

GBIF_Psora_updated <- GBIF_Psora_target %>%
  mutate(across(where(is.character), ~ str_replace_all(.x, c(
    "Psora crenata"    = "Psora chonion",
    "Psora himalayana" = "Psora elenkinii"
  ))))

GBIF_Psora <- GBIF_Psora_updated

# Save the filtered data
write.csv(GBIF_Psora, "GBIF_Psora.csv", row.names = FALSE)

library(ggplot2)
# I mapped each dataset to check if the points are in the correct grid
world_map <- ne_countries(returnclass = "sf")

ggplot()+
  geom_sf(data = world_map) +
  geom_point(
    data = GBIF_Psora,
    aes(x = decimalLongitude, y = decimalLatitude, color = species),
    size = 0.5) +
  theme(legend.position = "none")

# Change style and dimensions of map
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

# All coordinates are loaded
library(tidyverse)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(phyloregion)
library(sp)
library(raster)

# Merge datasets 

merged_all <- bind_rows(
  SEQ_Psora %>%
    dplyr::select(species = Species, latitude = Latitude, longitude = Longitude),
  GBIF_Psora %>%
    dplyr::select(species, latitude = decimalLatitude, longitude = decimalLongitude),
  SX_Psora %>%
    dplyr::select(species = Species, latitude = Latitude, longitude = Longitude)
)

# Map merged dataset
ggplot() +
  map_world +
  geom_point(data = merged_all,
             aes(x = longitude,
                 y = latitude,
                 colour = species),
             alpha = 1.0, size = 2) + coord_cartesian(xlim = c((xmin),(xmax)), ylim = c((ymin),(ymax))) +
  theme(legend.position = "bottom") 

# Merge with only North American taxa
target_taxa <- c(
  "Psora andersonii sp. nov.", "Psora brattiae sp. nov.","Psora brunneocarpa", 
   "Psora californica", "Psora cerebriformis", "Psora chonion", "Psora desertorum sp. nov.",
  "Psora hyporubescens", "Psora intermedia sp. nov.", "Psora luridella",
  "Psora montana", "Psora nitida", "Psora pacifica", "Psora peninsularis",
  "Psora pseudorussellii", "Psora pruinosa",
  "Psora russellii", "Psora tuckermanii", "Psora xerophila sp. nov. "
)

merged_all_NA <- merged_all %>%
  filter(species %in% target_taxa)

# Map with projection for North America
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
grid_map_noticks <- function(data, fill_var, title) {
  ggplot() +
    geom_sf(data = north_america_laea, colour = "black", fill = "white") +
    geom_sf(data = data, aes(fill = .data[[fill_var]]), colour = NA, alpha = 0.8) +
    coord_sf(crs = st_crs(laea_proj)) +
    labs(title = title) +
    plain
}

# Added Lat/long ticks and labels on the map edges, no lines across the map
lon_breaks <- seq(-160, -20, by = 20)
lat_breaks <- seq(10, 80, by = 10)

grid_map <- function(data, fill_var, title) {
  ggplot() +
    geom_sf(data = north_america_laea, colour = "black", fill = "white") +
    geom_sf(data = data, aes(fill = .data[[fill_var]]), colour = NA, alpha = 0.8) +
    scale_x_continuous(breaks = lon_breaks) +
    scale_y_continuous(breaks = lat_breaks) +
    coord_sf(
      crs = st_crs(laea_proj),
      datum = st_crs(4326),   # graticule and labels in degrees
      label_axes = list(bottom = "E", left = "N", top = "E", right = "N")
    ) +
    labs(title = title) +
    plain +
    theme(
      axis.text  = element_text(size = 8),
      axis.ticks = element_line(colour = "black"),
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA)
    )
}

# Import Phylogenetic Tree 
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

# Check number of species in the tree
n_distinct(psora_in_tree$species)   #28 total, #19 for North America
# Check if species have been dropped
setdiff(str_replace_all(unique(merged_all$species), " ", "_"), tree$tip.label)

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

# ---- Export grid results to KML (Google Earth) ----------------------------

# KML must be in lon/lat (WGS84). "Name" becomes the label of each cell and
# "Description" the pop-up text; the other columns appear as cell data.
poly_kml <- poly_shp %>%
  st_transform(4326) %>%
  mutate(
    Name = grids,
    Description = paste0(
      "Richness: ", richness,
      "<br>Records: ", abundance,
      "<br>PD: ", round(pd, 4)
    )
  ) %>%
  select(Name, Description, grids, richness, abundance, redundancy, pd)

st_write(poly_kml, "psora_NA_grid_results.kml", driver = "KML", delete_dsn = TRUE)

# ---- Export grid results to KML (Google Earth) ----------------------------

# One KML per variable, each cell filled on a yellow-to-red scale (same as
# the maps), plus a legend PNG that Google Earth shows in the corner. Keep
# each .kml next to its _legend.png. Clicking a cell shows all its values.
# The KML is written by hand so the colours work in Google Earth web and Pro.

poly_ll <- st_transform(poly_shp, 4326)   # KML must be lon/lat (WGS84)

# Any R colour ("red", "#FF0000") -> KML "aabbggrr"
kml_colour <- function(col, alpha = "cc") {
  hex <- rgb(t(col2rgb(col)), maxColorValue = 255)
  paste0(alpha, substr(hex, 6, 7), substr(hex, 4, 5), substr(hex, 2, 3))
}

# Pull the legend out of a ggplot without extra packages
get_legend <- function(p) {
  g    <- ggplotGrob(p)
  keep <- grepl("guide-box", g$layout$name) &
    !vapply(g$grobs, inherits, logical(1), "zeroGrob")
  g$grobs[[which(keep)[1]]]
}

write_grid_kml <- function(cells, var, label, file, log_scale = FALSE,
                           palette = NULL,
                           desc_vars = c("richness", "abundance", "pd"),
                           n_col = 100) {
  vals <- cells[[var]]
  
  if (is.null(palette)) {
    tv   <- if (log_scale) log10(vals) else vals
    rng  <- range(tv, na.rm = TRUE)
    pal  <- colorRampPalette(c("yellow", "red"))(n_col)
    idx  <- if (diff(rng) == 0) rep(1L, length(tv)) else
      1L + round((tv - rng[1]) / diff(rng) * (n_col - 1))
    fill <- ifelse(is.na(idx), "#cccccc", pal[pmax(1L, idx)])
    fill_scale <- scale_fill_gradient(
      low = "yellow", high = "red",
      trans = if (log_scale) "log10" else "identity", name = label)
  } else {
    # Unnamed palettes are matched to the categories in order, like ggplot
    if (is.null(names(palette))) {
      lv <- levels(factor(vals))
      palette <- setNames(palette[seq_along(lv)], lv)
    }
    fill <- unname(palette[as.character(vals)])
    fill[is.na(fill)] <- "#cccccc"
    fill_scale <- scale_fill_manual(values = palette, name = label)
  }
  
  # Legend image
  legend_png <- sub("\\.kml$", "_legend.png", file)
  leg <- ggplot(cells, aes(x = 0, y = 0, fill = .data[[var]])) +
    geom_tile() +
    fill_scale +
    theme_void() +
    theme(legend.background = element_rect(fill = "white", colour = NA),
          legend.margin = margin(6, 6, 6, 6))
  leg_grob <- get_legend(leg)
  png(legend_png, width = if (is.null(palette)) 160 else 230, height = 260,
      res = 110)
  grid::grid.newpage(); grid::grid.draw(leg_grob)
  dev.off()
  
  placemarks <- vapply(seq_len(nrow(cells)), function(i) {
    xy     <- st_coordinates(st_geometry(cells)[[i]])[, 1:2]
    coords <- paste(sprintf("%.5f,%.5f,0", xy[, 1], xy[, 2]), collapse = " ")
    desc   <- paste(vapply(desc_vars, function(v) {
      x <- cells[[v]][i]
      paste0(v, ": ", if (is.numeric(x)) format(signif(x, 4)) else x)
    }, character(1)), collapse = "<br>")
    sprintf(paste0(
      "<Placemark><name>%s</name><description><![CDATA[%s]]></description>",
      "<Style><LineStyle><color>66000000</color><width>0.5</width></LineStyle>",
      "<PolyStyle><color>%s</color></PolyStyle></Style>",
      "<Polygon><outerBoundaryIs><LinearRing><coordinates>%s</coordinates>",
      "</LinearRing></outerBoundaryIs></Polygon></Placemark>"),
      cells$grids[i], desc, kml_colour(fill[i]), coords)
  }, character(1))
  
  kml <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<kml xmlns="http://www.opengis.net/kml/2.2"><Document>',
    sprintf("<name>Psora North America: %s</name>", label),
    sprintf(paste0(
      '<ScreenOverlay><name>Legend</name><Icon><href>%s</href></Icon>',
      '<overlayXY x="0" y="0" xunits="fraction" yunits="fraction"/>',
      '<screenXY x="0.02" y="0.05" xunits="fraction" yunits="fraction"/>',
      '<size x="0" y="0" xunits="pixels" yunits="pixels"/></ScreenOverlay>'),
      basename(legend_png)),
    placemarks,
    "</Document></kml>"
  )
  writeLines(kml, file)
}

write_grid_kml(poly_ll, "richness",  "Species richness",
               "psora_NA_richness.kml")
write_grid_kml(poly_ll, "abundance", "Number of records",
               "psora_NA_abundance.kml", log_scale = TRUE)
write_grid_kml(poly_ll, "pd",        "Phylogenetic diversity",
               "psora_NA_pd.kml")

# ---- Export CANAPE results to KML (Google Earth) --------------------------

poly_end_ll <- st_transform(poly_shp_end, 4326)

canape_desc <- c("richness", "abundance", "pd", "endemism",
                 "pd_signif", "rpd_signif", "pe_signif", "rpe_signif")

write_grid_kml(poly_end_ll, "endemism", "CANAPE endemism type",
               "psora_NA_canape_endemism.kml",
               palette = mishler_endem_cols, desc_vars = canape_desc)
write_grid_kml(poly_end_ll, "pd_signif", "Phylogenetic Diversity",
               "psora_NA_pd_signif.kml",
               palette = cpr_signif_cols, desc_vars = canape_desc)
write_grid_kml(poly_end_ll, "rpd_signif", "Relative Phylogenetic Diversity",
               "psora_NA_rpd_signif.kml",
               palette = cpr_signif_cols, desc_vars = canape_desc)
write_grid_kml(poly_end_ll, "pe_signif", "Phylogenetic Endemism",
               "psora_NA_pe_signif.kml",
               palette = cpr_signif_cols, desc_vars = canape_desc)
write_grid_kml(poly_end_ll, "rpe_signif", "Relative Phylogenetic Endemism",
               "psora_NA_rpe_signif.kml",
               palette = cpr_signif_cols, desc_vars = canape_desc)

