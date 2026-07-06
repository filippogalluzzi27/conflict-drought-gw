#==============================================================================
# This script is used to create cropland and livestock dataset
# 
# Data cropland: GLAD - Global cropland expansion in the 21st century (Landsat)
# Data livestock: GLW - Gridded Livestock of the World database
#
#
#==============================================================================
# Libraries and paths
library(terra); library(exactextractr); library(sf); library(tidyr)
library(dplyr); library(ggplot2); library(scales); library(geodata)

raw <- "data/raw"        
inter <- "data/intermediate"
figs <- "output/figures"  
# Big data are outside the folder of this project
path_bigdata <- "B:/Dataset e Analisi finite/Dataset/tesi-magistrale"    

sahel <- st_read(file.path(inter, "1.admin_reg.gpkg"))




#==============================================================================
# Cropland download and crop
#==============================================================================
# Cropland mask download (2003, 2015, 2019)
anni_crop <- c(2003, 2015, 2019)
crop_all <- lapply(anni_crop, function(y)
  cropland(source = "GLAD", path = file.path(path_bigdata), year = y))
names(crop_all) <- paste0("y", anni_crop)



#==============================================================================
# CHECK stabilità cropland (2003, 2015, 2019): correlazione ranking regionale
crop_check <- sapply(anni_crop, function(y) {
  r <- crop(crop_all[[paste0("y", y)]], vect(sahel))
  exact_extract(r, sahel, fun = "mean")
})
colnames(crop_check) <- paste0("y", anni_crop)
cat("Correlazione cropland tra anni:\n")
print(round(cor(crop_check, use = "complete.obs"), 3))


# Variazione area coltivata totale (km²) tra anni
sahel$area_km2 <- as.numeric(st_area(sahel)) / 1e6
crop_area <- sapply(anni_crop, function(y) {
  r <- crop(crop_all[[paste0("y", y)]], vect(sahel))
  frac <- exact_extract(r, sahel, fun = "mean")   # quota coltivata per regione
  sum(frac * sahel$area_km2, na.rm = TRUE)         # km² coltivati totali
})
names(crop_area) <- paste0("y", anni_crop)
cat("Area coltivata totale (km²):\n"); print(round(crop_area))
cat("Variazione % vs 2003:\n")
print(round((crop_area / crop_area["y2003"] - 1) * 100, 1))




#==============================================================================
# Estrazione cropland per regione media su ogni regione (solo 2015)
crop_mask <- crop(crop_all[["y2015"]], vect(sahel))
sahel$cropland <- exact_extract(crop_mask, sahel, fun = "mean")



#==============================================================================
# Plot quota di coltivato per regione
p_crop <- ggplot(sahel) +
  geom_sf(aes(fill = cropland), colour = "grey40", linewidth = 0.1) +
  scale_fill_viridis_c(option = "viridis", name = "Cropland\nfraction",
                       labels = label_percent()) +
  labs(title = NULL) +
  theme_void(base_size = 11) +
  theme(legend.position = "right")
p_crop




#==============================================================================
# Livestock
#==============================================================================
path_glw  <- file.path(path_bigdata, "livestock")

anni_glw <- c(2010, 2015, 2020)
specie <- c(cattle = "ctl", goats = "gts", sheep = "shp")

# For each species and year, finds GLW file and crop it on Sahel geometries
# Then it extracts the sum of heads for each region
extract_one <- function(year, code) {
  f <- list.files(path_glw,
                  pattern = sprintf("%s-%d", code, year), 
                  full.names = TRUE)[1]
  if (is.na(f)) stop(sprintf("File non trovato per %s-%d", code, year))
  r <- crop(rast(f), vect(sahel))
  v <- exact_extract(r, sahel, fun = "sum")
  as.numeric(v)
}


# Builds the csv table region-year-species, extracting heads of each species-year
glw <- lapply(names(specie), function(sp) {
  lapply(anni_glw, function(y) {
    data.frame(orig = sahel$GID_1, year = y,
               value = extract_one(y, specie[sp]),
               species = sp)
  }) |> bind_rows()
}) |> bind_rows() |>
  pivot_wider(names_from = species, values_from = value) |>
  mutate(livestock_total = cattle + goats + sheep)




#==============================================================================
# CHECK stabilità livestock (2010, 2015, 2020)
lv_wide <- glw |>
  select(orig, year, livestock_total) |>
  pivot_wider(names_from = year, values_from = livestock_total, names_prefix = "y")
cat("Correlazione livestock_total tra anni:\n")
print(round(cor(lv_wide[,-1], use = "complete.obs"), 3))




#==============================================================================
# Profilo livestock statico (media 2010-2015-2020)
glw_static <- glw |>
  group_by(orig) |>
  summarise(across(c(cattle, goats, sheep, livestock_total), mean), .groups = "drop") |>
  mutate(log_livestock = log1p(livestock_total))



#==============================================================================
# Plot densità di bestiame per regione (log)
sahel_map <- sahel |>
  left_join(glw_static, by = c("GID_1" = "orig"))
p_lvst <- ggplot(sahel_map) +
  geom_sf(aes(fill = log_livestock), colour = "grey40", linewidth = 0.1) +
  scale_fill_viridis_c(option = "inferno", name = "Livestock\n(log head)") +
  labs(title = NULL) +
  theme_void(base_size = 11) +
  theme(legend.position = "right")
p_lvst



#==============================================================================
# Merge of cattle and crop data (csv)
#==============================================================================
crop_livestock <- st_drop_geometry(sahel)[, c("GID_1", "cropland")] |>
  rename(orig = GID_1) |>
  left_join(glw_static, by = "orig")



#==============================================================================
# Saving
#==============================================================================
ggsave(file.path(figs, "4.cropland_map.png"), p_crop,
       width = 7, height = 4.5, dpi = 300)
ggsave(file.path(figs, "4.livestock_map.png"), p_lvst,
       width = 7, height = 4.5, dpi = 300)

write.csv(crop_livestock, file.path(inter, "4.crop_livestock_region.csv"), row.names = FALSE)



