#==============================================================================
# This script is used to clean and manage data related to GW
# Data are selected from three models from ISIMIP
# An ensable has been used
# 
# Data: ISIMIP3a
# CWatM + WaterGAP2 + H08
#
#
#==============================================================================
# Libraries and paths

library(terra); library(exactextractr); library(sf)
library(dplyr); library(tidyr); library(ggplot2); library(scales)

raw <- "data/raw"          
inter <- "data/intermediate"  
figs <- "output/figures"  
# Big data are outside the folder of this project
path_bigdata <- "B:/Dataset e Analisi finite/Dataset/tesi-magistrale"   
path_gws <- file.path(path_bigdata, "gw_models")

sahel <- st_read(file.path(inter, "1.admin_reg.gpkg"))


#==============================================================================
# vedere i nomi esatti dei file scaricati e controllo CRS
#==============================================================================
list.files(path_gws, pattern = "\\.nc$")

cat("GWS CRS:", crs(rast(list.files(path_gws, pattern="cwatm.*\\.nc$", full.names=TRUE)[1]),
                    describe=TRUE)$name, "\n")
cat("GADM CRS:", crs(sahel, describe=TRUE)$name, "\n")



#==============================================================================
# Funzione di ritaglio temporale e geografico
#==============================================================================
crop_gws <- function(file_in, file_out) {
  r <- rast(file_in)
  keep <- time(r) >= as.Date("1995-01-01") & time(r) <= as.Date("2019-12-31")
  r <- r[[ which(keep) ]]
  r <- crop(r, sahel)
  writeCDF(r, file.path(inter, file_out), overwrite = TRUE)
  cat(file_out, "->", nlyr(r), "layer |",
      format(min(time(r))), "→", format(max(time(r))), "\n")
}


file_cwatm    <- list.files(path_gws, pattern="cwatm.*groundwstor.*\\.nc$",     full.names=TRUE)[1]
file_watergap <- list.files(path_gws, pattern="watergap.*groundwstor.*\\.nc$", full.names=TRUE)[1]
file_h08 <- list.files(path_gws, pattern="h08.*groundwstor.*\\.nc$", full.names=TRUE)[1]

crop_gws(file_cwatm,    "6.gws_cwatm_sahel.nc")
crop_gws(file_watergap, "6.gws_watergap_sahel.nc")
crop_gws(file_h08, "6.gws_h08_sahel.nc")


modelli <- c(cwatm = "6.gws_cwatm_sahel.nc",
             watergap = "6.gws_watergap_sahel.nc",
             h08 = "6.gws_h08_sahel.nc")
pop_w <- rast(file.path(inter, "2.pop_weight.tif"))



#==============================================================================
# Plot intermedio per differenza tra modelli
#==============================================================================
mesi_ref <- "2009-09-01"   # un mese qualsiasi per il confronto visivo

gws_map <- lapply(names(modelli), function(m) {
  r <- rast(file.path(inter, modelli[m]))
  r <- r[[ which(as.character(time(r)) == mesi_ref) ]]
  v <- exact_extract(r, st_as_sf(sahel), fun = "weighted_mean",
                     weights = resample(pop_w, r, "sum"), progress = FALSE)
  data.frame(GID_1 = sahel$GID_1, gws = v, model = m)
}) |> bind_rows()

gws_map <- gws_map |>
  group_by(model) |>
  mutate(gws = as.numeric(scale(gws))) |>   # z-score entro modello
  ungroup()

sahel_gws <- st_as_sf(sahel) |> left_join(gws_map, by = "GID_1")
lim <- max(abs(sahel_gws$gws), na.rm = TRUE) * c(-1, 1)


p_gws <- ggplot(sahel_gws) +
  geom_sf(aes(fill = gws), colour = "grey30", linewidth = 0.2) +
  scale_fill_gradient2(low = "#8B2500", mid = "grey95", high = "#08519C",
                       midpoint = 0, limits = lim, name = "GWS") +
  facet_wrap(~ model) +
  labs(title = paste("Groundwater storage —", mesi_ref)) +
  theme_void(base_size = 11)
p_gws




#==============================================================================
# estrazione region×month, pop-weighted, tre modelli
#==============================================================================
extract_gws <- function(m) {
  r <- rast(file.path(inter, modelli[m]))
  w <- resample(pop_w, r, "sum")
  ex <- exact_extract(r, st_as_sf(sahel), fun="weighted_mean", weights=w, progress=FALSE)
  colnames(ex) <- as.character(time(r)); ex$GID_1 <- sahel$GID_1
  pivot_longer(ex, -GID_1, names_to="date", values_to="gws") |>
    mutate(year = as.integer(format(as.Date(date), "%Y")), model = m)
}
gws_long <- bind_rows(lapply(names(modelli), extract_gws))

# mese->anno (media, GWS è stock) -> z-score per regione×modello (baseline 1995-2019)
gws_yr <- gws_long |>
  group_by(GID_1, model, year) |> summarise(gws = mean(gws), .groups="drop") |>
  group_by(GID_1, model) |> mutate(gws_z = as.numeric(scale(gws))) |> ungroup()

# media ensemble sugli z + spread (diagnostica) -> lag -> finestra panel
gws_ens <- gws_yr |>
  group_by(GID_1, year) |>
  summarise(gws_sd = sd(gws_z), gws_z = mean(gws_z), .groups="drop") |>
  arrange(GID_1, year) |> group_by(GID_1) |>
  mutate(gws_lag = dplyr::lag(gws_z)) |> ungroup() |>
  filter(year >= 2000, year <= 2019)


#==============================================================================
# Plot prova
#==============================================================================
anno_map <- 2009
sahel_ens <- st_as_sf(sahel) |> left_join(filter(gws_ens, year==anno_map), by="GID_1")

p_ens <- ggplot(sahel_ens) +
  geom_sf(aes(fill = gws_z), colour="grey30", linewidth=0.2) +
  scale_fill_gradient2(low="#8B2500", mid="grey95", high="#08519C",
                       midpoint=0, limits=c(-3,3), name="GWS z") +
  labs(title = paste("GWS ensemble (z-score) —", anno_map)) +
  theme_void(base_size=12)
p_ens



#==============================================================================
# Saving
#==============================================================================
write.csv(gws_ens, file.path(inter, "6.gws_region_year.csv"), row.names=FALSE)

ggsave(file.path(figs, "6.gws_ensemble_map.png"), p_ens,
       width=7, height=5, dpi=300, bg="white")
ggsave(file.path(figs, "6.gws_models_compare.png"), p_gws,
       width = 12, height = 4.5, dpi = 300, bg = "white")





