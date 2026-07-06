#==============================================================================
# This script is used to clean and manage data related to conflict events.
# 
# Data: UCDP Georeferenced Event Dataset (GED) Global
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

sahel <- st_read(file.path(inter, "1.admin_reg.gpkg"))
ged <- read.csv(file.path(path_bigdata,"conflicts.csv"))


#==============================================================================
# variable selection and temporal window
#==============================================================================
ged <- ged |>
  select(year, latitude, longitude, best, type_of_violence, where_prec) |>
  filter(year >= 2000, year <= 2019)


#==============================================================================
# Filter for precition of geolocalitazion
#==============================================================================
ged <- ged |>
  select(year, latitude, longitude, best, type_of_violence, where_prec) |>
  filter(year >= 2000, year <= 2019) |>
  filter(where_prec <= 4)            # <-- scarta eventi geo-vaghi (paese)
cat("Distribuzione where_prec (1=preciso ... 7=vago):\n")
print(table(ged$where_prec))
# 1-2 = villaggio/città (ottimo), 3-4 = provincia, 5-7 = paese/vago


#==============================================================================
# Selection of geometric points and data merge
#==============================================================================
ev <- st_as_sf(ged, coords = c("longitude","latitude"), crs = 4326)
ev <- st_transform(ev, st_crs(sahel))
ev <- st_join(ev, sahel[, c("GID_1","NAME_1","COUNTRY")], left = FALSE)



#==============================================================================
# Aggregation
#==============================================================================
conflict <- ev |>
  st_drop_geometry() |>
  group_by(GID_1, NAME_1, COUNTRY, year) |>
  summarise(
    n_events = n(),
    deaths   = sum(best, na.rm = TRUE),
    # separati per tipo (utile dopo)
    n_state    = sum(type_of_violence == 1),
    n_nonstate = sum(type_of_violence == 2),
    n_onesided = sum(type_of_violence == 3),
    .groups = "drop"
  ) |>
  rename(orig = GID_1, region = NAME_1, country = COUNTRY)

conflict |> group_by(country) |>
  summarise(eventi = sum(n_events), regioni = n_distinct(orig),
            morti = sum(deaths)) |>
  arrange(desc(eventi)) |> print(n = 30)


#==============================================================================
# Plot
#==============================================================================
conf_tot <- conflict %>%
  group_by(orig) %>%
  summarise(events = sum(n_events), .groups = "drop")

sahel_conf <- sahel %>%
  st_as_sf() %>%
  left_join(conf_tot, by = c("GID_1" = "orig")) %>%
  mutate(events = ifelse(is.na(events), 0, events))   # regioni senza eventi = 0

p_conf <- ggplot(sahel_conf) +
  geom_sf(aes(fill = events), colour = "grey30", linewidth = 0.2) +
  scale_fill_viridis_c(option = "inferno", trans = "log1p",
                       breaks = c(0, 10, 100, 1000, 5000),
                       name = "Events\n(2000–2019)") +
  labs(title = "UCDP conflict events by admin-1") +
  theme_void(base_size = 12)
p_conf




#==============================================================================
# Saving
#==============================================================================
write.csv(conflict, file.path("data/intermediate", "3.conflict_region_year.csv"), row.names = FALSE)

ggsave(file.path(figs, "3.conflict_events_map.png"), p_conf,
       width = 7, height = 5, dpi = 300, bg = "white")






