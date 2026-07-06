#==============================================================================
# Sript for the creation of administrative regions
# for Sahel + Horn of Africa

#==============================================================================
# Libraries and paths

library(geodata); library(terra); library(ggplot2); library(sf)

raw <- "data/raw"   
inter <- "data/intermediate" 
figs <- "output/figures"   


#==============================================================================
# Selection of administrative regions (level 1)
countries <- c(
  "MLI","NER","TCD","BFA","SEN","NGA","SSD","ETH","CMR","ERI", "MRT", "SDN",
  "DJI", "SOM","GMB","GNB","GIN")


#==============================================================================
# Download function
get_gadm <- function(iso3) {
  x <- try(gadm(iso3, version= 4.1, level = 1, path = raw), silent = TRUE)
  if (inherits(x, "try-error")) { message("FAILED L1: ", iso3); return(NULL) }
  x
}
gadm_list <- setNames(lapply(countries, get_gadm), countries)

# Failures
failed <- names(gadm_list)[sapply(gadm_list, is.null)]
if (length(failed)) cat("To re-check:", paste(failed, collapse = ", "), "\n")

# Union to sahel_sf
gadm_list <- gadm_list[!sapply(gadm_list, is.null)]
sahel_sf <- do.call(rbind, unname(gadm_list))


#==============================================================================
# Names and number of admin reg for each country
print(names(sahel_sf))
print(table(sahel_sf$COUNTRY))


#==============================================================================
# Map
sahel_sf_plot <- st_as_sf(sahel_sf)

ggplot(sahel_sf_plot) +
  geom_sf(fill = "grey90", colour = "grey35", linewidth = 0.25) +
  labs(
    title = "Administrative Regions (Lev. 1) of the Sahel and the Horn of Africa",
    x = "Longitude (°)",
    y = "Latitude (°)",
    caption = "Source: GADM v4.1"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 5),
    plot.caption = element_text(size = 10, hjust = 1)
  )


#==============================================================================
# Saving
ggsave(
  file.path(figs, "1.sahel_admin_regions.png"),
  width = 10,
  height = 8,
  dpi = 600
)

writeVector(sahel_sf, file.path(inter, "1.admin_reg.gpkg"), overwrite = TRUE)










