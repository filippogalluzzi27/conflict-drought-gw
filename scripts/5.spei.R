#==============================================================================
# This script is used to clean and manage data related to SPEI
# 
# Data: SPEIbase
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
path_spei    <- file.path(path_bigdata, "SPEI")   ## sottocartella SPEI (uniforma i path)

sahel <- vect(file.path(inter, "1.admin_reg.gpkg"))
pop_w <- rast(file.path(inter, "2.pop_weight.tif"))



#==============================================================================
# Parametri e CRS check
#==============================================================================
SPEI_TS   <- "06"     ## timescale primaria: 03/06/12
SPEI_VAR  <- "gs"     ## "gs" = growing-season | "min" = annual-min
GS_MONTH  <- 9        ## mese di fine stagione (Sahel JJAS -> settembre)

cat("SPEI CRS:", crs(rast(file.path(path_spei,"spei03.nc")), describe=TRUE)$name, "\n")
cat("GADM CRS:", crs(sahel, describe=TRUE)$name, "\n")
stopifnot(crs(sahel, describe=TRUE)$code ==
            crs(rast(file.path(path_spei,"spei03.nc")), describe=TRUE)$code)



#==============================================================================
# ritaglio: tempo (1995-2020, margine per lag) + bbox Sahel
#==============================================================================
crop_spei <- function(file_in, file_out) {
  r <- rast(file_in)
  keep <- time(r) >= as.Date("1995-01-01") & time(r) <= as.Date("2020-12-31")
  r <- r[[ which(keep) ]]
  r <- crop(r, sahel)
  writeCDF(r, file.path(inter, file_out), overwrite = TRUE)
  cat(file_out, "->", nlyr(r), "layer\n")
}
crop_spei(file.path(path_spei, "spei03.nc"), "5.spei03_sahel.nc")
crop_spei(file.path(path_spei, "spei06.nc"), "5.spei06_sahel.nc")
crop_spei(file.path(path_spei, "spei12.nc"), "5.spei12_sahel.nc")


#==============================================================================
# estrazione admin-1 pop-weighted
#==============================================================================
extract_spei <- function(ts) {
  r <- rast(file.path(inter, sprintf("5.spei%s_sahel.nc", ts)))
  w <- resample(pop_w, r, method = "sum")
  ex <- exact_extract(r, st_as_sf(sahel), fun = "weighted_mean",
                      weights = w, progress = FALSE)
  colnames(ex) <- as.character(time(r))
  ex$GID_1 <- sahel$GID_1
  long <- pivot_longer(ex, -GID_1, names_to = "date", values_to = "spei") %>%
    mutate(date  = as.Date(date),
           year  = as.integer(format(date, "%Y")),
           month = as.integer(format(date, "%m")))
  long
}

long <- extract_spei(SPEI_TS)


#==============================================================================
# aggregazione mese -> anno (toggle SPEI_VAR)
#==============================================================================
if (SPEI_VAR == "gs") {
  spei_yr <- long %>% filter(month == GS_MONTH) %>%
    select(GID_1, year, spei)               ## SPEI di fine growing-season
} else {
  spei_yr <- long %>% group_by(GID_1, year) %>%
    summarise(spei = min(spei), .groups = "drop")  ## annual-min
}
spei_yr <- filter(spei_yr, year >= 2000, year <= 2018)  ## finestra panel

cat("righe:", nrow(spei_yr), "| GID_1:", n_distinct(spei_yr$GID_1), "\n")  ## atteso 219*20



#==============================================================================
# controllo visivo - differenze tra due anni
#==============================================================================
anni_map <- c(2009, 2010)

sahel_map <- sahel %>%
  st_as_sf() %>%
  left_join(filter(spei_yr, year %in% anni_map), by = "GID_1")

lim <- max(abs(sahel_map$spei), na.rm = TRUE) * c(-1, 1)  # scala simmetrica condivisa

p_spei <- ggplot(sahel_map) +
  geom_sf(aes(fill = spei), colour = "grey30", linewidth = 0.2) +
  scale_fill_gradient2(low = "#8B2500", mid = "grey95", high = "#08519C",
                       midpoint = 0, limits = lim, name = "SPEI06") +
  facet_wrap(~ year) +
  labs(title = "SPEI06 (September)") +
  theme_void(base_size = 12)
p_spei




#==============================================================================
# Saving
#==============================================================================
write.csv(spei_yr, file.path(inter, sprintf("5.spei_region_year_%s_%s.csv",
                                            SPEI_TS, SPEI_VAR)), row.names = FALSE)
ggsave(file.path(figs, "5.spei06_map.png"), p_spei,
       width = 7, height = 5, dpi = 300)



