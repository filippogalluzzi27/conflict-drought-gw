#==============================================================================
# Script for data cleaning for population data


#==============================================================================
# Libraries and paths

library(terra); library(exactextractr); library(sf)
library(dplyr); library(tidyr); library(ggplot2); library(scales)
raw <- "data/raw"            
inter <- "data/intermediate"  
figs <- "output/figures"     
# Big data are outside the folder of this project
path_bigdata <- "B:/Dataset e Analisi finite/Dataset/tesi-magistrale"   


#==============================================================================
sahel <- st_read(file.path(inter, "1.admin_reg.gpkg"))
anni_pop <- c(2000, 2005, 2010, 2015, 2020)

# Upload pop data for each year
files <- file.path(path_bigdata,
                   sprintf("population/GHS_POP_E%d_GLOBE_R2023A_4326_30ss_V1_0.tif", anni_pop))
stopifnot(all(file.exists(files)))


#==============================================================================
# Population for weight extractions (2010)
pop_w <- crop(rast(files[anni_pop == 2010]), vect(sahel))


#==============================================================================
# Extraction of sum for each year
pop_snap <- lapply(seq_along(anni_pop), function(i) {
  r <- rast(files[i])
  r <- crop(r, sahel)
  s <- exact_extract(r, sahel, fun = "sum")    # SOMMA = totale abitanti
  data.frame(orig = sahel$GID_1, year = anni_pop[i], pop = s)
})
pop_snap <- bind_rows(pop_snap)


# Check
cat("Snapshot estratti. Esempio:\n")
print(head(pop_snap))
cat("Pop totale Sahel per anno:\n")
print(tapply(pop_snap$pop, pop_snap$year, sum))


#==============================================================================
# Check for exponential interpolation
interni <- c(2005, 2010, 2015)
err <- lapply(interni, function(yr) {
  base <- pop_snap[pop_snap$year != yr, ]      # nascondo lo snapshot yr
  vero <- pop_snap[pop_snap$year == yr, c("orig","pop")]
  pred <- base |>
    group_by(orig) |>
    summarise(
      lin = approx(year, pop,      xout = yr, rule = 2)$y,
      exp = exp(approx(year, log(pop), xout = yr, rule = 2)$y),
      .groups = "drop") |>
    left_join(vero, by = "orig")
  data.frame(year = yr,
             mape_lin = mean(abs(pred$lin - pred$pop)/pred$pop),
             mape_exp = mean(abs(pred$exp - pred$pop)/pred$pop))
})
err <- bind_rows(err)
cat("MAPE per anno (lineare vs esponenziale):\n"); print(err)
cat("Media  lineare:", round(mean(err$mape_lin), 4),
    "| esponenziale:", round(mean(err$mape_exp), 4), "\n")



#==============================================================================
# Annual exponential interpolation for the years in the middle
grid <- expand.grid(orig = unique(pop_snap$orig),
                    year = 2000:2020)
pop_annual <- grid |>
  left_join(pop_snap, by = c("orig","year")) |>
  arrange(orig, year) |>
  group_by(orig) |>
  mutate(pop = exp(approx(x = year[!is.na(pop)],
                          y = log(pop[!is.na(pop)]),
                          xout = year, rule = 2)$y)) |>
  ungroup()


#==============================================================================
# Add names for region/country
pop_annual <- pop_annual |>
  left_join(st_drop_geometry(sahel)[, c("GID_1","NAME_1","COUNTRY")],
            by = c("orig" = "GID_1")) |>
  rename(region = NAME_1, country = COUNTRY) |>
  select(orig, region, country, year, pop)


#==============================================================================
# log-popolazione
pop_annual$log_pop <- log(pop_annual$pop + 1)

#==============================================================================
# Final check
cat("\nRows (region×year):", nrow(pop_annual), "\n")
cat("Years:", paste(range(pop_annual$year), collapse="-"), "\n")
cat("Regions:", n_distinct(pop_annual$orig), "\n")
print(summary(pop_annual$pop))


#==============================================================================
# Plot
pop_tot <- pop_annual |>
  group_by(year) |>
  summarise(pop = sum(pop), .groups = "drop") |>
  mutate(snapshot = year %in% anni_pop)   # TRUE = dato originale, FALSE = interpolato

p <- ggplot(pop_tot, aes(year, pop)) +
  geom_line(linewidth = 0.7, colour = "grey30") +
  geom_point(aes(shape = snapshot, fill = snapshot), size = 2.6, colour = "grey30") +
  scale_shape_manual(values = c(`TRUE` = 21, `FALSE` = 21),
                     labels = c("Interpolated (exponential)", "Snapshot GHS-POP")) +
  scale_fill_manual(values = c(`TRUE` = "#1D9E75", `FALSE` = "white"),
                    labels = c("Interpolated (exponential)", "Snapshot GHS-POP")) +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  labs(x = NULL, y = "Total population",
       shape = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white", colour = NA))
p


#==============================================================================
# Saving
ggsave(file.path(figs, "2.pop_growth.png"), p,
       width = 7, height = 4.2, dpi = 300)
write.csv(pop_annual, file.path(inter, "2.pop_region_year.csv"), row.names = FALSE)
writeRaster(pop_w, file.path(inter, "2.pop_weight.tif"), overwrite = TRUE)






