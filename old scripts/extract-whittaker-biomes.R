library(plotbiomes)
library(sp)

clim_dat <- read.csv("~/Dropbox/Projects/NutNet/Data/clim_dat.csv", stringsAsFactors = FALSE)

# In order to intersect the study points with the Whittaker biomes polygons, we
# need to transform the climate data to spatial point object, forcing
# temperature and precipitation (cm) data as coordinates without a CRS.
points_sp <- sp::SpatialPoints(coords = clim_dat[, c("MAT_v2", "MAP")])


# Extract biomes for each study location. # Whittaker biomes as polygons (comes
# with the plotbiomes package)
Whittaker_biomes_df <- sp::over(x = points_sp,
                                y = plotbiomes::Whittaker_biomes_poly)

clim_dat <- cbind(clim_dat, Whittaker_biomes_df)

write.csv(clim_dat, file = "~/Dropbox/Projects/NutNet/Data/clim_dat_with_Whittaker_biomes.csv", row.names = FALSE)
