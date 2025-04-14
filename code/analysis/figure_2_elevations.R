dem <- '/Users/eyackulic/Downloads/ca_dem/mosaics/tcsi_dem.tif' |> terra::rast()
treats <- terra::vect('/Users/eyackulic/Desktop/facts_db.gpkg') |> terra::project(terra::crs(dem))
elevation <- matrix(nrow = nrow(treats)) |> data.frame()
elevation$distid <- treats$distid
treats <- terra::crop(treats, dem)

for(i in 1:length(treats)){
  
  elevation[i,] <- 
    dem |>
    terra::crop(y = treats[i], mask = T) |>
     terra::values() |> 
    mean(na.rm = T)
}
library(ggplot2)
elevation |>
  magrittr::set_colnames(c('elevation', 'distid')) |>
  ggplot(aes(y = elevation)) +
ggdist::stat_halfeye(
  adjust = 0.5,
  justification = -.2,
  .width = 0,
  point_colour = NA
) +
  geom_boxplot(
    width = .12,
    outlier.color = NA,
    alpha = 0.5,
  )+
  ggdist::stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = NA
#    color = 'black'
  )+
  tidyquant::scale_fill_tq()+
  tidyquant::theme_tq()+
  theme_classic() +
  coord_flip() +
  scale_y_continuous(breaks = c(800,1200,1600,2000))
