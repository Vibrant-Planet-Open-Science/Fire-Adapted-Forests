
#additional recommendations 
#consider removing fire scars from past 10 years
rm(list = ls())
gc()
file.remove(list.files(tempdir(), full.names = T))
require(terra);require(sf)
source("~/Desktop/ref_region.R")

# carbon <- c(
#   '/Users/eyackulic/Downloads/CA_agb_2015_30m.tif',
#   '/Users/eyackulic/Downloads/CA_agb_2020_30m.tif',
#   '/Users/eyackulic/Downloads/CA_agb_2021_30m.tif'
# )
carbon <- '/Users/eyackulic/workspace/TCSI_Carbon_Layers' |> list.files(full.names = T)
carbon <- carbon[-1]

projection = get_projection(carbon[1])
tcsi_eco_regions <- prepRR(
  aoi = '/Users/eyackulic/Desktop/exampleROI/Tahoe-Central_Sierra_Initiative.shp',
  projection = projection,
  l4eco =  '/Users/eyackulic/Downloads/us_eco_l4_state_boundaries/us_eco_l4.shp'
) |> 
  dplyr::filter(!US_L4CODE %in% '5b')#removing subalpine ecoregion with different fire regime

#download all facts database post 2015 for the TCSI region -- this is important for selecting candidate treatments,
#and also to screen out areas that have been treated from being included in the reference region development.
facts_subset <- 
  '/Users/eyackulic/Desktop/all_facts_since_2010.gpkg' |>
  sf::read_sf() |>
  dplyr::mutate(year = substring(newdate,1,4)) |>
  dplyr::filter(year > 2010)  |>
  sf::st_transform(sf::st_crs(projection)) |>
  sf::st_intersection(tcsi_eco_regions) |>
  dplyr::filter(
    sf::st_geometry_type(geom) %in% 
      c('POLYGON', 'MULTIPOLYGON', 'GEOMETRYCOLLECTION')
  ) 


f <- tcsi_eco_regions |> dplyr::mutate( n = 1) |> dplyr::group_by(n) |> dplyr::summarise() |> sf::st_transform(projection)
rdnbr = '/Users/eyackulic/Downloads/postfire_cbi_centralSierra.tif' |> terra::rast() |> max(na.rm = T) |> terra::project(terra::crs(f))
carb = '/Users/eyackulic/workspace/TCSI_Carbon_Layers/composite_2023_median.tif' |> terra::rast() |> terra::project(terra::crs(f))
carb_init = '/Users/eyackulic/workspace/TCSI_Carbon_Layers/composite_2015_median.tif' |> terra::rast() |> terra::project(terra::crs(f))
carb1 <- carb_init |> terra::crop(f, mask = T)
carb2 <- carb |> terra::crop(f, mask = T)
cbi <- rdnbr |> terra::crop(f, mask = T)

terra::plot(carb1/2, col = viridis::viridis(6)[-c(1,6)])
terra::plot(f$geometry, add = T)
terra::plot(carb2/2, col = viridis::viridis(6)[-c(1,6)])
terra::plot(f$geometry, add = T)
terra::plot(cbi, col = viridis::magma(6)[-c(1,6)])
terra::plot(f$geometry, add = T)

df <- '/Users/eyackulic/Desktop/final_cms_data_2025-03-31_111.rds' |> 
  readRDS() |> 
  dplyr::mutate(
    designation = dplyr::case_when(
      dataset %in% 'real' & area %in% 'project' ~ 'treatment',
      dataset %in% 'fake' & area %in% 'project' ~ 'placebo',
      dataset %in% 'real' & area %in% 'ref_region' ~ 'ref region',
      dataset %in% 'fake' & area %in% 'ref_region' ~ 'placebo ref region'
    ),
    designation = factor(designation, levels = c( 'treatment','ref region','placebo ref region', 'placebo')),
    composite_2015_median = composite_2015_median/2, #change biomass to carbon for all years (half of biomass is carbon)
    composite_2016_median = composite_2016_median/2,
    composite_2017_median = composite_2017_median/2,
    composite_2018_median = composite_2018_median/2,
    composite_2019_median = composite_2019_median/2,
    composite_2020_median = composite_2020_median/2,
    composite_2021_median = composite_2021_median/2,
    composite_2022_median = composite_2022_median/2,
    composite_2023_median = composite_2023_median/2,
    max_rdnbr_year = max_rdnbr_year - 1,
    fire_change = NA,
    fire_change = dplyr::case_when(
      max_rdnbr_year %in% 2015 ~ NA,
      max_rdnbr_year %in% 2016 ~ (composite_2017_median - composite_2015_median) / composite_2015_median,
      max_rdnbr_year %in% 2017 ~ (composite_2018_median - composite_2016_median) / composite_2016_median,
      max_rdnbr_year %in% 2018 ~ (composite_2019_median - composite_2017_median) / composite_2017_median,
      max_rdnbr_year %in% 2019 ~ (composite_2020_median - composite_2018_median) / composite_2018_median,
      max_rdnbr_year %in% 2020 ~ (composite_2021_median - composite_2019_median) / composite_2019_median,
      max_rdnbr_year %in% 2021 ~ (composite_2022_median - composite_2020_median) / composite_2020_median,
      max_rdnbr_year %in% 2022 ~ (composite_2023_median - composite_2021_median) / composite_2021_median
    ),    
    pre_fire = NA,
    pre_fire = dplyr::case_when(
      max_rdnbr_year %in% 2015 ~ NA,
      max_rdnbr_year %in% 2016 ~ composite_2015_median,
      max_rdnbr_year %in% 2017 ~ composite_2016_median,
      max_rdnbr_year %in% 2018 ~ composite_2017_median,
      max_rdnbr_year %in% 2019 ~  composite_2018_median,
      max_rdnbr_year %in% 2020 ~  composite_2019_median,
      max_rdnbr_year %in% 2021 ~ composite_2020_median,
      max_rdnbr_year %in% 2022 ~  composite_2021_median
    )
  )


treats <-             
  facts_subset |>
  dplyr::filter(distid %in% df$distid,
                disttype %in% c('Commercial Thinning', 'Commercial Thin'))

fires <- 
  '/Users/eyackulic/Downloads/Recent_Large_Fire_Perimeters_(%3E%3D5000_acres)/Recent_Large_Fire_Perimeters_(>%3D5000_acres).shp' |>
  sf::read_sf() |>
  sf::st_make_valid() |>
  sf::st_transform(projection) |> 
  sf::st_crop(y = f) 

fire_buffer <- sf::st_buffer(fires, dist = -500)
fire_buffer_out <- sf::st_buffer(fires, dist = 500)

fire_treats <- sf::st_intersection(treats, fires)
buffer_treats <- sf::st_intersection(treats, fire_buffer)
buffer_out_treats <- sf::st_intersection(treats, fire_buffer_out)

fire_test <-
  df |> 
  dplyr::filter(designation %in% 'treatment') |>
  dplyr::mutate(
    buffer = NA,
    buffer = dplyr::if_else(distid %in% buffer_out_treats$distid, 'outer_buffer', buffer),
 #   buffer = dplyr::if_else(distid %in% fire_treats$distid, 'perimeter', buffer),
    buffer = dplyr::if_else(distid %in% buffer_treats$distid, 'consumed', buffer)
  ) |> 
  dplyr::select(distid, buffer, disttype, max_rdnbr) |>
  dplyr::group_by(distid, disttype, buffer) |>
  dplyr::reframe(m = mean(max_rdnbr,na.rm = T),n = dplyr::n()) |>
  dplyr::distinct() |>
  dplyr::filter(!is.na(buffer)) |>
  dplyr::left_join(treats, by = c('distid', 'disttype'))

fire_test |>
ggplot(aes( x = buffer, y = polyarea/10000, fill = buffer)) + 
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
    # binwidth = 3,
    overflow = 'compress',
    color = 'black'
  )+
  #   facet_grid(~follow_up) |>
  #  tidyquant::scale_fill_tq()+
  tidyquant::theme_tq()+
  labs(title = "",
       subtitle = "",
       x = "",
       y = ""
  )+
  coord_flip() + theme_classic() +
  theme(axis.text.y = element_blank()) +
  scale_fill_manual(values = c('gray','blue4')) + facet_grid(~buffer)

mod <- lm( polyarea ~ buffer, data = fire_test)
mod <- lm( m ~ buffer, data = fire_test)
summary(mod)

fire_test$buffer |> table()

treats |> dplyr::filter(distid %in% fire_test$distid) |> sf::write_sf('/Users/eyackulic/Downloads/fire_treats.gpkg') 
major_fires <- fires |> dplyr::filter(FIRE_NAME %in% unique(fire_treats$FIRE_NAME))
outie <- matrix(nrow = 3, ncol = 3) |> data.frame()

for(i in 1:nrow(major_fires)){
  fire = major_fires[i,]
cbi_crop <- terra::crop(cbi,fire)
vals <- terra::values(cbi_crop, na.rm = T)  
hi_percent = length(which(vals > 2.25))/length(vals)
n = length(vals)
outie[i,] <- cbind(fire$FIRE_NAME, hi_percent,n)
}

cbi |> terra::values(na.rm = T) |> length()
terra::vect(fires) |> terra::plot()

#part 2 

fires |> terra::vect() |> terra::plot( add = T)
fire_treats$meters <- fire_treats |> sf::st_area()
plot(fire_treats$meters, fire_treats$polyarea)

d4 <- 
  df |>
  dplyr::filter(designation %in% 'treatment', distid %in% fire_treats$distid) |>
  dplyr::group_by(distid) |>
  dplyr::reframe(
    n = dplyr::n(),
    m = mean(max_rdnbr)
  ) |>
  dplyr::left_join(fire_treats, by = 'distid') |>
  dplyr::filter(m > 0, !duplicated(distid)) 

d4$polyacres <- d4$polyarea / 10000
mod <- lm(m ~ log(polyacres), d4)
summary(mod)

scaleFUN <- function(x) sprintf("%.2f", exp(x))


ggplot() + geom_point(data = d4, aes(x = log(polyacres), y = m, fill = FIRE_NAME), shape = 21, size = 5) +
  tidyquant::theme_tq() + ylab('mean CBI') + 
  xlab('') +
  tidyquant::scale_fill_tq() +
  geom_vline(xintercept = log(6.07), linewidth = .1) +
  geom_smooth(method = 'lm', data = d4, aes(x = log(polyacres), y = m), color = 'black', linetype = 'dashed', alpha = 0)+
  scale_x_continuous( 
    sec.axis = dup_axis(labels = scaleFUN, name = 'Treatment Area (Acres)'), position = 
      'top') +
  theme_classic()+
  theme(axis.text.x.top = element_blank())


                     