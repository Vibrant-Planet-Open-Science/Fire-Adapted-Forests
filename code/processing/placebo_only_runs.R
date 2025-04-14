
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
#building reference regions for an example shapefile 
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

##CURRENTLY PRODUCING OUTPUT FROM BELOW
#1g settings - seed 30112467
# 
# output <-
# runAnalyses(
ref_region = tcsi_eco_regions;
treatments = facts_subset; 
carbon = carbon;  
whp =  '/Users/eyackulic/Desktop/whp2014.tif';
evc = '/Users/eyackulic/Downloads/US_140_EVC/Tif/us_140evc.tif';
evt = '/Users/eyackulic/Downloads/LF2016_EVT_200_CONUS/Tif/LC16_EVT_200.tif';
dem = '/Users/eyackulic/Downloads/ca_dem/mosaics/tcsi_dem.tif';
roads = '/Users/eyackulic/Desktop/distance_2_road_raster.tif';
rdnbr = '/Users/eyackulic/Downloads/postfire_cbi_centralSierra.tif' ;
output_loc = '/Users/eyackulic/Desktop/';
runID = 'cms_1';
seed = 222;
fake_treatments = T;
ids = NA;  
samples = c(26, 45, 89,  5, 19, 44)
#)



df <- '/Users/eyackulic/Desktop/final_cms_data_2025-03-23_111.rds' |> 
  readRDS() |> 
  dplyr::mutate(
    designation = dplyr::case_when(
      dataset %in% 'real' & area %in% 'project' ~ 'treatment',
      dataset %in% 'fake' & area %in% 'project' ~ 'placebo',
      dataset %in% 'real' & area %in% 'ref_region' ~ 'ref region',
      dataset %in% 'fake' & area %in% 'ref_region' ~ 'placebo ref region'
    ),
    designation = factor(designation, levels = c( 'treatment','ref region','placebo ref region', 'placebo'))
  )

df |> 
  dplyr::filter(designation %in% 'treatment') |>
  dplyr::select(distid, ecoregion) |>
  dplyr::distinct() |>
  dplyr::select(ecoregion) |>
  table()


ids <-
  df |> 
  dplyr::filter(designation %in% 'treatment') |>
  dplyr::select(distid) |>
  unique()




rm(df)




#year should be an argument that choses treatment year and correct layers for building rr

#wrapper function for rr development and analysis

if(is.na(seed)){set.sexed(1)}else{set.seed(seed)} 

if(fake_treatments == T & is.na(sum(samples))){samples <- rep(25,length(ref_region$US_L4CODE))}

l = 1
#l2 <- list_out
list_out <- list()
z = 0
#i = 1

for(i in 1:length(ref_region$US_L4CODE)){
  
  l4_region <-  
    ref_region[i,]
  
  #Build stacks -- time intensive, should only have to do it once per ecoregion
  #build stack for selecting RRs
  #THIS function calls first carbon layer -- need to make sure that lines up with start of projects
  ecoregion_stack <- get_ecoregion_stack(
    l4_ref_region = l4_region, 
    carbon = carbon, 
    evc = evc, 
    evt = evt,
    whp = whp,
    dem = dem,
    roads = roads)
  
  evaluation_stack <- conform_evaluation_rasters(l4_ref_region = l4_region, carbon, 
                                                 rdnbr = rdnbr, 
                                                 dummy_raster = ecoregion_stack$Location,
                                                 stack = TRUE)
  ecoregion_stack <- clean_ecoregion_stack(ecoregion_stack, evaluation_stack)
  
  #trim facts to single ref region and buffer every treatment since 2015 -- this will
  #remove consideration from being in the RR
  all_treatments_buffered <- 
    treatments |>
    sf::st_intersection(l4_region) |>
    sf::st_make_valid() |>
    sf::st_transform(sf::st_crs('EPSG:5070')) |> #need a crs that uses meters as unit
    sf::st_buffer(dist = 50) |># check this number!  
    sf::st_transform(
      sf::st_crs(l4_region)
    )     
  #crop the dummy raster (all location numbers) to only have values inside the ref
  # region and outside of buffered treatment areas 
  rr_crop <- 
    terra::mask(ecoregion_stack$Location,
                all_treatments_buffered,
                inverse = T) |>
    terra::mask(terra::vect(l4_region))
  
  #pull all possible pixels within reference region; mutations begin after here
  
  ref_pixels <- 
    rr_crop |>
    terra::values() |>
    dplyr::as_tibble() |>
    tidyr::drop_na() |>
    unlist() |>
    as.numeric() |>
    treatmentFilters(ecoregion_stack = ecoregion_stack) 
  
  new_vals <- 
    ecoregion_stack |>
    terra::values() |>
    data.frame() 
  
  new_vals[which(!new_vals$Location %in% ref_pixels),] <- NA
  new_rr <- terra::setValues(ecoregion_stack, new_vals)
  
  #select all treatments within ecoregion
  if(fake_treatments == FALSE){
    
    all_treatments <- 
      treatments |>
      dplyr::filter(year %in% c(2016)) |>
      dplyr::filter(
        disttype %in% c('Commercial Thin', 'Commercial Thinning')#, 'Thinning','Machine Pile', 'Machine Pile Burn','Fuelbreak/Defensible Space',
        #        'Clearcut','Chipping','Crushing', 'Biomass Removal', 'Alternative Prescription', 'Broadcast Burn', 'Lop and Scatter',
        #         'Selection','Group Selection')
      ) |>
      sf::st_intersection(l4_region) 
    
  }else{
    
    treaties <- 
      treatments |>
      sf::st_intersection(l4_region) |>
      dplyr::filter(distid %in% ids$distid,
                    disttype %in% c('Commercial Thinning', 'Commercial Thin')
      ) 
    treaties$area <- sf::st_area(treaties)
    
    treaties2 <-
      treaties |> dplyr::arrange(distid, -area) |> dplyr::filter(!duplicated(distid))
    
    all_treatments  <- 
      fakeTreatments(l4_treats = treaties2, l4_region = l4_region, rrs = new_rr, good_rrs = ecoregion_stack, samples = nrow(treaties2)) 
    #|>          sf::st_crop(y = l4_region)
    #back check which distids arent in all_treatments but in treaties2
    treaties2[which(!treaties2$distid %in% all_treatments$distid),]$distid
    
    all_treatments <- all_treatments |> dplyr::arrange(distid, area_m) |> dplyr::filter(!duplicated(distid))
    
    #add a counter for number of failures and then increase search radius for new matches 
    
    # all_treatments$distid <- seq(1, length(all_treatments$area_m), by = 1)
    all_treatments |> sf::write_sf(paste0('/Users/eyackulic/Desktop/fake_treatments_',runID,'_',seed, '_',i,'.gpkg'))
  }  
  
  #add begin individually evaluating treatments here
  for(j in 1:nrow(all_treatments)){
    
    aoi <- all_treatments[j,]
    if(sf::st_geometry_type(aoi) == 'GEOMETRYCOLLECTION'){
      print('skip') 
      next
    }
    #aoi |> aoi_tester(new_rr)
    aoi <- aoi |> sf::st_cast()#to = 'MULTIPOLYGON')
    #pull values for aoi
    aoi_vals <- 
      aoi |>
      get_aoi_vars(ecoregion_stack = ecoregion_stack) |>
      dplyr::filter(Carbon > 10, EVT == 1) |>
      dplyr::mutate(area = 'Treatment', year = aoi$year)
    
    if(nrow(aoi_vals) < 10){
      print('next')
      next
    }
     aoi_evaluators <- pull_evaluation_variables(evaluation_stack, aoi_vals$Location) 
    
    aoi_evaluators$area <- 'project'
    
    out <-  aoi_evaluators
    out$distid <- aoi$distid
    out$ecoregion <- l4_region$US_L4CODE
    out$disttype <- aoi$disttype 
    out$trt_yr <- aoi$year
    
    final_out <- get_pixel_coordinates(pixels = unique(out$Location), raster = evaluation_stack$Location) |>
      dplyr::right_join(out, by = 'Location')
    
    print(glue::glue(unique(out$distid),' finished successfully!'))
    #final_out$tolerance <- tolerance 
    
    list_out[[l]] <- final_out
    l = l + 1
  }
}


df2 <- 
  purrr::map_df(list_out, ~as.data.frame((.)))

if(fake_treatments == T){
  out_var <- 'fake'
  df2$disttype <- 'fake'
  df2$trt_yr <- 0
}else{out_var <- 'real'}

df2 <-
  df2 |>
  dplyr::group_by(Location,composite_2015_median,
                  composite_2016_median,composite_2017_median,
                  composite_2018_median,composite_2019_median,composite_2020_median,
                  composite_2021_median,composite_2022_median,composite_2023_median,
                  lat,lon,area, distid,ecoregion,disttype,trt_yr) |>
  tidyr::pivot_longer(cols = -c('composite_2015_median','composite_2016_median','composite_2017_median',
                                'composite_2018_median','composite_2019_median','composite_2020_median',
                                'composite_2021_median','composite_2022_median','composite_2023_median',
                                'Location','lat','lon','area', 'distid','ecoregion','disttype','trt_yr'
  )) |>
  dplyr::reframe(
    max_rdnbr = no_na_max(value), #if no fire occurred, a max value of 0 is returned
    max_rdnbr_year = no_na_whichmax(value) + 2015 #if no fire occurred, fire year is 0.
    #value will be 1:6, or 2016:2021. invalid fires are set to 2015 for later removal.
  )


saveRDS(df2, 
        paste0(
          output_loc,
          out_var,
          '_treatments_',
          substring(Sys.time(),first = 1, last = 10),
          '_',
          runID,
          '_',
          seed,
          #  '_5c',
          '.rds'
        )
)
