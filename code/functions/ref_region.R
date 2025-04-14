#' Functions for developing reference regions based on carbon matching
#'
#' `get_projection` retreives a crs object from a raster path
#'
#' @param raster_path A character string to any raster. Can be used on a local copy
#' or AWS path
#' @returns crs object
#' @export
get_projection <- function(raster_path){
  raster_path |> 
    terra::rast() |>
    terra::crs()
}

#' l4 ecoregion retrieval 
#' `get_l4_rr` retrieves the fully extended l4 ecoregions for a given aoi.
#' @param aoi an sf vector object that represents the area for a past, present, 
#' or future treatment.
#' @param l4eco a character string that denotes the path (local or AWS) to the 
#' l4 ecoregion vector product
#' @param projection a crs object for consistent projections across data sources.
#' Can be returned from `get_projection` for any path, but it is recommended here to
#' source all data projections on the carbon layers that are ultimately used for reference
#' region development and evaluation.
#' @returns a unionized geometry of the extended l4 ecoregion. If the aoi encompasses
#' multiple l4 ecoregions, all will be returned with a warning.
#' @export

get_l4_rr <- function(aoi, l4eco, projection){
  
  ecomap <- 
    l4eco |>
    sf::read_sf()
  
  #because ecomap is huge, we will transform our aoi to that projection for cropping
  st_aoi <- 
    aoi |>
    sf::st_transform(sf::st_crs(ecomap))
  
  #step 1 : identify all l4 ecoregions that intersect with the aoi and represent
  #at least 5% of the aoi
  l4_project <- 
    ecomap |>
    sf::st_intersection(st_aoi) |> 
    dplyr::group_by(US_L4CODE) |>
    dplyr::summarise() |>
    dplyr::mutate(percent_area = as.numeric(sf::st_area(geometry)/sum(sf::st_area(geometry)))) |>
    dplyr::filter(percent_area > 0.05) 
  
  #step 2 : filter the l4codes identified in step 1 from the original ecomap, 
  #giving us the full l4 ecoregion extent
  l4_ref_region <- 
    ecomap |>
    dplyr::filter(US_L4CODE %in% l4_project$US_L4CODE) 
  
  #print warnings; does your aoi represent more than 1 ecoregion ?
  if(length(unique(l4_ref_region$US_L4CODE)) > 1){
    print('warning ref region is larger than a single ecozone; check your treatment area')
    ref_region <-
      l4_ref_region |>
      dplyr::group_by(US_L4CODE) |>
      dplyr::summarise()
  } else{
    print(glue::glue('producing l4 ref region for ', unique(l4_ref_region$US_L4CODE)))
    ref_region <-
      l4_ref_region |>
      sf::st_union()
  }

  #return ref region in ideal projection
  ref_region |>
    sf::st_transform(projection)
}

#' retrieve raster values for pixels present within aoi
#' `get_aoi_vars` retrieves the variables necessary for reference region development
#' and crops them to the aoi geometry.
#' @param aoi an sf vector object that represents the area for a past, present, 
#' or future treatment.
#' @param ecoregion_stack a terra raster stack object that contains layers for 
#' wildfire hazard potential, existing vegetation cover, forest type, and predisturbance 
#' carbon that has been cropped to the l4 reference region extent using `get_ecoregion_stack`
#' @returns a cropped version of the ecoregion stack that contains values present in the aoi.
#' Various warning are also printed that are related to methodology constraints. I.E., if your aoi 
#' does not meet the conditions of a FAF, it will be excluded from further consideration.
#' Current issues are centered around the sparse forest representation in the 2003ish forest 
#' type layer
#' @export
get_aoi_vars <- function(aoi, ecoregion_stack){
  
  
  aoi_filter <- 
    aoi |> 
    terra::svc() |>
    terra::vect() |> 
    terra::project(
      terra::crs(ecoregion_stack)
      ) |> 
    terra::crop(x = ecoregion_stack)
  
  aoi_filter_vals <- 
    aoi_filter |>
    terra::values() |> 
    data.frame()
  
  colnames(aoi_filter_vals) <- names(ecoregion_stack)
  #if only 1 forest type code is present, we will assume all forested pixels in the 
  #project area should have that same forest type code -- forest type layer is a 2003ish 
  #data product that is sparsely populated in comparison to aerial imagery / EVC estimation of 
  #forested cover
 

  aoi_filter_vals |>
    dplyr::filter(round(EVC, digits = 0) >= 102 & round(EVC, digits = 0) <= 109 & round(WHP, digits = 0) >= 4)
  
}

#' retrieve raster values at unique pixel locations
#' `get_pixel_values` extracts raster values at given pixel locations.
#' @param pixels a numeric vector representing all the pixels in a given l4 ecoregion,
#' labelled 1:n, where n is the single dimensional product of x * y of the raster matrix. 
#' @param raster A terra raster object. MUST be conformed to the same dimensions (i.e., cropped
#' and resampled) as the raster from which 'pixels' is derived 
#' #' @returns a dataframe of the pixel locations used, and the raster values acquired
#' at those locations
#' @export
get_pixel_values <- function(pixels, raster){
  raster_vals <- 
    raster |>
    terra::values()
  
  raster_sub_vals <- cbind(pixels,raster_vals[pixels]) |> data.frame()
  colnames(raster_sub_vals) <- c('location', 'raster_value')
  raster_sub_vals
}

#' conform rasters by cropping to the ref region and setting 
#' a uniform projection and resolution.
#' `conform_raster` conforms a single raster to the ref region extent and resolution and 
#' projection of a chosen raster layer -- in this case, I recommend using the carbon layer.
#' @param l4_ref_region an sf vector object that denotes the geometry of the extended
#' l4 ecoregion.
#' @param raster_path A character string denoting the path to the raster being transformed.
#' @param carbon_path A character string denoting the path to the raster with the desired
#' resolution and projection for replication.
#' @returns a cropped raster that is aligned with the target raster in terms of size, 
#' resolution, projection, extent and origin.
#' @export
conform_raster <- function(l4_ref_region, raster_path, carbon_path){
  #read in raster paths 
  raster <- terra::rast(raster_path)
  #only select a single layer from carbon for resampling
  carbon <- terra::rast(carbon_path)[[1]]
  #projec ref region in raster crs for faster cropping; transform to vect object
  projected_rr <- 
    l4_ref_region |>
    sf::st_transform(
      sf::st_crs(raster)
    ) |>
    terra::vect()
  
  #crop raster from newly projected rr, masking out all points outside of geometry
  croper <- 
    terra::crop(
      raster,
      projected_rr)
  
  crop <- 
    terra::mask(
      croper,
      projected_rr
    )
    
  
  #pull the name codes from the raster
  name = substring(names(raster),1,5)[1]
  
  #hardcoded for now; this will break with new layers
  if(name %in% c('CA_ag', 'compo')){ 
    year = substring(names(raster),8,11)[1]
    if(year %in% 2022){
      resample <- crop
    }else{
    #if its one of the carbon layers we are using, it doesnt need to be resampled --
    #its already in the correct projection / resolution
    
      names <- paste0('values_', seq(1,terra::nlyr(crop),1))
      
      resamp <- 
      crop |>
      terra::values() |>
      dplyr::as_tibble()
      
      new_max <- list()
      for(i in 1:ncol(resamp)){      
      n_max <- median(resamp[[i]], na.rm = T) + (2 * sd(resamp[[i]], na.rm = T))
        
      new_max[i] <-
        resamp |>
          dplyr::select(dplyr::all_of(i)) |>
          magrittr::set_colnames('values') |> 
          dplyr::mutate(
            values = ifelse(values < 0, NA, values),
            values = ifelse(values > n_max, NA, values)
          ) |>
          magrittr::set_colnames(names[i]) 
      }
      
      carbOut <- 
        new_max |> 
        dplyr::bind_cols() |> 
        dplyr::as_tibble()
      
    resample <- terra::setValues(crop, carbOut)
    names(resample) <- names(crop)
    }
  }else{
    #reproject raster in desired raster projection
    proj_crop <- terra::project(crop, terra::crs(carbon))
    #and resample
    resample <- terra::resample(proj_crop, carbon) 
  }
  
  if(name %in% 'whp20'){
    resample <- 
      resample |>
      terra::values() |>
      dplyr::as_tibble() |>
      magrittr::set_colnames('values') |>
      dplyr::mutate(
        values = round(values,digits = 0)
      ) |>
      terra::setValues(x = resample)
  }
  #another hardcoded spot; this could be handled elsewhere, but it makes sense 
  #to clean up the rdnbr files in one fell swoop. If the name starts with 'rdnbr',
  #pull the values and remove any non-negative values -- we dont care about these.
  if(name %in% 'rdnbr'){
    rdnbr_vals <- terra::values(resample)
    rdnbr_vals[which(rdnbr_vals < 1)] <- NA
    resample <- terra::setValues(resample, rdnbr_vals)
  }
  if(name %in% 'EVT_N'){
    year = substring(raster_path,nchar(raster_path) - 6,nchar(raster_path)- 4)[1]
    evt_vals <- terra::values(resample) |> data.frame() |> magrittr::set_colnames('VALUE')
    evt_vals
    csv_data <- 
      '/Users/eyackulic/Downloads/LF2016_EVT_200_CONUS/CSV_Data/LF16_EVT_200.csv' |> #hardcoded
      read.csv() |> 
      dplyr::select(VALUE, EVT_PHYS) |>
      dplyr::mutate(
        new_value = dplyr::if_else(EVT_PHYS %in% 'Conifer', 1, NA)
      )
    csv_data[nrow(csv_data)+1, ] <- c(NaN,NA,NA)
    
    new_vals <- dplyr::left_join(evt_vals, csv_data, by = 'VALUE')

    resample <- terra::setValues(resample, new_vals$new_value)
  }

  #return final object
  resample
}

#' Pull S3 paths for all necessary files for this workflow.
#' `rr_s3_paths` is a convenience function that hardcodes the paths to necessary rasters
#' as local environmental variables. Each NULL variable will default to the stored S3 path. 
#' For quicker access/processing, consider downloading local copies of each dataset and 
#' setting their local paths here, instead.
#' @param evc A character string denoting the path to the existing vegetation cover raster
#' @param whp A character string denoting the path to the wildfire hazard potential raster
#' @param forest_type A character string denoting the path to the forest type raster
#' @param carbon A character string denoting the path to the carbon stack used for selection
#' and analysis
#' @param l4eco A character string denoting the path to the l4eco vector dataset
#' @param rdnbr A character string denoting the path to the rdnbr raster stack
#' @param treatment A character string denoting the path to the knight et al curated
#' FACTs database
#' @param dem A character string denoting the path to the dem raster (if available)
#' @param roads A character string denoting the path to the roads gpkg
#' @returns hardcoded environmental variables that contain path names 
#' (in order: evc, whp, forest_type,carbon, l4eco, rdnbr, facts) 
#' @export 
rr_s3_paths <- function(
    evc = NULL,
    whp = NULL,
    carbon = NULL,
    l4eco = NULL,
    rdnbr = NULL,
    treatment = NULL,
    dem = NULL,
    roads = NULL)
{
  #rasters
  if(is.null(evc)){
    print('loading evc')
    evc <<-
      '/vsis3/vp-open-science/fire-adapted-forests/us_140evc.tif'
  }
  if(is.null(whp)){
    print('loading whp')
    whp <<- 
      '/vsis3/vp-open-science/fire-adapted-forests/whp2014.tif'
  }
  if(is.null(carbon)){
    print('loading carbon stack')
    carbon <<- c( 
      '/vsis3/vp-open-science/fire-adapted-forests/CA_agb_2015_30m.tif',
      '/vsis3/vp-open-science/fire-adapted-forests/CA_agb_2020_30m.tif',
      '/vsis3/vp-open-science/fire-adapted-forests/CA_agb_2021_30m.tif',
       )
  }
  if(is.null(l4eco)){
    print('loading l4eco')
    l4eco <<- 
      '/vsis3/vp-open-science/fire-adapted-forests/eco_l4_state_boundaries.zip'

  }
  if(is.null(rdnbr)){
    print('loading rdnbr stack')
    rdnbr <<- c(
      #'/vsis3/vp-sci-grp/fire-severity/processed/landscapes/tcsi_reference_region/postfire_rdnbr.tif'
      '/vsis3/vibrant-dragon/scratch-ethan/reference_region_inputs/rdnbr/tcsi_reference_region_rdnbr_v1.0_2016_extended.tif',
      '/vsis3/vibrant-dragon/scratch-ethan/reference_region_inputs/rdnbr/tcsi_reference_region_rdnbr_v1.0_2017_extended.tif',
      '/vsis3/vibrant-dragon/scratch-ethan/reference_region_inputs/rdnbr/tcsi_reference_region_rdnbr_v1.0_2018_extended.tif',
      '/vsis3/vibrant-dragon/scratch-ethan/reference_region_inputs/rdnbr/tcsi_reference_region_rdnbr_v1.0_2019_extended.tif',
      '/vsis3/vibrant-dragon/scratch-ethan/reference_region_inputs/rdnbr/tcsi_reference_region_rdnbr_v1.0_2020_extended.tif',
      '/vsis3/vibrant-dragon/scratch-ethan/reference_region_inputs/rdnbr/tcsi_reference_region_rdnbr_v1.0_2021_extended.tif'
    )  
  }
  if(is.null(treatment)){
    print('loading FACTs database')
    facts <<- '/vsis3/vibrant-dragon/scratch-ethan/reference_region_inputs/base_layers/FACTs/facts_thp_all.shp'
  }
  #dem step will not work outside of TCSI! Can be updated to California by changing end of path name to fullCA.tif 
  #Need an extra step to stop this from being an issue for 'conform_raster'
  if(is.null(dem)){
    print('loading dem')
    dem <<- '/vsis3/vibrant-dragon/scratch-ethan/reference_region_inputs/base_layers/dem/tcsi_dem.tif'
    #dem <<- '/vsis3/vibrant-dragon/scratch-ethan/reference_region_inputs/base_layers/dem/fullCA_dem.tif'
  }
  #read in roads
  if(is.null(roads)){
    print('loading roads')
    roads <<- '/Users/eyackulic/Desktop/distance_2_road_raster.tif'
    #roads <<- '/Users/eyackulic/Desktop/tcsi_conformed_roads.gpkg'
    #roads <<-  '/vsis3/vibrant-dragon/scratch-ethan/reference_region_inputs/base_layers/roads/tcsi_roads.gpkg'
    #roads <<-  '/vsis3/vibrant-dragon/scratch-ethan/reference_region_inputs/base_layers/roads/ca_nv_roads.gpkg'
  }
}

#' Match reference region pixels to aoi based on carbon values
#' `match_pixels` compares carbon values in the aoi to the reference region and creates a 
#' match that is x (set by tolerance) times  bigger than the original aoi.
#' @param carbon_values carbon values for all remaining pixels in the ref region pool
#' @param aoi_values all qualifying carbon values within the aoi geometry
#' @param tolerance integer used for multiplication. How much times bigger should the
#' final reference region be when compared with the aoi?
#' @returns a list of pixel locations and carbon values for remaining ref region pixels.
#' This constitutes the final reference region selection.
#' @export 
match_pixels <- function(carbon_values, aoi_values){
  
  #create bins of 10 carbon units between aoi min and max values
  bins <- seq(plyr::round_any(min(aoi_values$Carbon, na.rm = T), 10, f = floor),
              plyr::round_any(max(aoi_values$Carbon, na.rm = T), 10, f = ceiling), by = 5)
  
  #remove any rr pixels from consideration that exceed the aoi minmax boundaries
  carbon_values <- 
    carbon_values |>
    dplyr::mutate(carbon = dplyr::if_else(
      carbon < min(aoi_values$Carbon, na.rm = T) |
      carbon > max(aoi_values$Carbon, na.rm = T),
      NA,
      carbon
      )
      ) |>
    dplyr::filter(!is.na(carbon), !is.na(evc))
  
  #setting tolerance
 # tolerance <- getTolerance(bins = bins, carbon_values = carbon_values$carbon, aoi_values = aoi_values$Carbon)
  aoi_values <- 
    aoi_values |>
    dplyr::mutate(
      Carbon = ifelse(Carbon == min(bins), Carbon +.01, Carbon),
      Carbon = ifelse(Carbon == max(bins), Carbon - .01, Carbon)
    ) |>
    dplyr::filter(
      !is.na(Carbon),
      !is.na(EVC)
      )
  ##Changepoint :: switch to evc
  aoi_breaks <- paste0(aoi_values$EVC, '_',cut(aoi_values$Carbon, breaks = bins, right = F)) |> table()
  rr_breaks <- paste0(carbon_values$evc, '_',cut(carbon_values$carbon, breaks = bins, right = F)) |> table()

  rr_breaks <- rr_breaks[which(names(rr_breaks) %in% names(aoi_breaks))]
  aoi_breaks <- aoi_breaks[which(names(aoi_breaks) %in% names(rr_breaks))]
  percent_rr <- rr_breaks/aoi_breaks
  if(min(percent_rr) < 10){
  aoi_breaks <- aoi_breaks[which(percent_rr > 10)]
  rr_breaks <- rr_breaks[which(names(rr_breaks) %in% names(aoi_breaks))]
  aoi_breaks <- aoi_breaks[which(names(aoi_breaks) %in% names(rr_breaks))]
  percent_rr <- rr_breaks/aoi_breaks  
  }
 #if percent rr is < 1, remove band from rr and aoi tracking
  tolerance <- round((floor(min(percent_rr)) * 0.5), digits = 0)
  
  samples <- aoi_breaks * tolerance#tolerance

  #tolerance <- round((floor(min(rr_breaks/aoi_breaks)) * 0.5), digits = 0)
  if(tolerance == 0){tolerance <- 1; print('tolerance violated')}
  
  for(k in 1:length(names(aoi_breaks))){
    samps <- samples[k]
    n_name <- names(samps)
    evt_code <- stringr::str_sub(n_name, start = 0,end = 3)
    carb_start <- stringr::str_sub(n_name, start = 6, end = regexpr(pattern =',',text = n_name)[[1]]- 1) |> as.numeric()
    carb_end <- stringr::str_sub(n_name, start = regexpr(pattern =',',text = n_name)[[1]]+ 1, end = stringr::str_length(n_name)- 1) |> as.numeric()
  
    carb_sub <-
      carbon_values |>
      dplyr::filter(evc %in% evt_code &
                    carbon >= carb_start &
                    carbon <= carb_end) |>
      data.frame()
    
    sample_pixels <- dplyr::sample_n(tbl = carb_sub, size = samps, replace = F)
    
    keepers <-
      carbon_values |>
      dplyr::filter(pixels %in% sample_pixels$pixels) |>
      dplyr::mutate(tolerance = tolerance)
    
    #and store them
    if(k == 1){
      out <- keepers
    }else{
      out <- rbind(out,keepers)
    }
  }
  out
}
# 
#   if(tolerance == 0){tolerance <- 1; print('tolerance violated')}
#   #loop through bins
#   
  # for(k in 1:(length(bins)-1)){
  #   breaker = FALSE #if the breaker switches, match_pixels fails for this treatment
  #   #this ifelse statement ensures that the range of aoi values is fully represented
  #   #and deduces how many aoi values fall into each bin
  #   
  #   n <-
  #     aoi_values |>
  #     dplyr::as_tibble() |>
  # #    magrittr::set_colnames('carbon') |>
  #     dplyr::filter(
  #       dplyr::between(Carbon, left = bins[k], right = bins[k+1])
  #     ) |> nrow()
  #   #filter the full reference pool available between bin options
  #   samps <- 
  #     carbon_values |>
  #     dplyr::as_tibble() |>
  #     dplyr::filter(
  #       dplyr::between(carbon, left = bins[k], right = bins[k+1])
  #     )
  #   #quick check -- do these values exist in the reference region or aoi?
  #   if(dim(samps)[1] == 0 | n == 0){
  #  #   print('empty sample')
  #     next
  #   }else{ 
  #     size = tolerance * n
      #Add forest type logic in here
      
      
#       #sampling occurs here
#       randoms <- sample(samps$pixels, size = size, replace = FALSE)
#       
#       #filter out randomly selected pixels
#       keepers <-
#         carbon_values |>
#         dplyr::filter(pixels %in% samps$pixels &
#                         carbon_values$pixels %in% randoms) |>
#         dplyr::mutate(tolerance = tolerance)
#       
#       #and store them
#       if(k == 1){
#         out <- keepers
#       }else{
#         out <- rbind(out,keepers)
#       }
#     }
#   }
# 
# out
# }


#' Pulls pixel level values for evaluation from the list of evaluation rasters
#' `pull_evaluation_variables` takes a stack of evaluation rasters and returns a tibble
#' of observations of each raster at desired pixel locations.
#' @param evaluation_list raster stack of evaluation variables that has been conformed 
#' to the same size, resolution, and projection as the raster from which the pixel locations 
#' were pulled.
#' @param pixels locations of all pixels used in analysis
#' @returns a tibble with 10 columns : location, pre-disturbance carbon and then 8 evaluators
#' this is currently hardcoded and will break with more/less variables in the stack
#' @export 

pull_evaluation_variables <- function(evaluation_list, pixels){
  evaluators <- 
    evaluation_list |>
    terra::values() |>
    dplyr::as_tibble() |>
    dplyr::slice(pixels) 
  
  # assertthat::assert_that(
  #   ncol(evaluators) == 12,
  #   msg = "Currently, 12 columns are expected for evaluation."
  # )
  # 
  # colnames(evaluators) <- c('location','pre_carbon','mid_carbon','post_carbon',
  #                           'rdnbr2015','rdnbr2016','rdnbr2017','rdnbr2018','rdnbr2019',
  #                           'rdnbr2020','rdnbr2021','rdnbr2022')
  evaluators
}



#' Create a raster of pixel locations 
#' `create_dummy_raster` takes a raster and returns a raster of the same size with values
#' that are 1-dimensional locations that can be related to the x*y product of the original
#' raster.
#' @param raster the raster to be used for creating pixel locations. This should be cropped 
#' to its terminal extent.
#' @returns a raster object of locations
#' @export 
create_dummy_raster <- function(raster){
  cells <- dim(raster)[1] * dim(raster)[2] #all cells in raster
  dummy_raster_values <- seq(1, cells, 1) 
  dummy_raster <- terra::setValues(raster, dummy_raster_values) #reset values
  dummy_raster
}

#' A wrapper function to conform and stack multiple rasters that are used for 
#' identifying a reference region.
#' `get_ecoregion_stack` takes an sf geometry and crops/conforms rasters used for generating a 
#' reference region (evc, whp, forest_type, and carbon). These cropped/conformed rasters are 
#' output alongside a dummy raster of the same size that stores pixel locations for easy lookups.
#' @param l4_ref_region an sf vector object that denotes the extent of the l4_ref_region. used
#' for cropping.
#' @param carbon A character string denoting the path to the carbon stack used for selection
#' and analysis
#' @param evc A character string denoting the path to the existing vegetation cover raster
#' @param whp A character string denoting the path to the wildfire hazard potential raster
#' @param dem A character string denoting the path to a dem file; if null, elevations will be
#' calculated during this step (SLOW) and converted to slopes
#' @returns a raster stack of variables (forest type, evc, whp, carbon) and pixel locations
#' @export 
get_ecoregion_stack <- function(l4_ref_region, carbon, evc, whp, dem = NULL, roads, evt){

  carb_sub <- conform_raster(l4_ref_region = l4_ref_region, raster_path = carbon[1], carbon_path = carbon[1])
  evc_sub <- conform_raster(l4_ref_region, evc, carb_sub)
  whp_sub <- conform_raster(l4_ref_region, whp, carb_sub)
  roads_sub <- conform_raster(l4_ref_region, roads, carb_sub) *3.2808 #ft to m converison
  evt_sub <- conform_raster(l4_ref_region, evt, carb_sub)
  dummy_raster <- create_dummy_raster(carb_sub)
  
  if(dem %in% NULL){
    coords <- 
      dummy_raster |>
      terra::crds() |> 
      data.frame() |>
      sf::st_as_sf(coords = c("x", "y"),
                   crs = sf::st_transform(
                     sf::st_crs(l4_ref_region)
                     )
                   )
    
    slopes <- elevatr::get_elev_point(coords, prj = projection, src = "epqs") |>
      terra::rast() |>
      terra::terrain(elevations, v = 'slope', unit='degrees', neighbors=8)
  
  }else{
  slopes <- conform_raster(l4_ref_region, dem, carb_sub) |>
    terra::terrain(v='slope', unit='degrees', neighbors=8)
  }
  ecoregion_stack <- c(evc_sub, whp_sub, carb_sub, dummy_raster, slopes, roads_sub, evt_sub)
  names(ecoregion_stack) <- c('EVC', 'WHP','Carbon', 'Location', 'Slope', 'Road_Distance', 'EVT')
  ecoregion_stack
}


#' This function calculates change in carbon (relative and absolute), max rdnbr, and the 
#' year of max rdnbr for each pixel in the eval_stack that meets the criteria of the filter 
#' `pull_delta_carbon` takes a stack of evaluatory rasters and produces summary information,
#' based on an rdnbr filter.
#' @param eval_stack a raster stack used for evaluation. Currently hardcoded names are in use.
#' @param filter a filter for rdnbr values. Defaults to 0. Easy way to separately analyze all 
#' pixels (filter = 0), pixels that experienced any fire (filter >= 1), or pixels that exclusively
#' burned in a high severity fashion (filter >= 657)
#' @returns a dataframe of evaluatory variables
#' @export 
pull_delta_carbon <- function(eval_stack, filter = 0){
  
 #which pixels became NAs after fire?
#  pixels_lost = length(which(is.na(eval_stack$post_carbon)))
  #need to isolate na's and give them a value greater than 0 
#  eval_stack$post_carbon <- replace(eval_stack$post_carbon,is.na(eval_stack$post_carbon), values = 0.0001)
#  eval_stack$pre_carbon <- replace(eval_stack$pre_carbon,is.na(eval_stack$pre_carbon), values = 0.0001)
  
  eval_stack <- eval_stack |> dplyr::select(-rdnbr_v1.0_2022_extended)
  #pull remaining rdnbr_vals
  rdnbr_filter <- 
    eval_stack |>
    dplyr::mutate(
      total_delta = composite_2022_median - composite_2015_median,
      total_delta_percent = (composite_2022_median - composite_2015_median)/composite_2015_median
    ) |>
    dplyr::group_by(Location, total_delta, total_delta_percent,composite_2015_median,
                    composite_2016_median,composite_2017_median,
                    composite_2018_median,composite_2019_median,composite_2020_median,
                    composite_2021_median,composite_2022_median,composite_2023_median) |>
    tidyr::pivot_longer(cols = -c('composite_2015_median','composite_2016_median','composite_2017_median',
                                  'composite_2018_median','composite_2019_median','composite_2020_median',
                                  'composite_2021_median','composite_2022_median','composite_2023_median',
                                  'Location'#, #'evt',
                                  #'total_delta', 'total_delta_percent'
                                  )) |>
    dplyr::reframe(
      max_rdnbr = no_na_max(value), #if no fire occurred, a max value of 0 is returned
      max_rdnbr_year = no_na_whichmax(value) + 2015 #if no fire occurred, fire year is 0.
      #value will be 1:6, or 2016:2021. invalid fires are set to 2015 for later removal.
    ) |>
    dplyr::filter(max_rdnbr >= filter)  #filter results
  rdnbr_filter
}


pull_delta_carbon_b <- function(eval_stack, filter = 0){
  
  #which pixels became NAs after fire?
  #  pixels_lost = length(which(is.na(eval_stack$post_carbon)))
  #need to isolate na's and give them a value greater than 0 
  #  eval_stack$post_carbon <- replace(eval_stack$post_carbon,is.na(eval_stack$post_carbon), values = 0.0001)
  #  eval_stack$pre_carbon <- replace(eval_stack$pre_carbon,is.na(eval_stack$pre_carbon), values = 0.0001)
  
  eval_stack <- eval_stack |> dplyr::select(-rdnbr_v1.0_2022_extended)
  #pull remaining rdnbr_vals
  rdnbr_filter <- 
    eval_stack |>
  #  dplyr::mutate(
  #    total_delta = composite_2022_median - composite_2015_median,
  #    total_delta_percent = (composite_2022_median - composite_2015_median)/composite_2015_median
  #  ) |>
    dplyr::group_by(Location, 
                    CA_agb_2015_30m,CA_agb_2020_30m,CA_agb_2021_30m
                    #total_delta, total_delta_percent,composite_2015_median,
                    #composite_2016_median,composite_2017_median,
                    #composite_2018_median,composite_2019_median,composite_2020_median,
                    #composite_2021_median,composite_2022_median,composite_2023_median
                    ) |>
    tidyr::pivot_longer(cols = -c('CA_agb_2015_30m','CA_agb_2020_30m','CA_agb_2021_30m',
                    #              'composite_2018_median','composite_2019_median','composite_2020_median',
                    #              'composite_2021_median','composite_2022_median','composite_2023_median',
                                  'Location'#, 'evt',
                                  #'total_delta', 'total_delta_percent'
    )) |>
    dplyr::reframe(
      max_rdnbr = no_na_max(value), #if no fire occurred, a max value of 0 is returned
      max_rdnbr_year = no_na_whichmax(value) + 2015 #if no fire occurred, fire year is 0.
      #value will be 1:6, or 2016:2021. invalid fires are set to 2015 for later removal.
    ) |>
    dplyr::filter(max_rdnbr >= filter)  #filter results
  rdnbr_filter
}

pull_delta_carbon_c <- function(eval_stack, filter = 0){
  
  #which pixels became NAs after fire?
  #  pixels_lost = length(which(is.na(eval_stack$post_carbon)))
  #need to isolate na's and give them a value greater than 0 
  #  eval_stack$post_carbon <- replace(eval_stack$post_carbon,is.na(eval_stack$post_carbon), values = 0.0001)
  #  eval_stack$pre_carbon <- replace(eval_stack$pre_carbon,is.na(eval_stack$pre_carbon), values = 0.0001)
  
  eval_stack <- eval_stack |> dplyr::select(-rdnbr_v1.0_2022_extended)
  #pull remaining rdnbr_vals
  rdnbr_filter <- 
    eval_stack |>
    dplyr::group_by(EVC, WHP, Carbon, Location, Slope, Road_Distance, EVT, composite_2015_median,
                    composite_2016_median,composite_2017_median,
                    composite_2018_median,composite_2019_median,composite_2020_median,
                    composite_2021_median,composite_2022_median,composite_2023_median) |>
    tidyr::pivot_longer(cols = -c('EVC', 'WHP', 'Carbon', 'Location', 'Slope', 'Road_Distance', 'EVT',
                                  'composite_2015_median','composite_2016_median','composite_2017_median',
                                  'composite_2018_median','composite_2019_median','composite_2020_median',
                                  'composite_2021_median','composite_2022_median','composite_2023_median',
                                  'Location'#, #'evt',
                                  #'total_delta', 'total_delta_percent'
    )) |>
    dplyr::reframe(
      max_rdnbr = no_na_max(value), #if no fire occurred, a max value of 0 is returned
      max_rdnbr_year = no_na_whichmax(value) + 2015 #if no fire occurred, fire year is 0.
      #value will be 1:6, or 2016:2021. invalid fires are set to 2015 for later removal.
    ) |>
    dplyr::filter(max_rdnbr >= filter)  #filter results
  rdnbr_filter
}

#' This is a simple 'max' function that ensures that a value of 0 is returned when
#' it is fed a vector of only NAs
#' `no_na_max` returns the max entry in a numeric vector. If no values exist, 0 is returned
#' @param x a numeric vector
#' @returns the max value of x. If all values are NA, returns a 0
#' @export 
no_na_max <- function(x){ 
  ifelse(!all(is.na(x)), max(x, na.rm=T), 0)
}

#' This is a simple 'which.max' function that ensures that a value of -2015 is returned when
#' it is fed a vector of only NAs
#' `no_na_whichmax` returns the location of the max entry in a numeric vector. 
#' If no values exist, -2015 is returned
#' @param x a numeric vector
#' @returns the location of the max value within x. If all values are NA, returns a -2015 value
#' @export 
no_na_whichmax <- function(x){ 
  ifelse(!all(is.na(x)), which.max(x), -2015)
}

#' A wrapper function that pulls evaluation data for different levels of rdnbr interaction
#' at the project and ref region levels and returns a single, unified tibble that can be 
#' easily queried.
#' `full_stack_differences` takes project and reference region tibbles and 
#' returns a unified tibble of results for fire activity and carbon change 
#' in the reference region and project area.
#' @param project_tibble a collection of pixel-level observations in the project area
#' @param rr_tibble a collection of pixel-level observations in the reference region
#' @returns a unified tibble of results for fire activity and carbon change 
#' in the reference region and project area
#' @export 
full_stack_differences <- function(project_tibble, rr_tibble, vintage){
  # project_tibble$area <- 'project'
  # rr_tibble$area <- 'ref_region'
  
  full_tibble <- dplyr::bind_rows(project_tibble,rr_tibble)
  if(vintage == 'new'){
  out_values <- 
    full_tibble |>
    pull_delta_carbon(filter = 0) |>
    dplyr::mutate(subset = 'all')
  }else if(vintage == 'old'){
    out_values <- 
      full_tibble |>
      pull_delta_carbon_b(filter = 0) |>
      dplyr::mutate(subset = 'all')
  } else if(vintage == 'test'){
    out_values <- 
      full_tibble |>
      pull_delta_carbon_c(filter = 0) |>
      dplyr::mutate(subset = 'all')
  }else{print('error in vintage choice; must be either old, new or test')}
  
  out_values$area <- NA
  out_values[which(out_values$Location %in% project_tibble$Location),]$area <- 'project'
  out_values[which(out_values$Location %in% rr_tibble$Location),]$area <- 'ref_region'
  out_values #|> dplyr::rename(Location = location)
}

#' A wrapper function that conforms all of the evaluation rasters to the extent
#' of the l4_ref_region
#' `conform_evaluation_rasters` is a wrapper function that conforms 9 evaluatory 
#' rasters to the l4_ref_region geometry and outputs these 9 rasters along with a 
#' pixel location 'dummy' raster.
#' @param l4_ref_region an sf vector object that denotes the geometry of the extended
#' l4 ecoregion.
#' @param carbon A character string denoting the path to the carbon stack used for analysis
#' @param rdnbr A character string denoting the path to the rdnbr stack used for analysis
#' @param dummy_raster A terra rast obj that has the same projection, resolution, and extent
#' of the other evaluation rasters but only stores pixel locations.
#' @param stack Boolean. If true, expect rdnbr to come in pre-stacked (cbi solution)
#' @returns a raster stack for evaluating project and reference region changes.
#' @export 
conform_evaluation_rasters <- function(l4_ref_region,carbon,rdnbr, dummy_raster, stack = FALSE){

  carbon_stack <- conform_raster(l4_ref_region, raster_path = carbon, carbon_path = carbon[1])

  names <- paste0('values_', seq(1,terra::nlyr(carbon_stack),1))
  carbo_stack <-
    carbon_stack |>
    terra::values() |>
    dplyr::as_tibble() 
  
  carbo_stack[which(rowSums(is.na(carbo_stack)) != 0), ] <- rep(NA, terra::nlyr(carbon_stack))
  
carb_stack <-
    carbon_stack |>
    terra::setValues(values = carbo_stack)
  
  if(stack){
  rdnbr_sub <- conform_raster(l4_ref_region, rdnbr, carbon_path = carbon[1])
  names(rdnbr_sub) <- c('rdnbr_v1.0_2015_extended','rdnbr_v1.0_2016_extended','rdnbr_v1.0_2017_extended','rdnbr_v1.0_2018_extended',
                           'rdnbr_v1.0_2019_extended','rdnbr_v1.0_2020_extended','rdnbr_v1.0_2021_extended','rdnbr_v1.0_2022_extended')
  r2 <- terra::crop(rdnbr_sub, carb_stack)
  
  evaluation_list <- c(dummy_raster,carb_stack, r2)
  
  }else{
  rdnbr_sub <- conform_raster(l4_ref_region, rdnbr[1],carbon[1])
  rdnbr_sub2 <- conform_raster(l4_ref_region, rdnbr[2],carbon[1])
  rdnbr_sub3 <- conform_raster(l4_ref_region, rdnbr[3],carbon[1])
  rdnbr_sub4 <- conform_raster(l4_ref_region, rdnbr[4],carbon[1])
  rdnbr_sub5 <- conform_raster(l4_ref_region, rdnbr[5],carbon[1])
  rdnbr_sub6 <- conform_raster(l4_ref_region, rdnbr[6],carbon[1])
  
  evaluation_list <- c(dummy_raster,carb_stack, rdnbr_sub,rdnbr_sub2,rdnbr_sub3,rdnbr_sub4,rdnbr_sub5,rdnbr_sub6)
}
  evaluation_list
  
  }

clean_ecoregion_stack <- function(ecoregion_stack, evaluation_stack){
 eco_vals <- ecoregion_stack |> terra::values() |> data.frame()
 eval_vals <- evaluation_stack |> terra::values() |> data.frame()
 eco_vals[is.na(eval_vals[,2]),]$Carbon <- NA
 terra::setValues(ecoregion_stack, eco_vals)    
 
}
#' A wrapper function for the terra function 'crds' to pull lat long coordinates for 
#' all pixels in the ref region / aoi
#' `get_pixel_coordinates` isolates all valid pixels within a raster object and returns a 
#' tibble of lat and lon coordinates for all pixel locations
#' @param pixels a numeric vector representing all the pixels in a given l4 ecoregion,
#' labelled 1:n, where n is the single dimensional product of x * y of the raster matrix.
#' @param raster A terra rast obj that has the same projection, resolution, and extent
#' of the raster from which pixels were derived.
#' @returns a tibble of pixel locations and lat-lon coordinates
#' @export 
get_pixel_coordinates <- function(pixels, raster){
  pixels <- pixels[!is.na(pixels)]
  all_locations <- seq(1,(dim(raster)[1] * dim(raster)[2]), by = 1)  
  all_locations[which(!all_locations %in% pixels)] <- NA
  all_locations[!is.na(all_locations)] |> length()

  new_raster <-
    terra::setValues(raster$Location, all_locations)
  
  coordinates <- 
    new_raster |>
    terra::crds(na.rm = T) |>
    dplyr::bind_cols(pixels) 
  
  colnames(coordinates) <- c('lat', 'lon', 'Location')
  coordinates
}




###NEEDS DOCUMENTATION
#takes in an ecoregion stack and pixels for reference region and deduces all possible reference pixels based on treatment criteria
treatmentFilters <- function(ecoregion_stack, ref_pixels){
  
  carbon = ecoregion_stack$Carbon |> terra::values()
  slope = ecoregion_stack$Slope |> terra::values()
  roads = ecoregion_stack$Road_Distance |> terra::values()
  locations = ecoregion_stack$Location|> terra::values()
  EVC = ecoregion_stack$EVC |> terra::values() |> round(digits = 0)
  WHP = ecoregion_stack$WHP |> terra::values() |> round(digits = 0)
  EVT = ecoregion_stack$EVT |> terra::values() |> round(digits = 0)
  
  poss_pixels <- locations[which((roads > 1000 & roads <= 2000 & slope <= 35 & carbon > 0 & EVC >= 102 &
                                    EVC <= 109 & WHP >= 4 & WHP <= 5 & !is.na(EVT)) |
                                   (roads < 1000 & slope <= 50 & carbon > 0 & EVC >= 102 &
                                      EVC <= 109 & WHP >= 4 & WHP <= 5 & !is.na(EVT)))]
  
  poss_pixels[which(poss_pixels %in% ref_pixels)]
}

#sampling function - take in a sequence of numbers and produce a random sample
# simple, but saves a couple lines in run file when calling it iteratively
randomN <- function(change){
  change |> 
    dplyr::as_tibble() |>
    dplyr::sample_n(1)
}


getTolerance <- function(bins, carbon_values, aoi_values){
tolerance <- vector()
for(i in 1:(length(bins)-1)){
  
  carbon_sub <- 
    carbon_values |>
    dplyr::as_tibble() |>
    magrittr::set_colnames('carbon') |>
    dplyr::filter(
      dplyr::between(carbon, left = bins[i], right = bins[i+1])
    )
  
  aoi_sub <- 
    aoi_values |>
    dplyr::as_tibble() |>
    magrittr::set_colnames('carbon') |>
    dplyr::filter(
      dplyr::between(carbon, left = bins[i], right = bins[i+1])
    )
  
  if(nrow(aoi_sub) < 1){
    aoi_sub[1,1] <- 0.001 
  }
  
  tolerance[i] <- floor(nrow(carbon_sub) / nrow(aoi_sub))
  
}
min(tolerance)
}


#generate random sample in reference region
randomTreatment <- function(region, treatment){

  
  coin <-
    c(
      seq(-0.006,-0.0001, by = .0001),
      seq(0.0001, .006, by = .0001)      
      # seq(-1.3,-0.7, by = .1),
      # seq(0.7, 1.3, by = .1)
    )|>
    dplyr::tibble() |>
    dplyr::sample_n(size = 2) |>
    unlist() |>
    as.vector()
  
  n <-  
    region |>
    sf::st_sample(size = 1) |>
    sf::st_coordinates() |>
    as.numeric() + coin

  geo <-
    treatment |>
    sf::st_coordinates() |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      X2 = dplyr::if_else(
        X - dplyr::first(X) == 0,
        n[1],
        n[1] + ( (X - dplyr::first(X)))
      ),
      Y2 = dplyr::if_else(
        Y - dplyr::first(Y) == 0,
        n[2],
        n[2] + ( (Y - dplyr::first(Y)))
      )
    )|>
    dplyr::select(-c(X,Y)) |>
    dplyr::rename(X = X2,
                  Y = Y2) |>
    sf::st_as_sf(coords = c("X", "Y"), crs = sf::st_crs(region))

  if(length(geo$L3) > 0){
    geo2 <- 
      geo |>
      dplyr::group_by(L1,L2, L3) |>
      dplyr::summarise(do_union = F, .groups = 'drop') |>
      sf::st_cast("LINESTRING") |>
      sf::st_cast('POLYGON') |>
      sf::st_make_valid() |>
      sf::st_union()
    
  }else{
    geo2 <- 
      geo |>
      dplyr::group_by(L1,L2) |>
      dplyr::summarise(do_union = F, .groups = 'drop') |>
      sf::st_cast("LINESTRING") |>
      sf::st_cast('POLYGON') |>
      sf::st_make_valid() |>
      sf::st_union()
    }
  
  geo2
  
}


fakeTreatments <- function(rrs, good_rrs, l4_treats, samples, l4_region){

  h = 1
  k = 0
  fake_treats <- NULL
  #j = 1
 # failure_list <- vector()
 # for(h in 1:samples){
 while(h <= samples){
   if(k == 1001){
     print(paste0('complete failure for ', h, ', start over :('))
     #failure_list[j] <- l4_treats[rand_sample,]$distid
   #  j = j + 1
     h = h + 1
     k = 0
     next
   }
  #  if(h < 0){ h = abs(h)}
   # rand_sample <- floor(runif(1, min=1, max=nrow(l4_treats))) #pick a random treatment 
  rand_sample <- h #no longer random, because we want to use exact treatments from real projects   
    
  new_treat <-  #move it around
      randomTreatment(
        treatment = l4_treats[rand_sample,],
        region = l4_region)
    
    real <- 
      l4_treats[rand_sample,] |>
      get_aoi_vars(ecoregion_stack = good_rrs) |>
      dplyr::filter(WHP >= 4, EVC > 102, EVC <110, EVT == 1, !is.na(Carbon))
    
    fake <- 
      new_treat |>
      terra::vect() |>
      get_aoi_vars(ecoregion_stack = rrs)  
    
    fakey <- match_pixels_placebo(placebo_values = fake, aoi_values = real)

    if(
      is.null(dim(fakey))
    ){
      #print('null failure')
      h = h # if it doesnt, repeat with new randomness
      k = k +1
      next
    }else if(nrow(fakey) < 10){ #could be nrow of real
      h = h # if it doesnt, repeat with new randomness
      k = k +1
      #print('too few obs')
      next
    }else if(
      new_treat |> #see if it works
      terra::vect() |>
        aoi_tester(
          ecoregion_stack = rrs
        )
      == FALSE
      ){
      h = h # if it doesnt, repeat with new randomness
      k = k +1
      #print('failure')
      next
    }else if(
       
      ks.test(real$Carbon, fakey$Carbon)$p.value < 0.05){
     # print(paste0('failed carbon matching for ',h ))
      h = h  # if it doesnt, repeat with new randomness
      k = k + 0.001
      next
 # }else if(
 #   
 #   ks.test(real$EVC, fakey$EVC)$p.value < 0.05){
 #   print(paste0('failed evc matching for ',h ))
 #   h = h # if it doesnt, repeat with new randomness
 #   k = k + 0.001
 #   next
 }else{
      
      new_treat <-
        new_treat |>
        sf::st_as_sf() |>
        dplyr::mutate(distid =  l4_treats[rand_sample,]$distid)
      
      if(h == 1){
        fake_treats <- new_treat
      }else if(is.null(fake_treats)){
        fake_treats <- new_treat
      }else{
      fake_treats <- dplyr::bind_rows(fake_treats, new_treat)
      }

      print('yay!!!!!!!!!!!!!!!!!!!!!!!!!! fake treatment added!!')
      h = h+1 # 
      k = 0
      if (h == samples){
        print('hit sample requirement') #keep going till you get your samples
        break
      }
      next
 }
    next
 }

  fake_treats$area_m <- sf::st_area(fake_treats)
  fake_treats$prescribed_burn = FALSE
  fake_treats$Year = 2016
  fake_treats |>
    sf::st_as_sf()
}



aoi_tester <- function(aoi, ecoregion_stack){
  
aoi_vals <- 
  aoi |>
  get_aoi_vars(ecoregion_stack = ecoregion_stack) |>
  dplyr::filter(Carbon > 10, EVT == 1)

#determine whether the project area meets the conditions for approval   
if(nrow(aoi_vals) > 10){
TRUE
}else{
  FALSE
}
}



##now defunct --- for future reference!
# (1) pull in gpkg of roads
# (2) filter for only roads that are tnmfrc 1-4 (i.e., not 4wd roads)
# (3) rasterize in R; set to epsg:5070 (or any projection that uses meters rather than degrees)
# (4) pixel values for roads should equal 1; open in QGIS
# (5) run proximity (raster distance) in QGIS.
# (5a) target is 1 (value of roads); pixel height and width can be set to 5

readRoads <- function(roads_path, region){
  assertthat::assert_that(class(roads_path) == 'character')
 roads <- 
  roads_path |>
    sf::read_sf() 
 
 roads[which(is.na(roads$tnmfrc)),]$tnmfrc <- roads[which(is.na(roads$tnmfrc)),]$tnmfrc_2
 
 roads |>
   # cropRoads(region = region) |>
    dplyr::filter(tnmfrc < 5) #|>
#    dplyr::mutate(obj = 1) |>
 #   dplyr::summarise(obj)
  
}

cropRoads <- function(region, roads){
  if(sf::st_crs(region) != sf::st_crs(roads)){
  st_region <- 
    region |>
    sf::st_transform(
      sf::st_crs(roads)
    ) |>
    sf::st_buffer(dist = 200) 
  
  #crop roads to region and transform back
  
  roads |>
    sf::st_crop(st_region,
                crop = FALSE) |>
    sf::st_transform(
      sf::st_crs(region)
    )
  }else{
    roads |>
      sf::st_crop(region,
                  crop = FALSE)
  }
}


response_fxn <- function(df, filter, eco = TRUE){
  #filter is now a vector of two values
  options(digits = 2)
  
  totals <- 
    df |>
    dplyr::group_by(area,dataset) |> 
    dplyr::reframe(total_pixels = dplyr::n())
  
  if(filter[1] == 100){
    df <-
      df |>
      dplyr::filter(composite_2023_median <= 5 & max_rdnbr > 2.24 & max_rdnbr_year <= 2022)
    # }else if(filter[2] - filter[1] > 2.9){
    #   
  }else{
    df <-
      df |>
      dplyr::mutate(
        max_rdnbr = dplyr::if_else(is.na(max_rdnbr), 0, max_rdnbr)
      ) |>
      dplyr::filter(max_rdnbr >= filter[1] & max_rdnbr < filter[2]) |>
      dplyr::mutate(
        max_rdnbr = dplyr::if_else(max_rdnbr == 0, NA, max_rdnbr)
      )
  }
  
  df|>
    dplyr::group_by(area,dataset) |>
    dplyr::reframe(
      mean_pre = mean(composite_2015_median, na.rm = T),
      mean_2016 = mean(composite_2016_median, na.rm = T),
      mean_2017 = mean(composite_2017_median, na.rm = T),
      mean_2018 = mean(composite_2018_median, na.rm = T),
      mean_2019 = mean(composite_2019_median, na.rm = T),
      mean_2020 = mean(composite_2020_median, na.rm = T),
      mean_2021 = mean(composite_2021_median, na.rm = T),
      mean_2022 = mean(composite_2022_median, na.rm = T),
      mean_2023 = mean(composite_2023_median, na.rm = T),
      mean_cbi = mean(max_rdnbr, na.rm = T),
      mean_fire_change = mean(fire_change, na.rm = T),
      sd_pre = sd(composite_2015_median, na.rm = T),
      sd_2016 = sd(composite_2016_median, na.rm = T),
      sd_2017 = sd(composite_2017_median, na.rm = T),
      sd_2018 = sd(composite_2018_median, na.rm = T),
      sd_2019 = sd(composite_2019_median, na.rm = T),
      sd_2020 = sd(composite_2020_median, na.rm = T),
      sd_2021 = sd(composite_2021_median, na.rm = T),
      sd_2022 = sd(composite_2022_median, na.rm = T),
      sd_2023 = sd(composite_2023_median, na.rm = T),
      q25_2015 = quantile(composite_2015_median,0.25, na.rm = T),
      q25_2016 = quantile(composite_2016_median,0.25, na.rm = T),
      q25_2017 = quantile(composite_2017_median,0.25, na.rm = T),
      q25_2018 = quantile(composite_2018_median,0.25, na.rm = T),
      q25_2019 = quantile(composite_2019_median,0.25, na.rm = T),
      q25_2020 = quantile(composite_2020_median,0.25, na.rm = T),
      q25_2021 = quantile(composite_2021_median,0.25, na.rm = T),
      q25_2022 = quantile(composite_2022_median,0.25, na.rm = T),
      q25_2023 = quantile(composite_2023_median,0.25, na.rm = T),
      q75_2015 = quantile(composite_2015_median,0.75, na.rm = T),
      q75_2016 = quantile(composite_2016_median,0.75, na.rm = T),
      q75_2017 = quantile(composite_2017_median,0.75, na.rm = T),
      q75_2018 = quantile(composite_2018_median,0.75, na.rm = T),
      q75_2019 = quantile(composite_2019_median,0.75, na.rm = T),
      q75_2020 = quantile(composite_2020_median,0.75, na.rm = T),
      q75_2021 = quantile(composite_2021_median,0.75, na.rm = T),
      q75_2022 = quantile(composite_2022_median,0.75, na.rm = T),
      q75_2023 = quantile(composite_2023_median,0.75, na.rm = T),
      sd_cbi = sd(max_rdnbr, na.rm = T),
      n = dplyr::n()
    ) |> 
    dplyr::right_join(totals, by = c('area','dataset')) |>
    dplyr::mutate(percent = n/total_pixels)
}

response_fxn_old <- function(df, filter, eco = TRUE){
  #filter is now a vector of two values
options(digits = 2)
 
  totals <- 
    df |>
    dplyr::group_by(area) |> 
    dplyr::reframe(total_pixels = dplyr::n())
  
  if(filter[1] == 100){
    df <-
    df |>
      dplyr::filter(post_carbon == 0 & max_rdnbr > 2.24 & max_rdnbr_year <= 2022)
  # }else if(filter[2] - filter[1] > 2.9){
  #   
  }else{
    df <-
      df |>
      dplyr::mutate(
        max_rdnbr = dplyr::if_else(is.na(max_rdnbr), 0, max_rdnbr)
      ) |>
      dplyr::filter(max_rdnbr >= filter[1] & max_rdnbr < filter[2]) |>
      dplyr::mutate(
        max_rdnbr = dplyr::if_else(max_rdnbr == 0, NA, max_rdnbr)
      )
  }
    if(eco == TRUE){
      df <-
      df |>
        dplyr::group_by(ecoregion) 
    }
    df|>
        dplyr::group_by(area) |>
         dplyr::reframe(
          mean_pre = mean(pre_carbon, na.rm = T), 
          mean_post = mean(post_carbon, na.rm = T),
          mean_mid = mean(mid_carbon, na.rm = T),
          median_pre = median(pre_carbon, na.rm = T), 
          median_post = median(post_carbon, na.rm = T),
          median_mid = median(mid_carbon, na.rm = T),
          median_rdnbr = median(max_rdnbr, na.rm = T),
          mean_rdnbr = mean(max_rdnbr, na.rm = T),
          iqr_rdnbr = IQR(max_rdnbr, na.rm = T),
          sd_rdnbr = sd(max_rdnbr, na.rm = T),
          mean_delta14 = mean(change_1to4, na.rm = T),
          mean_delta45 = mean(change_4to5, na.rm = T),
          mean_delta15 = mean(change_1to5, na.rm = T),
          median_delta14 = median(change_1to4, na.rm = T),
          median_delta45 = median(change_4to5, na.rm = T),
          median_delta15 = median(change_1to5, na.rm = T),
          iqr_delta14 = IQR(change_1to4, na.rm = T),
          iqr_delta45 = IQR(change_4to5, na.rm = T),
          iqr_delta15 = IQR(change_1to5, na.rm = T),
          min_delta14 = min(change_1to4, na.rm = T),
          min_delta45 = min(change_4to5, na.rm = T),
          min_delta15 = min(change_1to5, na.rm = T),
          q95_delta14 = quantile(change_1to4, 0.95, na.rm = T),
          q95_delta45 = quantile(change_4to5, 0.95, na.rm = T),
          q95_delta15 = quantile(change_1to5, 0.95, na.rm = T),
          q05delta14 = quantile(change_1to4, 0.05, na.rm = T),
          q05delta45 = quantile(change_4to5, 0.05, na.rm = T),
          q05delta15 = quantile(change_1to5, 0.05, na.rm = T),
          max_delta14 = max(change_1to4, na.rm = T),
          max_delta45 = max(change_4to5, na.rm = T),
          max_delta15 = max(change_1to5, na.rm = T),
          sd_delta14 = sd(change_1to4, na.rm = T),
          sd_delta45 = sd(change_4to5, na.rm = T),
          sd_delta15 = sd(change_1to5, na.rm = T),
          mean_dif = (mean_post- mean_pre) / mean_pre,
          median_dif = (median_post - median_pre)/ median_pre,
          n = dplyr::n()
      ) |> 
        dplyr::right_join(totals, by = c('area')) |>
        dplyr::mutate(percent = n/total_pixels)
  }




prepRR <- function(aoi, projection, l4eco){
  
  #get a common projection; base off carbon data
  
  #input geometry of larger aoi -- in this case, the TCSI region
  tcsi <- 
    aoi |>
    sf::read_sf() |> 
    sf::st_transform(projection)
  
  #Pull ecoregions that make up more than 5% of the TCSI region
  tcsi_ref_region <- get_l4_rr(tcsi, l4eco, projection)   
}





#wrapper function for rr development and analysis
runAnalyses <- function(ref_region,
                        treatments,
                        carbon,
                        whp,
                        evc,
                        evt,
                        dem,
                        roads,
                        rdnbr,
                        output_loc,
                        runID,
                        ids,
                        seed,
                        fake_treatments = FALSE,
                        samples = NA){
  
  
  if(is.na(seed)){set.seed(1)}else{set.seed(seed)} 
  
  if(fake_treatments == T & is.na(sum(samples))){samples <- rep(25,length(ref_region$US_L4CODE))}
  
  l = 1
  #l2 <- list_out
  list_out <- list()
  z = 0
  
  for(i in 1:length(ref_region$US_L4CODE)){
    
    #  i =  
    l4_region <-  
      ref_region[i,]
    
    #Build stacks -- time intensive, should only have to do it once per ecoregion
    #build stack for selecting RRs
    ecoregion_stack <- get_ecoregion_stack(l4_region, carbon, evc, evt = evt, whp, dem = dem, roads)
    
    evaluation_stack <- conform_evaluation_rasters(l4_region, carbon, 
                                                   rdnbr = rdnbr, 
                                                   dummy_raster = ecoregion_stack$Location,
                                                   stack = TRUE)
    
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
        dplyr::filter(year == 2016) |>
        dplyr::filter(
          disttype == 'Commercial Thinning'
        ) |>
        sf::st_intersection(l4_region) 
      
    }else{
      
      all_treatments <- 
        treatments |>
        fakeTreatments(l4_region = l4_region, rrs = new_rr, good_rrs = ecoregion_stack, samples = samples[i]) |>
        sf::st_crop(y = l4_region)
      
      all_treatments$distid <- seq(1, length(all_treatments$area_m), by = 1)
      all_treatments |> sf::write_sf(paste0('/Users/eyackulic/Desktop/fake_treatments_',runID,'_',i,'.gpkg'))
    }  
    
    #add begin individually evaluating treatments here
    for(j in 1:nrow(all_treatments)){
      
      aoi <- all_treatments[j,]
      #aoi |> aoi_tester(new_rr)
      
      #pull values for aoi
      aoi_vals <- 
        aoi |>
        get_aoi_vars(ecoregion_stack = ecoregion_stack) |>
        dplyr::filter(Carbon > 10, EVT == 1) |>
        dplyr::mutate(area = 'Treatment')
      
      if(nrow(aoi_vals) < 10){
        next
      }
      #pull ref pixels
      pixels <- 
        ref_pixels |>
        get_pixel_values(raster = new_rr$EVC) |>
        dplyr::filter(
          raster_value >= min(aoi_vals$EVC, na.rm = T) & 
            raster_value <= max(aoi_vals$EVC, na.rm = T)) |>
        dplyr::select(location) |>
        unlist() |> 
        as.numeric() |>
        get_pixel_values(raster = new_rr$WHP) |>
        dplyr::mutate(raster_value = round(raster_value, digits = 0)) |>
        dplyr::filter(raster_value >= 4 & raster_value <=5) |>
        dplyr::select(location) |>
        unlist() |> 
        as.numeric()
      
      carbon_values <- 
        ecoregion_stack$Carbon |>
        terra::values() |>
        dplyr::as_tibble() |>
        dplyr::slice(pixels) |>
        cbind(pixels)
      
      carbon_values <-  
        ecoregion_stack$EVT |>
        terra::values() |>
        dplyr::as_tibble() |>
        dplyr::slice(pixels) |>
        dplyr::bind_cols(carbon_values)
      
      colnames(carbon_values) <- c('evt', 'carbon' ,'pixels')
      
      
      if(nrow(carbon_values) < 2){
        next
      }
      #testing next line here
      rr_pixels <- match_pixels(carbon_values = carbon_values, aoi_values = aoi_vals) |> 
        dplyr::mutate(area = 'RefRegion')
      #why do some projects fail over and over again?
      if(ks.test(aoi_vals$Carbon, rr_pixels$carbon)$p.value < 0.05){print(paste0('Violation! ', aoi$distid, ' project does not match RR'))}
      
      if(nrow(rr_pixels) < 2){
        next
      }
      tolerance <- rr_pixels$tolerance |> unique()
      
      rr_evaluators <- pull_evaluation_variables(evaluation_stack, rr_pixels$pixels)
      aoi_evaluators <- pull_evaluation_variables(evaluation_stack, aoi_vals$Location)
      
      out <- full_stack_differences(project_tibble = aoi_evaluators, rr_tibble = rr_evaluators)
      out$distid <- aoi$distid
      out$ecoregion <- l4_region$US_L4CODE
      final_out <- get_pixel_coordinates(unique(out$Location), evaluation_stack$Location) |>
        dplyr::right_join(out, by = 'Location')
      
      print(glue::glue(unique(out$distid),' finished successfully!'))
      final_out$tolerance <- tolerance 
      
      list_out[[l]] <- final_out
      l = l + 1
    }
  }
  
  df2 <- 
    purrr::map_df(list_out, ~as.data.frame((.)))
  
  df2 <-
    df2 |>
    dplyr::mutate(
      mid_carbon = dplyr::if_else(mid_carbon < 1, 1, mid_carbon),
      change_1to4 = (mid_carbon - pre_carbon) / pre_carbon,
      change_4to5 = (post_carbon - mid_carbon) / mid_carbon,
      change_1to5 = (post_carbon - pre_carbon) / pre_carbon,
      distid_eco = paste0(distid, '_', ecoregion)
    ) |>
    dplyr::filter(change_4to5 < 1) |>
    dplyr::filter(change_1to4 < 1) |>
    dplyr::filter(change_1to5 < 1)
  
  if(fake_treatments == T){
    out_var <- 'fake'
  }else{out_var <- 'real'}
  
  
  saveRDS(df1, 
          paste0(
            output_loc,
            out_var,
            '_treatments_',
            substring(Sys.time(),first = 1, last = 10),
            '_',
            runID,
            '_',
            seed,
            '.rds'
          )
  )
  df1
}







































#wrapper function for rr development and analysis
generateMatch <- function(ref_region,
                        treatments,
                        carbon,
                        whp,
                        evc,
                        evt,
                        dem,
                        roads,
                        rdnbr,
                        output_loc,
                        runID,
                        seed,
                        fake_treatments = FALSE,
                        samples = NA){
  
  if(is.na(seed)){set.seed(1)}else{set.seed(seed)} 
  
  if(fake_treatments == T & is.na(sum(samples))){samples <- rep(25,length(ref_region$US_L4CODE))}
  
  l = 1
  list_out <- list()
  z = 0
  
  for(i in 1:length(ref_region$US_L4CODE)){
    
    l4_region <-  
      ref_region[i,]
    
    #Build stacks -- time intensive, should only have to do it once per ecoregion
    #build stack for selecting RRs
    ecoregion_stack <- get_ecoregion_stack(l4_region, carbon, evc, whp, dem = dem, roads, evt = evt)

    evaluation_stack <- conform_evaluation_rasters(l4_region, carbon, 
                                                   rdnbr = rdnbr, evt = evt,
                                                   dummy_raster = ecoregion_stack$Location,
                                                   stack = TRUE)
    
    #trim facts to single ref region and buffer every treatment since 2015 -- this will
    #remove consideration from being in the RR
    all_treatments_buffered <- 
      treatments |>
      sf::st_intersection(l4_region) |>
      sf::st_make_valid() |>
      sf::st_transform(sf::st_crs('EPSG:5070')) |> #need a crs that uses meters as unit
      sf::st_buffer(dist = 50) |># check this number!  
      sf::st_transform(sf::st_crs(l4_region))     
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
        dplyr::filter(year == 2016) |>
        dplyr::filter(
          disttype == 'Commercial Thinning'
        ) |>
        sf::st_intersection(l4_region) 
      
    }else{
      
      all_treatments <- 
        treatments |>
        fakeTreatments(l4_region = l4_region, rrs = new_rr, samples = samples[i]) |>
        sf::st_crop(y = l4_region)
      
      all_treatments$distid <- seq(1, length(all_treatments$area_m), by = 1)
      all_treatments |> sf::write_sf(paste0('/Users/eyackulic/Desktop/fake_treatments_',runID,'_',i,'.gpkg'))
    }  
    
    #add begin individually evaluating treatments here
    for(j in 1:nrow(all_treatments)){
      
      aoi <- all_treatments[j,]
      #aoi |> aoi_tester(new_rr)
      
      #pull values for aoi
      aoi_vals <- 
        aoi |>
        get_aoi_vars(ecoregion_stack = ecoregion_stack) |>
        dplyr::filter(Carbon > 10) |>
      dplyr::mutate(area = 'Treatment')
      
      #pull ref pixels
      pixels <- 
        ref_pixels |>
        get_pixel_values(raster = new_rr$EVC) |>
        dplyr::filter(
          raster_value >= min(aoi_vals$EVC, na.rm = T) & 
            raster_value <= max(aoi_vals$EVC, na.rm = T)) |>
        dplyr::select(location) |>
        unlist() |> 
        as.numeric() |>
        get_pixel_values(raster = new_rr$WHP) |>
        dplyr::mutate(raster_value = round(raster_value, digits = 0)) |>
        dplyr::filter(raster_value >= 4 & raster_value <=5) |>
        dplyr::select(location) |>
        unlist() |> 
        as.numeric()
      
      carbon_values <- 
        ecoregion_stack$Carbon |>
        terra::values() |>
        dplyr::as_tibble() |>
        dplyr::slice(pixels) |>
        cbind(pixels)
      
      carbon_values <-  
        ecoregion_stack$EVT |>
        terra::values() |>
        dplyr::as_tibble() |>
        dplyr::slice(pixels) |>
        dplyr::bind_cols(carbon_values)
      
      colnames(carbon_values) <- c('evt', 'carbon' ,'pixels')
      if(nrow(carbon_values) < 2){
        next
      }
     # rr_pixels <- match_pixels(carbon_values = carbon_values, aoi_values = aoi_vals$Carbon) |> 
      rr_pixels <- match_pixels(carbon_values = carbon_values, aoi_values = aoi_val) |> 
        dplyr::mutate(area = 'RefRegion')
      if(nrow(rr_pixels) < 2){
        next
      }
      
      aoi_vals$tolerance <-  rr_pixels$tolerance |> unique()
      out <- dplyr::bind_rows(aoi_vals, rr_pixels)
      
      out$distid <- aoi$distid
      out$ecoregion <- l4_region$US_L4CODE
      #is next step necessary?
      final_out <- get_pixel_coordinates(unique(out$location), evaluation_stack$Location) |>
        dplyr::right_join(out, by = 'location')
      
      print(glue::glue(unique(out$distid),' finished successfully!'))
      
      
      list_out[[l]] <- final_out
      l = l + 1
    }
  }
  
  df1 <- 
    purrr::map_df(list_out, ~as.data.frame((.)))
  
  df1 <-
    df1 |>
    dplyr::mutate(
      mid_carbon = dplyr::if_else(mid_carbon < 1, 1, mid_carbon),
      change_1to4 = (mid_carbon - pre_carbon) / pre_carbon,
      change_4to5 = (post_carbon - mid_carbon) / mid_carbon,
      change_1to5 = (post_carbon - pre_carbon) / pre_carbon,
      distid_eco = paste0(distid, '_', ecoregion)
    ) |>
    dplyr::filter(change_4to5 < 1) |>
    dplyr::filter(change_1to4 < 1) |>
    dplyr::filter(change_1to5 < 1)
  
  if(fake_treatments == T){
    out_var <- 'fake'
  }else{out_var <- 'real'}
  
  
  saveRDS(df1, 
          paste0(
            output_loc,
            out_var,
            '_treatments_',
            substring(Sys.time(),first = 1, last = 10),
            '_',
            runID,
            '_',
            seed,
            '.rds'
          )
  )
  df1
}













match_pixels_placebo <- function(placebo_values, aoi_values){
  
  #create bins of 10 carbon units between aoi min and max values
  bins <- seq(plyr::round_any(min(aoi_values$Carbon, na.rm = T), 10, f = floor),
              plyr::round_any(max(aoi_values$Carbon, na.rm = T), 10, f = ceiling), by = 5)
  
  #remove any rr pixels from consideration that exceed the aoi minmax boundaries
  placebo_values <- 
    placebo_values |>
    dplyr::mutate(Carbon = dplyr::if_else(
      Carbon < min(aoi_values$Carbon, na.rm = T) |
        Carbon > max(aoi_values$Carbon, na.rm = T),
      NA,
      Carbon
    )
    ) |>
    dplyr::filter(!is.na(Carbon), !is.na(EVC))
  
  #setting tolerance
  # tolerance <- getTolerance(bins = bins, carbon_values = carbon_values$carbon, aoi_values = aoi_values$Carbon)
  aoi_values <- 
    aoi_values |>
    dplyr::mutate(
      Carbon = ifelse(Carbon == min(bins), Carbon +.01, Carbon),
      Carbon = ifelse(Carbon == max(bins), Carbon - .01, Carbon)
    ) |>
    dplyr::filter(
      !is.na(Carbon),
      !is.na(EVC)
    )
  #WIP here
  ##Changepoint :: switch to evc
  aoi_breaks <- paste0(aoi_values$EVC, '_',cut(aoi_values$Carbon, breaks = bins, right = F)) |> table()
  rr_breaks <- paste0(placebo_values$EVC, '_',cut(placebo_values$Carbon, breaks = bins, right = F)) |> table()
  
  rr_breaks <- rr_breaks[which(names(rr_breaks) %in% names(aoi_breaks))]
  aoi_breaks <- aoi_breaks[which(names(aoi_breaks) %in% names(rr_breaks))]
  
  percent_rr <- rr_breaks/aoi_breaks
  if(min(percent_rr) < 1){
    aoi_breaks <- aoi_breaks[which(percent_rr > 1)]
    rr_breaks <- rr_breaks[which(names(rr_breaks) %in% names(aoi_breaks))]
    aoi_breaks <- aoi_breaks[which(names(aoi_breaks) %in% names(rr_breaks))]
    percent_rr <- rr_breaks/aoi_breaks  
  }
  #if percent rr is < 1, remove band from rr and aoi tracking
  tolerance <- 1#round((floor(min(percent_rr)) * 0.5), digits = 0)
  
  samples <- aoi_breaks * tolerance#tolerance
  if(length(samples) == 0){
    out = NA
    }else{
  #tolerance <- round((floor(min(rr_breaks/aoi_breaks)) * 0.5), digits = 0)
  #if(tolerance == 0){tolerance <- 1; print('tolerance violated')}
  
  for(k in 1:length(names(aoi_breaks))){
    samps <- samples[k]
    n_name <- names(samps)
    evt_code <- stringr::str_sub(n_name, start = 0,end = 3)
    carb_start <- stringr::str_sub(n_name, start = 6, end = regexpr(pattern =',',text = n_name)[[1]]- 1) |> as.numeric()
    carb_end <- stringr::str_sub(n_name, start = regexpr(pattern =',',text = n_name)[[1]]+ 1, end = stringr::str_length(n_name)- 1) |> as.numeric()
    
    carb_sub <-
      placebo_values |>
      dplyr::filter(EVC %in% evt_code &
                      Carbon >= carb_start &
                      Carbon <= carb_end) |>
      data.frame()
 
     #  if(dim(carb_sub)[1] == 0){out = dplyr::bind_rows(out,rep(NA,7))}else{
    sample_pixels <- dplyr::sample_n(tbl = carb_sub, size = samps, replace = F)
    
    keepers <-
      placebo_values |>
      dplyr::filter(Location %in% sample_pixels$Location) |>
      dplyr::mutate(tolerance = tolerance)
    
    #and store them
    if(k == 1){
      out <- keepers
    }else{
      out <- rbind(out,keepers)
    }
  }
    }
  out
}