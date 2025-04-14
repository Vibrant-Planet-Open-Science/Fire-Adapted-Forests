rm(list = ls())
gc()
file.remove(list.files(tempdir(), full.names = T))
require(terra);require(sf)
source("~/Desktop/ref_region.R")
options(scipen = 999)
library(tidyverse)


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

df <- 
  df |>
  dplyr::mutate(
    cbi = dplyr::if_else(max_rdnbr < 0.1, NA, max_rdnbr),
    distid_eco = paste0(distid, '_', ecoregion))

equalSamplingResults <- function(sample, variable, id, group){
  
  proj <- 
    sample |>
    dplyr::filter(area %in% 'project') |>
    dplyr::filter(subset %in% group) |>
    dplyr::select(all_of(variable))
  
  ref <- 
    sample |>
    dplyr::filter(area %in% 'ref_region') |>
    dplyr::filter(subset %in% group) |>
    dplyr::select(all_of(variable))
  
  
  p_value <- vector(); st_error <- vector(); ref_estimate <- vector(); proj_estimate <- vector(); distid_eco <- vector(); subset <- vector(); n <- vector()
  if(nrow(proj) < 5){
    output <- cbind(id, group, 0, mean(ref$total_delta,na.rm = T), 0, 0, 0) |> data.frame()
  }else{
    for(i in 1:10000){
      
      new_data_all <-
        ref |>
        dplyr::sample_n(size = nrow(proj),replace = T)
      
      sum_stats <- t.test(new_data_all, proj)
      subset[i] <- group
      distid_eco[i] <- id
      st_error[i] <- sum_stats$stderr
      p_value[i] <- sum_stats$p.value
      ref_estimate[i] <- sum_stats$estimate[1]
      proj_estimate[i] <- sum_stats$estimate[2]
    }
    
    output <- cbind(unique(distid_eco), unique(subset), mean(p_value),mean(ref_estimate),mean(proj_estimate), mean(st_error), nrow(proj)) |> data.frame()
  }
  colnames(output) <- c('distid_eco','subset','p_value', 'ref_mean', 'proj_mean', 'st_error', 'n')
  if(is.na(as.numeric(output$p_value))){output$p_value <- 1}
  output
  
}

length_out <- length(unique(df$distid_eco))

carbon_proj_cbi_comparison_all <- data.frame(matrix(ncol = 7, nrow = length_out))
carbon_proj2023_comparison_all <- data.frame(matrix(ncol = 7, nrow = length_out))
carbon_proj2015_comparison_all <- data.frame(matrix(ncol = 7, nrow = length_out))
carbon_proj2016_comparison_all <- carbon_proj2017_comparison_all <- carbon_proj2018_comparison_all <- carbon_proj2019_comparison_all <-
  carbon_proj2020_comparison_all <- carbon_proj2021_comparison_all <- carbon_proj2022_comparison_all <- carbon_proj2023_comparison_all

df3 <- 
  df |>
  dplyr::filter(dataset %in% 'real') |>
  dplyr::mutate(subset = 'all')


for(h in 1:length(unique(df3$distid_eco))){
  df2 <-     
    df3 |>
    dplyr::filter(distid_eco %in% unique(df3$distid_eco)[h])

carbon_proj2015_comparison_all[h,] <- 
    df2 |>
    equalSamplingResults(variable = 'composite_2015_median', id = unique(df3$distid_eco)[h], group = c('all'))

  carbon_proj2016_comparison_all[h,] <- 
    df2 |>
    equalSamplingResults(variable = 'composite_2016_median', id = unique(df3$distid_eco)[h], group = c('all'))
  
  carbon_proj2017_comparison_all[h,] <- 
    df2 |>
    equalSamplingResults(variable = 'composite_2017_median', id = unique(df3$distid_eco)[h], group = c('all'))
  
  carbon_proj2018_comparison_all[h,] <- 
    df2 |>
    equalSamplingResults(variable = 'composite_2018_median', id = unique(df3$distid_eco)[h], group = c('all'))
  
  carbon_proj2019_comparison_all[h,] <- 
    df2 |>
    equalSamplingResults(variable = 'composite_2019_median', id = unique(df3$distid_eco)[h], group = c('all'))
  
  carbon_proj2020_comparison_all[h,] <- 
    df2 |>
    equalSamplingResults(variable = 'composite_2020_median', id = unique(df3$distid_eco)[h], group = c('all'))
  
  carbon_proj2021_comparison_all[h,] <- 
    df2 |>
    equalSamplingResults(variable = 'composite_2021_median', id = unique(df3$distid_eco)[h], group = c('all'))
  
  carbon_proj2022_comparison_all[h,] <- 
    df2 |>
    equalSamplingResults(variable = 'composite_2022_median', id = unique(df3$distid_eco)[h], group = c('all'))
  
  carbon_proj2023_comparison_all[h,] <- 
    df2 |>
    equalSamplingResults(variable = 'composite_2023_median', id = unique(df3$distid_eco)[h], group = c('all'))
 
  print(glue::glue(round(h/length(unique(df3$distid_eco)),digits = 3) * 100, ' % finished'))
}

colnames(carbon_proj2023_comparison_all)[1:7] <- c('distid_eco','group','p_value', 'ref_mean', 'proj_mean', 'st_error', 'n')
colnames(carbon_proj2015_comparison_all)[1:7] <- c('distid_eco','group','p_value', 'ref_mean', 'proj_mean', 'st_error', 'n')
colnames(carbon_proj_cbi_comparison_all)[1:7] <- c('distid_eco','group','p_value', 'ref_mean', 'proj_mean', 'st_error', 'n')

  colnames(carbon_proj2016_comparison_all) <- colnames(carbon_proj2017_comparison_all) <- 
  colnames(carbon_proj2018_comparison_all) <- colnames(carbon_proj2019_comparison_all) <-
  colnames(carbon_proj2020_comparison_all) <- colnames(carbon_proj2021_comparison_all) <- 
  colnames(carbon_proj2022_comparison_all) <- colnames(carbon_proj2023_comparison_all)

cleanOutput <- function(dataset){
  dataset |>
    dplyr::filter(!is.na(proj_mean)) |>
    #  dplyr::filter(!duplicated(proj_mean)) |>
    dplyr::mutate(mean_delta = as.numeric(proj_mean) - as.numeric(ref_mean)) |>
    dplyr::arrange(-mean_delta) |>
    dplyr::mutate(
      st_error = as.numeric(st_error),
      p_value = as.numeric(p_value),
      id = seq(1:length(p_value))
    )
}
carbon_proj_cbi_comparison_all <- cleanOutput(carbon_proj_cbi_comparison_all)
carbon_proj2015_comparison_all <- cleanOutput(carbon_proj2015_comparison_all) ###
carbon_proj2016_comparison_all <- cleanOutput(carbon_proj2016_comparison_all) 
carbon_proj2017_comparison_all <- cleanOutput(carbon_proj2017_comparison_all) 
carbon_proj2018_comparison_all <- cleanOutput(carbon_proj2018_comparison_all)
carbon_proj2019_comparison_all <- cleanOutput(carbon_proj2019_comparison_all) 
carbon_proj2020_comparison_all <- cleanOutput(carbon_proj2020_comparison_all)
carbon_proj2021_comparison_all <- cleanOutput(carbon_proj2021_comparison_all)
carbon_proj2022_comparison_all <- cleanOutput(carbon_proj2022_comparison_all)
carbon_proj2023_comparison_all <- cleanOutput(carbon_proj2023_comparison_all)

carbon_proj_cbi_comparison_all$variable <- 'cbi'###
carbon_proj2015_comparison_all$variable <- 'carbon2015'###
carbon_proj2016_comparison_all$variable <- 'carbon2016'
carbon_proj2017_comparison_all$variable <- 'carbon2017'
carbon_proj2018_comparison_all$variable <- 'carbon2018'
carbon_proj2019_comparison_all$variable <- 'carbon2019'
carbon_proj2020_comparison_all$variable <- 'carbon2020'
carbon_proj2021_comparison_all$variable <- 'carbon2021'
carbon_proj2022_comparison_all$variable <- 'carbon2022'
carbon_proj2023_comparison_all$variable <- 'carbon2023'



new_data_all <- rbind(
#  carbon_proj_cbi_comparison_all,
  carbon_proj2015_comparison_all,###
  carbon_proj2016_comparison_all,
  carbon_proj2017_comparison_all,
  carbon_proj2018_comparison_all,
  carbon_proj2019_comparison_all,
  carbon_proj2020_comparison_all,
  carbon_proj2021_comparison_all,
  carbon_proj2022_comparison_all,
  carbon_proj2023_comparison_all
)#, rdnbr_proj)
write.csv(new_data_all, '/Users/eyackulic/Desktop/comparison_results_3-31-111_real.csv')

new_data_all <- read.csv('/Users/eyackulic/Desktop/comparison_results_3-31-111_real.csv')

new_data_all$distid <- substring(text = new_data_all$distid_eco,first = 0,last = unlist(gregexpr(text = new_data_all$distid_eco, '_')) - 1)


#for each year in figure 6, filter by the carbon[year] for summation and individual plots

new_data_all|>
  dplyr::filter(
    variable %in% 'carbon2023'
  ) |>
  dplyr::reframe(sum = sum(mean_delta, na.rm = T), n = dplyr::n())


new_data_all |>
  dplyr::filter(
    variable %in% 'carbon2023' & 
      !is.na(mean_delta)
  ) |>
  dplyr::mutate(
    rmv_code = 1,
    class = dplyr::if_else(mean_delta > 0, 1, -1),
    class_no = dplyr::case_when( class == 1 ~ mean_delta - st_error,
                                 class == -1 ~ mean_delta + st_error),
    rmv_code = dplyr::case_when(class == 1 & class_no > 0 ~ 0,
                                class == -1 & class_no < 0 ~ 0),
    rmv_code = factor(rmv_code)
  ) |>
  dplyr::distinct() |>
  dplyr::arrange(-mean_delta) |>
  dplyr::mutate(
    id = seq(1:length(p_value))) |>
  ggplot() +
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = .5) +
  geom_ribbon(aes(x = id * (100/(length(id))), ymax = 1* mean_delta, fill = mean_delta > 0), ymin=0, alpha=0.3)+
  
  geom_errorbar(
    aes(x = id * (100/(length(id))), 
        ymin = 1*(mean_delta - st_error),#  as.numeric(st_error), 
        ymax =  1*(mean_delta + st_error)
    ), size = 1, width = .2, color = 'gray') +
  facet_grid(~variable) +
  geom_point(aes(x = id * (100/(length(id))), y = 1* mean_delta,
                 fill = mean_delta > 0, alpha = rmv_code#, shape = multi
                 #    size = n
  ), 
  size = 3,
  shape = 21
  )+
  theme_classic() +
  #  tidyquant::theme_tq() +
  xlab('% of Projects') + theme(legend.position = 'none') + #axis.text.x = element_blank(),
  scale_colour_manual(values = c( "#FFD8AA","#0066CC"))  +
  scale_fill_manual(values = c("#FFD8AA","#0066CC")) +
  scale_alpha_discrete( range = c(1,.1)) +
  ylab(expression(Delta*' 2015:2020 ROI (%)')) #+
#  ylim(-150,150)
