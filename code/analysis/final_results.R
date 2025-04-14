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
df |>dplyr::filter(designation %in% 'treatment', max_rdnbr > 0) |> dplyr::select(distid) |> unique()
df |> dplyr::filter(max_rdnbr < 1.25, max_rdnbr > 0) |> dplyr::group_by(designation) |> dplyr::reframe(q = quantile(fire_change, .9, na.rm = T))
df |> dplyr::filter(max_rdnbr < 2.25, max_rdnbr >= 1.25) |> dplyr::group_by(designation) |> dplyr::reframe(q = quantile(fire_change, .9, na.rm = T))
df |> dplyr::filter(max_rdnbr >= 2.25) |> dplyr::group_by(designation) |> dplyr::reframe(q = quantile(fire_change, .9, na.rm = T))

df[which(df$max_rdnbr > 2.24),]$fire_change |> quantile(.9, na.rm = T)
df[which(df$max_rdnbr > 2.24 & df$fire_change > -0.45),]$max_rdnbr <- NA

# df <-
#   df |>
#   dplyr::mutate(
#     max_rdnbr = dplyr::case_when(
#       max_rdnbr > 2.25 & fire_change > -0.6 ~ NA
#     )
#   )

ggplot(data = df |> dplyr::filter(max_rdnbr >= 2.25), aes(x = designation, y = fire_change)) + geom_boxplot() + facet_grid(~max_rdnbr_year)

#df <- df[-which(df$max_rdnbr > 2.25 & df$fire_change > -0.25),]

df |>
  dplyr::filter(max_rdnbr_year > 2014) |>
  dplyr::group_by(designation, max_rdnbr_year) |>
  dplyr::reframe(
    n = dplyr::n()
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(designation) |>
  dplyr::reframe(
    year = max_rdnbr_year,
    percent = 100 * (n / sum(n))
  ) |>
  ggplot() +
  geom_bar(
    aes(x = year, y = percent, fill = designation, group = designation),
    stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = c("#0066CC",colorRampPalette(c("#FFFFFF","#FF8C00"))(4)[-1]),
                    labels = c('Treated Projects', 'Project RRs', 'Placebo RRs', 'Placebos'), name = ''
  ) + theme_classic() + 
  xlab('Year') + 
  ylab('% of Burned Pixels')
  
df |>
  dplyr::filter(max_rdnbr_year > 2014) |>
  dplyr::mutate(
    max_rdnbr_year = max_rdnbr_year 
  ) |>
  dplyr::group_by(designation, max_rdnbr_year) |>
  dplyr::reframe(
    n = dplyr::n()
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(designation) |>
  dplyr::reframe(
    year = max_rdnbr_year,
    percent = 100 * (n / sum(n))
  ) |>
  dplyr::filter(year > 2019, year < 2022) |>
  dplyr::group_by(designation) |> 
  dplyr::reframe(
    sum = sum(percent)
  )
  
df |> 
  dplyr::filter(designation %in% 'treatment') |>
  dplyr::select(distid, ecoregion) |> 
  dplyr::distinct() |>
  dplyr::select(ecoregion) |>
  table()


all_response <-
  dplyr::bind_rows(
    response_fxn(df, filter = c(0,3.01), eco = FALSE),
    response_fxn(df, filter = c(0,0.01), eco = FALSE) ,
    response_fxn(df, filter = c(0.01,3.01), eco = FALSE),
    response_fxn(df, filter = c(0.01,1.25), eco = FALSE),
    response_fxn(df, filter = c(1.25,2.25), eco = FALSE),
    response_fxn(df, filter = c(2.25,3.01), eco = FALSE),
    response_fxn(df, filter = c(100,101), eco = FALSE)
  ) |>
  dplyr::mutate(group = c(rep('All', 4), rep('No Fire',4), rep('Fire',4), rep('Low',4), rep('Medium',4),rep('Hi',4), rep('Lost',4)),
                group = factor(group, levels = c('All', 'No Fire','Fire','Low','Medium','Hi','Lost')),
                designation = dplyr::case_when(
                  dataset %in% 'real' & area %in% 'project' ~ 'treatment',
                  dataset %in% 'fake' & area %in% 'project' ~ 'placebo',
                  dataset %in% 'real' & area %in% 'ref_region' ~ 'ref region',
                  dataset %in% 'fake' & area %in% 'ref_region' ~ 'placebo ref region'
                ),
                designation = factor(designation, levels = c( 'treatment','ref region','placebo ref region', 'placebo'))
  ) |>
  dplyr::arrange(group,desc(dataset),area) |>
  data.frame()


all_response |>
  dplyr::filter(group != 'Lost') |>
  dplyr::filter(group %in% 'Hi') |>
  dplyr::select(designation, group, dplyr::starts_with('mean'), percent) |>
  dplyr::mutate(
    percent = 100 * percent,
    change1 = 100 * ((mean_2023 - mean_pre)/ mean_pre),
    change2 = 100 * ((mean_2023 - mean_2017)/ mean_2017)
  ) |>
  dplyr::select(designation, dplyr::starts_with('mean'),dplyr::starts_with('change'), percent) |>
  kableExtra::kbl(digits = 0) |>
  kableExtra::kable_styling()
#Table 2 info

all_response |>
  dplyr::filter(group %in% 'All') |>
  dplyr::reframe(round(total_pixels * 0.2223945, digits = -3)) # total acres per group
#follow up evaluation stats :


df_trts <- 
  df |> 
  dplyr::filter(designation %in% 'treatment') |> 
  dplyr::select(distid) |> 
  dplyr::group_by(distid) |>
  dplyr::mutate(
    n = dplyr::n()
  ) |>
  dplyr::distinct() 

facts_subset <- 
  '/Users/eyackulic/Desktop/all_facts_since_2010.gpkg' |>
  sf::read_sf()

trts <-
  facts_subset |> 
  dplyr::filter(
    distid %in% df_trts$distid,
    disttype %in% c('Commercial Thinning', 'Commercial Thin')
  )

trts$distid |>
  unique() |>
  length()  

follow_ups <- 
  facts_subset |>
  dplyr::filter(year > 2016,
                disttype %in% c(
                  'Alternative Prescription', 'Lop and Scatter', 'Clearcut',
                  'Biomass Removal', 'Broadcast Burn',
                  'Commercial Thin', 'Commercial Thinning',
                  'Crushing', 'Group Selection',
                  'Machine Pile', 'Machine Pile Burn', 
                  'Selection','Thinning')
  ) |>
  dplyr::mutate(
    dist_simp = dplyr::if_else(disttype %in% 
                                 c('Broadcast Burn', 'Machine Pile', 'Machine Pile Burn'), 'Burn', 'Secondary Thinning'
                               )
  ) |>
  sf::st_intersection(trts)

follow_ids <-
  follow_ups$distid.1 |>
  unique() 

follow_ids |> 
  length()

follow_ups$disttype |> table()

df |>
  dplyr::filter(
   # distid %in% follow_ids,
    area %in% 'project') |>
  dplyr::reframe(round(dplyr::n() * 0.2223945, digits = -3)) # total acres per group
#follow up evaluation stats :

trts |> 
  dplyr::filter(distid %in% follow_ids)  |>
  sf::st_area() |>
  sum()

follow_ups |> 
  dplyr::filter(!duplicated(distid.1, year)) |>
  sf::st_area() |>
  sum()

follow_ups |> 
  dplyr::tibble() |>
 dplyr::filter(!duplicated(distid.1,disttype)) |>
  dplyr::select(year,distid.1,disttype) |> 
  dplyr::rename(distid = distid.1,
                second_dist = disttype) |>
  dplyr::right_join(df, by = 'distid') |>
  dplyr::filter(
    distid %in% follow_ids,
    designation %in% 'treatment') |>
  #dplyr::left_join(follow_ups[c('year','distid')], by = 'distid')
  tidyr::pivot_longer(cols = dplyr::starts_with('composite'), values_to = 'vals', names_to = 'names') |>
  dplyr::mutate(names = as.numeric(stringr::str_sub(names, 11,14)),
                dist_simp = dplyr::if_else(second_dist %in% 
                                             c('Broadcast Burn', 'Machine Pile', 'Machine Pile Burn'), 'Burn', 'Secondary Thinning'
                )) |>
  dplyr::filter(names < 2021) |>
  ggplot() + geom_boxplot(aes(x = factor(names), y = vals, fill = year)) + facet_grid(year~dist_simp) +
  scale_fill_gradient(low = 'orange', high = 'purple') +
  tidyquant::theme_tq() + theme(axis.text.x = element_text(angle = 90))
  
fire_trts <-
  df |> 
  dplyr::filter(designation %in% 'treatment', max_rdnbr > 0.01) |>
  dplyr::select(distid, max_rdnbr) |> 
  dplyr::group_by(distid) |>
  dplyr::mutate(
    fire_n = dplyr::n(),
    cbi = mean(max_rdnbr)
  )|>
  #dplyr::distinct()  |>
  dplyr::right_join(df_trts, by = 'distid')

fire_trts2 <-
  fire_trts |>
  dplyr::mutate(
    percent = fire_n / n,
    distid.1 = distid
  ) |>
  dplyr::left_join(follow_ups[c('dist_simp','distid.1')], by = 'distid.1')

ggplot(data = fire_trts2, aes( x = dist_simp, y =  max_rdnbr)) + geom_boxplot() #+ geom_jitter() 

df |> 
  dplyr::mutate(
    max_rdnbr = dplyr::if_else(max_rdnbr <= 0.01, NA, max_rdnbr)
  ) |>
  ggplot(
    aes (x = max_rdnbr,
         y = designation, 
         fill = designation,
         color = designation))  +
  ggdist::stat_halfeye(
    adjust = 0.5,
    justification = -.2,
    .width = 0.1
  ) +
  geom_boxplot(
    width = .05,
    outlier.color = NA,
    alpha = 0.5,
  ) +
  scale_fill_manual(values = c("#0066CC",colorRampPalette(c("#FFFFFF","#FF8C00"))(4)[-1]),
                    labels = c('Treated Projects', 'Project RRs', 'Null Projects', 'Null RRs'), name = ''
  ) +
  scale_colour_manual(values = c("#0066CC",colorRampPalette(c("#FFFFFF","#FF8C00"))(4)[-1]),
                      labels = c('Treated Projects', 'Project RRs', 'Null Projects', 'Null RRs'), name = ''
  ) + theme_classic() + 
  xlab('Fire Intensity (CBI)') + 
  ylab('')


long_data <- 
  all_response |>
  dplyr::select(-mean_cbi, -dplyr::starts_with('sd')) |>
  dplyr::rename(
    mean_2015 = mean_pre
    ) |>
  tidyr::pivot_longer(cols = dplyr::starts_with('mean'), names_to = 'year', values_to = 'biomass') |>
  dplyr::mutate(
    year = as.numeric(stringr::str_sub(year, 6,9))
  ) 

long_data <-
all_response |>
  dplyr::select(-sd_cbi, -area, -dataset, -n, -total_pixels, -percent, -dplyr::starts_with('mean')) |>
  dplyr::rename(
    sd_2015 = sd_pre
  ) |>
  tidyr::pivot_longer(cols = dplyr::starts_with('sd'), names_to = 'year', values_to = 'error') |>
  dplyr::mutate(
    year = as.numeric(stringr::str_sub(year, 4,7))
  ) |>
  dplyr::left_join(long_data, by = c('year','designation', 'group'))

long_data <- 
  all_response |>
  dplyr::select(designation, group, starts_with('q25')) |>
  tidyr::pivot_longer(cols = dplyr::starts_with('q25'), names_to = 'year', values_to = 'q25') |>
  dplyr::mutate(
    year = as.numeric(stringr::str_sub(year, 5,8))
  )|>
  dplyr::left_join(long_data, by = c('year','designation', 'group'))

long_data <- 
  all_response |>
  dplyr::select(designation, group, starts_with('q75')) |>
  tidyr::pivot_longer(cols = dplyr::starts_with('q75'), names_to = 'year', values_to = 'q75') |>
  dplyr::mutate(
    year = as.numeric(stringr::str_sub(year, 5,8))
  )|>
  dplyr::left_join(long_data, by = c('year','designation', 'group'))

long_data |>
  dplyr::filter(group %in% c(
  'All'
  #  'No Fire', 'Fire'
  #  'Low','Medium','Hi'
    )#,
# designation != 'placebo ref region'
) |>
  dplyr::mutate(
    error = error / sqrt(216),
    designation = factor(designation, ordered = T, levels = c('treatment','ref region', 'placebo','placebo ref region'))
  ) |>
  dplyr::arrange(desc(designation)) |>
  ggplot(aes(x = year, y = biomass, 
             #color = designation,
             fill = designation)) +
  geom_path(aes(linetype = designation), color = 'black') +
 # geom_point(aes(size = percent, shape = designation)) + 
  geom_ribbon(aes(ymax =biomass + (1.96 * error), ymin = biomass - (1.96 * error)), alpha = .36)+
 # geom_errorbar(aes(x = year, ymax = biomass + (1.96 * error), ymin = biomass - (1.96 * error)), width = .1) + 
  facet_grid(~group)  +
  scale_shape_manual(values = 21:24) +
  scale_fill_manual(values = c("#0066CC",colorRampPalette(c("#FFFFFF","#FF8C00"))(4)[-1]),
                    labels = c('Treatments', 'Reference Regions', 'Placebos', 'Placebo RRs'),
                    #labels = c('Null RRs','Null Treatments', 'Treatments', 'Treatment RRs'), 
                    name = ''
  )  +
  scale_color_manual(values = c("#0066CC",colorRampPalette(c("#FFFFFF","#FF8C00"))(4)[-1]),
                     labels = c('Treatments', 'Reference Regions', 'Placebos', 'Placebo RRs'),
                    #labels = c('Null RRs','Null Treatments', 'Treatments', 'Treatment RRs'),
                    name = ''
  )  +
  scale_linetype_manual(values = c('solid','longdash','dotted','dotdash'),
                        labels = c('Treatments', 'Reference Regions', 'Placebos', 'Placebo RRs'),
                        #labels = c('Null RRs','Null Treatments', 'Treatments', 'Treatment RRs'),
                        name = ''
  )  +
    
  theme_classic() + xlab('Year') + ylab('Biomass Over Time') +
  scale_size_binned(breaks = c(0.01,0.05,0.08,0.1,0.15,0.75,0.8),range = c(1,4))

all_response |>
  dplyr::filter(group %in% 'Fire') |>
  ggplot() +
  geom_bar(aes(x = designation, y = percent, 
               fill = designation),
           stat = 'identity') + 
  facet_grid(~group) + 
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  scale_fill_manual(values = c("#0066CC",colorRampPalette(c("#FFFFFF","#FF8C00"))(4)[-1]),
                    labels = c('Treatments', 'Reference Regions', 'Placebos', 'Placebo RRs'),
                    #labels = c('Null RRs','Null Treatments', 'Treatments', 'Treatment RRs'), 
                    name = ''
  ) 

long_data |>
  dplyr::filter(group %in% c(
    #'All'
    'No Fire', 'Fire'
    #'Low','Medium','Hi'
  )) |>
  dplyr::mutate(
    se = error / sqrt(n),
    designation = factor(designation, ordered = T, levels = c('treatment','ref region', 'placebo','placebo ref region'))
  ) |>
  dplyr::arrange(desc(designation)) |>
  ggplot(aes(x = year, y = biomass, fill = designation)) +
  geom_ribbon(aes(x = year , ymax = biomass + error, ymin = biomass -  error))+
  # geom_path() +
  #geom_point(aes(size = percent, shape = designation)) + 
  # geom_errorbar(aes(x = year, ymax = biomass + (1.96 * error), ymin = biomass - (1.96 * error)), width = .1) + 
  facet_grid(~group)  +
  scale_shape_manual(values = 21:24) +
  scale_fill_manual(values = c("#0066CC",colorRampPalette(c("#FFFFFF","#FF8C00"))(4)[-1]),
                    labels = c('Null RRs','Null Treatments', 'Treatments', 'Treatment RRs'), name = ''
  )  +
  theme_classic() + xlab('Year') + ylab('Biomass Over Time') +
  scale_size_binned(breaks = c(0.01,0.05,0.08,0.1,0.15,0.75,0.8),range = c(2,8))

all_response$mean_delta15 <- (all_response$mean_2023 - all_response$mean_pre) / all_response$mean_pre



###STILL NEEDS WORK
#Figure 5
viridis_colors <- viridis::magma(6)

all_response |>
  dplyr::filter(!group %in% c('All', 'Lost','Fire')) |>
  dplyr::mutate(
    variable = forcats::fct_relevel(group,
                                    c('Hi','Medium','Low','No Fire'))
#                                    c('No Fire','Hi', 'Medium', 'Low')) 
  ) |>
  ggplot() + 
  geom_bar(aes(x = designation,
               y = (((mean_2023 - mean_pre)/ mean_pre) * percent) * 100,
               fill = variable),
           stat = 'identity') +
  theme_classic() + xlab('Year') + ylab('Relative Contribution of Carbon Loss') +
  scale_fill_manual(values = c(viridis_colors[c(3,4,5)] , 'gray')) +
  scale_y_continuous(limits = c(-30,5),breaks = seq(-35, 5, by = 5))

  
  #supplemental figures around reentry
    
    df |> 
      dplyr::filter(designation %in% 'treatment', max_rdnbr > 0.01) |>
      dplyr::mutate(
        follow_up = dplyr::if_else(distid %in% follow_ups$distid.1, 'follow-up', 'none')) |> 
      dplyr::group_by(distid) |>
      dplyr::reframe(mean = mean(max_rdnbr),
                     follow_up = unique(follow_up))|>
      ggplot(aes(x = follow_up, y = mean, fill = follow_up)) +
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
      scale_fill_manual(values = c('skyblue','navy')) + 
      facet_grid(~follow_up)
    
  
  
    df |> 
      dplyr::filter(designation %in% 'treatment', max_rdnbr > 0.01) |>
      dplyr::mutate(
        dif = (composite_2015_median - composite_2023_median)/composite_2015_median,
        follow_up = dplyr::if_else(distid %in% follow_ups$distid.1, 'follow-up', 'none')) |> 
      dplyr::group_by(distid) |>
      dplyr::reframe(mean = mean(fire_change),
                     follow_up = unique(follow_up))|>
      ggplot(aes(x = follow_up, y = mean, fill = follow_up)) +
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
      scale_fill_manual(values = c('darkred','red'))
    
  
  
  