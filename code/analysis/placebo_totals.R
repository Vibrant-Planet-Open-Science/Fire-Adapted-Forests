files <- list.files('/Users/eyackulic/Desktop', pattern = 'fake_treatments_2025', full.names = T)

for(i in 1:length(files)){
  file = 
    files[i] |>
    readRDS() #|>
    #dplyr::filter(area %in% 'project')
  
  file$seed <- stringr::str_sub(files[i],stringr::str_length(files[i]) - 6,   stringr::str_length(files[i]) - 4)
  
  if(i == 1){out <- file}else{
    out <- dplyr::bind_rows(out, file)
  }  
}
out$dataset <- paste0('placebo',out$seed)

all_response_placebo <- 
dplyr::bind_rows(
  response_fxn(out, filter = c(0,3.01), eco = FALSE),
  response_fxn(out, filter = c(0,0.01), eco = FALSE) ,
  response_fxn(out, filter = c(0.01,3.01), eco = FALSE),
  response_fxn(out, filter = c(0.01,1.25), eco = FALSE),
  response_fxn(out, filter = c(1.25,2.25), eco = FALSE),
  response_fxn(out, filter = c(2.25,3.01), eco = FALSE),
  response_fxn(out, filter = c(100,101), eco = FALSE)
)  |>
  dplyr::mutate(group = c(rep('All', 20), rep('No Fire',20),
                          rep('Fire',20), rep('Low',20),
                          rep('Medium',20),rep('Hi',20), rep('Lost',20)),
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

# 
# out |> dplyr::group_by(seed) |> dplyr::reframe(
#   c2015 = mean(composite_2015_median)/2,
#   c2023 = mean(composite_2023_median)/2,
#   diff = (c2023 - c2015)/c2015
#   )


saveRDS(all_response_placebo, '/Users/eyackulic/Desktop/all_placebo_responses.rds')
long_data_placebo <- 
  all_response_placebo |>
  dplyr::select(-mean_cbi, -dplyr::starts_with('sd'),
                -dplyr::starts_with('q')) |>
  dplyr::rename(
    mean_2015 = mean_pre
  ) |>
  tidyr::pivot_longer(cols = dplyr::starts_with('mean'), names_to = 'year', values_to = 'biomass') |>
  dplyr::mutate(
    year = as.numeric(stringr::str_sub(year, 6,9))
  ) 

long_data_placebo |>
  dplyr::filter(group %in% 'All') |>
  dplyr::mutate(id = paste0(dataset,'_',area)) |>
ggplot(
  aes(
    x = year, y = biomass, color = area, group = id
    )
  ) +
  geom_path() + #facet_grid(~ area) +
  tidyquant::scale_colour_tq() +
  theme_classic() + 
  xlab('Year') +
  ylab('Carbon Mg ha-1')

long_data_placebo$area <- dplyr::if_else(
  long_data_placebo$area %in% 'project', 'Placebos', 'Placebo RRs'
)

long_data_placebo |>
  dplyr::filter(group %in% 'Fire') |>
  dplyr::mutate(
    id = paste0(dataset,'_',area),
    id = substring(id, 8, str_length(id))) |>
  ggplot(
    aes(
      x = id, y = 100* (percent/9), group = area, fill = area
    )
  ) +
  geom_bar(stat = 'identity') +
  tidyquant::scale_fill_tq() +
  #tidyquant::theme_tq() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) + ylab()

  ggplot() +
  geom_boxplot(
    data = long_data_placebo |>
      dplyr::filter(group %in% 'Fire', year %in% 2023),
    aes(
      x = area, y = 100* (percent), group = area, fill = area
    ), alpha = .25
  ) +
  geom_jitter(
    data = long_data_placebo |>
      dplyr::filter(group %in% 'Fire', year %in% 2023),
    aes(
      x = area, y = 100* percent, group = area, fill = area
      ),
      shape = 21, size = 4
  ) +
  tidyquant::scale_fill_tq() +
  #tidyquant::theme_tq() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
    ylab('Percentage of Fire on Landscape (%)') +
    xlab('Area Grouping')
