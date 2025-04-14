#install the following 2 packages for easy Bayesian modeling 
#remotes::install_github('easystats/easystats')
#install.packages('rstanarm')

library(easystats)
library(rstanarm)
library(ggplot2)


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
  )  |>
  dplyr::rename(max_cbi = max_rdnbr)

#remove cbi values = 0 and replace with NA
df[which(df$max_cbi == 0),]$max_cbi <- NA

#calculate a couple other variables
df$total_delta <- (df$composite_2023_median - df$composite_2015_median)/df$composite_2015_median
df$post_delta <- (df$composite_2023_median - df$composite_2017_median)/df$composite_2017_median

#glm models are created for each subset of data and all data (df)
no_fire <- df |> dplyr::filter(is.na(max_cbi))
fire <- df |> dplyr::filter(!is.na(max_cbi))
low <- df |> dplyr::filter(max_cbi < 1.25 & !is.na(max_cbi))
med <- df |> dplyr::filter(max_cbi >= 1.25 & max_cbi < 2.25 & !is.na(max_cbi))
hi <- df |> dplyr::filter(max_cbi >= 2.25 & !is.na(max_cbi))
lost <- df |> dplyr::filter(max_cbi >=  2.25 & !is.na(max_cbi) & post_carbon == 0)

#example run for 1 response variable
#all response variables are max_cbi(cbi), mid_delta(change from 2015 -2020),
#late_delta( change from 2020 - 2021) and total_delta(2015-2021)
b_mod <- stan_glm(total_delta ~ designation, data = df) #area is either 'project' or 'ref_region'
b_mod |> saveRDS('/Users/eyackulic/workspace/model_runs/comp_td_model_all.rda') #optional local saving 
describe_posterior(b_mod)
ps <- get_parameters(b_mod)

ggplot(ps) + 
  geom_density(aes(x = `designationref region`), color = 'blue')+
  geom_density(aes(x = `designationplacebo ref region`), color = 'red')+
  geom_density(aes(x = designationplacebo), color = 'purple')
  #density plot of parameters

#rope_value <- 0.1 * sd(med$max_rdnbr)
rope_range <- rope_range(b_mod) # rope range is +/- 10% of the standard deviation ^^

#calculate how many values within the 89th percentile  fall into the rope range
# 0 = highly significant / 100 = not significant at all
rope(ps$designationplacebo, range = rope_range, ci = 0.89) 
rope(ps$`designationplacebo ref region`, range = rope_range, ci = 0.89) 
rope(ps$`designationref region`, range = rope_range, ci = 0.89) 

#cbi is significantly different in projects vs untreated all
#% biomass losses (post-pre/pre) are significantly different across all burned areas in projects vs untreated all
#same with no fire areas, due to treatment effect -- need to work on this still