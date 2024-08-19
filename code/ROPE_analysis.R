#install the following 2 packages for easy Bayesian modeling 
#remotes::install_github('easystats/easystats')
#install.packages('rstanarm')

library(easystats)
library(rstanarm)
library(ggplot2)
#run individually for 'real' data or 'fake' data -- files are too large to combine and read
#path <- '/vsis3/vp-sci-grp/sandbox-ethan/tcsi-manuscript/real_treatments_7_4_CBI.csv' # 'real' data
path <- '/vsis3/vp-sci-grp/sandbox-ethan/tcsi-manuscript/fake_treatments_4_19_CBI_new.csv'#'fake' data

df <- path |>
  readr::read_csv() |> 
  dplyr::rename(max_cbi = max_rdnbr, #rename some columns -- study originally just used rdnbr
                max_cbi_year = max_rdnbr_year)

#remove cbi values = 0 and replace with NA
df[which(df$max_cbi == 0),]$max_cbi <- NA

#calculate a couple other variables
df$mid_delta <- df$mid_carbon -df$pre_carbon
df$late_delta <- df$post_carbon -df$mid_carbon

#glm models are created for each subset of data and all data (df)
no_fire <- df |> dplyr::filter(is.na(max_cbi))
low <- df |> dplyr::filter(max_cbi < 1.25 & !is.na(max_cbi))
med <- df |> dplyr::filter(max_cbi >= 1.25 & max_cbi < 2.25 & !is.na(max_cbi))
hi <- df |> dplyr::filter(max_cbi >= 2.25 & !is.na(max_cbi))
lost <- df |> dplyr::filter(max_cbi >=  2.25 & !is.na(max_cbi) & post_carbon == 0)

#example run for 1 response variable
#all response variables are max_cbi(cbi), mid_delta(change from 2015 -2020),
#late_delta( change from 2020 - 2021) and total_delta(2015-2021)

b_mod <- stan_glm(max_cbi ~ area, data = med) #area is either 'project' or 'ref_region'
#b_mod |> saveRDS('/Users/eyackulic/Desktop/null_cbi_model_med.rda') #optional local saving 

ps <- get_parameters(b_mod)

ggplot(ps, aes(x = arearef_region)) + geom_density() #density plot of parameters

#rope_value <- 0.1 * sd(med$max_rdnbr)
rope_range <- rope_range(b_mod) # rope range is +/- 10% of the standard deviation ^^

#calculate how many values within the 89th percentile  fall into the rope range
# 0 = highly significant / 100 = not significant at all
rope(ps$arearef_region, range = rope_range, ci = 0.89) 


