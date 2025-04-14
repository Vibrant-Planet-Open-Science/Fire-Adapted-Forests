library(tidyverse)
df2 <- '/Users/eyackulic/Desktop/real_treatments_2025-03-23_cms_1_111.rds' |> readRDS()

df2$fire <- dplyr::if_else(df2$max_rdnbr > 0.1, 'fire', 'no fire')

df2$percent <- (df2$composite_2023_median - df2$composite_2017_median)/df2$composite_2017_median
df2$percent <- df2$pre_fire
summary(df2$percent)

df2$percent_break <- cut(df2$percent, breaks = seq(from = min(df2$percent,na.rm =T),to = max(df2$percent,na.rm =T),by = 25))
df2$fire_break <- cut(df2$max_rdnbr, breaks = seq(0,3, by = .25))
df2[which(df2$percent < -1),]$percent <- -1  
df2$percent_break |> summary()
n_val <- df2[which(df2$percent >= 181),]$percent_break |> unique()

df2[which(is.na(df2$percent_break)),]$percent_break <- n_val[1]
n_val2 <- df2[which(df2$max_rdnbr < 0.01),]$fire_break |> unique()
df2[which(is.na(df2$fire_break)),]$fire_break <- n_val2[2]

table(df2$percent_break)

heat <- 
  df2 |> dplyr::group_by(designation, fire_break, percent_break, fire) |> 
  dplyr::reframe(n = dplyr::n())

heat |> dplyr::group_by(designation, fire) |> dplyr::reframe(s = sum(n))

#heat$n_perc <- dplyr::if_else(heat$area %in% 'project', 100* (heat$n / 62289), 100 *(heat$n/5520962))
heat$n_perc <- dplyr::case_when(
  heat$designation %in% 'treatment'~ 100* (heat$n / 6078),
  heat$designation %in% 'ref region'~ 100 *(heat$n/1643421),
  heat$designation %in% 'placebo ref region'~ 100 *(heat$n/2388571),
  heat$designation %in% 'placebo'~ 100 *(heat$n/30292)
  )

heat |>
  dplyr::filter(fire %in% 'fire', designation %in% 'treatment', n_perc > 0.15) |>
  dplyr::arrange(percent_break)

ggplot(data = heat |> dplyr::filter(fire %in% 'fire', n_perc > 0.6)) + 
  geom_tile(aes(x = percent_break, y = fire_break, fill = (n_perc)))+ 
  facet_grid(~designation)+
  scale_fill_viridis_c(option = 'magma', limits = c(1,8), oob = scales::squish) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) 

