#SI Figure 1 

EVC <- seq(20,90, by = 10)
WHP <- seq(4,5, by = 1)
Carbon <- seq(50,130, by = 1)

study <- 
  dplyr::bind_cols(
    sample(EVC, 100, replace = T),
    sample(WHP, 100, replace = T),
    sample(Carbon, 100, replace = T),
    rep('study', 100)
  )
colnames(study) <- c('EVC','WHP','Carbon','Group')

placebo <- 
  dplyr::bind_cols(
    sample(EVC, 100, replace = T),
    sample(WHP, 100, replace = T),
    sample(Carbon, 100, replace = T),
    rep('placebo', 100)
  )
colnames(placebo) <- c('EVC','WHP','Carbon','Group')

df <- dplyr::bind_rows(study, placebo)
ks.test(placebo$EVC, study$EVC)
ks.test(placebo$Carbon, study$Carbon)
ks.test(placebo$WHP, study$WHP)
ggplot() +
  geom_density(data = df, 
                 aes( y = Carbon, 
                      fill = Group),
               alpha = .5
               ) +
  theme_classic() +
  scale_fill_manual(values = c(
    'chartreuse3', 'dodgerblue'
  ))

ggplot() +
  geom_density(data = df, 
               aes( y = EVC, 
                    fill = Group),
               alpha = .5
  ) +
  theme_classic() +
  scale_fill_manual(values = c(
    'chartreuse3', 'dodgerblue'
  ))

ggplot() +
  geom_bar(data = df, 
               aes( 
                # x = Group,
                 y = as.character(WHP), 
                    fill = Group),
               alpha = 1,
           stat = 'count',
           position = 'dodge'
  ) +
  theme_classic() +
  scale_fill_manual(values = c(
    'chartreuse3', 'dodgerblue'
  ))
