# setup ----


library(here)
here::i_am('figs/figNetworkEM.R')

source(here('analysis/networksEM.R')) #tmp

library(dplyr) # %>%
library(tibble) #::rownames_to_column()
library(stringr) #::str_extract()
library(tidyr) #::pivot_longer()
library(forcats) #::fct_[]()
library(ggplot2)


networkTbl <- 
  nets_cals %>% 
  t() %>% 
  as.data.frame() %>%
  
  rownames_to_column('PLOT_SESSION') %>% 
  as_tibble() %>%
  
  mutate(
    'PLOT' = PLOT_SESSION %>% 
      str_remove('\\d+') %>% 
      as.factor(),
    'SESSION' = PLOT_SESSION %>% 
      str_extract('\\d+') %>% 
      as.numeric(),
    .after = 'PLOT_SESSION'
  ) %>% 
  
  pivot_longer(
    !c(SESSION,
       PLOT,
       PLOT_SESSION), 
    names_to = 'network_metric'
  )


# fig ----


networkTbl %>% 
  
  filter(
    !(network_metric %in%
        c('Clustering_coefficient',
          'Network_diameter',
          'Average_path_length')
    )
  ) %>% 
  
  ggplot(aes(
    SESSION, 
    value,
    color = PLOT
  )) + 
  # geom_point() +
  
  # geom_line() + 
  stat_smooth(
    se = F,
    n = 7
  ) +
  
  theme_classic() + 
  facet_wrap(
    ~ network_metric, 
    scales = 'free'
  )


# write ----


ggsave(here('figs/figNetworkEM.pdf'))
ggsave(here('figs/figNetworkEM.png'))


#quickStat

library(mgcv) #::gam()
networkTbl %>% 
  nest(!network_metric) %>% 
  mutate(
    'quickStat' = map(
      data,
      ~ .x %>% 
        gam(
          value ~ 
            s(SESSION, k = 7),
          data = .x,
          method = 'REML'
        )
    )
  )