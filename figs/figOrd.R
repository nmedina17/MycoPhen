# setup ----


library(here)
here::i_am('figs/figOrd.R')

source(here('analysis/ordEM.R'))


library(ggplot2)


# theme & text size instead adjusted per figure content


# theme_set(
#   theme_classic() +
#     theme(
#       text = element_text(
#         size = 24
#       )
#     )
# )


###plot----


ASVsITSrarFull_EMFfilt_commTbl %>%
  mutate(WEEK = week(YYYYMMDD)) %>% 

  select(!contains('ASV')) %>%
  cbind(ordEMF_xy) %>% as_tibble() %>%
  
  na.omit() %>% 
  
  left_join(keyPlots) %>% 
  
  # group_by(PLOT) %>% 

  ggplot(aes(PC1, PC2, 
             color = as.factor(WEEK))) +
  geom_point() + #geom_line() +
  stat_ellipse() +
  # facet_grid(SUBPLOT ~ PLOT, scales = 'free') +
  facet_wrap(
    # ~ LeafHabit,
    ~ PLOT_G.spp,
    scales = 'free'
  ) +
  theme_classic() +
  theme(text = element_text(size = 24),
        legend.position = 'inside',
        legend.position.inside = c(0.85, 0.25)
        ) +
  labs(color = 'Week of year') +
  scale_color_viridis_d() #+

  # geom_segment(data = head(ordEMF_spp, n = 3),
  #              aes(0,
  #                  0,
  #                  xend = PC1/10,
  #                  yend = PC2/10),
  #              inherit.aes = F,
  #              color = 'gray',
  #              arrow = arrow(length = unit(3, 'mm'))) +
  # geom_text(data = head(ordEMF_spp, n = 3),
  #           inherit.aes = F,
  #           aes(PC1/10,
  #               PC2/10,
  #               label = Genus),
  #           size = 2)


makeFigOrg <- function(COMM_TBL, ordXY) {
  
  
  figOrd <- COMM_TBL %>%
    
    select(!contains('ASV')) %>%
    cbind(ordXY) %>% as_tibble() %>%
    
    # group_by(PLOT) %>% 
    
    ggplot(aes(PC1, PC2, 
               color = as.factor(SESSION))) +
    geom_point() + #geom_line() +
    stat_ellipse() +
    # facet_grid(SUBPLOT ~ PLOT, scales = 'free') +
    facet_wrap(~ PLOT, scales = 'free') +
    theme_classic() +
    scale_color_viridis_d() #+
  
  
  return(figOrd)
}


# QUBI ----
figOrd_QUBI <- commTbl_QUBI %>% 
  makeFigOrg(ord_QUBI[[2]])

# QUAL ----
figOrd_QUAL <- commTbl_QUAL %>% 
  makeFigOrg(ord_QUAL[[2]])

# CAOV ----
figOrd_CAOV <- commTbl_CAOV %>% 
  makeFigOrg(ord_CAOV[[2]])

# PIST ----
figOrd_PIST <- commTbl_PIST %>% 
  makeFigOrg(ord_PIST[[2]])

# PIAB ----
figOrd_PIAB <- commTbl_PIAB %>% 
  makeFigOrg(ord_PIAB[[2]])


###plot.sum----


figOrdPhase <- 
  ASVsITSrarFull_EMFfilt_commTbl %>%
  mutate(WEEK = week(YYYYMMDD) %>% 
           as.factor()) %>%
  select(!contains('ASV')) %>%
  cbind(ordEMF_xy) %>% as_tibble() %>%
  left_join(keyPlots) %>%
  
  filter(PLOT %in% c(
    'PIST', 'PIAB', 'CAOV', 'QUAL', 'QUBI'
  )) %>% 
  mutate(SESSION = as.factor(SESSION)) %>% 
  
  group_by(SESSION, 
           WEEK,
           LeafHabit, 
           PLOT_G.spp) %>%
  summarize(
    'PC1_mean' = mean(PC1),
    'PC1_se' = sd(PC1) / 
      sqrt(length(PC1)),
    'PC2_mean' = mean(PC2),
    'PC2_se' = sd(PC2) / 
      sqrt(length(PC2)),
    'PC3_mean' = mean(PC3),
    'PC3_se' = sd(PC3) / 
      sqrt(length(PC3)),
    
    'PC1_med' = median(PC1),
    'PC1_mad' = mad(PC1),
    'PC2_med' = median(PC2),
    'PC2_mad' = mad(PC2),
    'PC3_med' = median(PC3),
    'PC3_mad' = mad(PC3)) %>%
  
  ggplot(aes(
    PC1_med, PC2_med,
    # PC1_mean, PC2_mean, 
    color = WEEK
  )) +
  geom_point(size = 3) + #geom_line() +
  geom_errorbar(aes(
    ymin = PC2_med - PC2_mad,
    ymax = PC2_med + PC2_mad
    # ymin = PC2_mean - PC2_se,
    # ymax = PC2_mean + PC2_se
  )) +
  geom_errorbarh(aes(
    xmin = PC1_med - PC1_mad,
    xmax = PC1_med + PC1_mad
    # xmin = PC1_mean - PC1_se,
    # xmax = PC1_mean + PC1_se
  )) +
  # stat_ellipse() +
  facet_wrap(
    ~ PLOT_G.spp,
    # ~ LeafHabit,
    scales = 'free'
  ) +
  labs(
    color = 'Week of year',
    x = 'PC1', 
    y = 'PC2'
  ) +
  theme_classic() +
  scale_color_viridis_d(option = 'turbo') +
  theme(
    text = element_text(size = 24),
    legend.position = 'inside',
    legend.position.inside = c(0.85, 0.25)
  )

# geom_segment(data = head(ordEMF_spp, n = 3),
#              aes(0, 0, xend = PC1/100, yend = PC2/100),
#              inherit.aes = F, color = 'gray',
#              arrow = arrow(length = unit(3, 'mm'))) +
# geom_text(data = head(ordEMF_spp, n = 3), inherit.aes = F,
# aes(PC1/100, PC2/100, label = Genus), size = 2)


# write ----

ggsave(plot = figOrdPhase,
       here('figs/figOrdPhase.pdf'))
ggsave(plot = figOrdPhase,
       here('figs/figOrdPhase.png'))


# byPLOTs ----


makeFigOrdPhase <- function(COMM_TBL, ordXY) {
  
  
  figOrdPhase0 <- COMM_TBL %>%
    select(!contains('ASV')) %>%
    cbind(ordXY) %>% as_tibble() %>%
    
    group_by(SESSION) %>%
    summarize(
      'PC1_mean' = median(
        PC1
      ),
      'PC1_se' = mad(PC1),
      'PC2_mean' = median(
        PC2
      ),
      'PC2_se' = mad(PC2),
      'PC3_mean' = median(
        PC3
      ),
      'PC3_se' = mad(PC3),
      
      'PC1_med' = median(
        PC1
      ),
      'PC1_mad' = mad(PC1),
      'PC2_med' = median(
        PC2
      ),
      'PC2_mad' = mad(PC2),
      'PC3_med' = median(PC3),
      'PC3_mad' = mad(PC3)
    ) %>%
    
    
    ggplot(aes(PC1_med, PC2_med, color = SESSION)) +
    geom_point() + #geom_line() +
    geom_errorbar(aes(ymin = PC2_mean - PC2_se,
                      ymax = PC2_mean + PC2_se)) +
    geom_errorbarh(aes(xmin = PC1_mean - PC1_se,
                       xmax = PC1_mean + PC1_se)) +
    # stat_ellipse() +
    # facet_wrap(~ PLOT, scales = 'free') +
    theme_classic() +
    scale_color_viridis_c() #+
  
  
  return(figOrdPhase0)
  
  
}


# BELOW CODE IS LEGACY


# QUBI ----

figOrdPhase_QUBI <- commTbl_QUBI %>% 
  makeFigOrdPhase(ord_QUBI[[2]])

# QUAL ----

figOrdPhase_QUAL <- commTbl_QUAL %>% 
  makeFigOrdPhase(ord_QUAL[[2]])

# CAOV ----

figOrdPhase_CAOV <- commTbl_CAOV %>% 
  makeFigOrdPhase(ord_CAOV[[2]])

# PIST ----

figOrdPhase_PIST <- commTbl_PIST %>% 
  makeFigOrdPhase(ord_PIST[[2]])

# PIAB ----

figOrdPhase_PIAB <- commTbl_PIAB %>% 
  makeFigOrdPhase(ord_PIAB[[2]])
