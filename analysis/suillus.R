# here::i_am(
#   'analysis/suillus.R'
# )

suillus <- 
  ASVsITSrarFull_EMFfiltSum %>% 
  filter(
    Genus == 'Suillus'
  ) 

suillus_raw <- 
  ASVsITSrarFull_EM_QC %>% 
  filter(
    Genus == 'Suillus' &
      PLOT == 'PIST'
  ) %>% 
  mutate(
    WEEK = 
      lubridate::week(
        YYYYMMDD
      )
  ) %>%
  select(
    PLOT,
    WEEK,
    # YYYYMMDD, 
    SUBPLOT, 
    Species,
    abund
  ) %>% 
  mutate(
    'n_unique' = 
      n_distinct(
        Species
      ),
    Species = 
      as.factor(
        Species
      )
  )

suillus_sum <-
  suillus_raw %>%
  group_by(
    PLOT,
    # PLOT_G.spp, 
    # LeafHabit,
    WEEK, 
    SUBPLOT
  ) %>%
  summarise(
    n = n(),
    abund_sum = 
      sum(abund_sum)
  ) 
  
# suillus_sum %>%
  suillus_raw %>%
  ggplot(aes(
    x = WEEK,
    # y = abund_sum,
    y = abund,
    shape = Species
  )) +
  # geom_point(
  #   color = 'grey'
  # ) +
  # stat_summary(
  #   fun.data = 
  #     ggpubr::median_mad
  # ) +
  stat_summary(
    fun.data = 
      ggpubr::mean_se_
  ) +
  stat_smooth(
    alpha = 0.125,
    linewidth = 2,
    n = 7,
    color = 'darkgreen',
    se = F
  ) +
  theme_classic() +
  labs(
    x = 'Week of year',
    y = 'Suillus ECM fungal \nabundance'
  ) +
  scale_y_continuous(
    labels = 
      scales::label_number()
  ) +
  annotate(
    geom = 'text',
    x = 40,
    # y = 3.5e6,
    # y = 1,
    y = 600,
    label = 
      's(WEEK), EDF = 1, P 0.001 *** '
  ) +
    theme(
      legend.position = 'top'
    )


library(mgcv)
gam(
  # abund_sum ~ 
  abund ~
    s(WEEK, 
      by = Species,
      m = 1,
      k = 7) +
    s(WEEK, k = 7),
  # data = suillus_sum,
  data = suillus_raw,
  family = tw(),
  method = 'REML'
) %>%
  # gratia::appraise() #tw()
  summary()