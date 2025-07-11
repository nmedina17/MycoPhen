# setup ----


library(here); 
here::i_am('figs/figET.R')

# source(here('data/import.R'))
# source(here('analysis/commEM.R'))
source(here('stats/statET.R'))

library(ggplot2)


loess_n <- 7


figET_data <-
  ASVsITSrarFull_EMFfiltExploreSum %>%
  
  group_by(
    YYYYMMDD, 
    # SUBPLOT,
    LeafHabit,
    PLOT_G.spp,
    ET
  ) %>%
  summarise(
    'abund_sum_mean' = 
      mean(abund_sum),
    'abund_sum_se' = 
      sd(abund_sum) /
      sqrt(n()),
    'abund_sum_med' = 
      median(abund_sum),
    'abund_sum_mad' = 
      mad(abund_sum),
    'abund_sum_prop_mean' = 
      mean(abund_sum_prop),
    'abund_sum_prop_se' = 
      sd(abund_sum_prop) /
      sqrt(n()),
    'abund_sum_prop_med' = 
      median(abund_sum_prop),
    'abund_sum_prop_mad' = 
      mad(abund_sum_prop)) %>%
  
  filter(
    ET != 'Unknown' &
      !is.na(ET) &
      ET != 'Mat'
  )


# theme_set(
#   theme_classic() +
#     theme(
#       text = element_text(
#         size = 24
#       )
#     )
# )


figET <- 
  figET_data %>%
  
  
  ggplot(aes(
    week(YYYYMMDD),
    abund_sum_med + 
      1, #contact
    
    shape = LeafHabit,
    color = LeafHabit,
    # color = fct_reorder(
    #   ET,
    #   abund_sum_med,
    #   .desc = T
    # )
    # shape = as.factor(SUBPLOT)
  )) +
  # geom_point(color = 'gray') +
  # scale_y_log10() +
  stat_summary(
    fun.data = ggpubr::median_mad,
    size = 0.75, 
    alpha = 0.75,
    color = 'gray'#,
    # position = 
    #   position_dodge(
    #     width = 0.5
    #   ) #!facets
  ) +
  scale_y_continuous(
    trans = 'log10',
    labels = scales::trans_format(
      trans = 'log10',
      # trans = 'identity',
      format = scales::math_format()
    ),
    # n.breaks = 4
    breaks = c(
      1, 10, 100, 1000, 1e4
    )
  ) +
  # scale_y_continuous(
  #   limits = c(NA, NA),
  #   n.breaks = 4 #!-1k
  # ) +
  # geom_errorbar(aes(
  #   ymin = abund_sum_prop_med - 
  #     abund_sum_prop_mad,
  #   ymax = abund_sum_prop_med + 
  #     abund_sum_prop_mad
  #   ),
  #   color = 'gray'
  # ) +
  # stat_summary(
  #   fun.data = ggpubr::mean_se_,
  #   color = 'gray'
  # ) +
  # geom_line() +
  geom_smooth(
    se = T,
    n = loess_n,
    linewidth = 2,
    alpha = 0.125#,
    # color = 'black'
  ) + #, 
      # method = 'gam', 
      # formula = y ~ s(x, k = 7),
  # facet_wrap(~ PLOT_G.spp) +
  facet_wrap(
    ~ ET,
    scales = 'free_y'
  ) +
  # facet_grid(Ectomycorrhiza_exploration_type_template ~ PLOT_G.spp) +
  
  theme_classic() +
  labs(
    # color = 'ECM fungal \n exploration type',
    color = 'Leaf Habit',
    shape = 'Leaf Habit',
    x = 'Time (week of year)', 
    y = 'Soil ECM fungal exploration type \n abundance (ITS2 DNA reads)'
  ) +
  # scale_colour_viridis_d(
  #   option = 'turbo') +
  # scale_x_datetime(
  #   breaks = keyDates$YYYYMMDD,
  #   labels = keyDates$MMDD
  # ) +
  scale_x_continuous(
    breaks = week(keySeasonDates),
    limits = c(10, 52)
  ) +
  scale_color_manual(
    values = c(
      'lightgreen',
      'darkgreen'
    )
  ) +
  
  theme(
    text = element_text(
      size = 14
    ),
    # axis.text.x = element_text(
    # angle = -45, 
    # hjust = 0, vjust = 1),
    legend.position = 'top',
    # legend.position = 'inside',
    # # legend.background = element_blank(),
    # legend.position.inside = 
    #   c(0.85, 0.2),
    plot.margin = unit(
      c(5, 5, 5, 5), 
      'mm'
    )
  )


# write ----

# ggsave(
#   plot = figET,
#   width = 10,
#   height = 7,
#   units = 'in',
#   here('figs/figET.pdf')
# )
# ggsave(
#   plot = figET,
#   width = 10,
#   height = 7,
#   units = 'in',
#   here('figs/figET.png')
# )


#which----

ASVsITSrarFull_EMFfiltSum %>% 
  group_by(
    ET,
    Genus,
    PLOT,
    YYYYMMDD
  ) %>% 
  summarize(
    'abund_sum1' = 
      sum(abund_sum) + 
      0
  ) %>%
  arrange(
    ET,
    - abund_sum1
  ) %>% 
  distinct() %>%
  
  filter(
    !is.na(ET)
  ) %>% 
  
  ggplot(aes(
    abund_sum1, 
    fct_reorder(
      Genus, 
      abund_sum1)
  )) + 
  # geom_point(color = 'grey') + 
  stat_summary(
    fun.data = ggpubr::median_mad
  ) + 
  # scale_x_log10() +
  scale_x_continuous(
    trans = 'log10',
    labels = scales::trans_format(
      trans = 'log10',
      format = scales::math_format()
    ),
    n.breaks = 4,
    # breaks = c(10^8.5, 10^9, 10^9.5)
    # breaks = c(10^9, 10^9.5, 10^9.75)#,
    limits = c(NA, 10^9)
  ) +
  facet_wrap(
    ~ ET,
    scales = 'free_y'
  ) +
  
  labs(
    y = 'Soil ECM fungal\n exploration type',
    x = 'Abundance (ITS2 DNA reads)'
  ) +
  
  theme_classic() +
  theme(
    text = 
      element_text(
        size = 10
      )
  )


# ggpubr::ggexport(fig_ET, here('figs/fig_ET.pdf'))


# contact ----


figET_contact <- 
  ASVsITSrarFull_EMFfiltExploreSum %>%
  
  group_by(
    YYYYMMDD, 
    # SUBPLOT,
    LeafHabit,
    PLOT_G.spp,
    # Ectomycorrhiza_exploration_type_template
    ET
  ) %>%
  summarise(
    'abund_sum_mean' = 
      mean(abund_sum),
    'abund_sum_se' = 
      sd(abund_sum) /
      sqrt(n()),
    'abund_sum_med' = 
      median(abund_sum),
    'abund_sum_mad' = 
      mad(abund_sum),
    'abund_sum_prop_mean' = 
      mean(abund_sum_prop),
    'abund_sum_prop_se' = 
      sd(abund_sum_prop) /
      sqrt(n()),
    'abund_sum_prop_med' = 
      median(abund_sum_prop),
    'abund_sum_prop_mad' = 
      mad(abund_sum_prop)) %>%
  
  filter(
    # Ectomycorrhiza_exploration_type_template !=
    #   'Unknown' &
    !is.na(
      ET) &
      # Ectomycorrhiza_exploration_type_template) &
      #     Ectomycorrhiza_exploration_type_template !=
      #     'Mat' 
      ET == 'Contact'
  ) %>%
  
  
  ggplot(aes(
    week(YYYYMMDD),
    abund_sum_med + 0,
    
    shape = PLOT_G.spp
  )) +
  scale_y_continuous(
    limits = c(NA, NA),
    n.breaks = 4 #!-1k
  ) +
  stat_summary(
    fun.data = ggpubr::mean_se_,
    color = 'gray'
  ) +
  # geom_line() +
  geom_smooth(
    se = T,
    alpha = 0.125,
    color = 'black'
  ) + 
  facet_wrap(
    # ~ Ectomycorrhiza_exploration_type_template,
    ~ ET,
    scales = 'free_y') +
  
  theme_classic() +
  labs(
    shape = 'Leaf habit',
    x = 'Time (week of year)', 
    y = 'Soil ECM fungal exploration type \n abundance (ITS2 DNA reads)'
  ) +
  scale_x_continuous(
    breaks = week(keySeasonDates)
  ) +
  theme(
    text = element_text(size = 12),
    # legend.position = 'inside',
    # legend.position.inside = c(0.85, 0.2),
    plot.margin = unit(c(5, 5, 5, 5), 'mm')
  )


library(ggpubr)
# ggarrange(
#   figET,
#   figET_contact
# )