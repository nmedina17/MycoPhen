# setup ----

library(here)
here::i_am('figs/fig_qPCR_EM.R')

# source(here('data/import.R'))
#stat_qPCR_curves
source(here('stats/statPCR.R')) 
source(here('figs/figWeather.R'))

library(ggplot2)
library(lubridate)
library(ggpubr)


loess_n <- 7


theme_qPCR <- 
  theme_classic() +
  # theme_bw() +
  theme(
    text = element_text(
      size = 12
    ),
    axis.text.x = element_text(
      # angle = -45, hjust = 0,
      size = 10),
    # strip.text = element_text(size = 12),
    panel.border = element_rect(
      color = 'black',
      fill = 'transparent'
    ),
    # legend.position = 'right'
    legend.position = 'inside',
    legend.position.inside = 
      c(0.85, 0.2),
    legend.background = 
      element_blank(),
    plot.margin = unit(
      c(5, 5, 5, 5), 
      'mm'
    )
  )


#K----
# K = 7


fig_qPCR_EM_data <- 
  # stat_qPCR_fitted %>% 
  stat_qPCR_data %>%
  mutate(
    PLOT = PLOT %>% 
      factor(
        levels = c(
          'CAOV', 
          'QUAL', 
          'QUBI',
          'PIAB', 'PIST'
        )
      ),
    Species = PLOT %>% 
      fct_recode(
        'Carya\novata' = 'CAOV',
        'Quercus\nalba' = 'QUAL',
        'Quercus\nbicolor' = 'QUBI',
        'Picea\nabies' = 'PIAB',
        'Pinus\nstrobus' = 'PIST'
      )
    #!work
    # PLOT_G.spp = PLOT_G.spp %>% 
    #   fct_reorder(
    #     as.numeric(PLOT)
    #   )
  )
  # stat_qPCR_data
  # qPCRfullQC_EM %>%
  # 
  # left_join(keyPlots, 
  #           by = 'PLOT') %>% 
  # select(!`...1`) %>% 
  # # easy plotting
  # mutate('WEEK' = week(YYYYMMDD))
  # .$PLOT_G.spp


#curves----


# fig_qPCR_EM_data_stat <- 
#   stat_qPCR_curves %>% 
#   filter(
#     !is.na(PLOT_G.spp) |
#       !is.na(LeafHabit)
#   ) %>%
#   left_join(
#     keyPlots, 
#     by = c('PLOT_G.spp')
#   ) %>% 
#   filter(
#     !is.na(PLOT_G.spp)
#   ) %>%
#   
#   mutate(
#     # WEEK = round(WEEK),
#     
#     #QC----
#     'curvesQC' = 
#       curves
#       # ifelse(
#       # PLOT == 'PIST' &
#       #   curves < 0 |
#       #   PLOT == 'PIST' &
#       #   WEEK < 25,
#       # NA,
#       # ifelse(
#       #   PLOT == 'QUAL' &
#       #     WEEK > 45,
#       #   NA,
#       #   curves
#       #   )
#       # )
#   ) %>% 
#   
#   mutate(
#     'curvesQC_y' = ifelse(
#       !is.na(curvesQC),
#       ifelse(
#         curvesQC > 0,
#         sign(curvesQC) * 10^5,
#         sign(curvesQC) * -10^6.7
#       ),
#       NA
#     )
#   ) %>% 
#   
#   filter(
#     !is.na(curvesQC_y)
#   )


# tree_names <- c(
#   'PIST' = 'Pinus\n strobus',
#   'QUAL' = 'Quercus\n alba',
#   'PIAB' = 'Picea\n abies',
#   'QUBI' = 'Quercus\n bicolor',
#   'CAOV' = 'Carya\n ovata'
# ) 


fig_qPCR_EM <- 
  
  fig_qPCR_EM_data %>% 
  
  ggplot(aes(
    y = copies_ECM,
    x = WEEK,
    color = LeafHabit
  )) +
  # geom_point() +
  stat_summary(
    fun.data = ggpubr::median_mad,
    size = 0.75, 
    alpha = 0.75,
    color = 'grey'
    # aes(color = PLOT_G.spp)
  ) +
  stat_smooth(
    n = loess_n,
    se = T,
    linewidth = 3, #2
    method = 'loess',
    # formula = y ~ s(x, k = K),
    linetype = 'solid',
    # color = 'black',
    # aes(color = PLOT_G.spp)#,
    alpha = 0.125#,
    # family = 'poisson',
    # formula = round(y) ~
    #   s(x, k = K) #+ #'tp'=lessFlat?
    # MycoType * LeafHabit + Group
  ) +
  # geom_ribbon(
  #   aes(
  #     x = WEEK,
  #     # y = .fitted,
  #     ymin = .lower_ci,
  #     ymax = .upper_ci
  #   ),
  #   colour = 'grey',
  #   alpha = 0.125
  # ) +
  # !geom_line
  # geom_smooth(
  #   se = F,
  #   aes(
  #     x = WEEK,
  #     y = .fitted
  #   ),
  #   color = 'black',
  #   linewidth = 4
  # ) +
  
  labs(y = 'Soil ECM fungal \n abundance (18S DNA copies)',
       x = 'Time (week of year)', #MM-DD
       color = 'Leaf Habit'
       # color = 'Host tree \nspecies', 
       # shape = 'Host tree species'
  ) +
  # guides(color = guide_legend(
  #   order = 1)) +
  scale_x_continuous(
    breaks = week(
      keySeasonDates
    ),
    limits = c(10, 52)
  ) +
  scale_y_continuous(
    # trans = 'log10',
    # trans = 'identity',
    # labels = scales::trans_format(
    #   trans = 'log10',
    #   format = scales::math_format()
    # ),
    labels = scales::label_comma(
      scale = 1e-6,
      suffix = ' M'
    )#,
    # n.breaks = 4
    # breaks = c(10^8.5, 10^9, 10^9.5)
    # breaks = c(10^9, 10^9.5, 10^9.75)#,
    # breaks = c(10^5, 10^6, 5e6, 10^7)#,
    # limits = c(NA, 10^6.3)
  ) +
  # scale_color_viridis_d() +
  facet_wrap(
    # LeafHabit 
    ~ Species#,
    # scales = 'free_y'
    # labeller = 
    #   as_labeller(
    #     tree_names
    #   )
  ) +
  scale_color_manual(
    values = c(
      'lightgreen',
      'darkgreen'
    )
  ) +
  
  theme_qPCR #+

  # geom_text(
  #   data = fig_qPCR_EM_data_stat,
  #   aes(x = WEEK,
  #       y = curvesQC_y,
  #       label = '*'),
  #   size = 7,
  #   inherit.aes = F
  # )


# write ----


# ggsave(
#   plot = fig_qPCR_EM,
#   here('figs/fig_qPCR_EM.pdf'),
#   width = 10,
#   height = 7,
#   units = 'in'
# )
# ggsave(
#   plot = fig_qPCR_EM,
#   here('figs/fig_qPCR_EM.png'),
#   width = 10,
#   height = 7,
#   units = 'in'
# )


# overall ----

fig_qPCR_EM_overall <-
  
  fig_qPCR_EM_data %>% 
  
  ggplot(aes(
    y = copies_ECM,
    x = WEEK
  )) +
  stat_summary(
    fun.data = ggpubr::median_iqr,
    # fun.data = ggpubr::mean_se_,
    position = position_dodge(width=0.5),
    size = 0.75, 
    alpha = 0.75,
    color = 'grey'
  ) +
  stat_smooth(
    linewidth = 2, 
    color = 'black',
    alpha = 0.125,
    n = loess_n
  ) +
  
  labs(
    y = 'Soil ECM fungal \n abundance (18S DNA copies)',
    x = 'Time (week of year)'
  ) +
  scale_x_continuous(
    breaks = week(keySeasonDates),
    limits = c(10, 52)
  ) +
  scale_y_continuous(
    trans = 'log10',
    labels = scales::trans_format(
      trans = 'log10',
      format = scales::math_format()
    # labels = scales::label_comma(
    #   scale = 1e-6,
    #   suffix = ' M'
    )#,
    # n.breaks = 4,
    # breaks = c(10^8.5, 10^9, 10^9.5)
    # breaks = c(10^9, 10^9.5, 10^9.75)#,
    # limits = c(10, 3e6)
  ) +
  
  theme_qPCR #+
  
  # geom_text(
  #   data = fig_qPCR_EM_data_stat,
  #   aes(x = WEEK,
  #       y = curvesQC_y,
  #       label = '*'),
  #   size = 7
  # )


# join ----

library(ggpubr)
fig_qPCR_weather <- 
  ggpubr::ggarrange(
    ggpubr::ggarrange(
      fig_qPCR_EM_overall,
      figWeather,
      ncol = 2,
      labels = 'auto'
    ),
    fig_qPCR_EM,
    ncol = 1,
    labels = c(' ', 'c')
  )
fig_qPCR_weather


# save ----

ggsave(
  plot = fig_qPCR_weather,
  here('figs/fig_qPCR_EM.pdf'),
  width = 7,
  height = 7,
  units = 'in'
)

ggsave(
  plot = fig_qPCR_weather,
  here('figs/fig_qPCR_EM.png'),
  width = 7,
  height = 7,
  units = 'in'
)

ggsave(
  plot = fig_qPCR_weather,
  here('figs/fig_qPCR_EM.jpg'),
  width = 7,
  height = 7,
  units = 'in'
)



#CVp----


fig_qPCR_CVp <- 
  stat_qPCR_data %>% 
  ungroup() %>%
  dplyr::select(
    Species,
    PLOT,
    SUBPLOT,
    CVp_med
  ) %>% 
  distinct() %>% 
  
  ggplot(aes(
    x = Species,
    y = CVp_med
  )) +
  # geom_point() +
  ggbeeswarm::geom_quasirandom(
    color = 'grey'
  ) +
  geom_violin(
    alpha = 0.125,
    color = 'grey',
    fill = 'grey'
  ) +
  stat_summary(
    fun.data = ggpubr::median_mad
  ) +
  EnvStats::stat_n_text() +
  labs(
    y = 'Annual variation\n coefficient (CVp, %)',
    x = 'Host tree species'
  ) +
  # ggpubr::stat_compare_means(
  #   comparisons = list(
  #     c('CAOV', 'QUBI')
  #   )
  # ) 
  ggpubr::geom_signif(
    comparisons = list(
      c('Carya\novata', 
        'Quercus\nbicolor')
    ),
    map_signif_level = F
  ) +
  
  theme_qPCR 

# 
# ggsave(
#   here('figs/fig_qPCR_CVp.pdf'),
#   fig_qPCR_CVp,
#   width = 10,
#   height = 7,
#   units = 'in'
# )
# ggsave(
#   here('figs/fig_qPCR_CVp.png'),
#   fig_qPCR_CVp,
#   width = 10,
#   height = 7,
#   units = 'in'
# )