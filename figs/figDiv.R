# setup ----


library(here); 
here::i_am('figs/figDiv.R')

# source(here('data/import.R'))
# keyDates

# source(here('analysis/commEM.R'))
# ASVsITSrarFull_EMFfilt_richDiv

library(ggplot2)
library(ggpubr)


theme_Div <- 
  theme_classic() +
  # theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(
      # angle = -45, hjust = 0,
      size = 10),
    strip.text = element_text(size = 12),
    panel.border = element_rect(
      color = 'black',
      fill = 'transparent'
    ),
    # legend.position = 'right'
    # legend.position = 'inside',
    # legend.position.inside = c(0.85, 0.2),
    # legend.background = element_blank(),
    plot.margin = unit(c(5, 5, 5, 5), 'mm')
  )


rich_color <- 'black'; 
div_color <- 'gray'; 
rich_div_scale <- 20

loess_n <- 7


# figDiv ----


figDiv <- 
  ASVsITSrarFull_EMFfilt_richDiv %>%
  
  na.omit() %>% 
  
  left_join(keyPlots) %>% 
  
  ggplot(
    aes(
      week(YYYYMMDD), 
      rich,
      color = LeafHabit,
      shape = LeafHabit
    )
  ) +
  # ggbeeswarm::geom_quasirandom(aes(YYYYMMDD, rich / 40), color = 'gray') +
  
  stat_summary(
    fun.data = ggpubr::median_mad, 
    size = 0.75,
    alpha = 0.75,
    color = 'grey'
    # color = rich_color
  ) +
  stat_smooth(
    alpha = 0.125,
    linewidth = 2,
    n = loess_n
    # color = rich_color
  ) + #color = 'darkred'
  
  # stat_summary(
  #   aes(
  #     y = div * 
  #       rich_div_scale
  #   ),
  #   fun.data = ggpubr::median_mad,
  #   color = div_color
  # ) +
  # stat_smooth(
  #   aes(
  #     y = div * 
  #       rich_div_scale
  #   ),
  #   alpha = 0.125, 
  #   color = div_color
  # ) +
  
  # facet_wrap(
  #   # ~ Group,
  #   ~ PLOT_G.spp,
  #   # ~ LeafHabit,
  #   scales = 'fixed'
  # ) +
  theme_classic() +
  labs(
    x = 'Time (week of year)', 
    y = 'Soil ECM fungal species \nrichness (ASV count)',
    # color = 'Metric'
    color = 'Leaf Habit',
    shape = 'Leaf Habit'
  ) +
  scale_color_manual(
    values = c(
      'Deciduous' = 'lightgreen',
      'Evergreen' = 'darkgreen'
    )
  ) +
  scale_x_continuous(
    breaks = week(
      keySeasonDates
    ),
    limits = c(10, 52)
  ) +
  
  theme_Div +
  
  # scale_y_continuous(
  #   sec.axis = sec_axis(
  #     ~ . / rich_div_scale,
  #   name = 'Diversity (Shannon)')
  # ) +
  
  theme(
    text = element_text(size = 14),
    # text = element_text(size = 12),
    axis.title.y.left = 
      element_text(color = rich_color),
    axis.title.y.right = 
      element_text(color = div_color),
    legend.position = 'top'
    # legend.position = 'inside',
    # legend.position.inside =
    #   # c(0.2, 0.9)
    #   c(0.9, 0.2)
  ) +
  
  annotate(
    'text',
    label =
      'Dev. expl. = 45.6%
    WEEK: EDF = 1, P = 0.194
    WEEK(Deciduous): EDF = 3.3, P = 0.019 *
    WEEK(Evergreen): EDF = 0.001, P = 0.144
    tensor(Moist,Temp): EDF = 5.2, P = 0.032 *
    Leaf habit: P < 0.001 ***',
    x = 40,
    y = 57
  )


figDiv_plot <- 
  figDiv +
  facet_wrap(
    ~ PLOT_G.spp,
    scales = 'fixed'
  ) 


# write ----


ggsave(
  plot = figDiv,
  height = 7,
  width = 10,
  here('figs/figDiv.pdf')
)
ggsave(
  plot = figDiv,
  height = 7,
  width = 10,
  here('figs/figDiv.png')
)
ggsave(
  plot = figDiv,
  height = 7,
  width = 10,
  here('figs/figDiv.jpg')
)


# PFT ----


rich_div_scale2 <- 2

figDiv_Leaf <- 
  ASVsITSrarFull_EMFfilt_richDiv %>% 
  
  na.omit() %>% 
  
  left_join(keyPlots) %>% 
  
  ggplot(aes(LeafHabit, rich)) +
  
  ggbeeswarm::geom_quasirandom(
    color = 'gray'
  ) +
  
  stat_summary(
    fun.data = ggpubr::median_mad,
    color = rich_color) +
  
  # stat_summary(aes(
  #   y = rich * rich_div_scale2
  # ),
  # fun.data = ggpubr::median_mad,
  # color = div_color#,
  # # position = position_dodge(width = 1)
  # ) +
  
  labs(y = 'Richness (species count)',
       x = 'Leaf habit') + 
  
  # scale_y_continuous(
  #   sec.axis = sec_axis(
  #     ~ . / rich_div_scale,
  #     name = 'Diversity (Shannon)')) +
  
  theme_Div +
  
  theme(
    axis.title.y.left = 
      element_text(color = rich_color),
    axis.title.y.right = 
      element_text(color = div_color)
  ) +
  
  # annotate('*')
  
  annotate(
    'text',
    label = 
      'Dev. expl. = 45.4% 
    Leaf habit: P < 0.001 ***',
    x = 2,
    y = 80
  ) 


figDiv_Plot <- 
  ASVsITSrarFull_EMFfilt_richDiv %>% 
  
  na.omit() %>% 
  
  left_join(keyPlots) %>% 
  
  ggplot(aes(PLOT_G.spp, rich)) +
  
  stat_summary(
    fun.data = ggpubr::median_mad,
    color = rich_color) +
  
  # stat_summary(aes(
  #   y = rich * rich_div_scale2
  # ),
  # fun.data = ggpubr::median_mad,
  # color = div_color#,
  # # position = position_dodge(width = 1)
  # ) +
  
  labs(y = 'Richness (species count)',
       x = 'Leaf habit') + 
  
  # scale_y_continuous(
  #   sec.axis = sec_axis(
  #     ~ . / rich_div_scale,
  #     name = 'Diversity (Shannon)')) +
  
  theme_Div #+
  
  # theme(
  #   axis.title.y.left = 
  #     element_text(color = rich_color),
  #   axis.title.y.right = 
  #     element_text(color = div_color)
  # ) #+

  # annotate('*')


# ggpubr::ggarrange(
#   figDiv,
#   figDiv_Leaf,
#   ncol = 1
# )
