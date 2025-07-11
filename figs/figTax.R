# setup ----


library(here); 
here::i_am('figs/figTax.R')

# source(here('data/import.R'))
# ASVsITSrarFull_EM_QC

# source(here('analysis/commEM.R'))
# ASVsITSrarFull_EMFfiltSum_mids

library(ggplot2)
library(ggpubr)

# theme_set(
#   theme(
#     text = element_text(
#       size = 14
#     )
#   )
# )


##fig----


ASVsITSrarFull_EMFfiltSum_mids <- 
  
  ASVsITSrarFull_EMFfiltSum %>%
  
  filter(
    Genus %in% 
      # topEMFgenera |
      # Genus %in% 
      # choiceEMFgenera
      c('Clavulina')
  ) %>%
  
  group_by(
    YYYYMMDD, 
    PLOT_G.spp, 
    LeafHabit,
    Genus
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
      mad(abund_sum_prop)
  ) %>% 
  mutate(
    Genus = Genus %>% 
      fct_reorder(
        # abund_sum_med,
        Genus,
        # .fun = NULL,
        .desc = T
      )#,
    # Genus = Genus %>%
    #   factor(
    #     levels = c(
    #       'Amphinema',
    #       'Wilcoxina',
    #       'Tuber',
    # 
    #       'Cenococcum',
    #       'Lactarius',
    #       'Cortinarius',
    # 
    #       'Russula',
    #       'Tomentella',
    #       'Elaphomyces',
    # 
    #       'Inocybe',
    #       'Hymenogaster',
    #       'Hygrophorus',
    # 
    # 
    #       'Piloderma',
    #       'Sebacina',
    #       'Scleroderma',
    # 
    #       'Suillus',
    #       'Rhizopogon',
    #       'Boletus',
    # 
    #       'Amanita',
    #       'Helvella'
    # 
    # 
    #     )
    #   )
  ) %>% 
  
  mutate(
    WEEK = week(YYYYMMDD)
  ) %>% 
  group_by(
    Genus,
    LeafHabit,
    PLOT_G.spp,
    WEEK
  ) %>%
  filter(
    !all(
      abund_sum_med == 0
    ) #&
      # n_distinct(
      #   WEEK
      # ) >= 7
  ) %>%
  ungroup() 


loess_n <- 7


figTax <- 
  ASVsITSrarFull_EMFfiltSum_mids %>% 
  
  # filter(
  #   Genus %in% 
  #     c(
  #       'Amanita',
  #       'Clavulina'
  #     )
  # ) %>%
  
  ggplot(aes(
    week(YYYYMMDD), 
    abund_sum_med + 1,
    shape = LeafHabit,
    color = LeafHabit
    # color = fct_reorder(
    #   Genus, 
    #   abund_sum_prop_med, 
    #   .desc = T)
    # shape = as.factor(SUBPLOT)
  )) +
  # geom_point() + #color = 'gray'
  stat_summary(
    fun.data =
      ggpubr::median_mad,
    color = 'grey',
    position =
      position_dodge(
        width = 1
      )
  ) +
  # scale_y_log10() +
  scale_y_continuous(
    trans = 'log10',
    # labels = scales::trans_format(
    #   trans = 'log10',
    #   # trans = 'identity',
    #   format = scales::math_format()
    # ),
    limits = c(1, 10000)
  ) +
  # geom_errorbar(aes(ymin = abund_sum_prop_med - abund_sum_prop_mad,
  #                   ymax = abund_sum_prop_med + abund_sum_prop_mad),
  #               color = 'gray') +
  # geom_line() +
  geom_smooth(
    # color = 'black',
    alpha = 0.125,
    linewidth = 2,
    n = loess_n,
    se = F
  ) + #method = 'gam', formula = y ~ s(x, k = 7)
  # facet_wrap(~ PLOT_G.spp) +
  facet_wrap(
    ~ Genus,
    scales = 'fixed'#,
    # ncol = 3
  ) +
  theme_classic() +
  labs(
    # color = 'Genus',
    color = 'Leaf Habit',
    shape = 'Leaf Habit',
    x = 'Time (week of year)', 
    y = 'Soil ECM fungal \n abundance (ITS2 rDNA reads)') +
  # scale_colour_viridis_d(option = 'turbo') +
  # scale_x_datetime(
  #   breaks = keyDates$YYYYMMDD, 
  #   labels = keyDates$MMDD) +
  scale_x_continuous(
    breaks = week(
      keySeasonDates
    ),
    limits = c(10, 52)
  ) +
  scale_color_manual(
    values = c(
      'lightgreen', 
      'darkgreen'
    )
  ) +
  theme(
    # text = element_text(size = 18),
    text = element_text(
      size = 12
    ),
    axis.text.x = element_text(
      # angle = -45, 
      # hjust = 0, 
      # vjust = 1,
      size = 10
    ),
    # legend.position = 'inside',
    legend.position = 'top',
    legend.background = element_blank(),
    # legend.position.inside = c(0.75, 0.15),
    # strip.background = element_blank(),
    plot.margin = unit(c(5, 5, 5, 5), 'mm')
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = 1
      )
    ),
    shape = guide_legend(
      override.aes = list(
        size = 1
      )
    )
  ) 


# ggsave(
#   plot = figTax,
#   height = 7,
#   width = 10,
#   here('figs/figTax.pdf')
# )
# ggsave(
#   plot = figTax,
#   height = 7,
#   width = 10,
#   here('figs/figTax.jpg')
# )
# ggsave(
#   plot = figTax,
#   height = 7,
#   width = 10,
#   here('figs/figTax.png')
# )

# ggsave(
#   plot = figTax,
#   height = 10,
#   width = 7,
#   here('figs/figTax_talk.jpg')
# )


figTax_plot <- 
  figTax +
  aes(
    color = PLOT_G.spp,
    shape = LeafHabit
  ) +
  # scale_color_manual(
  #   values = c(
  #     'red',
  #     'orange', 
  #     'green',
  #     'blue',
  #     'black'
  #   )
  # ) +
  scale_color_viridis_d(
    option = 'turbo'
  ) +
  facet_wrap(
    ~ Genus,
    scales = 'free_y'
  ) +
  labs(
    color = 'Tree species',
    shape = 'Leaf habit',
    x = 'Time (week of year)', 
    y = 'Soil ECM fungal \n abundance (ITS2 rDNA reads)'
  ) +
  guides(
    color = guide_legend(
      nrow = 3
    ),
    shape = guide_legend(
      nrow = 2
    )
  ) 


# write ----


ggsave(
  plot = figTax_plot,
  height = 7,
  width = 10,
  here('figs/figTax_plot.pdf')
)
ggsave(
  plot = figTax_plot,
  height = 7,
  width = 10,
  here('figs/figTax_plot.jpg')
)


# all ----


figTax_all <- 
  ASVsITSrarFull_EMFfiltSum_mids %>% 
  #filter(abund_sum_med == 0) #check
  
  ggplot(aes(
    week(YYYYMMDD), 
    abund_sum_med + 1,
    color = fct_reorder(
      Genus, 
      abund_sum_prop_med, 
      .desc = T)
    # shape = as.factor(SUBPLOT)
  )) +
  # geom_point() + #color = 'gray'
  # scale_y_log10() +
  # ggbeeswarm::geom_quasirandom(
  #   color = 'gray'
  # ) +
  stat_summary(
    fun.data = ggpubr::median_mad,
    # color = 'gray'
    alpha = 0.25,
    position = position_dodge(
      width = 0.5
    )
  ) +
  stat_summary(
    fun.data = ggpubr::median_mad,
    # color = 'gray'
    alpha = 0.5,
    position = position_dodge(
      width = 0.5
    ),
    geom = 'line'
  ) +
  scale_y_continuous(
    trans = 'log10',
    labels = scales::trans_format(
      trans = 'log10',
      format = scales::math_format()
    )
  ) +
  # geom_errorbar(
  #   aes(
  #     ymin = abund_sum_med - 
  #       abund_sum_mad,
  #     ymax = abund_sum_med + 
  #       abund_sum_mad
  #   ),
  #   color = 'gray'
  # ) +
  # geom_line() +
  # geom_smooth(
  #   se = F,
  #   n = loess_n,
  #   linewidth = 2,
  #   alpha = 0.0575
  # ) + #method = 'gam', formula = y ~ s(x, k = 7)
  # facet_wrap(~ PLOT_G.spp) +
  theme_classic() +
  labs(color = 'Genus',
       x = 'Time (week of year)', 
       y = 'Soil ECM fungal \nabundance (ITS2 rDNA reads)') +
  scale_colour_viridis_d(option = 'turbo') +
  # scale_x_datetime(
  #   breaks = keyDates$YYYYMMDD, 
  #   labels = keyDates$MMDD) +
  theme(
    # text = element_text(size = 18),
    axis.text.x = element_text(
      # angle = -45, 
      # hjust = 0, 
      # vjust = 1,
      size = 12
    ),
    # legend.position = 'inside', 
    # legend.background = element_blank(),
    # legend.position = 'none',
    # legend.position.inside = c(0.85, 0.17),
    plot.margin = unit(c(5, 5, 5, 5), 'mm')
  )


# ASV variation ----

topASV <- 
  ASVsITSrarFull_EM_QC %>%
  
  filter(
    Genus %in% 
      c(
        'Russula', 
        'Tuber'#,
        # 'Sebacina',
        # 'Cortinarius',
        # 'Tomentella'
      )
  ) %>% 
  
  ## topASVs ----

  group_by(Genus, 
           Species, 
           asv_id) %>%
  summarize('abund_sum' = 
              sum(abund)) %>% 
  
  nest(data = !Genus) %>%
  mutate(
    data = data %>% 
      modify(
        ~ .x %>%
          # filter(!is.na(Species)) %>% 
          arrange(
            desc(abund_sum)
          ) %>% 
          
          filter(
            !all(
              abund_sum == 0
            )
          ) %>%
          
          head(3)
      )
  ) %>% 
  unnest(data) 


figTax_topASV <- 
  ASVsITSrarFull_EM_QC %>% 
  filter(
    asv_id %in% 
      topASV$asv_id 
  ) %>%
  
  mutate(
    'G.spp' = paste(
      Genus,
      Species,
      sep = ' '
    ),
    'WEEK' = week(YYYYMMDD)
  ) %>%
  
  ## sum subplots ----

  group_by(
    WEEK, 
    Genus, 
    LeafHabit,
    PLOT_G.spp,
    asv_id,
    G.spp
  ) %>%
  summarize(
    'abund_sum' = sum(abund),
    .groups = 'keep'
  ) %>%
  
  filter(
    !all(
      abund_sum == 0
    )
  ) %>%
  
  mutate(
    'G.spp_label' = 
      fct_relevel(
        G.spp,
        "Russula parazurea", 
        "Russula recondita", 
        "Russula hortensis",
        "Tuber mexiusanum", 
        "Tuber rufum",
        "Tuber NA"
      )
  ) %>% 
  
  
  ggplot(aes(
    WEEK, 
    abund_sum + 1,
    # color = fct_reorder(
    #   G.spp,
    #   # abund_sum,
    #   G.spp,
    #   .desc = F
    # )#,
    color = G.spp_label
    # shape = fct_reorder(
    #   G.spp,
    #   # abund_sum,
    #   G.spp,
    #   .desc = F
    # )#,
    # shape =
    #   as.factor(LeafHabit)
  )) +
  # geom_point() +
  stat_summary(
    fun.data =
      ggpubr::median_mad#,
    # position =
    #   position_dodge(
    #     width = 1
    #   ),
    # color = 'grey'
  ) +
  labs(
    x = 'Time (week of year)',
    y = 'Soil ECM fungal \nabundance (ITS2 DNA reads)',
    color = 'Fungal \nspecies'#,
    # shape = 'Host tree \nleaf habit'
  ) +
  # scale_y_log10() +
  scale_y_continuous(
    trans = 'log10',
    labels = scales::trans_format(
      trans = 'log10',
      format = scales::math_format()
    ),
    n.breaks = 4,
  ) +
  geom_smooth(
    se = F,
    n = loess_n,
    linewidth = 3,
    alpha = 0.0575
  ) +
  theme_classic() +
  theme(
    # legend.position = 'inside',
    # legend.position.inside = c(0.85, 0.25)
    legend.position = 'top',
    text = element_text(
      size = 18
    )
  ) +
  scale_color_viridis_d(
    option = 'turbo'
  ) +
  facet_wrap(
    ~ Genus, 
    dir = 'h'
  ) +
  guides(
    color = guide_legend(
      nrow = 3#,
      # byrow = F
    )
  ) 


# ggsave(
#   plot = figTax_topASV,
#   height = 7,
#   width = 10,
#   here('figs/figTax_topASV.pdf')
# )
# ggsave(
#   plot = figTax_topASV,
#   height = 7,
#   width = 10,
#   here('figs/figTax_topASV.png')
# )
# ggsave(
#   plot = figTax_topASV,
#   height = 7,
#   width = 10,
#   here('figs/figTax_topASV.jpg')
# )



# merge ----

ggpubr::ggarrange(
  ggpubr::ggarrange(
    figTax_all,
    figTax_topASV,
    # label = c('A', 'B'),
    nrow = 1
  ),
  figTax,
  # label = 'AUTO'
  nrow = 2
)


ggpubr::ggarrange(
  figTax_all,
  figTax_topASV,
  label = c('A', 'B'),
  nrow = 2
) 


# leaf habit ----

figTax_leafHabit <- 
  ASVsITSrarFull_EMFfiltSum_mids %>% 
  
  left_join(keyPlots) %>%
  filter(
    Genus %in% 
      c(
        'Russula', 
        'Amphinema'
      )
  ) %>%
  
  ggplot(aes(
    LeafHabit,
    # week(YYYYMMDD),
    abund_sum_med + 1,
    color = as.factor(Genus)
  )) +
  ggbeeswarm::geom_quasirandom() +
  stat_summary(
    fun.data = ggpubr::median_mad
  ) +
  labs(
    color = 'Genus',
    x = 'Leaf habit',
    y = 'Soil ECM fungal abundance (ITS2 rDNA reads)'
  ) +
  theme_classic() +
  scale_y_log10() +
  scale_color_manual(
    values = c(
      'darkblue', 
      'darkred'
    )
  )


# trial ----

# ASVsITSrarFull_EMFfiltSum_mids %>%
# 
#   ggplot(aes(
#     week(YYYYMMDD),
#     abund_sum_med + 1,
#     fill = fct_reorder(
#       Genus,
#       abund_sum_med,
#       .desc = T
#     )
#   )) +
#   geom_bar(stat = 'identity') +
#   labs(
#     y = 'Soil ECM fungal abundace (ITS2 rDNA reads)',
#     x = 'Week of Year',
#     fill = 'Genus'
#   ) +
#   facet_wrap(~ PLOT_G.spp) +
#   scale_fill_viridis_d(option = 'turbo') +
#   theme(
#     legend.position = 'inside',
#     legend.position.inside = c(0.85, 0.25)
#   ) +
#   scale_y_continuous(
#     trans = 'log10',
#     labels = scales::trans_format(
#       trans = 'log10',
#       format = scales::math_format()
#     )
#   )