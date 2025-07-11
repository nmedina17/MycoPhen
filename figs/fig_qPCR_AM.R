# setup ----


library(here); 
i_am('figs/fig_qPCR_AM.R')


# plot ----


AMplot <- qPCRfullQC %>% filter(`Myco Type` == 'AM') %>%
  # filter(`Log copies final` < 8) %>%
  
  ggplot(aes(y = round(10^`Log copies final`),
             x = lubridate::ymd(YYYYMMDD)#,
             # color = Common#, shape = SUBPLOT
             # shape = PLOT
  )) +
  # scale_color_manual(values = c('black', 'blue', 'orange')) +
  # geom_point(#color = 'grey',
  #            aes(shape = SUBPLOT, color = Common), alpha = 0.25) +
  # ggbeeswarm::geom_quasirandom(#color = 'grey',
  #                              aes(shape = SUBPLOT)) +
  
  ##lines----

# # geom_line(color = 'grey',
# #           aes(shape = SUBPLOT)) +
# geom_line(data = qPCRfullQC %>% filter(SUBPLOT == 1),
#           alpha = 0.25,
#           aes(shape = SUBPLOT, color = Common)) +
# geom_line(data = qPCRfullQC %>% filter(SUBPLOT == 2),
#           alpha = 0.25,
#           aes(shape = SUBPLOT, color = Common)) +
# geom_line(data = qPCRfullQC %>% filter(SUBPLOT == 3),
#           alpha = 0.25,
#           aes(shape = SUBPLOT, color = Common)) +
# geom_line(data = qPCRfullQC %>% filter(SUBPLOT == 4),
#           alpha = 0.25,
#           aes(shape = SUBPLOT, color = Common)) +
# geom_line(data = qPCRfullQC %>% filter(SUBPLOT == 5),
#           alpha = 0.25,
#           aes(shape = SUBPLOT, color = Common)) +
# geom_line(data = qPCRfullQC %>% filter(SUBPLOT == 6),
#           alpha = 0.25,
#           aes(shape = SUBPLOT, color = Common)) +

facet_wrap(~ `Myco Type`, drop = T, ncol = 1,
           strip.position = 'right', scales = 'free_y') +
  
  ##bins----
stat_summary(fun.data = ggpubr::mean_se_,
             position = position_dodge(width=0.25),
             size = 0.75, alpha = 0.75,
             aes(color = Species)) +
  
  # EnvStats::stat_n_text(size = 3, y.pos = 8.5) + #ungroup
  # ggplot2::scale_y_log10()
  
  ##trends----
# stat_summary(fun.data = ggpubr::mean_se_,
#            position = position_dodge(width=0.25),
#            geom = 'line', size = 2) +
stat_smooth(se = T, size = 2, method = 'gam',
            linetype = 'solid', aes(color = Species), alpha = 0.1,
            family = 'poisson',
            formula = round(y) ~
              s(x, bs = 'tp', k = K, m = 2) #+ #'tp'=lessFlat?
            # MycoType * LeafHabit + Group
) +
  # stat_smooth(se = F, size = 1.5, method = 'gam', color = 'black',
  #             family = 'poisson', linetype = 'dashed', alpha = 0.1,
  #             formula = round(y) ~
  #               s(x, bs = 'tp', k = K, m = 2) #+ #'tp'=lessFlat?
  #             # MycoType * LeafHabit + Group
  # ) +
  # stat_identity(size = 2, geom = 'line') +
  
  ##PFT----
# facet_wrap(~ SUBPLOT, ncol = 2) +
# facet_grid(`Myco Type` ~ `Leaf Habit`, scales = 'free_y') +

# facet_wrap(~ `Myco Type`, #Group
#            scales = 'free_y',
#            strip.position = 'right') +

##viz----
labs(y = 'Mycorrhizal fungal biomass proxy (18S DNA copies)',
     x = 'Time (Date)',
     color = 'Host tree species', shape = 'Host tree species'
) +
  guides(color = guide_legend(order = 1, position = 'right')) +
  # scale_x_continuous(n.breaks = 9) +
  scale_x_continuous(breaks = sample_dates) +
  # scale_y_continuous(limits = c(8.5, 11)) +
  scale_y_continuous(trans = 'log10',
                     labels = scales::trans_format(
                       trans = 'log10',
                       format = scales::math_format()),
                     n.breaks = 4,
                     # limits = c(10^9.3, 10^10.2),
                     # breaks = c(1e9, 10^9.5, 1e10, 10^10.5)
  ) +
  scale_color_viridis_d() +
  # scale_color_brewer(palette = 'Paired', direction = 1) +
  # scale_color_manual(values = c('darkgreen', 'tan', 'black', 'brown', 'lightgreen')) +
  
  theme_classic() + theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # strip.background = element_blank()#,
    # legend.position = c(0.825, 0.88)
    # legend.position = 'top'
    strip.text = element_text(size = 18),
    panel.border = element_rect(color = 'black',
                                fill = 'transparent'),
    legend.position = 'right'
  ) #+ #theme_bw() +
# theme(strip.background = element_rect(fill = 'white'))