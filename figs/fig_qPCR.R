# setup ----


library(here)
here::i_am('figs/fig_qPCR.R')


# theme ----


theme_set(#theme_classic() #+
  theme(text = element_text(size = 12))
)


# plot ----


fig_qPCR <- qPCRfullQC %>%
  # filter(`Log copies final` < 8) %>%
  
  ggplot(aes(y = round(10^`Log copies final`),
             x = lubridate::ymd(YYYYMMDD),
             # color = Common#, shape = SUBPLOT
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

# facet_wrap(~ `Myco Type`, drop = T, ncol = 1,
#            strip.position = 'right', scales = 'free_y') +

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
  stat_smooth(se = T, linewidth = 2, #method = 'gam',
            linetype = 'solid', aes(color = Species), alpha = 0.1,
            family = 'poisson'#,
            # formula = round(y) ~
            #   s(x, bs = 'tp', k = K, m = 2) #+ #'tp'=lessFlat?
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
#            scales = 'free_y', #ncol = 1,
#            # strip.position = 'right'
#            ) +

  ##viz----
  labs(y = 'Mycorrhizal fungal biomass proxy (18S DNA copies)',
       x = 'Time (Date)',
       color = 'Host tree \nspecies'#,
       # shape = 'Myco type'
  ) +
  guides(color = guide_legend(order = 1),
         shape = 'none') +
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
  ggbreak::scale_y_break(breaks = c(10^7, 1e9)) +
  scale_color_viridis_d(option = 'turbo') +
  annotate('rect',
           xmin = lubridate::ymd('2023-06-29'),
           # xmax =
           xmax = lubridate::ymd('2023-07-20'),
           ymin = 1e5, ymax = 10^10.3,
           alpha = 0.5, fill = 'lightblue') +
  # scale_color_brewer(palette = 'Paired', direction = 1) +
  # scale_color_manual(values = c('darkgreen', 'tan', 'black', 'brown', 'lightgreen')) +
  annotate('text',
           label = 'EM',
           x = lubridate::as_date('2023-11-01'),
           y = 10^10.5,
           size = 5) +
  annotate('text',
           label = 'AM',
           x = lubridate::as_date('2023-11-01'),
           y = 1e5,
           size = 5) +
  
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # strip.background = element_blank()#,
    # legend.position = c(0.825, 0.88)
    # strip.text = element_text(size = 18),
    panel.border = element_rect(color = 'black',
                                fill = 'transparent')
  ) #+ #theme_bw() +
# theme(strip.background = element_rect(fill = 'white'))


#MIX----
ggpubr::ggarrange(EMplot, AMplot, ncol = 2,
                  labels = c('a', 'b'))


# add in morton-level soil moisture data, bin at weekly intervals
# then, water potential in all plots (-PIST-W)


