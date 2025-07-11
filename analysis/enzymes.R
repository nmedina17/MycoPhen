library(tidyverse)
#import----
Activity <- read_csv('~/Downloads/activity052224.csv') %>%
  add_case(read_csv('~/Downloads/activity041924.csv')); Activity
ActivityJM <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1wML0Saeq7cTFb80M5pyEu1xDsGd6y_4dO3V9L2vPwHE/edit?usp=sharing') %>% #!.xlsx
  rename('ENZYME' = Enzyme, 'ACTIVITY' = Activity) %>%
  mutate('SPECIES' = str_sub(`Sample ID`, 1, 4),
         'SESSION' = str_extract(`Sample ID`, '\\d$') %>% #(?<=\\.)\\d+
           as.numeric(),
         ENZYME = if_else(ENZYME == 'Perox', 'PEROX',
                          if_else(ENZYME == 'Phenox', 'PHENOX',
                                  ENZYME)))
Key <- read_csv('~/Downloads/activity050924key.csv') %>%
  add_case(read_csv('~/Downloads/activity041924key.csv')) %>%
  add_case(read_csv('~/Downloads/activity052224key.csv')) %>%
  distinct(); Key


Table <- left_join(Key, Activity, by = c('SAMPLEnums')) %>%

  mutate('SESSION' = stringr::str_extract(SAMPLEid, '.[:digit:]$') %>%
           as.numeric(),
         'SUBPLOT' = stringr::str_extract(SAMPLEid, '.[:digit:].') %>%
           stringr::str_extract('\\d\\d'),
         'SPECIES' = stringr::str_extract(SAMPLEid, '[:alpha:]+')) %>%

  na.omit() %>%

  add_row(ActivityJM %>% select(ENZYME, SPECIES, SESSION, ACTIVITY)); Table


keyDates <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1Th50ipxAeV17_t0ysi6l28wsIo28JZFPga_gGrW4quw/edit?usp=sharing',
                                      sheet = 'Dates')


#merge----
Table1 <- Table %>% mutate(SESSION = as.numeric(SESSION)) %>%
  full_join(keyDates, by = c('SESSION' = 'Session')) %>%

  filter(!is.na(SPECIES) & ACTIVITY < 10
         ) %>%
  mutate(SPECIES = as.factor(SPECIES),
         SUBPLOT = as.factor(SUBPLOT),
         'WEEK' = lubridate::week(YYYYMMDD),
         ENZYME = as.factor(ENZYME),
         'SEASON' = if_else(
           lubridate::month(YYYYMMDD) %in% c(3:5), 'Spring',
           if_else(
             lubridate::month(YYYYMMDD) %in% c(6:8), 'Summer',
             if_else(
               lubridate::month(YYYYMMDD) %in% c(9:11), 'Fall',
               'Winter'))) %>%
           as.factor()
         ))


Table %>%
  filter(SUBPLOT == '01' & SPECIES == 'QUAL') %>%
  select(SESSION, ENZYME, ACTIVITY) %>%
  pivot_wider(names_from = 'ENZYME', values_from = 'ACTIVITY') %>%
  unnest() %>%
  arrange(SESSION) %>% select(!SESSION) %>% as.ts() %>%
  decompose() %>% plot() #%>%


#plot----
Table1 %>%
  ggplot(aes(lubridate::ymd(YYYYMMDD), ACTIVITY,
             color = forcats::fct_reorder(SPECIES, ACTIVITY))) +
  ggbeeswarm::geom_quasirandom(color = 'gray'#,
                               # aes(shape = SUBPLOT)
                               ) +
  # geom_line(data = Table %>% filter(SUBPLOT == '01'), aes(shape = SUBPLOT)) +
  # geom_line(data = Table %>% filter(SUBPLOT == '02'), aes(shape = SUBPLOT)) +
  # geom_line(data = Table %>% filter(SUBPLOT == '03'), aes(shape = SUBPLOT)) +
  # geom_line(data = Table %>% filter(SUBPLOT == '04'), aes(shape = SUBPLOT)) +
  # geom_line(data = Table %>% filter(SUBPLOT == '05'), aes(shape = SUBPLOT)) +
  # geom_line(data = Table %>% filter(SUBPLOT == '06'), aes(shape = SUBPLOT)) +
  stat_summary(size = 0.5, fun.data = ggpubr::median_mad) +
  stat_smooth(#method = 'gam',
              #formula = y ~ s(x, k=5),
              alpha = 0.125
              ) +
  scale_color_viridis_d() +
  # facet_grid(SPECIES ~ ENZYME, scales = 'free_y') +
  facet_wrap(~ ENZYME, scales = 'free_y') +
  # scale_y_continuous(limits = c(NA, 4.5)) +
  # EnvStats::stat_n_text(size = 2) +
  labs(x = 'Time (Date)',
       y = expression(paste(
         'Potential soil enzyme activity (g soil'^-1, ' hr'^-1, ')')),
       color = 'Host tree \nspecies'
       )+
  theme_classic() +
  theme(strip.background = element_blank(),
        legend.position = 'inside',
        legend.justification = c(0.85, 0.15))


#stats----
modE <- gamm4::gamm4(
  data = Table1,
  formula = ACTIVITY ~
    s(WEEK,
      k = 7,
      by = ENZYME) +
    SPECIES,
  random = ~ (1 | SUBPLOT),
  family = 'Gamma')
modE$gam %>% gratia::concrvity()
modE$gam %>% mgcv::gam.check()
modE$gam %>% gratia::appraise()
modE$gam %>% mgcv::anova.gam()

modEseason <- lme4::glmer(
  data = Table1,
  formula = ACTIVITY ~
    SEASON +
    SPECIES +
    (1 | ENZYME / SUBPLOT),
  family = 'Gamma'
)
# par(mfrow = c(2,2)); plot(modEseason); #par(mfrow = c(1,1))
modEseason %>% summary()


#phaseSetup----
Table1sum <- Table1 %>%
  pivot_wider(names_from = ENZYME, values_from = ACTIVITY) %>%
  mutate('BG' = BG,
         'NAG' = NAG,
         'AP' = AP,
         'BG_NAG' = BG / NAG,
         'BG_AP' = BG / AP) %>%

  group_by(YYYYMMDD, SPECIES) %>%
  # mutate(across(c(BG, NAG, AP, PHENOX, PEROX), ~ mean(.x)))
  mutate('BG_mean' = mean(BG),
         'BG_se' = sd(BG)/sqrt(n()),
         'NAG_mean' = mean(NAG),
         'NAG_se' = sd(NAG)/sqrt(n()),
         'AP_mean' = mean(AP),
         'AP_se' = sd(AP)/sqrt(n()),
         'PEROX_mean' = mean(PEROX),
         'PEROX_se' = sd(PEROX)/sqrt(n()),
         'PHENOX_mean' = mean(PHENOX),
         'PHENOX_se' = sd(PHENOX)/sqrt(n()),

         'BG_NAG_mean' = mean(BG_NAG),
         'BG_NAG_se' = sd(BG_NAG)/sqrt(n()),
         'BG_AP_mean' = mean(BG_AP),
         'BG_AP_se' = sd(BG_AP)/sqrt(n())
         ) %>%
  ungroup()

Table1sum_PIAB <- Table1sum %>%
  filter(SPECIES == 'PIAB')
Table1sum_QUAL <- Table1sum %>%
  filter(SPECIES == 'QUAL')


#phasePIAB----


phasePIAB <- Table1sum_PIAB %>%

  ggplot(aes(BG_NAG_mean, BG_AP_mean,
             color = as.factor(YYYYMMDD)#,
             # shape = as.factor(YYYYMMDD)
             )
         ) +
  theme_classic() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.85, 0.45),
        # legend.position.inside = c(0.465, 0.9),
        # legend.direction = 'horizontal',
        legend.background = element_blank()
        ) +
  # geom_segment(xend = c(tail(Table1sum_PIAB$BG_NAG_mean, n = -1), NA),
  #              yend = c(tail(Table1sum_PIAB$BG_AP_mean, n = -1), NA),
  #              linewidth = 1.5#,
  #              # color = 'black',
  #              # arrow = arrow()
  # ) +
  geom_segment(x = 3.15, y = 0.79, xend = 2.5, yend = 0.81,
               linewidth = 1.5, color = 'black'#,
               # arrow = arrow(type = 'closed')
               ) +
  geom_segment(x = 2.5, y = 0.81, xend = 2.95, yend = 1.06,
               linewidth = 1.5, color = 'black'#,
               # arrow = arrow(type = 'closed')
               ) +
  geom_segment(x = 2.95, y = 1.06, xend = 1.9, yend = 0.74,
               linewidth = 1.5, color = 'black'
               ) +
  geom_segment(x = 1.9, y = 0.74, xend = 2.125, yend = 0.775,
               linewidth = 1.5, color = 'black'
               ) +
  # geom_line(linewidth = 1.25,
  #           color = 'black'
  #           ) +
  geom_point(size = 5) +
  geom_errorbarh(aes(xmin = BG_NAG_mean - BG_NAG_se,
                     xmax = BG_NAG_mean + BG_NAG_se)) +
  geom_errorbar(aes(ymin = BG_AP_mean - BG_AP_se,
                    ymax = BG_AP_mean + BG_AP_se)) +
  labs(
    x = 'Soil N demand (BG:NAG activity)',
      # expression(paste(
      # 'Soil N demand (BG:NAG enzyme activity ratio g soil'^-1,
      # ' hr'^-1, ')')),
    y = 'Soil P demand (BG:AP activity)',
      # expression(paste(
      # 'Soil P demand (BG:AP enzyme activity g soil'^-1,
      # ' hr'^-1, ')')),
    color = 'Sample date'
    ) +
  annotate('text', label = expression(italic('Picea \nabies')),
           x = 2.25, y = 1.15, size = 5) +
  # annotate('text', label = 'PIAB', x = 3.5, y = 1.2, size = 5) +
    # guides(shape = 'none') +
  # facet_wrap(~ SPECIES) +
  scale_color_viridis_d(option = 'turbo')


#stats2----
modE2 <- gamm4::gamm4(
  data = Table1sum,
  formula = BG_AP ~
    s(WEEK,
      k = 7) +
    SPECIES,
  random = ~ (1 | SUBPLOT),
  family = 'Gamma')
modE2$gam %>% gratia::concrvity()
modE2$gam %>% mgcv::gam.check()
modE2$gam %>% gratia::appraise()
modE2$gam %>% mgcv::anova.gam()


#phaseQUAL----


phaseQUAL <- Table1sum_QUAL %>%

  ggplot(aes(BG_NAG_mean, BG_AP_mean,
             color = as.factor(YYYYMMDD)#,
             # shape = as.factor(YYYYMMDD)
  )
  ) +
  theme_classic() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.9, 0.75),
        # legend.position.inside = c(0.465, 0.9),
        # legend.direction = 'horizontal',
        legend.background = element_blank()
  ) +
  # geom_segment(xend = c(tail(Table1sum_PIAB$BG_NAG_mean, n = -1), NA),
  #              yend = c(tail(Table1sum_PIAB$BG_AP_mean, n = -1), NA),
  #              linewidth = 1.5#,
  #              # color = 'black',
  #              # arrow = arrow()
  # ) +
  geom_segment(x = 2.1, y = 0.7, xend = 1.9, yend = 0.61,
               linewidth = 1.5, color = 'black'#,
               # arrow = arrow(type = 'closed')
  ) +
  geom_segment(x = 1.9, y = 0.61, xend = 2.25, yend = 0.59,
               linewidth = 1.5, color = 'black'#,
               # arrow = arrow(type = 'closed')
  ) +
  geom_segment(x = 2.25, y = 0.59, xend = 2.6, yend = 0.61,
               linewidth = 1.5, color = 'black'
  ) +
  # geom_segment(x = 1.9, y = 0.74, xend = 2.125, yend = 0.775,
  #              linewidth = 1.5, color = 'black'
  # ) +
  # geom_line(linewidth = 1.25,
  #           color = 'black'
  #           ) +
  geom_point(size = 5) +
  geom_errorbarh(aes(xmin = BG_NAG_mean - BG_NAG_se,
                     xmax = BG_NAG_mean + BG_NAG_se)) +
  geom_errorbar(aes(ymin = BG_AP_mean - BG_AP_se,
                    ymax = BG_AP_mean + BG_AP_se)) +
  labs(
    x = 'Soil N demand (BG:NAG activity)',
      # expression(paste(
      # 'Soil N demand (BG:NAG enzyme activity g soil'^-1,
      # ' hr'^-1, ')')),
    y = 'Soil P demand (BG:AP activity)',
      # expression(paste(
      # 'Soil P demand (BG:AP enzyme activity g soil'^-1,
      # ' hr'^-1, ')')),
    color = 'Sample date'
  ) +
  annotate('text', label = expression(italic('Quercus \nalba')),
           x = 2.6, y = 0.75, size = 5) +
  # annotate('text', label = 'PIAB', x = 3.5, y = 1.2, size = 5) +
  # guides(shape = 'none') +
  # facet_wrap(~ SPECIES) +
  scale_color_viridis_d(option = 'turbo')


#phaseJoin----
ggpubr::ggarrange(phasePIAB, phaseQUAL,
                  # labels = c('A', 'B'),
                  common.legend = T, legend = 'right')


#subset----

table <- Table %>%
  group_by(ENZYME, SESSION) %>%
  slice_sample(n = 3)

ggplot(table, aes(SESSION, ACTIVITY)) +
  ggbeeswarm::geom_quasirandom(color = 'gray', aes(shape = SUBPLOT)) +
  stat_summary(color = 'black') +
  facet_wrap(~ ENZYME, scales = 'free_y') +
  EnvStats::stat_n_text(size = 3) +
  theme_bw()


#ratios----

Table %>%
  pivot_wider(names_from = 'ENZYME', values_from = 'ACTIVITY') %>%
  mutate('BG:NAG' = BG / NAG,
         'BG:AP' = BG / AP) %>%

  ggplot(aes(log1p(`BG:NAG`), log1p(`BG:AP`),
             color = as.numeric(SESSION))) +
  geom_point() + #geom_line() +
  facet_wrap(~ SPECIES) +
  theme_bw()
  # stat_summary()
