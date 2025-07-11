library(dplyr); library(ggplot2)
raw <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1RNkgzmEYRdCPf8z2msgD8tzXFubtY4rBcgKTvPP4lFk/edit?usp=sharing')


rawQCsum <- raw %>%
  tidyr::unnest(tidyr::everything()) %>%
  filter(Total_Hyphae_calc < 90) %>%

  group_by(Date) %>%
  mutate(DATE = lubridate::as_date(Date) %>% lubridate::date(),
         'Avail' = Total_calc + NONFUNGI,
         'Rhizomorphs_sum' = sum(Total_Rhizo_calc),
         'Hyphae_sum' = sum(Total_Hyphae_calc)) #%>%


maxRhizomorphs <- max(rawQCsum$Rhizomorphs_sum)
maxHyphae <- max(rawQCsum$Hyphae_sum)


rawQCprop <- rawQCsum %>% ungroup() %>%
  mutate('Rhizomorphs' = Rhizomorphs_sum / maxRhizomorphs,
         'Hyphae' = Hyphae_sum / maxHyphae) %>%

  tidyr::pivot_longer(c(Rhizomorphs, Hyphae),
                      names_to = 'ORGAN', values_to = 'EMF_extramat') %>%
  mutate(ORGAN = factor(ORGAN, levels = c('Rhizomorphs', 'Hyphae')),
         DEPTH = ifelse(Window == 1, '0-10 cm', Window)) %>%

  filter(!is.na(Tube) &
           !c(Tube %in% c(613)) &
           !is.na(Window) & Window == 1) #%>% #613


rawQCprop %>%
  ggplot(aes(x = lubridate::ymd(DATE),
             y = EMF_extramat,
             color = ORGAN ,
             # shape = as.character(Tube)
             )) +
  geom_point(size = 2, alpha = 0.5) +
  geom_line(size = 2, linewidth = 1, alpha = 0.5) +
  # stat_summary(fun.data = ggpubr::mean_se_, size = 1) +
  # stat_summary(fun.data = ggpubr::mean_se_, size = 2, geom = 'line') +

  ##lines----
  # geom_line(data = rawQC %>% filter(Tube == 611)) +
  # # geom_line(data = rawQC %>% filter(Tube == 613)) +
  # geom_line(data = rawQC %>% filter(Tube == 615)) +
  # geom_line(data = rawQC %>% filter(Tube == 616)) +
  # geom_line(data = rawQC %>% filter(Tube == 617)) +
  # geom_line(data = rawQC %>% filter(Tube == 618)) +
  # geom_line(data = rawQC %>% filter(Tube == 619)) +
  # geom_line(data = rawQC %>% filter(Tube == 620)) +

  ##trends----
  stat_smooth(linewidth = 5, se = T) +

  ##viz----
  scale_color_manual(values = c('tan', 'darkgrey')) +
  # scale_linetype_manual(values = c('solid', 'dash')) +
  labs(y = 'Ectomycorrhizal fungal standing crop (relative timing of presence)',
       x = 'Time (Date)', color = 'Organ',
       title = 'Norway Spruce') +
  scale_x_date(breaks = lubridate::as_date(unique(rawQCprop$Date))) +
  # facet_grid(DEPTH ~ Tube) +
  # facet_wrap(~ DEPTH, strip.position = 'right') +
  # EnvStats::stat_n_text(size = 2) +
  theme_classic() + theme(
    legend.position.inside = c(0.25, 0.2),
    plot.title = element_text(hjust = 0.5, color = 'black', size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1)
    )

# raw %>%
#   mutate('Total2' = Total2_calib_raw_counter,
#          'Total_prop' = Total2 / 90) %>%
#   #group_by(Date) %>% mutate(Total2_sum = median(Total2))
#   ggplot(aes(x = Date, y = Total_prop)) + geom_point(color = 'grey') + #geom_line() +
#   stat_summary(fun.data = ggpubr::median_mad)


#KP----

rawKP <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1RNkgzmEYRdCPf8z2msgD8tzXFubtY4rBcgKTvPP4lFk/edit?usp=sharing',
                                   sheet = 'Kelsey') #%>% na.omit()
rawNM <- filter(raw, Tube == 615 & Window == 1)
rawNM_QC <- rawQCprop %>% filter(Tube == 615 & Window == 1)


rawNM %>%
  ggplot(aes(Date, RHIZOONLY)) +
  scale_x_datetime(date_breaks = '2 weeks') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  stat_identity() + geom_line() + #stat_smooth() +
  stat_identity(data = rawKP,
                aes(DATE, RHIZOMORPHS),
                color = 'orange') +
  geom_line(data = rawKP,
                aes(DATE, RHIZOMORPHS),
                color = 'orange') +
  # stat_smooth(data = rawKP, #method = 'gam',
  #               aes(DATE, RHIZOMORPHS),
  #               color = 'orange') +#
  # stat_identity(data = rawNM_QC,
  #               aes(Date, Total_Rhizo_calc),
  #               color = 'purple') +
  # stat_smooth(data = rawNM_QC,
  #             aes(Date, Total_Rhizo_calc),
  #             color = 'purple')
  # stat_identity(data = rawQCprop %>% filter(Tube == 616 & Window == 1),
  #               aes(Date, Total_Rhizo_calc),
  #               color = 'yellow') +
  # stat_smooth(data = rawQCprop %>% filter(Tube == 616 & Window == 1),
  #             aes(Date, Total_Rhizo_calc),
  #             color = 'yellow') +
  # stat_identity(data = raw %>% filter(Tube == 616 & Window == 1),
  #               # aes(Date, Total_Rhizo_calc),
  #               color = 'yellow') +
  # stat_smooth(data = raw %>% filter(Tube == 616 & Window == 1),
  #             # aes(Date, Total_Rhizo_calc),
  #             color = 'yellow')


plot(rawNM %>% .$RHIZOONLY %>% na.omit() %>% as.vector() %>% .[1:17],
     rawKP$RHIZOMORPHS)
cor(rawNM %>% .$RHIZOONLY %>% na.omit() %>% as.vector() %>% .[1:17],
    rawKP$RHIZOMORPHS)
cor.test(rawNM %>% .$RHIZOONLY %>% na.omit() %>% as.vector() %>% .[1:17],
    rawKP$RHIZOMORPHS)

plot(filter(rawQCprop, Tube == 615 & Window == 1) %>% .$Total_Rhizo_calc %>%
       na.omit() %>% as.vector() %>% .[1:17],
     rawKP$HYPHAE)
cor(filter(raw, Tube == 615 & Window == 1) %>% .$Total_Rhizo_calc %>%
      na.omit() %>% as.vector() %>% .[1:17],
    rawKP$HYPHAE)
cor.test(filter(raw, Tube == 615 & Window == 1) %>% .$Total_Rhizo_calc %>%
      na.omit() %>% as.vector() %>% .[1:17],
    rawKP$HYPHAE)

rawKP %>%
  left_join(rawNM %>%
              mutate(RHIZOMORPHS = RHIZOONLY),
            by = c('DATE' = 'Date')) %>%
  pivot_longer(c(RHIZOMORPHS.x, RHIZOMORPHS.y)) %>%
  mutate(name = if_else(name == 'RHIZOMORPHS.x', 'KP', 'NM')) %>%

  ggplot(aes(DATE, value, color = name)) + geom_point() + geom_line() +
  labs(y = 'RHIZOMORPHS')


#CALIB----
raw; rawKP
rawKPv2 <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1RNkgzmEYRdCPf8z2msgD8tzXFubtY4rBcgKTvPP4lFk/edit?usp=sharing',
                                   sheet = 'Kelsey_V2') %>%
  tidyr::unnest() %>% mutate(TUBE = stringr::str_extract(TUBE, '\\d+'),
                             RHIZOONLY = RHIZOMORPHS, HyphOnly = HYPHAE,
                             Date = lubridate::as_datetime(DATE),
                             RHIZOMORPHS = RHIZOONLY + SHARED) #na.omit()
raw %>% filter(Tube %in% c(615, 616) & Window < 4 & is.na(NONFUNGI)) %>%
  mutate(RHIZOMORPHS = RHIZOONLY + SHARED) %>%

  ggplot(aes(Date, RHIZOMORPHS)) + geom_point() + geom_line() + geom_smooth() +
  scale_x_datetime(date_breaks = '2 weeks') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  geom_point(data = rawKPv2, color = 'green') +
  geom_line(data = rawKPv2, color = 'green') +
  geom_smooth(data = rawKPv2, color = 'green') +
  facet_wrap(~ Window, ncol = 1)


ggplot(aes()) +
  geom_point(color = 'green') + #OR 'Treatment'
  stat_summary(fun.data = ggpubr::median_mad) +
  theme_classic() +
  theme(text = element_text(size = 18))
