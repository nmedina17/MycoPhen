data_qPCR_EM_y2 <- read_sheet('https://docs.google.com/spreadsheets/d/10S9C0zwwEi63OJDsgPewKdOcLMOrF57yQslatRDF7dc/edit?usp=sharing')
data_qPCR_EM_y2 %>% 
  filter(
    str_detect(
      `Sample ID`,
      'BLANK',
      negate = T
    ) &
      str_detect(
        `Sample ID`,
        'None',
        negate = T
      ) &
      str_detect(
        `Sample ID`,
        'MOCK',
        negate = T
      ) &
      str_detect(
        `Sample ID`,
        'CTRL',
        negate = T
      )
  ) %>% 
  mutate(
    PLOT = str_extract(
      `Sample ID`, 
      "\\D+"
    ) %>% 
      as.factor(),
    SESSION = str_extract(
      `Sample ID`, 
      "\\d$"
    ) %>% as.numeric()
  ) %>% 
  rename(
    'Abund_molec_ul' = 
      `Copy number (molecules/ul)`
  ) %>% 
  
  
  # mgcv::gam(
  #   Abund_molec_ul ~
  #     s(
  #       SESSION,
  #       by = PLOT,
  #       k = 7
  #     ),
  #   data = .,
  #   family = 'Gamma',
  #   method = 'REML'
  # ) %>%
  # # gratia::appraise()
  # summary()
  
  
  ggplot(aes(
    x = SESSION,
    y = Abund_molec_ul
  )) +
  geom_point(
    color = 'grey'
  ) +
  stat_summary(
    fun.data = ggpubr::median_mad
  ) +
  facet_wrap(
    ~ PLOT,
    scales = 'free_y'
  ) +
  geom_smooth(
    n = 7
  ) #+ scale_y_log10() 