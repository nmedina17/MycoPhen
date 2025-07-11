# setup ----


library(here); 
here::i_am('figs/figWeather.R')
# library(tidyverse);

# source(here('data/import.R'))
# weather23QC
# sample_dates


library(ggplot2)


# plots ----


## tseries ----


### moisture ----


# weather_processed_raw %>% 
#   filter(lubridate::year(Datetime) == 2023) %>%
#   ggplot(aes(Datetime, Temp1)) + geom_point()
# 
# 
# weather_processed %>%
#   # filter(lubridate::year(Datetime) == 2023 #&
#   #          # lubridate::month(Datetime) == 1
#   #          ) %>%
#   
#   ggplot(aes(Datetime, Watercontent1)) +
#   facet_wrap(~ Station) +
#   geom_point(size = 0.1) +
#   # scale_y_continuous(sec.axis = Temp1) +
#   geom_vline(xintercept = sample_dates, color = 'brown')
# 
# 
# ### temp ----
# 
# 
# weather_processed %>%
#   filter(Year == 2023) %>%
#   ggplot(aes(Datetime, Temp1)) +
#   geom_point(size = 0.1, color = 'orange') +
#   geom_vline(xintercept = sample_dates, color = 'brown')


### joint ----


figWeather <- weather23QC %>%
  # filter(Year == 2023) %>%
  
  ggplot(aes(x = Datetime, 
             y = Moisture1_avg)) +
  
  #### temp ----

  scale_y_continuous(
    sec.axis = sec_axis(
      transform = ~ . * 50,
      name = 'Soil temperature (C, 10 cm depth)'
    ) 
  ) +
  # geom_point(aes(
  #   Datetime, 
  #   Temp1_avg/50
  # ),
  # color = 'orange', 
  # size = 0.1
  # ) +
  geom_line(aes(
    Datetime, 
    Temp1_avg/50
  ),
  linewidth = 0.5,
  color = 'orange'
  ) +

  #### moisture ----

  # geom_point(size = 0.1, 
  #            color = 'lightblue') +
  geom_line(color = 'lightblue',
            linewidth = 0.5) +
  
  ##### trends ----

  stat_smooth(linewidth = 1,
              color = 'blue') +
  stat_smooth(aes(
    Datetime, 
    Temp1_avg/50
  ), 
  linewidth = 1,
  color = 'darkred'
  ) +
  
  #### sampling ----

  geom_vline(xintercept = sample_dates,
             color = 'grey',
             linetype = 'dashed') +
  
  ### viz ----
  
  labs(x = 'Time (week of year)', 
       y = 'Soil moisture (%, 10 cm depth)') +
  scale_x_continuous(
    # breaks = sample_dates,
    breaks = keySeasonDates %>% 
      as_datetime(),
    # labels = week(sample_dates)
    labels = week(keySeasonDates)
  ) +
  theme_classic() + 
  theme(
    # axis.text.x = element_text(
    #   angle = 45,
    #   hjust = 1
    # ),
    text = element_text(
      size = 12
    ),
    axis.title.y.left = 
      element_text(
        color = 'blue'
      ),
    axis.title.y.right = 
      element_text(
        color = 'orange'
      )
  )


# write ----


ggsave(
  plot = figWeather,
  here('figs/figWeather.pdf')
)
ggsave(
  plot = figWeather,
  here('figs/figWeather.png')
)


#smooths----


### check ----


# weather_rolled %>% 
#   filter(Year == 2023) %>%
#   ggplot(aes(Datetime, Watercontent1)) +
#   geom_line(color = 'lightblue') +
#   geom_point(aes(Datetime, 
#                  Moisture1_14d_avg), 
#              color = 'blue', 
#              size = 0.1) +
#   geom_vline(
#     xintercept = as_datetime(sample_dates),
#     color = 'brown', 
#     linetype = 'dashed')
# 
# 
# ### viz ----
# 
# 
# weather_rolled %>% filter(Year == 2023) %>%
#   ggplot(aes(Datetime, Watercontent1)) +
#   geom_point(color = 'black', size = 0.3) +
#   geom_line(color = 'lightblue') +
#   geom_point(aes(Datetime, Moisture1_14d_avg), color = 'blue', size = 0.1) +
#   geom_vline(xintercept = lubridate::as_datetime(sample_dates),
#              color = 'brown', linetype = 'dashed') +
#   # stat_summary() + #needDateFilter
#   geom_point(data = weather_rolled_sampleDates,
#              aes(Datetime, Moisture1_14d_avg),
#              size = 3)
# 
# 
# 
# ##phase----
# weather_processed %>%
#   filter(Year == 2019) %>%
#   ggplot(aes(Temp1, Watercontent1, color = Datetime)) +
#   geom_point(size = 0.1)
# 
# ##lags----
# 
# weather_processed %>%
#   ggplot(aes(Watercontent1_lastyear, Watercontent1)) +
#   geom_point(size = 0.1, color = 'blue') +
#   theme_bw()
# weather_processed %>%
#   ggplot(aes(Datetime, Watercontent1)) +
#   geom_point(size = 0.1, color = 'blue') +
#   # geom_point(aes(y = Temp1), size = 0.1, color = 'red') +
#   geom_point(aes(Datetime, Watercontent1_lastyear)) +
#   theme_bw()
# weather_processed %>%
#   ggplot(aes(Temp1, Watercontent1, color = lubridate::month(Datetime))) +
#   geom_point(size = 0.1) +
#   theme_bw() + theme(legend.position = 'top') +
#   labs(title = " 'Our flag was still there' ",
#        subtitle = "We went back a different way than we came (hysteresis) \n and got stuck in a flag-polar vortex \n at the Arb in 2019",
#        caption = "Nicholas Medina, Jan 2024") +
#   scale_color_gradient(low = 'blue', high = 'red')


#save----
# weather_processed_raw %>%
#   write.csv(here('data/weather_processed_raw.csv'))
