# setup ----


library(here); 
here::i_am('stats/statET.R')

# source(
#   here('data/import.R')
# )

# source(
#   here('analysis/commEM.R')
# )
source(
  here('stats/statDiv.R')
)
# ASVsITSrarFull_EMFfiltExploreSum

library(lubridate)
library(gamm4)
library(gratia)
theme_set(theme_void())

library(furrr)
# future_map()
library(tictoc)

library(emmeans)


statET_data <- 
  ASVsITSrarFull_EMFfiltExploreSum %>%
  
  na.omit() %>%
  
  ungroup() %>%
  mutate(
    PLOT_G.spp = 
      as.factor(PLOT_G.spp),
    LeafHabit = 
      as.ordered(LeafHabit),
    ET = as.ordered(ET),
    'LeafHabiit_ET' = paste(
      LeafHabit, 
      ET, 
      sep = '_'
    ) %>% 
      factor()
  )


#full----


K = 7

tictoc::tic() #tw/nb=2h
#w/o t2, by=ET = 20sec!
gam_ET0 <-
  statET_data %>%

  # filter(
  #   !c(
  #     ET %in% 
  #       c(
  #         'Unknown',
  #         'Mat'
  #       )
  #     )
  # ) %>%
  
  mutate(
    ET = 
      factor(
        ET,
        ordered = F
      ),
    LeafHabit =
      factor(
        LeafHabit,
        ordered = F
      )
  ) %>% 

  #~bam
  gam(
    formula =
      abund_sum + 1 ~

      s(
        WEEK,
        # m = 1,
        # by = LeafHabit:Ectomycorrhiza_exploration_type_template,
        by = ET,
        # by = LeafHabiit_ET,
        k = K
      ) +
      # s(
      #   WEEK,
      #   m = 1,
      #   by = LeafHabit,
      #   k = K
      # ) +
      # s(
      #   WEEK,
      #   m = 1,
      #   by = PLOT_G.spp,
      #   k = K
      # ) +

      te(
        Temp1_7d_avg,
        Moisture1_7d_avg,
        # by = ET,
        # by = LeafHabiit_ET, #2slow
        k = K
      ) +
      # t2(LeafHabit,
      #    Ectomycorrhiza_exploration_type_template,
      #    k = 3) +

      ET +
      LeafHabit +
      # LeafHabiit_ET +
      # LeafHabit : ET +
      # LeafHabit * Ectomycorrhiza_exploration_type_template +

      # ET : PLOT_G.spp +
      # PLOT_G.spp +

      s(
        SUBPLOT,#_unique,
        bs = 're'
      ),
      # LeafHabit,
      # PLOT_G.spp,
    # family = 'poisson' #bad-no
    # family = negbin(1), #no
    family = nb() #0.328
    # family = tw() #1.717 #still long
    # select = T,
    # random = ~ (
    #   1 |
    #     # PLOT_G.spp /
    #     SUBPLOT_unique
    # )
  ); tictoc::toc() #%>%


# saveRDS(
#   gam_ET0,
#   here('stats/gam_ET0.rds')
# )


# gam_ET0 <- readRDS(
#   here('stats/gam_ET0.rds')
# )


gam_ET0 %>%
  # .$mer %>%
  # AIC()
  # .$gam %>%
  # gratia::appraise() #OK
  # gratia::draw()

  gratia::fitted_values(
    data = ASVsITSrarFull_EMFfiltExploreSum
  ) %>% 
  ggplot(aes(
    WEEK,
    .fitted,
    shape = LeafHabit
  )) +
  stat_summary(
    fun.data = ggpubr::mean_se_,
    data = ASVsITSrarFull_EMFfiltExploreSum,
    aes(WEEK, abund_sum),
    color = 'grey'
  ) +
  stat_summary(
    fun.data = ggpubr::median_mad
  ) +
  facet_wrap(
    ~ ET,
    scales = 'free_y'
  ) 
  

gam_ET0 %>% 
# gam.check() # !OK but OK?
# car::Anova()
summary()



# gam_ET0 %>% 
#   as_tibble(.name_repair = 'unique') %>% 
#   tidygam::predict_gam() #%>% 
#   plot.tidygam()


# library(emmeans)
# # emmeans(
# #   gam_ET0$mer, 
# #   pairwise ~ Ectomycorrhiza_exploration_type_template#, 
# #   # re.form = NULL
# # ) #%>% 
# # emmeans::pairwise.emmc()
# 
# 
# library(lmerTest)
# ASVsITSrarFull_EMFfiltExploreSum %>%
#   
#   na.omit() %>%
#   
#   mutate(
#     PLOT_G.spp = as.factor(PLOT_G.spp)
#   ) %>% 
#   
#   glmer(
#     formula =
#       abund_sum ~
#       WEEK +
#       Ectomycorrhiza_exploration_type_template +
#       # LeafHabit +
#       PLOT_G.spp +
#       (1 | SUBPLOT_unique),
#     family = negbin(0.3)
#   ) #%>% 
# # residuals() %>%
# # qqnorm()
# # DHARMa::testResiduals()
# # summary()
# # car::Anova()
# # sjPlot::tab_model()
# # AIC()
# # performance::r2()
# 
# 
### purrr ----
# 
# 
# # ADDED LeafHabit to IMPROVE MODEL FIT
# 
# gam_ET <-
#   ASVsITSrarFull_EMFfiltExploreSum %>%
#   
#   na.omit() %>% 
#   
#   mutate(SUBPLOT = as.factor(SUBPLOT),
#          'WEEK' = week(YYYYMMDD)) %>%
#   ungroup() %>%
#   
#   nest('ETdata' = !Ectomycorrhiza_exploration_type_template) %>% #.$Gdata %>% .[[1]]
#   
#   filter(Ectomycorrhiza_exploration_type_template != 'unknown') %>% 
#   
#   mutate(
#     'gGAM' = ETdata %>%
#       furrr::future_imap(
#         ~ gamm4::gamm4(
#           data = .x,
#           # family = 'poisson', #better
#           family = negbin(0.1),
#           # family = 'Gamma', #neg
#           random = ~ (
#             1 | 
#               # PLOT_G.spp /
#               SUBPLOT_unique
#           ),
#           formula =
#             # abund_sum_prop ~
#             abund_sum ~
#             LeafHabit +
#             # PLOT_G.spp +
#             s(WEEK,
#               by = LeafHabit,
#               # by = PLOT_G.spp,
#               k = 7)
#         ) %>% 
#           .$gam
#       ),
#     'gGAM_k_p' = gGAM %>% 
#       modify(
#         ~ .x %>% 
#           # .$gam %>% 
#           k.check() %>% 
#           .[,'p-value']
#       ),
#     'gGAM_R2' = gGAM %>% 
#       modify(
#         ~ .x %>% 
#           summary() %>% 
#           .$r.sq
#       ),
#     'gGAM_derivCurves' = gGAM %>% 
#       modify(
#         ~ .x %>% 
#           gratia::derivatives() %>% 
#           mutate(
#             'curves' = 
#               .derivative %>%
#               sign() %>% 
#               diff() %>% 
#               c(.,NA)
#           )
#       ),
#     'gGAM_p' = gGAM %>% 
#       modify(
#         ~ .x %>% 
#           # .$gam %>% 
#           summary() %>%
#           .$s.table %>% 
#           .[4] # !work w/ 2 fixed terms
#       ),
#     'gGAM_edf' = gGAM %>% 
#       modify(
#         ~ .x %>% 
#           # .$gam %>% 
#           summary() %>%
#           .$s.table %>% 
#           .[1]
#       )
#   ) %>% 
#   unnest(c(
#     gGAM_k_p,
#     gGAM_R2,
#     gGAM_p, 
#     gGAM_edf
#   )) %>% 
#   arrange(Ectomycorrhiza_exploration_type_template)
# 
# gam_ET %>%
#   unnest(gGAM_derivCurves) %>% 
#   filter(curves != 0)
# 
# 
# gam_ET$gGAM[[1]]$gam %>% 
#   # gam.check() # OK
#   # broom::tidy()
#   summary()
# gam_ET$gGAM[[2]]$gam %>% 
#   # gam.check() # OK
#   summary()
# gam_ET$gGAM[[3]]$gam %>% 
#   # gam.check() # OK
#   summary()
# gam_ET$gGAM[[4]]$gam %>% 
#   # gam.check() # OK
#   summary()
# gam_ET$gGAM[[5]]$gam %>% 
#   # gam.check() # OK
#   summary()
# 
# gam_ET$gGAM[[6]]$gam %>% 
#   gam.check() # NOT OK
# # summary()
# 
# gam_ET$gGAM[[7]]$gam %>% 
#   # gam.check() # OK
#   summary()
# 
# gam_ET$gGAM[[8]]$gam %>% 
#   gam.check() # NOT OK
# # summary()
# 


# sep ----


# par(mfrow = c(2,2)) #gratia
library(gamm4)


## func ----


run_gam_structure <- 
  function(
    data,
    response,
    # by_factor,
    # by_factor2,
    family_used
  ) {
    library(mgcv)
    
    response <-
      substitute(response)
    formula_string <-
      paste(
        deparse(response),
        "~
        
        s(WEEK,
          k = 7
        ) +
        s(WEEK,
          by = LeafHabit,
          m = 1,
          k = 7
        ) +
        s(WEEK,
          by = PLOT_G.spp,
          m = 1,
          k = 7
        ) +
        
        s(SUBPLOT_unique,
          bs = 're') + 
          
        te(
          Moisture1_7d_avg,
          Temp1_7d_avg,
          k = 7
        ) +
        t2(
          Moisture1_7d_avg,
          Temp1_7d_avg,
          by = LeafHabit,
          m = 1,
          full = F,
          k = 7
        ) +
        t2(
          Moisture1_7d_avg,
          Temp1_7d_avg,
          by = PLOT_G.spp,
          m = 1,
          full = F,
          k = 7
        ) + 
        
        LeafHabit +
        PLOT_G.spp"#,
        # deparse(by_factor)
      )
    formula_used <- 
      formula_string %>% 
      as.formula()
    
    model <- data %>% 
      gam(
        formula = 
          formula_used,
        method = 'REML',
        family = 
          family_used
      )
    
    
    return(model)
  }


get_inflection_points <- 
  function(model) {
    model %>% 
      gratia::derivatives(
        's(WEEK)',
        partial_match = T
      ) %>% 
      mutate(
        'curves' = 
          .derivative %>%
          sign() %>% 
          diff() %>% 
          c(.,NA)
      ) %>% 
      select(
        WEEK,
        LeafHabit,
        PLOT_G.spp,
        curves
      ) %>% 
      filter(curves != 0)
  }


get_emmeans_table <- 
  function(
    model, 
    factor = c(
      'LeafHabit', 
      'PLOT_G.spp'
    ), 
    contrast_method = "pairwise", 
    adjust_method = "tukey", 
    set_infer = TRUE
  ) {
  model %>% 
    emmeans(
      specs = factor
    ) %>% 
    contrast(
      method = contrast_method, 
      adjust = adjust_method
    ) %>% 
    summary(
      infer = c(
        set_infer, 
        set_infer
      )
    ) %>%
    as.data.frame() %>%
    rename_with(
      ~ gsub(" ", "_", .x)
    )  # clean column names
}


# gamm4 func not work yet

run_gamm4_structure <- 
  function(
    data,
    response,
    K = 7,
    by_factor,
    family_used
  ) {
    
    
    by_factor <- 
      substitute(by_factor)
    formula_string <-
      paste(
        response,
        "~
        s(WEEK,
          k = ",
        K,
        ") +
        s(WEEK,
          by = ",
        deparse(by_factor),
        ",
          m = 1,
          full = TRUE,
          k = ",
        K,
        ") +
        t2(
          Moisture1_7d_avg,
          Temp1_7d_avg,
          k = ",
        K,
        ") +
        t2(
          Moisture1_7d_avg,
          Temp1_7d_avg,
          by = ",
        deparse(by_factor),
        ",
          m = 1,
          full = TRUE,
          k = ",
        K,
        ") +",
        deparse(by_factor)
      )
    formula_used <- 
      formula_string %>% 
      as.formula()
    
    model <- data %>%
      gamm4(
        formula =
          formula_used,
        family =
          family_used,
        random = ~ (
          1 |
            SUBPLOT_unique
        )
      )
    
    
    return(model)
  }


# long ----

# 
long_dist <-
  ASVsITSrarFull_EMFfiltExploreSum %>%
  filter(ET == 'Long-dist.')


statET_long <-
  long_dist %>%
  run_gam_structure(
    response = abund_sum,
    family_used = tw()
    #nb(theta = 0.654)
    #tw=1.705
  )
# statET_long %>% 
#   # gratia::appraise()
#   # gratia::draw()
#   # gratia::concrvity() 
#   #implication of high?
#   #acknowledge keven li!
#   # car::Anova() #factor-pvals
#   summary() #dev.expl.
# 
#   
# statET_long_curves <- 
#   statET_long %>% 
#   get_inflection_points() %>% 
#   mutate(
#     'ET' = 'Long-dist.'
#   )
# 
# statET_long_comparisons <-
#   statET_long %>% 
#   get_emmeans_table()
# 
# 
# # contact ----
# 
# 
# contact <-
#   ASVsITSrarFull_EMFfiltExploreSum %>% 
#   filter(ET == 'Contact')
# 
# statET_contact <- 
#   contact %>% 
#   run_gam_structure(
#     response = abund_sum,
#     family_used = tw()
#   )
# statET_contact %>% 
#   # gratia::appraise()
#   #tw(theta = 1.668)
#   # car::Anova() #factor-pvals
#   summary() #dev.expl.
# 
#   
# statET_contact_curves <- 
#   statET_contact %>% 
#   get_inflection_points() %>% 
#   mutate(
#     'ET' = 'Contact'
#   )
# 
# statET_contact_comparisons <- 
#   statET_contact %>% 
#   get_emmeans_table()
# 
# 
# # medF ----
# 
# 
# medFringe <- 
#   ASVsITSrarFull_EMFfiltExploreSum %>% 
#   filter(
#     ET == 
#       'Medium-dist. \n fringe'
#   )
# 
# statET_medFringe <- 
#   medFringe %>% 
#   run_gam_structure(
#     response = abund_sum,
#     family_used = nb() 
#     #nb=0.585 
#     #tw=1.764 #wider error
#   )
#    
# statET_medFringe %>% 
#   # gratia::draw()
#   # gratia::appraise() #negbin/tw
#   # gam.check()
#   # car::Anova() #factor-pvals
#   summary()
# 
# statET_medFringe_curves <- 
#   statET_medFringe %>% 
#   get_inflection_points() %>% 
#   mutate(
#     'ET' = 
#       'Medium-dist. \n fringe'
#   )
# 
# statET_medFringe_comparisons <- 
#   statET_medFringe %>% 
#   get_emmeans_table()
# 
# 
# # medS ----
# 
# 
# medSmooth <- 
#   ASVsITSrarFull_EMFfiltExploreSum %>% 
#   filter(
#     ET == 
#       'Medium-dist. \n smooth'
#   )
# 
# statET_medSmooth <- 
#   medSmooth %>% 
#   run_gam_structure(
#     response = abund_sum,
#     family_used = tw() 
#     #nb=1.001 #worse
#     #tw=1.554
#   )
# 
# statET_medSmooth %>% 
#   # .$mer %>% AIC()
#   # gratia::draw()
#   # gratia::appraise()
#   # gam.check()
#   # car::Anova() #factor-pvals
#   summary()
# 
# statET_medSmooth_curves <- 
#   statET_medSmooth %>% 
#   get_inflection_points() %>% 
#   mutate(
#     'ET' = 
#       'Medium-dist. \n smooth'
#   )
# 
# statET_medSmooth_comparisons <-
#   statET_medSmooth %>% 
#   get_emmeans_table()
# 
# 
# # shortD ----
# 
# 
# shortDelicate <- 
#   ASVsITSrarFull_EMFfiltExploreSum %>% 
#   filter(
#     ET == 
#       'Short-dist. \n delicate'
#   ) 
# 
# statET_shortDelicate <-
#   shortDelicate %>%
#   run_gam_structure(
#     response = abund_sum,
#     family_used = tw()
#     #nb=1.397 #worse
#     #tw=1.572
#   )
# 
# statET_shortDelicate %>% 
#   # .$mer %>% AIC()
#   # gratia::draw()
#   # gratia::appraise()
#   # gam.check()
#   # car::Anova() #factor-pvals
#   summary()
# 
# statET_shortDelicate_curves <- 
#   statET_shortDelicate %>% 
#   get_inflection_points() %>% 
#   mutate(
#     'ET' = 
#       'Short-dist. \n delicate'
#   )
# 
# statET_shortDelicate_comparisons <- 
#   statET_shortDelicate %>% 
#   get_emmeans_table()
# 
# 
# # shortC ----
# 
# 
# shortCoarse <- 
#   ASVsITSrarFull_EMFfiltExploreSum %>% 
#   filter(
#     ET == 
#       'Short-dist. \n coarse'
#   ) 
# 
# statET_shortCoarse <- 
#   shortCoarse %>%
#   run_gam_structure(
#     response = abund_sum,
#     family_used = tw()
#     #nb=0.868
#     #tw=1.654
#   )
# 
# statET_shortCoarse %>% 
#   # .$mer %>% AIC()
#   # gratia::draw()
#   # gratia::appraise()
#   # gam.check()
#   # car::Anova() #factor-pvals
#   summary()
# 
# statET_shortCoarse_curves <- 
#   statET_shortCoarse %>% 
#   get_inflection_points() %>% 
#   mutate(
#     'ET' = 
#       'Short-dist. \n coarse'
#   )
# 
# statET_shortCoarse_comparisons <- 
#   statET_shortCoarse %>% 
#   get_emmeans_table()



#CVp----


statET_CVp_data <-
  
  ASVsITSrarFull_EMFfiltExploreSum %>% 
  
  mutate(
    'CVp_med' = 
      mad(abund_sum) / 
      median(abund_sum)
  ) 


statET_CVp <-
  
  statET_CVp_data %>% 
  
  glmer(
    formula = 
      CVp_med ~
      
      #overall fx
      LeafHabit +
      PLOT_G.spp +
      
      #random
      (
        1 |
          SUBPLOT_unique
      ),
    
    # family = 'poisson' #
    family = 'Gamma'
  )


statET_CVp %>%
  performance::check_model()