library(here)
here::i_am('stats/statBRMS.R')

source(
  here('analysis/commEM.R')
)

library(brms)
library(tictoc)


#data----

data_test <- 
  ASVsITSrarFull_EMFfiltExploreSum %>% 
  #output
  mutate(
    'ET' = Ectomycorrhiza_exploration_type_template %>% 
      fct_reorder(
        as.numeric(Ectomycorrhiza_exploration_type_template),
        .na_rm = T
      ),
  ) %>% 
  filter(
    !c(
      ET %in%
        c(
          'Unknown',
          'Mat'
        )
    )
  ) %>% 
  mutate(
    'ET_PLOT_G.spp' = paste(
      ET, 
      PLOT_G.spp, 
      sep = '_'
    ) %>% 
      factor(),
    ET_PLOT_G.spp = 
      ET_PLOT_G.spp %>%
      fct_reorder(
        as.numeric(ET_PLOT_G.spp)
      ),
    'ET_LeafHabit' = paste(
      ET, 
      LeafHabit, 
      sep = '_'
    ) %>% 
      factor(),
    ET_LeafHabit = 
      ET_LeafHabit %>% 
      fct_reorder(
        as.numeric(ET_LeafHabit)
      )
    # across(
    #   where(
    #     is.numeric
    #   ),
    #   scale
    # )
  )
  
  # pivot_wider(
  #   # cols = c(
  #   #   tarsus, 
  #   #   back
  #   # ),
  #   names_from = 
  #     Ectomycorrhiza_exploration_type_template,
  #   values_from = abund_sum,
  #   values_fill = 0
  # ) 


#formula----

K = 7

bform1 <- 
  bf(
    abund_sum ~ 
      
      #overall fx
      LeafHabit + 
      PLOT_G.spp +
      s(
        WEEK,
        k = K
      ) +
      t2(
        Temp1_7d_avg,
        Moisture1_7d_avg,
        k = K
      ) +
      
      #genus-level fixed fx
      0 +
      ET +
      LeafHabit : ET +
      PLOT_G.spp : ET +
      
      #genus-level smooths
      
      s(
        WEEK,
        by = ET,
        k = K,
        m = 1
      ) +
      s(
        WEEK,
        by = LeafHabit,
        k = K,
        m = 1
      ) +
      s(
        WEEK,
        by = PLOT_G.spp,
        k = K,
        m = 1
      ) +
      # Plot-level deviations per Genus
      #2complex
      # s(
      #   WEEK,
      #   factor = ET_PLOT_G.spp,
      #   bs = 'fs',
      #   m = 1
      # ) +
      # s(
      #   WEEK, 
      #   by = ET_LeafHabit,
      #   m = 1
      # ) +
      
      #RhatOKw/fixed priors
      t2(
        Temp1_7d_avg,
        Moisture1_7d_avg,
        by = ET,
        k = K,
        m = 1
      ) +
      t2(
        Temp1_7d_avg,
        Moisture1_7d_avg,
        by = LeafHabit,
        k = K,
        m = 1
      ) +
      t2(
        Temp1_7d_avg,
        Moisture1_7d_avg,
        by = PLOT_G.spp,
        k = K,
        m = 1
      ) +
        # Plot-level deviations per Genus
      #2complex
      # t2(
      #   Temp1_7d_avg,
      #   Moisture1_7d_avg,
      #   factor = ET_PLOT_G.spp,
      #   bs = 'fs',
      #   m = 1
      # ) +
      # t2(
      #   Temp1_7d_avg,
      #   Moisture1_7d_avg,
      #   by = ET_LeafHabit,
      #   m = 1
      # ) +
      
      #random
      (
        1 |
          # p |
          SUBPLOT_unique
      ) +
      
      #group-level ints/slopes
      (
        1 |
          #partial-pooling
          #for fixed/linear fx
          # q | 
          ET
      ) #+
    #only1plot-each
      # (
      #   1 |
      #     # s | #partial-pooling
      #     PLOT_G.spp
      # )
  )


#fit----

tic()
fit1 <- 
  brm(
    formula = bform1, 
    data = 
      data_test, 
    family = negbinomial(),
    
    #fixedFX=speed!
    prior = set_prior(
      "normal(0, 1)", 
      class = "b"
    ),
    
    iter = 1000,
    # warmup = 1000,
    # control = list(
    #   adapt_delta = 0.8, #>0.8
    #   max_treedepth = 10 #>10
    # ),
    backend = "cmdstanr",
    #optional,recommended
    
    #per-chain
    #need 'cmdstanr' backend
    # threads = threading(3), #experimental!
    
    #6x=45min-16h
    #2x=2h
    chains = 3, #total
    # threads*chains < cores
    cores = 3 #total
  ); toc()


#save----
saveRDS(
  fit1, 
  here('stats/fit1.rds')
)


#validity----

##R-hat<1.01----
fit1 %>% 
  rhat() %>%
  bayesplot::mcmc_rhat()

rhats <- rhat(fit1)
high_rhat <- sort(
  rhats[rhats > 1.01], 
  decreasing = T
); high_rhat
#n_eff>1000
plot(fit1)


##sim==obs----
fit1 %>% 
  pp_check() # Density overlay
  # pp_check(
  #   type = "boxplot"
  # )   # Grouped summaries
  # pp_check(
  # type = "scatter_avg"
  # )  # Means vs. observed

  ##resids----
  # library(DHARMa)
  # simulateResiduals(plot = T) 
  #!work


#result----


##tbl----

fit1 #summary

library(broom.mixed)
tidy(fit1)

##sum>chains----

# draws_sum_across_chains <- 
#   fit1 %>%
#   as_draws_df() %>% 
#   pivot_longer(
#     cols = - .chain, 
#     names_to = "parameter", 
#     values_to = "value"
#   ) %>%
#   group_by(.chain, parameter) %>%
#   summarise(
#     median = median(value),
#     sd = sd(value),
#     .groups = "drop"
#   )

library(parameters) #better
stat_table <- 
  fit1 %>%
  # draws_sum_across_chains %>% 
  model_parameters(
    ci = 0.95, 
    exponentiate = T, #fam
    test = "pd" #prob.dir.
  ) %>% 
  
  filter(
    str_detect(
      Parameter, 
      '1$' #chain_1only
    )
  ) %>% 
  
  ##pval----
  mutate(
    sig = case_when(
      pd >= 0.99 ~ "***",
      pd >= 0.95 ~ "**",
      pd >= 0.90 ~ "*",
      TRUE ~ ""
    ),
    note = ifelse(
      pd < 0.9, 
      "uncertain", 
      "likely"
    ),
    across(
      where(
        is.numeric
      ),
      ~ round(., 3)
    )
  ) 

library(flextable)
stat_table %>% 
  as_grouped_data('Component') %>% 
  flextable() %>% 
  bold(
    i = ~ pd >= 0.9, 
    bold = T
  ) %>% 
  autofit() %>% 
  theme_booktabs()


posterior_summary(fit1)


#hypoth----
# hypothesis(
#   fit1, 
#   "LeafHabitEvergreen = 0"
# )


##fx----


conditional_smooths(
  fit1, 
  effects = 
    "WEEK"
  # "Temp1_7d_avg:Moisture1_7d_avg"#, 
  # conditions = 
  #   list(
  #     Ectomycorrhiza_exploration_type_template = 
  #       "Contact"
  #   )
)



library(tidybayes)

plot_smooth_response <- 
  function(
    model,
    smooth_var,
    group_var = NULL,
    data = NULL,
    resolution = 100,
    center_other_predictors = TRUE,
    include_re = FALSE,
    ci_level = 0.95
  ) {
    require(tidybayes)
    require(modelr)
    
    if (is.null(data)) data <- 
        model$data
    
    # Generate grid over smooth_var (+ optional group_var)
    grid_vars <- c(
      smooth_var, 
      group_var
    )
    
    newdata <- data_grid(
      data, 
      .size = resolution, 
      !!!rlang::syms(grid_vars)
    )
    
    # Optionally center other numeric predictors not in the grid
    if (center_other_predictors) {
      fixed_vars <- setdiff(
        names(data), 
        grid_vars
      )
      numeric_vars <- 
        fixed_vars[sapply(data[fixed_vars], is.numeric)]
      newdata[numeric_vars] <- 
        lapply(
          numeric_vars, 
          function(var) {
            rep(
              mean(
                data[[var]], 
                na.rm = TRUE
              ), 
              nrow(newdata)
            )
          })
    }
    
    # Get expected predictions on response scale
    draws <- model %>%
      add_epred_draws(
        newdata = newdata,
        re_formula = if (include_re) NULL else NA,
        dpar = NULL,
        .prob = ci_level
      )
    
    # Plot
    p <- ggplot(
      draws, 
      aes_string(
        x = smooth_var, 
        y = ".epred")
    ) +
      stat_lineribbon(
        aes(y = .epred), 
        .width = ci_level, 
        fill = "#3182bd", 
        alpha = 0.3
      ) +
      stat_summary(
        fun = mean, 
        geom = "line", 
        color = "#08519c", 
        size = 1.1
      ) +
      labs(
        title = paste(
          "Smooth effect of", 
          smooth_var, 
          "on response"
        ),
        y = "Predicted response",
        x = smooth_var
      ) +
      theme_minimal()
    
    if (!is.null(group_var)) {
      p <- p + 
        facet_wrap(
          as.formula(
            paste(
              "~", 
              group_var
            )
          )
        )
    }
    
    return(p)
  }

plot_smooth_response(
  fit1, 
  smooth_var = "WEEK", 
  group_var = "ET"
)