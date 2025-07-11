# setup ----


library(here)
i_am('analysis/run_meco_netcomp.R')

library(microeco)
library(meconetcomp)
library(magrittr) #%<>%


run_meco_netcomp <- function(
    
  
    microtable,
    
    SET_PLOT = NULL,
    SET_SESSION = NULL,
    
    SET_FILTER_THRES = 0.0005,
    
    # COR_p_thres represents the p value threshold
    SET_COR_P_THRES = 0.01,
    
    # COR_cut denotes the correlation coefficient threshold
    SET_COR_CUT = 0.6
  
      
) {
  
  
  FILTER_THRES <- SET_FILTER_THRES
  
  # COR_p_thres represents the p value threshold
  COR_P_THRES = SET_COR_P_THRES
  
  # COR_cut denotes the correlation coefficient threshold
  COR_CUT = SET_COR_CUT
  
  
  ## 1 subset ----
  
  
  # select samples of "IW" group
  
  
  # use clone to get a deep copy of soil_amp (R6 object)
  tmp <- clone(microtable)
  
  # change sample_table directly
  tmp$sample_table %<>% 
    
    subset(PLOT == SET_PLOT &
             SESSION == SET_SESSION) # [USER]
  
  # trim all files in the object
  tmp$tidy_dataset()
  
  # use filter_thres parameter to filter the feature with low relative abundance
  tmp <- 
    trans_network$new(
      dataset = tmp,
      cor_method = "spearman", 
    
      filter_thres = FILTER_THRES
    ) # [USER]
  
  tmp$cal_network(
    
    COR_p_thres = COR_P_THRES, # [USER]
    
    COR_cut = COR_CUT
  ) # [USER]
  
  # tmp$res_network %>% 
  #   as_tbl_graph() %>% 
  #   ggraph::autograph() #check
  
  # tmp$cal_network_attr(); 
  # tmp$res_network_attr #check
  
  
  
  # put the network into the list
  
  # nets_QUBI$one <- tmp
  
  return(tmp)
  
  
}