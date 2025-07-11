# setup ----


library(here); here::i_am('stats/AMF.R')


##sum----
#notAMF
ASVs18SrarFull_filtSum <- ASVs18SrarFull %>%
  filter(abund > 0) %>%
  group_by(PLOT, SESSION, SUBPLOT, Genus, G.spp) %>%
  # rstatix::get_summary_stats(abund) %>%
  summarise(n = n(),
            'abund_sum' = sum(abund)) %>%
  arrange(desc(abund_sum))
ASVs18SrarFullAMF_filtSum <- ASVs18SrarFullAMF_filt %>%
  group_by(PLOT, SESSION, SUBPLOT, Genus, G.spp) %>%
  # rstatix::get_summary_stats(abund) %>%
  summarise(n = n(),
            'abund_sum' = sum(abund)) %>%
  arrange(desc(abund_sum))

##top10----
ASVs18SrarFullAMF_filt %>%
  group_by(Genus) %>%
  summarise('abund_sum' = sum(abund)) %>%
  arrange(desc(abund_sum))

##fig----

ASVs18SrarFullAMF_filtSum %>% #arrange(abund_sum)
  
  # filter(!(Genus %in% c('Glomus', 'Paraglomus'))) %>% #temp
  
  # group_by(SESSION, PLOT, Genus) %>%
  # summarise('abund_sum_mean' = mean(abund_sum),
  #           'abund_sum_se' = sd(abund_sum) /
  #             sqrt(n()),
  #           'abund_sum_med' = median(abund_sum),
  #           'abund_sum_mad' = mad(abund_sum)) %>%
  
  ggplot(aes(SESSION, abund_sum,
             # fill = Genus
             color = fct_reorder(Genus, abund_sum, .desc = T)
             # shape = as.factor(SUBPLOT)
  )) +
  # geom_col() +
  # geom_point(size = 0.25) +
  stat_summary(fun.data = ggpubr::mean_se_) +
  # geom_point() +
  # geom_errorbar(aes(ymin = abund_sum_med - abund_sum_mad,
  #                   ymax = abund_sum_med + abund_sum_mad)) +
  # scale_y_continuous(limits = c(NA, 75)) +
  scale_y_log10() +
  geom_line(stat = 'summary') +
  facet_wrap(~ PLOT) +
  theme_classic() +
  labs(color = 'Genus') +
  scale_color_viridis_d(option = 'turbo')

##phase----
ASVs18SrarFullAMF_filt %>% #ASVs18SrarFullAMF_filtSum
  filter(Genus %in% c('Glomus', 'Paraglomus')) %>% #most!4k-->3.3k
  group_by(PLOT, SESSION, SUBPLOT, Genus) %>%
  # rstatix::get_summary_stats(abund) %>%
  summarise(n = n(), 'abund_sum' = sum(abund)) %>% select(!n) %>%
  group_by(PLOT, SESSION, Genus) %>%
  summarize('abund_sum_med' = median(abund_sum),
            'abund_sum_mad' = mad(abund_sum)) %>%
  pivot_wider(id_cols = !abund_sum_mad, values_fill = 0,
              names_from = Genus, values_from = abund_sum_med) %>%
  ungroup() %>%
  
  ggplot(aes(Paraglomus, Glomus, color = as.factor(SESSION))) +
  # stat_summary_bin(fun.data = ggpubr::median_mad, bins = 9) +
  scale_color_viridis_d(option = 'turbo') +
  geom_point(size = 3) + #color = 'gray'
  # scale_x_log10() + scale_y_log10() +
  facet_wrap(~ PLOT) +
  theme_classic()

##ord----

###comm----
ASVs18SrarFullAMF_filt_commTbl <- ASVs18SrarFullAMF_filt %>%
  select(asv_id, seqID:PLATE_ID) %>%
  pivot_wider(names_from = asv_id,
              values_from = abund,
              values_fill = 0) #%>%
# select(!c(seqID, PLOT:PLATE_ID))
ASVs18SrarFullAMF_filt_comm <- ASVs18SrarFullAMF_filt_commTbl %>%
  select(!c(1:6)) #%>% #glimpse()

###PCA----
ordAMF <- rda(ASVs18SrarFullAMF_filt_comm); #==PCA
ordAMF_sum <- summary(ordAMF)
ordAMF_xy <- ordAMF_sum$sites #%>% as_tibble()
ordAMF_prop <- ordAMF_sum$cont$importance %>% .[2, ]
ordAMF_spp <- ordAMF_sum$species %>% as_tibble(rownames = NA) %>%
  rownames_to_column('asv_id') %>%
  left_join(unique(ASVs18SrarFullAMF_filt[, c('asv_id', 'G.spp', 'Genus')]))
###NMDS
# metaMDS(ASVsITSrarFull_EMFfilt_commTbl %>%
#           select(!c(1:7))) #2slow?
###PCoA
# stats::cmdscale() #Euclidean=no
Sys.time()
PCoA_AMF <- ASVs18SrarFullAMF_filt_comm %>% vegdist() %>%
  cmdscale(eig = T, add = T, k = 3); Sys.time() #fast2sec
PCoA_xy <- PCoA_AMF$points
PCoA_AMF$eig #manual sum = %
PCoA_AMF$x; plot(PCoA_AMF$points)

###plot----
ASVs18SrarFullAMF_filt_commTbl %>%
  select(!contains('ASV')) %>%
  cbind(ordAMF_xy) %>% as_tibble() %>%
  
  ggplot(aes(PC1, PC2, color = SESSION)) +
  geom_point() + #geom_line() +
  # stat_ellipse() +
  facet_grid(SUBPLOT ~ PLOT, scales = 'free') +
  theme_classic() +
  scale_color_viridis_c()

###plot.sum----
ASVs18SrarFullAMF_filt_commTbl %>%
  select(!contains('ASV')) %>%
  cbind(ordAMF_xy) %>% as_tibble() %>%
  
  group_by(SESSION, PLOT) %>%
  summarize('PC1_mean' = mean(PC1),
            'PC1_se' = sd(PC1) / sqrt(n()),
            'PC2_mean' = mean(PC2),
            'PC2_se' = sd(PC2) / sqrt(n()),
            'PC3_mean' = mean(PC3),
            'PC3_se' = sd(PC3) / sqrt(n())) %>%
  
  ggplot(aes(PC1_mean, PC2_mean, color = SESSION)) +
  geom_point() + #geom_line() +
  geom_errorbar(aes(ymin = PC2_mean - PC2_se,
                    ymax = PC2_mean + PC2_se)) +
  geom_errorbarh(aes(xmin = PC1_mean - PC1_se,
                     xmax = PC1_mean + PC1_se)) +
  # stat_ellipse() +
  facet_wrap(~ PLOT, scales = 'free') +
  theme_classic() +
  scale_color_viridis_c() +
  
  geom_segment(data = head(ordAMF_spp, n = 3),
               aes(0, 0, xend = PC1/10, yend = PC2/10),
               inherit.aes = F, color = 'gray',
               arrow = arrow(length = unit(3, 'mm'))) +
  geom_text(data = head(na.omit(ordAMF_spp), n = 3), inherit.aes = F,
            aes(PC1/10, PC2/10, label = G.spp), size = 2)

##net----
# https://medium.com/analytics-vidhya/how-to-create-co-occurrence-networks-with-the-r-packages-cooccur-and-visnetwork-f6e1ceb1c523

###occur----
focal_plot <- ASVs18SrarFullAMF_filt_commTbl %>%
  filter(PLOT == 'ACSA') #%>% # & SESSION == 1
# select(!c(1:6)) %>% mutate('rsum' = rowSums(.)) %>% #.$rsum
# filter(rsum > 0)
focal_plot_comm <- focal_plot %>% select(!c(1:6))
cooc_mat <- focal_plot_comm %>% t()
colnames(cooc_mat) <- focal_plot$ID; cooc_mat #check
# Sys.time();
# cooc_mat %>% cooccur::cooccur(spp_names = T); Sys.time(); #2slow!

###cor----
focal_corr <- Hmisc::rcorr(as.matrix(focal_plot_comm))
focal_corr_p <- focal_corr$P %>% as_tibble(rownames = NA)
focal_corr_p_list <- focal_corr_p %>%
  rownames_to_column('asv_id') %>%
  pivot_longer(!asv_id, names_to = 'asv_id2', values_to = 'P')
focal_corr_p_list_sig <- focal_corr_p_list %>% filter(P < 1e-20) #min

###edges----
focal_net <- focal_corr_p_list_sig %>% select(!P) %>% as.matrix() %>%
  head() %>%
  igraph::graph_from_edgelist(directed = F)

###plot----
focal_net %>% tidygraph::as_tbl_graph() %>%
  ggraph::ggraph()

###stat----
# igraph::degree()
# igraph::fit_power_law()

###meco----
library(microeco); library(tidygraph); library(ggraph)
####init----
AMFtax_tbl <- ASVs18SrarFullAMF_filt %>% select(asv_id:Species) %>%
  distinct() %>% mutate('G.spp' = paste(Genus, Species, sep = '.'))
meco_asv_tbl <- microtable$new(as.data.frame(cooc_mat),
                               tax_table = AMFtax_tbl,
                               sample_table = key18S %>%
                                 left_join(DNAprocess) %>%
                                 column_to_rownames('ID')); Sys.time()
####init_net----
net1 <- trans_network$new(meco_asv_tbl,
                          cor_method = 'spearman',
                          filter_thres = 1e-16); Sys.time() #5min
net1$cal_network() #MAIN

# V(net1$res_network)$deg <- degree(net1$res_network)
net1_tbl <- net1$res_network %>% tidygraph::as_tbl_graph()
####graph----
net1_tbl %>%
  left_join(AMFtax_tbl,
            by = c('name' = 'asv_id')) %>%
  mutate('deg' = degree(., normalized = F)) %>%
  
  # ggraph::autograph()
  ggraph(layout = 'fr') +
  geom_edge_link(color = 'gray', alpha = 0.25) +
  geom_node_point(color = 'brown', alpha = 0.25, aes(size = deg)) +
  geom_node_text(aes(label = G.spp, size = deg), repel = T) +
  theme_graph() #+ legend()

####stat----
net1$cal_network_attr()
net1$cal_module(); #subgroups!

net1$get_node_table();
net1$res_node_table %>% as_tibble() %>% #glimpse()
  mutate('dd' = degree / sum(degree)) %>% #glimpse()
  
  ggplot(aes(degree)) +
  stat_bin(geom = 'point', bins = 30) + #stat_density()
  theme_classic()
# net1$cal_powerlaw()
