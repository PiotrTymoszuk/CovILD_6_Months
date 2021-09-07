# This script clusters binary modeling variables together with the CT pathology responses
# distance simple-matching distance

# data and toolbox ----

  source('./tools/sys_tools.R')
  source('./tools/clust_tools.R')

  insert_head()

# data container and globals -----
  
  cov_multiclust <- list()

  ## cluster colors 
  
  cov_multiclust$clust_colors <- c(k1 = 'cornsilk4', 
                                   k2 = 'coral3', 
                                   k3 = 'cornflowerblue', 
                                   k4 = 'darkgoldenrod3')
  
  cov_multiclust$clust_labs <- paste('Cluster #', 1:4, sep = '') %>% 
    set_names(names(cov_multiclust$clust_colors))
  
# analysis and distance calculation table ----
  
  insert_msg('Preparing the analysis data set with binary variables')
  
  cov_multiclust$id_vec <- cov_data$mod_tbl$ID
  
  cov_multiclust$analysis_tbl <-  cov_data$mod_tbl %>% 
    select( - ID) %>% 
    map_dfc(function(x) ifelse(x == 'yes', 2, 1))
  
  cov_multiclust$dist_calc_tbl <- cov_multiclust$analysis_tbl %>% 
    t %>% 
    as.data.frame
  
# K-means clustering with 4 centers, simple matching distance -----
  
  insert_msg('Generating clustering object, k means')
  
  cov_multiclust$clust_obj <- kcluster_data(inp_tbl = cov_multiclust$dist_calc_tbl, 
                                            distance_method = 'smc', 
                                            clust_fun = kmeans, 
                                            k = 4, 
                                            nstart = 4)
  
  ## cluster assignment table
  
  cov_multiclust$clust_obj$clust_assignment <- cov_multiclust$clust_obj$clust_assignment %>% 
    mutate(clust_id = paste0('k', clust_id))
  
# representation of the results: MDS with the cluster assignment -----
  
  insert_msg('Summary scatter plot with the results of k-means clustering')
  
  cov_multiclust$scatter_plot <- plot_clust_mds(cluster_analysis = cov_multiclust$clust_obj, 
                                                k_dim = 2, 
                                                red_fun = 'mds', 
                                                cluster_labs = cov_multiclust$clust_labs, 
                                                cluster_colors = cov_multiclust$clust_colors, 
                                                cust_theme = globals$common_theme, 
                                                plot_title = 'Unsupervised k-means clstering', 
                                                point_alpha = 0.6)
  
  ## highlighting the CT responses in the plot
  
  cov_multiclust$scatter_plot$highlights <- cov_multiclust$scatter_plot$plot$data %>% 
    filter(variable %in% globals$mod_resp$response) %>% 
    mutate(label = set_names(globals$mod_resp$label, 
                             globals$mod_resp$response)[variable], 
           label = stri_replace(label, 
                                fixed = '\n', 
                                replacement = ' '))
  
  cov_multiclust$scatter_plot$plot <- cov_multiclust$scatter_plot$plot + 
    geom_point(data = cov_multiclust$scatter_plot$highlights, 
               size = 4, 
               shape = 21, 
               show.legend = F) + 
    geom_label_repel(data = cov_multiclust$scatter_plot$highlights, 
                     aes(label = label), 
                     size = 2.75, 
                     box.padding = 0.1, 
                     label.padding = 0.1, 
                     show.legend = F,
                     nudge_x = 0.1)

# Plotting the matching distances between the responses and their nearest neighbors -----  
  
  insert_msg('Radial plots of the matching distances: the CT responses and their nearest neighbors in clusters')
  
  ## plotting tables
  
  cov_multiclust$heat_maps$plotting_tbls <- cov_multiclust$clust_obj$clust_assignment %>% 
    dlply(.(clust_id), function(x) cov_multiclust$clust_obj$dist_tbl[x$variable, x$variable]) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 
        'feature1') %>% 
    map2(., 
         dlply(cov_multiclust$clust_obj$clust_assignment, 
               .(clust_id), 
               function(x) x$variable), 
         function(x, y) gather(x, 
                               key = 'feature2', 
                               value = 'matching_dist', 
                               all_of(y))) %>% 
    map2(., 
         names(.), 
         function(x, y) mutate(x, 
                               kmeans_id = y, 
                               matching_coeff = 1 - matching_dist, 
                               highlight = ifelse(feature1 %in% globals$mod_resp$response | 
                                                    feature2 %in% globals$mod_resp$response, 
                                                  'yes', 
                                                  'no'))) %>% 
    map(as_tibble)
  
  ## radial plots
  
  cov_multiclust$radial_plots <- globals$mod_resp$response %>% 
    map(function(x) 
      map(cov_multiclust$heat_maps$plotting_tbls, 
          function(y) neighbour_plot(plotting_tbl = y, 
                                     n_NN = 10, 
                                     feature = x)) %>% 
        set_names(names(cov_multiclust$heat_maps$plotting_tbls)) %>% 
        compact) %>% 
    unlist(recursive = F) %>% 
    set_names(globals$mod_resp$response)
  
# END ----
  
  insert_tail()