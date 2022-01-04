# This script exports the cluster objects and ml models for the Shiny scoring App


  save(ml, file = './shiny feed/ml.RData')
  save(part_clust, file = './shiny feed/clust.RData')
  save(cov_data, file = './shiny feed/data.RData')
  save(clust_outcome, file = './shiny feed/clust_outcome.RData')
  