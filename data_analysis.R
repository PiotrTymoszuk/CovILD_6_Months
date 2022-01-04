# The mother script for the data analyses -----

# tools -----

  c('./tools/sys_tools.R', 
    './tools/project_tools.R', 
    './tools/counting_tools.R', 
    './tools/modeling_tools.R',
    './tools/clust_tools2.R', 
    './tools/lm_qc_tools.R') %>% 
  walk(source)

  library(furrr)
  library(doParallel)
  library(caret)
  library(kknn)
  library(C50)
  library(caTools)
  library(naivebayes)
  library(rpart)
  library(caretEnsemble)
  library(plotROC)

  select <- dplyr::select

# analysis scripts -----

  c('./analysis scripts/symptom_recovery.R', ## symptom recovery
    './analysis scripts/lung_recovery.R', ## pulmonary recovery
    './analysis scripts/endpoint_overlap.R', ## overlap between the outcome variables
    './analysis scripts/univariate_modeling.R', ## univariate risk modeling, outcome variables
    './analysis scripts/pca_features.R', ## general clustering tendency of the explanatory variables
    './analysis scripts/feature_clustering.R', ## clustering of the explanatory variables, prediction for the outcomes
    './analysis scripts/pca_participants.R', ## general clustering tendency for the participants
    './analysis scripts/participant_clustering.R', ## participant cluster definition
    './analysis scripts/part_clust_outcome.R', ## outcome variables in the participant clusters
    './analysis scripts/machine_learning.R', ## machine learning classifiers developed with Caret and caretEnsemble
    './analysis scripts/ml_plots.R', ## Visualization of the model stats
    './analysis scripts/ml_detail.R') %>%  ## ROC analysis in the cohort subsets
  walk(source)