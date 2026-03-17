################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
require(glmnet)
require(caret)

# Function to compare models for a single IQ domain
compare_models_for_domain <- function(data, iq_domain, feature_cols, base_feature) {
  set.seed(123)
  train_control <- trainControl(method = "cv",number = 10, savePredictions = "final",allowParallel = TRUE,returnResamp = "all")
  
  ## Prepare data (remove missing)
  model_data <- data %>% dplyr::select(all_of(c(iq_domain, base_feature, feature_cols))) %>%
    drop_na()%>%mutate_all(.funs = function(x)scale(x)[,1])
  if (nrow(model_data) < 20) {warning(paste("Not enough data for", iq_domain));return(NULL)}
  
  ## Train base model
  cat("Training base model for", iq_domain, "...\n")
  base_model <- train(as.formula(paste(iq_domain, "~", base_feature)),data = model_data,method = "lm",trControl = train_control)

  ## Train full models
  if(length(feature_cols)==1){
    print("number of features is not enough")
    return(NULL)
  }else {
    ## three different models
    
    ## FULL no penalty
    cat("Training full model with no penalty for", iq_domain, "...\n")
    full_model.np <- train(as.formula(paste0(iq_domain, "~ .")),data = model_data,method = "glmnet",
                           trControl = train_control,standardize = FALSE)
    
    ## FULL with a 0 penalty for base feature
    cat("Training full model with a base feature penalty for", iq_domain, "...\n")
    all_features <- setdiff(colnames(model_data), iq_domain)
    penalty_factor <- as.numeric(all_features!=base_feature)
    full_model.p <- train(as.formula(paste0(iq_domain, "~ .")),data = model_data, method = "glmnet",
                          penalty.factor = penalty_factor,
                          trControl = train_control,standardize = FALSE)
    
    ## FULL* without the base feature
    cat("Training full* model with no penalty for", iq_domain, "...\n")
    full_model.as <- train(as.formula(paste0(iq_domain, "~ .")),data=model_data %>% select(-all_of(base_feature)),method = "glmnet",
                           trControl = train_control,standardize = FALSE)
  }
  ## Compare performance
  model_comparison <- list(domain = iq_domain,base_model = base_model,full_model.p = full_model.p,full_model.np = full_model.np,full_model.as = full_model.as)
  return(model_comparison)
}
