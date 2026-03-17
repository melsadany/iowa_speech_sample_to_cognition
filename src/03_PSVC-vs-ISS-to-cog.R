################################################################################
################################################################################
rm(list = ls());gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),"/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
library(glmnet)
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language")
setwd(project.dir)
################################################################################
################################################################################
load("../shared_data/data/all-data-raw-and-resid-for-age-and-sex-and-their-PCs.rda")
################################################################################
################################################################################
################################################################################
iss.feat <- all.resid$lang_ext[rowSums(is.na(all.resid$lang_ext))<400,]
iss.feat <- iss.feat[,colSums(is.na(iss.feat))<10]
iss.feat.cl <- iss.feat[,colSums(is.na(iss.feat))==0]
psvc.cl <- all.resid$lang_sep
iq <- all.resid$iq
rownames(iss.feat.cl)<-iss.feat.cl$te_id
rownames(psvc.cl)<-psvc.cl$te_id
rownames(iq)<-iq$te_id

psvc.ids <- rownames(psvc.cl)
iss.ids <- rownames(iss.feat.cl)
iss.feat.cl.use <- iss.feat.cl[iss.ids,-1]
psvc.cl.use <- psvc.cl[psvc.ids,-1]
################################################################################
################################################################################
################################################################################
## Function to perform repeated CV ridge regression
rep_cv_ridge <- function(X, y, n_folds = 5, n_repeats = 10, seed = 123) {
  # X : feature matrix (n x p)
  # y : outcome vector (length n)
  # n_folds : number of CV folds
  # n_repeats : number of times to repeat the CV
  # Returns a vector of MSE for each fold and repeat
  
  n <- length(y)
  set.seed(seed)
  
  # Store all MSE values
  all_mse <- numeric(n_folds * n_repeats)
  
  # Create fold indices for each repeat (to ensure same splits for both feature sets)
  fold_ids <- list()
  for (r in 1:n_repeats) {
    fold_ids[[r]] <- sample(rep(1:n_folds, length.out = n))
  }
  
  idx <- 1
  for (r in 1:n_repeats) {
    folds <- fold_ids[[r]]
    for (fold in 1:n_folds) {
      # Split into training and test
      test_idx <- which(folds == fold)
      train_idx <- setdiff(1:n, test_idx)
      
      X_train <- X[train_idx, , drop = FALSE]
      y_train <- y[train_idx]
      X_test  <- X[test_idx, , drop = FALSE]
      y_test  <- y[test_idx]
      
      # Use cv.glmnet to select lambda on the training set (internal 10-fold CV)
      # alpha = 0 for ridge
      cv_fit <- cv.glmnet(X_train%>%as.matrix(), y_train, alpha = 0, nfolds = 10)
      best_lambda <- cv_fit$lambda.min
      
      # Fit final model on training data with best lambda
      fit <- glmnet(X_train, y_train, alpha = 0, lambda = best_lambda)
      
      # Predict on test set
      pred <- predict(fit, newx = X_test%>%as.matrix())
      
      # Compute MSE
      mse <- mean((y_test - pred)^2)
      all_mse[idx] <- mse
      idx <- idx + 1
    }
  }
  
  return(all_mse)
}


## Run repeated CV for both feature sets
mse_psvc <- rep_cv_ridge(psvc.cl.use[iss.ids,], iq[iss.ids,4], n_folds = 5, n_repeats = 5000, seed = 42)
mse_iss <- rep_cv_ridge(iss.feat.cl.use, iq[iss.ids,4], n_folds = 5, n_repeats = 5000, seed = 42) 

# Compute cross-validated R²
var_y <- var(iq[iss.ids,4])
r2_psvc <- 1 - mse_psvc / var_y
r2_iss <- 1 - mse_iss / var_y

# Compare performance
# Mean R² across all folds/repeats
mean(r2_psvc)
mean(r2_iss)

# Difference in MSE (V2 - V1) – negative means V2 is better
mse_diff <- mse_iss - mse_psvc
mean(mse_diff)   # average improvement (negative if V2 better)
# Equivalent difference in R²
r2_diff <- r2_iss - r2_psvc
mean(r2_diff)

# Boxplot of the differences
pdf("figs/ISS-vs-PSVC.pdf",width=3.5,height=5)
boxplot(mse_diff, ylab = "MSE difference (ISS.v1.10.B - ISS.v1.0)", 
        main = "Improvement with Version 1.10.B")
abline(h = 0, col = "red", lty = 2)
dev.off()
################################################################################
################################################################################
## Significance test (permutation test)

# Permute the assignment of which MSE comes from which feature set within each fold pair
set.seed(123)
n_perm <- 5000
perm_diffs <- numeric(n_perm)

# We have paired observations: each element of mse_V1 corresponds to same fold/repeat as mse_V2
for (perm in 1:n_perm) {
  # Randomly swap or not swap each pair
  swap <- rbinom(length(mse_diff), 1, 0.5)
  mse1_perm <- ifelse(swap, mse_iss, mse_psvc)
  mse2_perm <- ifelse(swap, mse_psvc, mse_iss)
  perm_diffs[perm] <- mean(mse2_perm - mse1_perm)
}

# Two-tailed p-value: proportion of permuted differences as extreme as observed
p_value <- mean(abs(perm_diffs) >= abs(mean(mse_diff)))
p_value

# ------------------------------
# Confidence interval for the mean R² improvement (bootstrap)
# ------------------------------
boot_diff <- replicate(2000, mean(sample(r2_diff, replace = TRUE)))
ci <- quantile(boot_diff, c(0.025, 0.975))
ci


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
