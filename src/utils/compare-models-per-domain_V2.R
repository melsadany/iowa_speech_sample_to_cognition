require(caret)
require(glmnet)

# Common CV setup
ctrl <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

compare_models <- function(df.0){
  # -----------------------------
  # Model 1: word_count only (OLS)
  # -----------------------------
  set.seed(1)
  m1 <- train(y ~ base, data = df.0, method = "lm", trControl = ctrl)
  r2_vals_m1 <- m1$resample$Rsquared
  r2_m1 <- mean(r2_vals_m1)
  ci_m1 <- confint(lm(r2_vals_m1~1))
  
  # -----------------------------
  # Model 2: all features (OLS)
  # -----------------------------
  set.seed(2)
  m2 <- train(y ~ ., data = df.0, method = "lm", trControl = ctrl)
  r2_vals_m2 <- m2$resample$Rsquared
  r2_m2 <- mean(r2_vals_m2)
  ci_m2 <- confint(lm(r2_vals_m2~1))
  
  # -----------------------------
  # Model 3: all features, 0 penalty for word_count (ridge)
  # -----------------------------
  x <- model.matrix(y ~ ., data = df.0)[, -1]
  y <- df.0$y
  penalty_factors <- ifelse(colnames(x) == "base", 0, 1)
  
  ridge_model <- train(
    x, y,
    method = "glmnet",
    tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 1, length = 20)),
    trControl = ctrl,
    preProcess = c("center", "scale"),
    penalty.factor = penalty_factors
  )
  r2_vals_m3 <- ridge_model$resample$Rsquared
  r2_m3 <- mean(r2_vals_m3)
  ci_m3 <- confint(lm(r2_vals_m3~1))
  
  # -----------------------------
  # Model 4: all features except word_count (ridge)
  # -----------------------------
  x_no_wc <- x[, colnames(x) != "base"]
  ridge_no_wc <- train(
    x_no_wc, y,
    method = "glmnet",
    tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 1, length = 20)),
    trControl = ctrl,
    preProcess = c("center", "scale")
  )
  r2_vals_m4 <- ridge_no_wc$resample$Rsquared
  r2_m4 <- mean(r2_vals_m4)
  ci_m4 <- confint(lm(r2_vals_m4~1))
  
  # -----------------------------
  # Combine results
  # -----------------------------
  results <- data.frame(
    Model = c(
      "Word count only",
      "All features (incl. word_count)",
      "All + 0 penalty for word_count",
      "All except word_count"
    ),
    R2 = round(c(r2_m1, r2_m2, r2_m3, r2_m4), 3),
    CI_lower = round(c(ci_m1[[1]], ci_m2[[1]], ci_m3[[1]], ci_m4[[1]]), 3),
    CI_upper = round(c(ci_m1[[2]], ci_m2[[2]], ci_m3[[2]], ci_m4[[2]]), 3)
  )
  all <- tribble(~R2,~Model,
                 r2_vals_m1,"Word count only",
                 r2_vals_m2,"All features (incl. word_count)",
                 r2_vals_m3,"All + 0 penalty for word_count",
                 r2_vals_m4,"All except word_count")
  return(list(results,all))
}