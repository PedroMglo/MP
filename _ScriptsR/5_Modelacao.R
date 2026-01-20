###############################################################################
# 5_Modelacao.R
# Capitulo 5: Modelacao + Comparacao por CV (no treino) + Test final
###############################################################################

# salvar previsoes (teste)
save_preds <- function(model_id, y_true, y_pred, out_dir) {
  dfp <- data.frame(
    y_true = y_true,
    y_pred = as.numeric(y_pred),
    y_pred_clipped = clip_1_5(as.numeric(y_pred))
  )
  write.csv(dfp, file.path(out_dir, paste0("previsoes_", model_id, ".csv")), row.names = FALSE)
}

# baseline: prever media
baseline_predict <- function(y_train, n) rep(mean(y_train, na.rm = TRUE), n)

# Avaliar por CV um "runner" que treina e prevê (yhat) dado train/val
cv_evaluate <- function(folds, y, predict_fun) {
  rmses <- c()
  maes  <- c()
  r2s   <- c()
  p05   <- c()
  for (i in seq_along(folds)) {
    val_idx <- folds[[i]]
    tr_idx  <- setdiff(seq_along(y), val_idx)
    out <- predict_fun(tr_idx, val_idx)
    y_val <- out$y_true
    y_hat <- out$y_pred
    rmses <- c(rmses, rmse(y_val, y_hat))
    maes  <- c(maes,  mae(y_val, y_hat))
    r2s   <- c(r2s,   r2(y_val, y_hat))
    p05   <- c(p05,   within_tolerance(y_val, y_hat, tol = 0.5))
  }
  c(RMSE = mean(rmses), MAE = mean(maes), R2 = mean(r2s), PCT_0_5 = mean(p05))
}

run_cap5 <- function(train, test, train_s, test_s, out_dir, seed = 1, k_folds = 5) {
  
  set.seed(seed)
  
  # Folds estratificados no treino
  folds <- make_folds_stratified(train$score_review, k = k_folds, seed = seed)
  
  # Tabelas de resultados
  res_cv   <- data.frame()
  res_test <- data.frame()
  
  # ============================
  # 5.0 Baseline (media)
  # ============================
  y_tr <- train$score_review
  cv_base <- cv_evaluate(folds, y_tr, function(tr_idx, val_idx) {
    yhat <- baseline_predict(y_tr[tr_idx], length(val_idx))
    list(y_true = y_tr[val_idx], y_pred = yhat)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(model_id="baseline", modelo="Baseline (media treino)", t(cv_base)))
  
  # Test baseline
  y_te <- test$score_review
  yhat_base <- baseline_predict(y_tr, nrow(test))
  save_preds("baseline", y_te, yhat_base, out_dir)
  res_test <- dplyr::bind_rows(res_test, data.frame(
    model_id="baseline", modelo="Baseline (media treino)",
    RMSE=rmse(y_te, yhat_base), MAE=mae(y_te, yhat_base), R2=r2(y_te, yhat_base),
    PCT_0_5=within_tolerance(y_te, yhat_base, 0.5)
  ))
  
  # ============================
  # 5.1 Regressao Linear (lm) - usa dados escalados (train_s/test_s)
  # ============================
  y_trs <- train_s$score_review
  
  cv_lm <- cv_evaluate(folds, y_trs, function(tr_idx, val_idx) {
    tr <- train_s[tr_idx, , drop=FALSE]
    va <- train_s[val_idx, , drop=FALSE]
    fit <- stats::lm(score_review ~ ., data = tr)
    pred <- stats::predict(fit, newdata = va)
    list(y_true = va$score_review, y_pred = pred)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(model_id="lm", modelo="Regressao Linear (lm)", t(cv_lm)))
  
  lm_fit <- stats::lm(score_review ~ ., data = train_s)
  lm_pred <- stats::predict(lm_fit, newdata = test_s)
  saveRDS(lm_fit, file.path(out_dir, "modelo_lm.rds"))
  save_preds("lm", y_te, lm_pred, out_dir)
  
  res_test <- dplyr::bind_rows(res_test, data.frame(
    model_id="lm", modelo="Regressao Linear (lm)",
    RMSE=rmse(y_te, lm_pred), MAE=mae(y_te, lm_pred), R2=r2(y_te, lm_pred),
    PCT_0_5=within_tolerance(y_te, lm_pred, 0.5)
  ))
  
  # ============================
  # 5.2 Ridge / 5.3 Lasso (glmnet) - usa dados escalados via model.matrix
  # ============================
  mm_trs <- mmatrix(train_s, target="score_review")
  x_all <- mm_trs$x
  y_all <- mm_trs$y
  
  # --- Ridge CV externo (cada fold faz cv.glmnet no sub-treino)
  cv_ridge <- cv_evaluate(folds, y_all, function(tr_idx, val_idx) {
    x_tr <- x_all[tr_idx, , drop=FALSE]
    y_tr <- y_all[tr_idx]
    x_va <- x_all[val_idx, , drop=FALSE]
    y_va <- y_all[val_idx]
    
    cv <- glmnet::cv.glmnet(x_tr, y_tr, alpha=0, nfolds=5, standardize=FALSE)
    fit <- glmnet::glmnet(x_tr, y_tr, alpha=0, lambda=cv$lambda.min, standardize=FALSE)
    pred <- as.numeric(stats::predict(fit, newx = x_va))
    list(y_true=y_va, y_pred=pred)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(model_id="ridge", modelo="Ridge (glmnet)", t(cv_ridge)))
  
  # Treino final ridge (cv no treino inteiro)
  cv_r <- glmnet::cv.glmnet(x_all, y_all, alpha=0, nfolds=10, standardize=FALSE)
  ridge_fit <- glmnet::glmnet(x_all, y_all, alpha=0, lambda=cv_r$lambda.min, standardize=FALSE)
  
  mm_tes <- mmatrix(test_s, target="score_review")
  ridge_pred <- as.numeric(stats::predict(ridge_fit, newx = mm_tes$x))
  saveRDS(cv_r,      file.path(out_dir, "cv_ridge.rds"))
  saveRDS(ridge_fit, file.path(out_dir, "modelo_ridge.rds"))
  save_lines(c(paste("lambda.min:", cv_r$lambda.min), paste("lambda.1se:", cv_r$lambda.1se)),
             file.path(out_dir, "ridge_lambdas.txt"))
  save_preds("ridge", y_te, ridge_pred, out_dir)
  
  res_test <- dplyr::bind_rows(res_test, data.frame(
    model_id="ridge", modelo="Ridge (glmnet)",
    RMSE=rmse(y_te, ridge_pred), MAE=mae(y_te, ridge_pred), R2=r2(y_te, ridge_pred),
    PCT_0_5=within_tolerance(y_te, ridge_pred, 0.5)
  ))
  
  # --- Lasso
  cv_lasso <- cv_evaluate(folds, y_all, function(tr_idx, val_idx) {
    x_tr <- x_all[tr_idx, , drop=FALSE]
    y_tr <- y_all[tr_idx]
    x_va <- x_all[val_idx, , drop=FALSE]
    y_va <- y_all[val_idx]
    
    cv <- glmnet::cv.glmnet(x_tr, y_tr, alpha=1, nfolds=5, standardize=FALSE)
    fit <- glmnet::glmnet(x_tr, y_tr, alpha=1, lambda=cv$lambda.min, standardize=FALSE)
    pred <- as.numeric(stats::predict(fit, newx = x_va))
    list(y_true=y_va, y_pred=pred)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(model_id="lasso", modelo="Lasso (glmnet)", t(cv_lasso)))
  
  cv_l <- glmnet::cv.glmnet(x_all, y_all, alpha=1, nfolds=10, standardize=FALSE)
  lasso_fit <- glmnet::glmnet(x_all, y_all, alpha=1, lambda=cv_l$lambda.min, standardize=FALSE)
  lasso_pred <- as.numeric(stats::predict(lasso_fit, newx = mm_tes$x))
  saveRDS(cv_l,      file.path(out_dir, "cv_lasso.rds"))
  saveRDS(lasso_fit, file.path(out_dir, "modelo_lasso.rds"))
  save_lines(c(paste("lambda.min:", cv_l$lambda.min), paste("lambda.1se:", cv_l$lambda.1se)),
             file.path(out_dir, "lasso_lambdas.txt"))
  lasso_coefs <- as.matrix(stats::coef(lasso_fit))
  write.csv(lasso_coefs, file.path(out_dir, "lasso_coeficientes.csv"), row.names = TRUE)
  save_preds("lasso", y_te, lasso_pred, out_dir)
  
  res_test <- dplyr::bind_rows(res_test, data.frame(
    model_id="lasso", modelo="Lasso (glmnet)",
    RMSE=rmse(y_te, lasso_pred), MAE=mae(y_te, lasso_pred), R2=r2(y_te, lasso_pred),
    PCT_0_5=within_tolerance(y_te, lasso_pred, 0.5)
  ))
  
  # ============================
  # 5.4 Arvore (rpart) - tune simples por CV (cp + maxdepth)
  # ============================
  grid_tree <- expand.grid(
    cp = c(0.0005, 0.001, 0.005, 0.01),
    maxdepth = c(3, 5, 8, 12)
  )
  
  tree_cv_rows <- data.frame()
  for (g in seq_len(nrow(grid_tree))) {
    cpv <- grid_tree$cp[g]
    md  <- grid_tree$maxdepth[g]
    met <- cv_evaluate(folds, y_tr, function(tr_idx, val_idx) {
      tr <- train[tr_idx, , drop=FALSE]
      va <- train[val_idx, , drop=FALSE]
      fit <- rpart::rpart(
        score_review ~ ., data = tr, method = "anova",
        control = rpart::rpart.control(cp = cpv, maxdepth = md, xval = 0)
      )
      pred <- stats::predict(fit, newdata = va)
      list(y_true = va$score_review, y_pred = pred)
    })
    tree_cv_rows <- dplyr::bind_rows(tree_cv_rows, data.frame(model_id="rpart", cp=cpv, maxdepth=md, t(met)))
  }
  write.csv(tree_cv_rows, file.path(out_dir, "tree_cv_grid.csv"), row.names = FALSE)
  best_tree <- tree_cv_rows |> dplyr::arrange(.data$RMSE) |> dplyr::slice(1)
  
  res_cv <- dplyr::bind_rows(res_cv, data.frame(
    model_id="rpart",
    modelo=paste0("Arvore (rpart) cp=", best_tree$cp, " maxdepth=", best_tree$maxdepth),
    RMSE=best_tree$RMSE, MAE=best_tree$MAE, R2=best_tree$R2, PCT_0_5=best_tree$PCT_0_5
  ))
  
  tree_fit <- rpart::rpart(
    score_review ~ ., data = train, method = "anova",
    control = rpart::rpart.control(cp = best_tree$cp, maxdepth = best_tree$maxdepth, xval = 10)
  )
  tree_pred <- stats::predict(tree_fit, newdata = test)
  saveRDS(tree_fit, file.path(out_dir, "modelo_rpart.rds"))
  save_lines(c(
    paste("best_cp:", best_tree$cp),
    paste("best_maxdepth:", best_tree$maxdepth)
  ), file.path(out_dir, "tree_best_params.txt"))
  save_preds("rpart", y_te, tree_pred, out_dir)
  
  grDevices::png(file.path(out_dir, "arvore_rpart.png"), width = 1200, height = 700)
  rpart.plot::rpart.plot(tree_fit, main = "Arvore de Regressao (rpart)")
  grDevices::dev.off()
  
  res_test <- dplyr::bind_rows(res_test, data.frame(
    model_id="rpart", modelo="Arvore (rpart)",
    RMSE=rmse(y_te, tree_pred), MAE=mae(y_te, tree_pred), R2=r2(y_te, tree_pred),
    PCT_0_5=within_tolerance(y_te, tree_pred, 0.5)
  ))
  
  # ============================
  # 5.5 Random Forest - tune mtry por CV
  # ============================
  p <- ncol(train) - 1
  mtry_grid <- unique(pmax(1, c(floor(sqrt(p)), floor(p/3), floor(p/2))))
  rf_cv_rows <- data.frame()
  
  for (mtryv in mtry_grid) {
    met <- cv_evaluate(folds, y_tr, function(tr_idx, val_idx) {
      tr <- train[tr_idx, , drop=FALSE]
      va <- train[val_idx, , drop=FALSE]
      fit <- randomForest::randomForest(
        score_review ~ ., data = tr,
        ntree = 400, mtry = mtryv, importance = TRUE
      )
      pred <- stats::predict(fit, newdata = va)
      list(y_true = va$score_review, y_pred = pred)
    })
    rf_cv_rows <- dplyr::bind_rows(rf_cv_rows, data.frame(model_id="rf", mtry=mtryv, t(met)))
  }
  write.csv(rf_cv_rows, file.path(out_dir, "rf_cv_grid.csv"), row.names = FALSE)
  best_rf <- rf_cv_rows |> dplyr::arrange(.data$RMSE) |> dplyr::slice(1)
  
  res_cv <- dplyr::bind_rows(res_cv, data.frame(
    model_id="rf",
    modelo=paste0("Random Forest (mtry=", best_rf$mtry, ")"),
    RMSE=best_rf$RMSE, MAE=best_rf$MAE, R2=best_rf$R2, PCT_0_5=best_rf$PCT_0_5
  ))
  
  rf_fit <- randomForest::randomForest(
    score_review ~ ., data = train,
    ntree = 600, mtry = best_rf$mtry, importance = TRUE
  )
  rf_pred <- stats::predict(rf_fit, newdata = test)
  saveRDS(rf_fit, file.path(out_dir, "modelo_random_forest.rds"))
  save_lines(paste("best_mtry:", best_rf$mtry), file.path(out_dir, "rf_best_params.txt"))
  save_preds("rf", y_te, rf_pred, out_dir)
  
  rf_imp <- randomForest::importance(rf_fit)
  write.csv(rf_imp, file.path(out_dir, "rf_importancia.csv"), row.names = TRUE)
  
  res_test <- dplyr::bind_rows(res_test, data.frame(
    model_id="rf", modelo="Random Forest",
    RMSE=rmse(y_te, rf_pred), MAE=mae(y_te, rf_pred), R2=r2(y_te, rf_pred),
    PCT_0_5=within_tolerance(y_te, rf_pred, 0.5)
  ))
  
  # ============================
  # Guardar metricas e escolha por CV (SEM usar teste)
  # ============================
  res_cv_sorted <- res_cv |> dplyr::arrange(.data$RMSE)
  res_test_sorted <- res_test |> dplyr::arrange(.data$RMSE)
  
  write.csv(res_cv_sorted,   file.path(out_dir, "metricas_cv.csv"),   row.names = FALSE)
  write.csv(res_test_sorted, file.path(out_dir, "metricas_test.csv"), row.names = FALSE)
  
  best_model_id <- res_cv_sorted$model_id[1]
  best_model_desc <- res_cv_sorted$modelo[1]
  save_lines(c(
    paste("Melhor modelo escolhido por CV (treino):", best_model_id),
    paste("Descricao:", best_model_desc)
  ), file.path(out_dir, "best_model_by_cv.txt"))
  
  summary_lines <- c(
    "CAPITULO 5 - MODELACAO concluido.",
    paste("Seed:", seed),
    paste("CV folds:", k_folds),
    paste("Melhor por CV:", best_model_id, "-", best_model_desc),
    paste("Metricas CV em:", file.path(out_dir, "metricas_cv.csv")),
    paste("Metricas Test em:", file.path(out_dir, "metricas_test.csv")),
    paste("Previsoes em:", out_dir)
  )
  save_lines(summary_lines, file.path(out_dir, "cap5_resumo_final.txt"))
  
  print(res_cv_sorted)
  print(res_test_sorted)
  print(summary_lines)
  
  invisible(list(metrics_cv = res_cv_sorted, metrics_test = res_test_sorted, best_model_id = best_model_id))
}
