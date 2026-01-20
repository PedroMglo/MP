###############################################################################
# 5_Modelacao.R
# Capitulo 5: Modelacao + Comparacao por CV (no treino) + Teste final (1x)
###############################################################################

# salvar previsoes (genérico)
save_preds_generic <- function(filename, y_true, y_pred, out_dir) {
  dfp <- data.frame(
    y_true = as.numeric(y_true),
    y_pred = as.numeric(y_pred),
    y_pred_clipped = clip_1_5(as.numeric(y_pred))
  )
  write.csv(dfp, file.path(out_dir, filename), row.names = FALSE)
}

# baseline: prever media
baseline_predict <- function(y_train, n) rep(mean(y_train, na.rm = TRUE), n)

# Avaliar por CV um "runner" que devolve list(y_true, y_pred)
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

run_cap5 <- function(train, val, test, train_s, val_s, test_s, out_dir, seed = 1, k_folds = 7) {
  
  set.seed(seed)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # ------------------------------------------------------------
  # CV estratificado apenas no TREINO
  # ------------------------------------------------------------
  folds <- make_folds_stratified(train$score_review, k = k_folds, seed = seed)
  
  # Tabela de resultados CV (comparacao)
  res_cv <- data.frame()
  
  # Helpers
  y_tr  <- train$score_review
  y_trs <- train_s$score_review
  y_te  <- test$score_review
  
  # ============================================================
  # 5.0 Baseline (media)
  # ============================================================
  cv_base <- cv_evaluate(folds, y_tr, function(tr_idx, val_idx) {
    yhat <- baseline_predict(y_tr[tr_idx], length(val_idx))
    list(y_true = y_tr[val_idx], y_pred = yhat)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(model_id = "baseline", modelo = "Baseline (media treino)", t(cv_base)))
  
  # ============================================================
  # 5.1 Regressao Linear (lm) - usa train_s no CV
  # ============================================================
  cv_lm <- cv_evaluate(folds, y_trs, function(tr_idx, val_idx) {
    tr <- train_s[tr_idx, , drop = FALSE]
    va <- train_s[val_idx, , drop = FALSE]
    fit  <- stats::lm(score_review ~ ., data = tr)
    pred <- stats::predict(fit, newdata = va)
    list(y_true = va$score_review, y_pred = pred)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(model_id = "lm", modelo = "Regressao Linear (lm)", t(cv_lm)))
  
  # ============================================================
  # 5.2 Ridge / 5.3 Lasso (glmnet) - model.matrix em train_s
  # ============================================================
  mm_trs <- mmatrix(train_s, target = "score_review")
  x_all  <- mm_trs$x
  y_all  <- mm_trs$y
  
  # Ridge (outer-CV; cada fold faz cv.glmnet no sub-treino)
  cv_ridge <- cv_evaluate(folds, y_all, function(tr_idx, val_idx) {
    x_tr <- x_all[tr_idx, , drop = FALSE]
    y_tr <- y_all[tr_idx]
    x_va <- x_all[val_idx, , drop = FALSE]
    y_va <- y_all[val_idx]
    
    cv  <- glmnet::cv.glmnet(x_tr, y_tr, alpha = 0, nfolds = 5, standardize = FALSE)
    fit <- glmnet::glmnet(x_tr, y_tr, alpha = 0, lambda = cv$lambda.min, standardize = FALSE)
    pred <- as.numeric(stats::predict(fit, newx = x_va))
    list(y_true = y_va, y_pred = pred)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(model_id = "ridge", modelo = "Ridge (glmnet)", t(cv_ridge)))
  
  # Lasso
  cv_lasso <- cv_evaluate(folds, y_all, function(tr_idx, val_idx) {
    x_tr <- x_all[tr_idx, , drop = FALSE]
    y_tr <- y_all[tr_idx]
    x_va <- x_all[val_idx, , drop = FALSE]
    y_va <- y_all[val_idx]
    
    cv  <- glmnet::cv.glmnet(x_tr, y_tr, alpha = 1, nfolds = 5, standardize = FALSE)
    fit <- glmnet::glmnet(x_tr, y_tr, alpha = 1, lambda = cv$lambda.min, standardize = FALSE)
    pred <- as.numeric(stats::predict(fit, newx = x_va))
    list(y_true = y_va, y_pred = pred)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(model_id = "lasso", modelo = "Lasso (glmnet)", t(cv_lasso)))
  
  # ============================================================
  # 5.4 Arvore (rpart) - tune simples por CV (cp + maxdepth)
  # ============================================================
  grid_tree <- expand.grid(
    cp = c(0.0005, 0.001, 0.005, 0.01),
    maxdepth = c(3, 5, 8, 12)
  )
  
  tree_cv_rows <- data.frame()
  for (g in seq_len(nrow(grid_tree))) {
    cpv <- grid_tree$cp[g]
    md  <- grid_tree$maxdepth[g]
    
    met <- cv_evaluate(folds, y_tr, function(tr_idx, val_idx) {
      tr <- train[tr_idx, , drop = FALSE]
      va <- train[val_idx, , drop = FALSE]
      fit <- rpart::rpart(
        score_review ~ ., data = tr, method = "anova",
        control = rpart::rpart.control(cp = cpv, maxdepth = md, xval = 0)
      )
      pred <- stats::predict(fit, newdata = va)
      list(y_true = va$score_review, y_pred = pred)
    })
    
    tree_cv_rows <- dplyr::bind_rows(tree_cv_rows, data.frame(cp = cpv, maxdepth = md, t(met)))
  }
  write.csv(tree_cv_rows, file.path(out_dir, "tree_cv_grid.csv"), row.names = FALSE)
  best_tree <- tree_cv_rows |> dplyr::arrange(.data$RMSE) |> dplyr::slice(1)
  
  res_cv <- dplyr::bind_rows(
    res_cv,
    data.frame(
      model_id = "rpart",
      modelo = paste0("Arvore (rpart) cp=", best_tree$cp, " maxdepth=", best_tree$maxdepth),
      RMSE = best_tree$RMSE, MAE = best_tree$MAE, R2 = best_tree$R2, PCT_0_5 = best_tree$PCT_0_5
    )
  )
  
  # ============================================================
  # 5.5 Random Forest - tune mtry por CV
  # ============================================================
  p <- ncol(train) - 1
  mtry_grid <- unique(pmax(1, c(floor(sqrt(p)), floor(p/3), floor(p/2))))
  rf_cv_rows <- data.frame()
  
  for (mtryv in mtry_grid) {
    met <- cv_evaluate(folds, y_tr, function(tr_idx, val_idx) {
      tr <- train[tr_idx, , drop = FALSE]
      va <- train[val_idx, , drop = FALSE]
      fit <- randomForest::randomForest(score_review ~ ., data = tr, ntree = 400, mtry = mtryv, importance = TRUE)
      pred <- stats::predict(fit, newdata = va)
      list(y_true = va$score_review, y_pred = pred)
    })
    rf_cv_rows <- dplyr::bind_rows(rf_cv_rows, data.frame(mtry = mtryv, t(met)))
  }
  write.csv(rf_cv_rows, file.path(out_dir, "rf_cv_grid.csv"), row.names = FALSE)
  best_rf <- rf_cv_rows |> dplyr::arrange(.data$RMSE) |> dplyr::slice(1)
  
  res_cv <- dplyr::bind_rows(
    res_cv,
    data.frame(
      model_id = "rf",
      modelo = paste0("Random Forest (mtry=", best_rf$mtry, ")"),
      RMSE = best_rf$RMSE, MAE = best_rf$MAE, R2 = best_rf$R2, PCT_0_5 = best_rf$PCT_0_5
    )
  )
  
  # ============================================================
  # 5.6 GBM - tune minimo por CV (depth + shrinkage)
  # ============================================================
  if (!("gbm" %in% (.packages()))) {
    # se o gbm nao foi carregado, tenta carregar; se nao existir, falha com msg clara
    if (!requireNamespace("gbm", quietly = TRUE)) {
      stop("Pacote 'gbm' nao instalado. Instala com install.packages('gbm') ou remove o bloco GBM.")
    }
  }
  
  grid_gbm <- expand.grid(
    interaction.depth = c(1, 2, 3),
    shrinkage = c(0.05, 0.1),
    n.trees = c(1500)
  )
  
  gbm_cv_rows <- data.frame()
  for (g in seq_len(nrow(grid_gbm))) {
    dep <- grid_gbm$interaction.depth[g]
    shr <- grid_gbm$shrinkage[g]
    nts <- grid_gbm$n.trees[g]
    
    met <- cv_evaluate(folds, y_tr, function(tr_idx, val_idx) {
      tr <- train[tr_idx, , drop = FALSE]
      va <- train[val_idx, , drop = FALSE]
      
      fit <- gbm::gbm(
        score_review ~ .,
        data = tr,
        distribution = "gaussian",
        n.trees = nts,
        interaction.depth = dep,
        shrinkage = shr,
        bag.fraction = 0.7,
        n.minobsinnode = 10,
        verbose = FALSE
      )
      pred <- stats::predict(fit, newdata = va, n.trees = nts)
      list(y_true = va$score_review, y_pred = pred)
    })
    
    gbm_cv_rows <- dplyr::bind_rows(gbm_cv_rows, data.frame(interaction.depth = dep, shrinkage = shr, n.trees = nts, t(met)))
  }
  write.csv(gbm_cv_rows, file.path(out_dir, "gbm_cv_grid.csv"), row.names = FALSE)
  best_gbm <- gbm_cv_rows |> dplyr::arrange(.data$RMSE) |> dplyr::slice(1)
  
  res_cv <- dplyr::bind_rows(
    res_cv,
    data.frame(
      model_id = "gbm",
      modelo = paste0("GBM (depth=", best_gbm$interaction.depth, ", shrink=", best_gbm$shrinkage, ", n.trees=", best_gbm$n.trees, ")"),
      RMSE = best_gbm$RMSE, MAE = best_gbm$MAE, R2 = best_gbm$R2, PCT_0_5 = best_gbm$PCT_0_5
    )
  )
  
  # ============================================================
  # Escolha por CV (SEM usar teste)
  # ============================================================
  res_cv_sorted <- res_cv |> dplyr::arrange(.data$RMSE)
  write.csv(res_cv_sorted, file.path(out_dir, "metricas_cv.csv"), row.names = FALSE)
  
  best_model_id   <- res_cv_sorted$model_id[1]
  best_model_desc <- res_cv_sorted$modelo[1]
  
  save_lines(
    c(paste("Melhor modelo escolhido por CV (treino):", best_model_id),
      paste("Descricao:", best_model_desc)),
    file.path(out_dir, "best_model_by_cv.txt")
  )
  
  # ============================================================
  # AJUSTE FINAL + TESTE FINAL (1x) - refit em train+val
  # ============================================================
  train_tv   <- dplyr::bind_rows(train,   val)
  train_tv_s <- dplyr::bind_rows(train_s, val_s)
  
  final_pred_test <- NULL
  
  if (best_model_id == "baseline") {
    final_pred_test <- baseline_predict(train_tv$score_review, nrow(test))
    saveRDS(list(mean = mean(train_tv$score_review, na.rm = TRUE)), file.path(out_dir, "modelo_baseline.rds"))
  }
  
  if (best_model_id == "lm") {
    lm_fit <- stats::lm(score_review ~ ., data = train_tv_s)
    final_pred_test <- stats::predict(lm_fit, newdata = test_s)
    
    saveRDS(lm_fit, file.path(out_dir, "modelo_lm.rds"))
    capture.output(summary(lm_fit), file = file.path(out_dir, "lm_summary.txt"))
    grDevices::png(file.path(out_dir, "lm_diagnosticos.png"), width = 1200, height = 900)
    par(mfrow = c(2, 2))
    plot(lm_fit)
    grDevices::dev.off()
  }
  
  if (best_model_id == "ridge") {
    mm_tv <- mmatrix(train_tv_s, target = "score_review")
    mm_te <- mmatrix(test_s,     target = "score_review")
    
    cv_r <- glmnet::cv.glmnet(mm_tv$x, mm_tv$y, alpha = 0, nfolds = 10, standardize = FALSE)
    fit  <- glmnet::glmnet(mm_tv$x, mm_tv$y, alpha = 0, lambda = cv_r$lambda.min, standardize = FALSE)
    final_pred_test <- as.numeric(stats::predict(fit, newx = mm_te$x))
    
    saveRDS(cv_r, file.path(out_dir, "cv_ridge.rds"))
    saveRDS(fit,  file.path(out_dir, "modelo_ridge.rds"))
    save_lines(c(paste("lambda.min:", cv_r$lambda.min), paste("lambda.1se:", cv_r$lambda.1se)),
               file.path(out_dir, "ridge_lambdas.txt"))
  }
  
  if (best_model_id == "lasso") {
    mm_tv <- mmatrix(train_tv_s, target = "score_review")
    mm_te <- mmatrix(test_s,     target = "score_review")
    
    cv_l <- glmnet::cv.glmnet(mm_tv$x, mm_tv$y, alpha = 1, nfolds = 10, standardize = FALSE)
    fit  <- glmnet::glmnet(mm_tv$x, mm_tv$y, alpha = 1, lambda = cv_l$lambda.min, standardize = FALSE)
    final_pred_test <- as.numeric(stats::predict(fit, newx = mm_te$x))
    
    saveRDS(cv_l, file.path(out_dir, "cv_lasso.rds"))
    saveRDS(fit,  file.path(out_dir, "modelo_lasso.rds"))
    save_lines(c(paste("lambda.min:", cv_l$lambda.min), paste("lambda.1se:", cv_l$lambda.1se)),
               file.path(out_dir, "lasso_lambdas.txt"))
    
    lasso_coefs <- as.matrix(stats::coef(fit))
    write.csv(lasso_coefs, file.path(out_dir, "lasso_coeficientes.csv"), row.names = TRUE)
  }
  
  if (best_model_id == "rpart") {
    tree_fit <- rpart::rpart(
      score_review ~ ., data = train_tv, method = "anova",
      control = rpart::rpart.control(cp = best_tree$cp, maxdepth = best_tree$maxdepth, xval = 10)
    )
    final_pred_test <- stats::predict(tree_fit, newdata = test)
    
    saveRDS(tree_fit, file.path(out_dir, "modelo_rpart.rds"))
    save_lines(c(paste("best_cp:", best_tree$cp), paste("best_maxdepth:", best_tree$maxdepth)),
               file.path(out_dir, "tree_best_params.txt"))
    grDevices::png(file.path(out_dir, "arvore_rpart.png"), width = 1200, height = 700)
    rpart.plot::rpart.plot(tree_fit, main = "Arvore de Regressao (rpart)")
    grDevices::dev.off()
  }
  
  if (best_model_id == "rf") {
    rf_fit <- randomForest::randomForest(
      score_review ~ ., data = train_tv,
      ntree = 600, mtry = best_rf$mtry, importance = TRUE
    )
    final_pred_test <- stats::predict(rf_fit, newdata = test)
    
    saveRDS(rf_fit, file.path(out_dir, "modelo_random_forest.rds"))
    save_lines(paste("best_mtry:", best_rf$mtry), file.path(out_dir, "rf_best_params.txt"))
    rf_imp <- randomForest::importance(rf_fit)
    write.csv(rf_imp, file.path(out_dir, "rf_importancia.csv"), row.names = TRUE)
  }
  
  if (best_model_id == "gbm") {
    fit <- gbm::gbm(
      score_review ~ .,
      data = train_tv,
      distribution = "gaussian",
      n.trees = 5000,
      interaction.depth = best_gbm$interaction.depth,
      shrinkage = best_gbm$shrinkage,
      bag.fraction = 0.7,
      cv.folds = 5,
      n.minobsinnode = 10,
      verbose = FALSE
    )
    best_iter <- gbm::gbm.perf(fit, method = "cv", plot.it = FALSE)
    final_pred_test <- stats::predict(fit, newdata = test, n.trees = best_iter)
    
    saveRDS(fit, file.path(out_dir, "modelo_gbm.rds"))
    save_lines(c(paste("best_iter:", best_iter),
                 paste("depth:", best_gbm$interaction.depth),
                 paste("shrinkage:", best_gbm$shrinkage)),
               file.path(out_dir, "gbm_best_params.txt"))
  }
  
  if (is.null(final_pred_test)) {
    stop("Falha: nao foi possivel gerar previsoes finais para o modelo escolhido: ", best_model_id)
  }
  
  # Guardar previsoes finais do TESTE (apenas do melhor)
  save_preds_generic("previsoes_teste_final.csv", y_te, final_pred_test, out_dir)
  
  # Metricas finais no TESTE (com e sem clipping)
  met_test <- data.frame(
    modelo = c(paste0("FINAL - ", best_model_desc), paste0("FINAL - ", best_model_desc, " - clipped[1,5]")),
    RMSE   = c(rmse(y_te, final_pred_test), rmse(y_te, clip_1_5(final_pred_test))),
    MAE    = c(mae(y_te, final_pred_test), mae(y_te, clip_1_5(final_pred_test))),
    R2     = c(r2(y_te, final_pred_test),  r2(y_te, clip_1_5(final_pred_test))),
    PCT_0_5= c(within_tolerance(y_te, final_pred_test, 0.5),
               within_tolerance(y_te, clip_1_5(final_pred_test), 0.5))
  )
  write.csv(met_test, file.path(out_dir, "metricas_teste_final.csv"), row.names = FALSE)
  
  # Resumo
  summary_lines <- c(
    "CAPITULO 5 - MODELACAO concluido.",
    paste("Seed:", seed),
    paste("CV folds:", k_folds),
    paste("Melhor por CV:", best_model_id, "-", best_model_desc),
    paste("Metricas CV em:", file.path(out_dir, "metricas_cv.csv")),
    paste("Metricas Teste FINAL em:", file.path(out_dir, "metricas_teste_final.csv")),
    paste("Previsoes Teste FINAL em:", file.path(out_dir, "previsoes_teste_final.csv"))
  )
  save_lines(summary_lines, file.path(out_dir, "cap5_resumo_final.txt"))
  
  print(res_cv_sorted)
  print(met_test)
  print(summary_lines)
  
  invisible(list(metrics_cv = res_cv_sorted, metrics_test_final = met_test, best_model_id = best_model_id))
}

