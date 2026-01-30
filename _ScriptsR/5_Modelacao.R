run_cap5 <- function(train, val, test, train_s, val_s, test_s,
                     out_dir, seed = 1, k_folds = 7) {
  
  set.seed(seed)
  ensure_dir(out_dir)
  
  y_tr  <- train$score_review
  y_trs <- train_s$score_review
  y_te  <- test$score_review
  
  folds <- make_folds_stratified(y_tr, k = k_folds, seed = seed)
  
  save_base_png <- function(path, expr, width = 1200, height = 800, res = 150) {
    grDevices::png(path, width = width, height = height, res = res)
    on.exit(grDevices::dev.off(), add = TRUE)
    force(expr)
    invisible(path)
  }
  
  res_cv <- data.frame()
  
  cv_base <- cv_evaluate(folds, y_tr, function(tr_idx, val_idx) {
    y_hat <- baseline_predict(y_tr[tr_idx], length(val_idx))
    list(y_true = y_tr[val_idx], y_pred = y_hat)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(
    model_id = "baseline",
    modelo   = "Baseline (media treino)",
    t(cv_base)
  ))

  cv_lm <- cv_evaluate(folds, y_trs, function(tr_idx, val_idx) {
    tr <- train_s[tr_idx, , drop = FALSE]
    va <- train_s[val_idx, , drop = FALSE]
    fit  <- stats::lm(score_review ~ ., data = tr)
    pred <- stats::predict(fit, newdata = va)
    list(y_true = va$score_review, y_pred = pred)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(
    model_id = "lm",
    modelo   = "Regressao Linear (lm) [dados escalados]",
    t(cv_lm)
  ))
  
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Pacote 'glmnet' nao instalado. Instala com install.packages('glmnet').")
  }
  
  mm_trs <- mmatrix(train_s, target = "score_review")
  x_all  <- mm_trs$x
  y_all  <- mm_trs$y
  
  cv_ridge <- cv_evaluate(folds, y_all, function(tr_idx, val_idx) {
    x_tr <- x_all[tr_idx, , drop = FALSE]
    y_trf <- y_all[tr_idx]
    x_va <- x_all[val_idx, , drop = FALSE]
    y_va <- y_all[val_idx]
    
    cv_fit <- glmnet::cv.glmnet(x_tr, y_trf, alpha = 0, standardize = FALSE)
    pred   <- as.numeric(stats::predict(cv_fit, newx = x_va, s = "lambda.1se"))
    list(y_true = y_va, y_pred = pred)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(
    model_id = "ridge",
    modelo   = "Ridge (glmnet) [lambda.1se, nested-CV]",
    t(cv_ridge)
  ))
  
  cv_lasso <- cv_evaluate(folds, y_all, function(tr_idx, val_idx) {
    x_tr <- x_all[tr_idx, , drop = FALSE]
    y_trf <- y_all[tr_idx]
    x_va <- x_all[val_idx, , drop = FALSE]
    y_va <- y_all[val_idx]
    
    cv_fit <- glmnet::cv.glmnet(x_tr, y_trf, alpha = 1, standardize = FALSE)
    pred   <- as.numeric(stats::predict(cv_fit, newx = x_va, s = "lambda.1se"))
    list(y_true = y_va, y_pred = pred)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(
    model_id = "lasso",
    modelo   = "Lasso (glmnet) [lambda.1se, nested-CV]",
    t(cv_lasso)
  ))
  
  if (!requireNamespace("rpart", quietly = TRUE)) {
    stop("Pacote 'rpart' nao instalado. Instala com install.packages('rpart').")
  }
  
  grid_tree <- expand.grid(
    cp = c(0.001, 0.005, 0.01, 0.02, 0.05),
    maxdepth = c(2, 3, 4, 5, 6)
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
    
    tree_cv_rows <- dplyr::bind_rows(
      tree_cv_rows,
      data.frame(cp = cpv, maxdepth = md, t(met))
    )
  }
  write.csv(tree_cv_rows, file.path(out_dir, "tree_cv_grid.csv"), row.names = FALSE)
  best_tree <- tree_cv_rows |> dplyr::arrange(RMSE) |> dplyr::slice(1)
  
  res_cv <- dplyr::bind_rows(res_cv, data.frame(
    model_id = "rpart",
    modelo   = paste0("Arvore (rpart) cp=", best_tree$cp, " maxdepth=", best_tree$maxdepth),
    RMSE     = best_tree$RMSE, MAE = best_tree$MAE, R2 = best_tree$R2, PCT_0_5 = best_tree$PCT_0_5
  ))

  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Pacote 'randomForest' nao instalado. Instala com install.packages('randomForest').")
  }
  
  p <- ncol(train) - 1
  mtry_grid <- unique(pmax(1, c(2, floor(sqrt(p)), floor(p/3), seq(2, p, by = 2))))
  mtry_grid <- sort(mtry_grid[mtry_grid <= p])
  
  rf_cv_rows <- data.frame()
  for (mtryv in mtry_grid) {
    met <- cv_evaluate(folds, y_tr, function(tr_idx, val_idx) {
      tr <- train[tr_idx, , drop = FALSE]
      va <- train[val_idx, , drop = FALSE]
      set.seed(seed + mtryv)
      fit <- randomForest::randomForest(
        score_review ~ ., data = tr,
        ntree = 400, mtry = mtryv, importance = TRUE
      )
      pred <- stats::predict(fit, newdata = va)
      list(y_true = va$score_review, y_pred = pred)
    })
    rf_cv_rows <- dplyr::bind_rows(rf_cv_rows, data.frame(mtry = mtryv, t(met)))
  }
  write.csv(rf_cv_rows, file.path(out_dir, "rf_cv_grid.csv"), row.names = FALSE)
  best_rf <- rf_cv_rows |> dplyr::arrange(RMSE) |> dplyr::slice(1)

  save_base_png(file.path(out_dir, "cap5_rf_tuning_rmse_vs_mtry.png"), {
    o <- rf_cv_rows[order(rf_cv_rows$mtry), , drop = FALSE]
    
    plot(o$mtry, o$RMSE, type = "b",
         xlab = "mtry", ylab = "RMSE (CV)",
         main = "Random Forest tuning: RMSE (CV) vs mtry (ntree=400)",
         col = "steelblue", pch = 16)
    
    best_i <- which.min(o$RMSE)
    abline(v = o$mtry[best_i], lty = 2, col = "red")
    points(o$mtry[best_i], o$RMSE[best_i], pch = 19, col = "red")
    
    text(o$mtry, o$RMSE, labels = round(o$RMSE, 3), pos = 3, cex = 0.8)
  })
  
  
  res_cv <- dplyr::bind_rows(res_cv, data.frame(
    model_id = "rf",
    modelo   = paste0("Random Forest (mtry=", best_rf$mtry, ", ntree=400)"),
    RMSE     = best_rf$RMSE, MAE = best_rf$MAE, R2 = best_rf$R2, PCT_0_5 = best_rf$PCT_0_5
  ))
  
  if (!requireNamespace("gbm", quietly = TRUE)) {
    stop("Pacote 'gbm' nao instalado. Instala com install.packages('gbm').")
  }
  
  grid_gbm <- expand.grid(
    interaction.depth = c(1, 2, 3),
    shrinkage         = c(0.05, 0.1, 0.2),
    n.trees           = c(500, 1000, 2000)
  )
  
  gbm_cv_rows <- data.frame()
  for (g in seq_len(nrow(grid_gbm))) {
    dep <- grid_gbm$interaction.depth[g]
    shr <- grid_gbm$shrinkage[g]
    nts <- grid_gbm$n.trees[g]
    
    met <- cv_evaluate(folds, y_tr, function(tr_idx, val_idx) {
      tr <- train[tr_idx, , drop = FALSE]
      va <- train[val_idx, , drop = FALSE]
      set.seed(seed + dep * 100 + round(shr * 100) + nts)
      
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
    
    gbm_cv_rows <- dplyr::bind_rows(
      gbm_cv_rows,
      data.frame(interaction.depth = dep, shrinkage = shr, n.trees = nts, t(met))
    )
  }
  write.csv(gbm_cv_rows, file.path(out_dir, "gbm_cv_grid.csv"), row.names = FALSE)
  best_gbm <- gbm_cv_rows |> dplyr::arrange(RMSE) |> dplyr::slice(1)
  
  # --- FIGURA NOVA: GBM tuning (heatmap RMSE por depth x n.trees; painéis por shrinkage) ---
  save_base_png(file.path(out_dir, "cap5_gbm_tuning_heatmap_rmse.png"), {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)
    
    sh_vals <- sort(unique(gbm_cv_rows$shrinkage))
    par(mfrow = c(1, length(sh_vals)), mar = c(4, 4, 4, 1))
    
    for (sh in sh_vals) {
      sub <- gbm_cv_rows[gbm_cv_rows$shrinkage == sh, , drop = FALSE]
      depths <- sort(unique(sub$interaction.depth))
      nts    <- sort(unique(sub$n.trees))
      
      z <- matrix(NA_real_, nrow = length(depths), ncol = length(nts),
                  dimnames = list(depths, nts))
      for (i in seq_along(depths)) {
        for (j in seq_along(nts)) {
          z[i, j] <- sub$RMSE[sub$interaction.depth == depths[i] & sub$n.trees == nts[j]]
        }
      }

      image(x = nts, y = depths, z = t(z),
            xlab = "n.trees", ylab = "interaction.depth",
            main = paste0("GBM tuning (shrinkage=", sh, ")\nRMSE (CV)"),
            axes = FALSE,
            col = grDevices::hcl.colors(50, "YlOrRd", rev = TRUE))
      
      axis(1, at = nts)
      axis(2, at = depths)
      
      best_k <- which(z == min(z, na.rm = TRUE), arr.ind = TRUE)[1, , drop = FALSE]
      best_depth <- depths[best_k[1]]
      best_nts   <- nts[best_k[2]]
      points(best_nts, best_depth, pch = 19, col = "blue")
      
      box()
    }
  })
  
  
  res_cv <- dplyr::bind_rows(res_cv, data.frame(
    model_id = "gbm",
    modelo   = paste0("GBM (depth=", best_gbm$interaction.depth,
                      ", shrinkage=", best_gbm$shrinkage,
                      ", n.trees=", best_gbm$n.trees, ")"),
    RMSE     = best_gbm$RMSE, MAE = best_gbm$MAE, R2 = best_gbm$R2, PCT_0_5 = best_gbm$PCT_0_5
  ))
  
  res_cv_sorted <- res_cv |> dplyr::arrange(RMSE)
  write.csv(res_cv_sorted, file.path(out_dir, "metricas_cv.csv"), row.names = FALSE)
  
  best_model_id   <- res_cv_sorted$model_id[1]
  best_model_desc <- res_cv_sorted$modelo[1]
  
  
  train_tv   <- dplyr::bind_rows(train,   val)
  train_tv_s <- dplyr::bind_rows(train_s, val_s)
  
  dataset_final_best <- if (best_model_id %in% c("lm","ridge","lasso")) train_tv_s else train_tv
  
  write.csv(dataset_final_best,
            file.path(out_dir, "dataset_final_best_model.csv"),
            row.names = FALSE)
  
  final_pred_test <- NULL

  if (best_model_id == "baseline") {
    final_pred_test <- baseline_predict(train_tv$score_review, nrow(test))
    saveRDS(list(mean = mean(train_tv$score_review, na.rm = TRUE)), file.path(out_dir, "modelo_baseline.rds"))
  }

  if (best_model_id == "lm") {
    lm_fit <- stats::lm(score_review ~ ., data = train_tv_s)
    final_pred_test <- stats::predict(lm_fit, newdata = test_s)
    
    saveRDS(lm_fit, file.path(out_dir, "modelo_lm.rds"))
    
    sm <- summary(lm_fit)
    coefs <- as.data.frame(sm$coefficients)
    coefs$term <- rownames(coefs)
    rownames(coefs) <- NULL
    coefs <- coefs[, c("term", names(coefs)[1:4])]
    write.csv(coefs, file.path(out_dir, "lm_coeficientes.csv"), row.names = FALSE)
    
    
    save_base_png(file.path(out_dir, "lm_diagnosticos.png"), {
      par(mfrow = c(2, 2))
      plot(lm_fit)
    }, width = 1400, height = 1000)
  }
  
  if (best_model_id == "ridge") {
    mm_tv <- mmatrix(train_tv_s, target = "score_review")
    x_tv  <- mm_tv$x
    y_tv  <- mm_tv$y
    
    cv_fit <- glmnet::cv.glmnet(x_tv, y_tv, alpha = 0, standardize = FALSE)
    final_pred_test <- as.numeric(stats::predict(cv_fit, newx = mmatrix(test_s)$x, s = "lambda.1se"))
    
    saveRDS(cv_fit, file.path(out_dir, "modelo_ridge_cvglmnet.rds"))
    
    save_base_png(file.path(out_dir, "ridge_cv_plot.png"), {
      plot(cv_fit)
    })
    
    ridge_coefs <- as.matrix(stats::coef(cv_fit, s = "lambda.1se"))
    write.csv(ridge_coefs, file.path(out_dir, "ridge_coeficientes_lambda1se.csv"), row.names = TRUE)
  }
  
  if (best_model_id == "lasso") {
    mm_tv <- mmatrix(train_tv_s, target = "score_review")
    x_tv  <- mm_tv$x
    y_tv  <- mm_tv$y
    
    cv_fit <- glmnet::cv.glmnet(x_tv, y_tv, alpha = 1, standardize = FALSE)
    final_pred_test <- as.numeric(stats::predict(cv_fit, newx = mmatrix(test_s)$x, s = "lambda.1se"))
    
    saveRDS(cv_fit, file.path(out_dir, "modelo_lasso_cvglmnet.rds"))

    save_base_png(file.path(out_dir, "lasso_cv_plot.png"), {
      plot(cv_fit)
    })
    
    lasso_coefs <- as.matrix(stats::coef(cv_fit, s = "lambda.1se"))
    write.csv(lasso_coefs, file.path(out_dir, "lasso_coeficientes_lambda1se.csv"), row.names = TRUE)
  }
  
  if (best_model_id == "rpart") {
    tree_fit <- rpart::rpart(
      score_review ~ ., data = train_tv, method = "anova",
      control = rpart::rpart.control(cp = best_tree$cp, maxdepth = best_tree$maxdepth, xval = 10)
    )
    final_pred_test <- stats::predict(tree_fit, newdata = test)
    
    saveRDS(tree_fit, file.path(out_dir, "modelo_rpart.rds"))
    
    
    cpt <- as.data.frame(tree_fit$cptable)
    write.csv(cpt, file.path(out_dir, "tree_cptable.csv"), row.names = FALSE)
    
    save_base_png(file.path(out_dir, "tree_plotcp.png"), {
      rpart::plotcp(tree_fit)
    })
    
    if (requireNamespace("rpart.plot", quietly = TRUE)) {
      save_base_png(file.path(out_dir, "arvore_rpart.png"), {
        rpart.plot::rpart.plot(tree_fit, main = "Arvore de Regressao (rpart)")
      }, width = 1400, height = 800)
    } else {
      save_base_png(file.path(out_dir, "arvore_rpart.png"), {
        plot(tree_fit, uniform = TRUE, margin = 0.1)
        text(tree_fit, use.n = TRUE, cex = 0.8)
      }, width = 1400, height = 800)
    }
    
    imp <- tree_fit$variable.importance
    if (!is.null(imp)) {
      imp_df <- data.frame(variavel = names(imp), importancia = as.numeric(imp))
      imp_df <- imp_df |> dplyr::arrange(dplyr::desc(importancia))
      write.csv(imp_df, file.path(out_dir, "tree_importancia.csv"), row.names = FALSE)
    }
  }
  
  if (best_model_id == "rf") {
    set.seed(seed)
    rf_fit <- randomForest::randomForest(
      score_review ~ ., data = train_tv,
      ntree = 600, mtry = best_rf$mtry, importance = TRUE
    )
    final_pred_test <- stats::predict(rf_fit, newdata = test)
    
    saveRDS(rf_fit, file.path(out_dir, "modelo_random_forest.rds"))
    
    rf_imp <- randomForest::importance(rf_fit)
    rf_imp_df <- data.frame(variavel = rownames(rf_imp), rf_imp, row.names = NULL)
    write.csv(rf_imp_df, file.path(out_dir, "rf_importancia.csv"), row.names = FALSE)
    
    save_base_png(file.path(out_dir, "rf_varImpPlot.png"), {
      randomForest::varImpPlot(
        rf_fit,
        main = "Random Forest - Importancia das Variaveis",
        col = "steelblue"
      )
    }, width = 1400, height = 900)
    
    save_base_png(file.path(out_dir, "rf_oob_error.png"), {
      plot(
        rf_fit,
        main = "Random Forest - Erro OOB vs Numero de Arvores",
        col = "tomato"
      )
    }, width = 1400, height = 900)
    
  if (best_model_id == "gbm") {
    set.seed(seed)
    fit <- gbm::gbm(
      score_review ~ .,
      data = train_tv,
      distribution = "gaussian",
      n.trees = max(5000, best_gbm$n.trees),
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
    
    rel <- gbm::summary.gbm(fit, n.trees = best_iter, plotit = FALSE)
    rel <- rel |> dplyr::arrange(dplyr::desc(rel.inf))
    write.csv(rel, file.path(out_dir, "gbm_rel_influence.csv"), row.names = FALSE)
    
    save_base_png(file.path(out_dir, "gbm_rel_influence.png"), {
      gbm::summary.gbm(fit, n.trees = best_iter, main = "GBM - Relative Influence", plotit = TRUE)
    }, width = 1400, height = 900)

    top_vars <- head(rel$var, 3)
    for (v in top_vars) {
      outp <- file.path(out_dir, paste0("gbm_pdp_", v, ".png"))
      save_base_png(outp, {
        gbm::plot.gbm(fit, i.var = v, n.trees = best_iter, main = paste0("GBM - PDP: ", v))
      }, width = 1400, height = 900)
    }
  }
  
  if (is.null(final_pred_test)) {
    stop("Falha: nao foi possivel gerar previsoes finais para o modelo escolhido.")
  }

  df_preds <- data.frame(
    y_true = y_te,
    y_pred = as.numeric(final_pred_test)
  )
  df_preds$y_pred_clipped <- pmin(pmax(df_preds$y_pred, 1), 5)
  
  write.csv(
    df_preds,
    file.path(out_dir, "previsoes_teste_final.csv"),
    row.names = FALSE
  )
  
  met_test <- data.frame(
    modelo = c(paste0("FINAL - ", best_model_desc),
               paste0("FINAL - ", best_model_desc, " - clipped[1,5]")),
    RMSE   = c(rmse(y_te, final_pred_test), rmse(y_te, clip_1_5(final_pred_test))),
    MAE    = c(mae(y_te, final_pred_test), mae(y_te, clip_1_5(final_pred_test))),
    R2     = c(r2(y_te, final_pred_test),  r2(y_te, clip_1_5(final_pred_test))),
    PCT_0_5= c(within_tolerance(y_te, final_pred_test, 0.5),
               within_tolerance(y_te, clip_1_5(final_pred_test), 0.5))
  )
  
  
  write.csv(met_test, file.path(out_dir, "metricas_teste_final.csv"), row.names = FALSE)
  
  cv_best <- res_cv_sorted[res_cv_sorted$model_id == best_model_id,
                           c("RMSE", "MAE", "R2", "PCT_0_5")]

  test_best <- met_test[1, c("RMSE", "MAE", "R2", "PCT_0_5")]
  
  tab_cv_vs_test <- rbind(
    data.frame(Avaliacao = "CV (treino, media k-fold)", cv_best, row.names = NULL),
    data.frame(Avaliacao = paste0("Teste (holdout, n=", length(y_te), ")"), test_best, row.names = NULL)
  )
  
  tab_cv_vs_test$Modelo <- paste0("FINAL - ", best_model_desc)
  tab_cv_vs_test <- tab_cv_vs_test[, c("Modelo", "Avaliacao", "RMSE", "MAE", "R2", "PCT_0_5")]
  
  write.csv(tab_cv_vs_test, file.path(out_dir, "cap5_cv_vs_teste_best_model.csv"), row.names = FALSE)
  
  print(res_cv_sorted)
  print(met_test)
  
  invisible(list(
    metrics_cv = res_cv_sorted,
    metrics_test_final = met_test,
    best_model_id = best_model_id
  ))
}
}
run_cap5(train, val, test, train_s, val_s, test_s, out_cap5, seed = seed)
