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

  save_preds_generic <- function(fname, y_true, y_pred, dir_out) {
    dfp <- data.frame(y_true = y_true, y_pred = y_pred)
    write.csv(dfp, file.path(dir_out, fname), row.names = FALSE)
    invisible(dfp)
  }

  # ---------------------------------------------------------------------------
  # Tabelas de resultados
  # ---------------------------------------------------------------------------
  res_cv <- data.frame()

  # ============================================================
  # 5.0 Baseline (média do treino)
  # ============================================================
  baseline_predict <- function(y_train, n) rep(mean(y_train, na.rm = TRUE), n)

  cv_base <- cv_evaluate(folds, y_tr, function(tr_idx, val_idx) {
    y_hat <- baseline_predict(y_tr[tr_idx], length(val_idx))
    list(y_true = y_tr[val_idx], y_pred = y_hat)
  })
  res_cv <- dplyr::bind_rows(res_cv, data.frame(
    model_id = "baseline",
    modelo   = "Baseline (media treino)",
    t(cv_base)
  ))

  # ============================================================
  # 5.1 Regressao Linear (lm) - CV em train_s
  # ============================================================
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

  # ============================================================
  # 5.2 Ridge / 5.3 Lasso (glmnet) - Nested CV (cv.glmnet dentro de cada fold)
  # ============================================================
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

  # ============================================================
  # 5.4 Arvore de Regressao (rpart) - grid (cp x maxdepth) via CV
  # ============================================================
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

  # ============================================================
  # 5.5 Random Forest - tune mtry via CV (ntree fixo)
  # ============================================================
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
      set.seed(seed + mtryv)  # reprodutibilidade por configuração
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

  res_cv <- dplyr::bind_rows(res_cv, data.frame(
    model_id = "rf",
    modelo   = paste0("Random Forest (mtry=", best_rf$mtry, ", ntree=400)"),
    RMSE     = best_rf$RMSE, MAE = best_rf$MAE, R2 = best_rf$R2, PCT_0_5 = best_rf$PCT_0_5
  ))

  # ============================================================
  # 5.6 GBM - grid simples via CV (depth + shrinkage + n.trees)
  # ============================================================
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

  res_cv <- dplyr::bind_rows(res_cv, data.frame(
    model_id = "gbm",
    modelo   = paste0("GBM (depth=", best_gbm$interaction.depth,
                      ", shrinkage=", best_gbm$shrinkage,
                      ", n.trees=", best_gbm$n.trees, ")"),
    RMSE     = best_gbm$RMSE, MAE = best_gbm$MAE, R2 = best_gbm$R2, PCT_0_5 = best_gbm$PCT_0_5
  ))

  # ============================================================
  # Escolha por CV (SEM usar teste)
  # ============================================================
  res_cv_sorted <- res_cv |> dplyr::arrange(RMSE)
  write.csv(res_cv_sorted, file.path(out_dir, "metricas_cv.csv"), row.names = FALSE)

  best_model_id   <- res_cv_sorted$model_id[1]
  best_model_desc <- res_cv_sorted$modelo[1]

  save_lines(
    c(paste("Melhor modelo escolhido por CV (treino):", best_model_id),
      paste("Descricao:", best_model_desc)),
    file.path(out_dir, "best_model_by_cv.txt")
  )

 
  train_tv   <- dplyr::bind_rows(train,   val)
  train_tv_s <- dplyr::bind_rows(train_s, val_s)

  final_pred_test <- NULL

  # --- Baseline
  if (best_model_id == "baseline") {
    final_pred_test <- baseline_predict(train_tv$score_review, nrow(test))
    saveRDS(list(mean = mean(train_tv$score_review, na.rm = TRUE)), file.path(out_dir, "modelo_baseline.rds"))
  }

  # --- LM
  if (best_model_id == "lm") {
    lm_fit <- stats::lm(score_review ~ ., data = train_tv_s)
    final_pred_test <- stats::predict(lm_fit, newdata = test_s)

    saveRDS(lm_fit, file.path(out_dir, "modelo_lm.rds"))
    capture.output(summary(lm_fit), file = file.path(out_dir, "lm_summary.txt"))


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

  # --- Ridge
  if (best_model_id == "ridge") {
    mm_tv <- mmatrix(train_tv_s, target = "score_review")
    x_tv  <- mm_tv$x
    y_tv  <- mm_tv$y

    cv_fit <- glmnet::cv.glmnet(x_tv, y_tv, alpha = 0, standardize = FALSE)
    final_pred_test <- as.numeric(stats::predict(cv_fit, newx = mmatrix(test_s)$x, s = "lambda.1se"))

    saveRDS(cv_fit, file.path(out_dir, "modelo_ridge_cvglmnet.rds"))
    save_lines(c(
      paste("lambda_min:", cv_fit$lambda.min),
      paste("lambda_1se:", cv_fit$lambda.1se)
    ), file.path(out_dir, "ridge_lambdas.txt"))

    # plot CV
    save_base_png(file.path(out_dir, "ridge_cv_plot.png"), {
      plot(cv_fit)
    })

    ridge_coefs <- as.matrix(stats::coef(cv_fit, s = "lambda.1se"))
    write.csv(ridge_coefs, file.path(out_dir, "ridge_coeficientes_lambda1se.csv"), row.names = TRUE)
  }

  # --- Lasso
  if (best_model_id == "lasso") {
    mm_tv <- mmatrix(train_tv_s, target = "score_review")
    x_tv  <- mm_tv$x
    y_tv  <- mm_tv$y

    cv_fit <- glmnet::cv.glmnet(x_tv, y_tv, alpha = 1, standardize = FALSE)
    final_pred_test <- as.numeric(stats::predict(cv_fit, newx = mmatrix(test_s)$x, s = "lambda.1se"))

    saveRDS(cv_fit, file.path(out_dir, "modelo_lasso_cvglmnet.rds"))
    save_lines(c(
      paste("lambda_min:", cv_fit$lambda.min),
      paste("lambda_1se:", cv_fit$lambda.1se)
    ), file.path(out_dir, "lasso_lambdas.txt"))

    # plot CV
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
    save_lines(c(
      paste("best_cp:", best_tree$cp),
      paste("best_maxdepth:", best_tree$maxdepth)
    ), file.path(out_dir, "tree_best_params.txt"))


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

    # Importância (rpart)
    imp <- tree_fit$variable.importance
    if (!is.null(imp)) {
      imp_df <- data.frame(variavel = names(imp), importancia = as.numeric(imp))
      imp_df <- imp_df |> dplyr::arrange(dplyr::desc(importancia))
      write.csv(imp_df, file.path(out_dir, "tree_importancia.csv"), row.names = FALSE)
    }
  }

  # --- Random Forest + importância + OOB
  if (best_model_id == "rf") {
    set.seed(seed)
    rf_fit <- randomForest::randomForest(
      score_review ~ ., data = train_tv,
      ntree = 600, mtry = best_rf$mtry, importance = TRUE
    )
    final_pred_test <- stats::predict(rf_fit, newdata = test)

    saveRDS(rf_fit, file.path(out_dir, "modelo_random_forest.rds"))
    save_lines(paste("best_mtry:", best_rf$mtry), file.path(out_dir, "rf_best_params.txt"))

    rf_imp <- randomForest::importance(rf_fit)
    rf_imp_df <- data.frame(variavel = rownames(rf_imp), rf_imp, row.names = NULL)
    write.csv(rf_imp_df, file.path(out_dir, "rf_importancia.csv"), row.names = FALSE)
    

    # VarImpPlot (matéria: importância de variáveis)
    save_base_png(file.path(out_dir, "rf_varImpPlot.png"), {
      randomForest::varImpPlot(rf_fit, main = "Random Forest - Importancia das Variaveis")
    }, width = 1400, height = 900)

    # OOB error plot
    save_base_png(file.path(out_dir, "rf_oob_error.png"), {
      plot(rf_fit, main = "Random Forest - Erro OOB vs Numero de Arvores")
    }, width = 1400, height = 900)
  }

  # --- GBM + importância (relative influence) + perf + PDPs
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
    save_lines(c(
      paste("best_iter:", best_iter),
      paste("depth:", best_gbm$interaction.depth),
      paste("shrinkage:", best_gbm$shrinkage),
      paste("grid_n.trees:", best_gbm$n.trees)
    ), file.path(out_dir, "gbm_best_params.txt"))

    # Relative influence (matéria: importância no boosting)
    rel <- gbm::summary.gbm(fit, n.trees = best_iter, plotit = FALSE)
    rel <- rel |> dplyr::arrange(dplyr::desc(rel.inf))
    write.csv(rel, file.path(out_dir, "gbm_rel_influence.csv"), row.names = FALSE)

    save_base_png(file.path(out_dir, "gbm_rel_influence.png"), {
      gbm::summary.gbm(fit, n.trees = best_iter, main = "GBM - Relative Influence", plotit = TRUE)
    }, width = 1400, height = 900)

    # Partial Dependence Plots (top 3 variáveis)
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

  # Guardar previsoes finais do TESTE
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
  

  # Metricas finais no TESTE (com e sem clipping)
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

  summary_lines <- c(
    "=== CAP 5 - RESUMO (MODELACAO) ===",
    paste("Folds CV (treino):", k_folds),
    paste("Seed:", seed),
    "",
    paste("Melhor modelo por CV (treino):", best_model_id),
    paste("Descricao:", best_model_desc),
    "",
    paste("Metricas CV (ordenadas por RMSE):", file.path(out_dir, "metricas_cv.csv")),
    paste("Metricas Teste FINAL em:", file.path(out_dir, "metricas_teste_final.csv")),
    paste("Previsoes Teste FINAL em:", file.path(out_dir, "previsoes_teste_final.csv")),
    "",
    "Artefactos de interpretacao guardados (quando aplicavel):",
    "- lm: lm_summary.txt, lm_diagnosticos.png, lm_coeficientes.csv",
    "- glmnet: *_lambdas.txt, *_coeficientes_*.csv, *_cv_plot.png",
    "- rpart: tree_cptable.csv, tree_plotcp.png, arvore_rpart.png, tree_importancia.csv",
    "- rf: rf_importancia.csv, rf_varImpPlot.png, rf_oob_error.png",
    "- gbm: gbm_rel_influence.csv, gbm_rel_influence.png, gbm_pdp_*.png"
  )
  save_lines(summary_lines, file.path(out_dir, "cap5_resumo_final.txt"))

  print(res_cv_sorted)
  print(met_test)

  invisible(list(
    metrics_cv = res_cv_sorted,
    metrics_test_final = met_test,
    best_model_id = best_model_id
  ))
}

run_cap5(train, val, test, train_s, val_s, test_s, out_cap5, seed = seed)
