run_cap6 <- function(out_cap5, out_dir, tol = 0.5) {
  
  ensure_dir(out_dir)
  
  f_cv   <- file.path(out_cap5, "metricas_cv.csv")
  f_pred <- file.path(out_cap5, "previsoes_teste_final.csv")
  f_met  <- file.path(out_cap5, "metricas_teste_final.csv")
  
  must_exist <- c(f_cv, f_pred, f_met)
  missing_must <- must_exist[!file.exists(must_exist)]
  if (length(missing_must) > 0) {
    stop(paste0(
      "Faltam ficheiros essenciais do Cap.5:\n- ",
      paste(missing_must, collapse = "\n- "),
      "\n\nCorre primeiro o Cap.5 com sucesso."
    ))
  }
  
  metrics_cv   <- read.csv(f_cv, stringsAsFactors = FALSE)
  metrics_test <- read.csv(f_met, stringsAsFactors = FALSE)
  preds_test   <- read.csv(f_pred, stringsAsFactors = FALSE)
  
  if (!("y_true" %in% names(preds_test)) || !("y_pred" %in% names(preds_test))) {
    stop("previsoes_teste_final.csv tem de ter colunas 'y_true' e 'y_pred'.")
  }
  if (!("y_pred_clipped" %in% names(preds_test))) {
    preds_test$y_pred_clipped <- clip_1_5(preds_test$y_pred)
  }

  metrics_cv_ord <- metrics_cv |> dplyr::arrange(RMSE)
  write.csv(metrics_cv_ord, file.path(out_dir, "cap6_metricas_cv_ordenadas.csv"), row.names = FALSE)
  
  ranking <- metrics_cv_ord |>
    dplyr::mutate(rank_rmse = dplyr::row_number()) |>
    dplyr::select(rank_rmse, model_id, modelo, RMSE, MAE, R2, PCT_0_5)
  write.csv(ranking, file.path(out_dir, "cap6_ranking_modelos_cv.csv"), row.names = FALSE)

  grDevices::png(file.path(out_dir, "cap6_fig_rmse_cv_por_modelo.png"), width = 1400, height = 900, res = 150)
  op <- par(mar = c(10, 5, 3, 1))
  barplot(metrics_cv_ord$RMSE, names.arg = metrics_cv_ord$model_id, las = 2,
          main = "RMSE (CV no Treino) por Modelo", ylab = "RMSE",
          col = "steelblue", border = NA)
  abline(h = min(metrics_cv_ord$RMSE, na.rm = TRUE), lty = 2, col = "red")
  par(op)
  grDevices::dev.off()
  
  preds_diag <- add_pred_diagnostics(preds_test,
                                     y_true_col = "y_true",
                                     y_pred_col = "y_pred",
                                     y_pred_clip_col = "y_pred_clipped",
                                     tol = tol)
  
  write.csv(preds_diag, file.path(out_dir, "cap6_predicoes_com_diagnosticos.csv"), row.names = FALSE)
  
  save_base_png <- function(path, expr, width = 1400, height = 900, res = 150) {
    grDevices::png(path, width = width, height = height, res = res)
    on.exit(grDevices::dev.off(), add = TRUE)
    force(expr)
    invisible(path)
  }
  
  save_base_png(file.path(out_dir, "cap6_obs_vs_prev.png"), {
    plot(preds_diag$y_true, preds_diag$y_pred,
         xlab = "Observado (y_true)", ylab = "Previsto (y_pred)",
         main = "Teste Final: Observado vs Previsto",
         pch = 16, col = rgb(0.2, 0.4, 0.8, 0.35))
    abline(0, 1, lty = 2, col = "red")
  })
  
  save_base_png(file.path(out_dir, "cap6_residuos_vs_prev.png"), {
    plot(preds_diag$y_pred, preds_diag$residuo,
         xlab = "Previsto (y_pred)", ylab = "Resíduo (y_true - y_pred)",
         main = "Teste Final: Resíduos vs Previsto",
         pch = 16, col = rgb(0.8, 0.3, 0.2, 0.35))
    abline(h = 0, lty = 2, col = "grey40")
  })
  
  save_base_png(file.path(out_dir, "cap6_hist_residuos.png"), {
    hist(preds_diag$residuo, breaks = 25,
         main = "Teste Final: Histograma dos Resíduos",
         xlab = "Resíduo",
         col = "skyblue", border = "white")
  })
  
  save_base_png(file.path(out_dir, "cap6_hist_erro_abs.png"), {
    hist(preds_diag$residuo_abs, breaks = 25,
         main = "Teste Final: Histograma do Erro Absoluto",
         xlab = "|Resíduo|",
         col = "orange", border = "white")
  })
  
  perf_by_level <- preds_diag |>
    dplyr::group_by(y_true) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_pred = mean(y_pred, na.rm = TRUE),
      mae = mean(residuo_abs, na.rm = TRUE),
      pct_0_5 = mean(acc_0_5, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(y_true)

  write.csv(perf_by_level, file.path(out_dir, "cap6_desempenho_por_nivel_score.csv"), row.names = FALSE)
  
  save_base_png(file.path(out_dir, "cap6_pct_0_5_por_nivel_score.png"), {
    op <- par(mar = c(6, 5, 3, 1))
    
    bp <- barplot(
      height = perf_by_level$pct_0_5,
      names.arg = perf_by_level$y_true,
      ylim = c(0, 1),
      xlab = "score_review observado (y_true)",
      ylab = "PCT_0_5 (|erro| ≤ 0,5)",
      main = "Teste Final: Taxa dentro ±0,5 por nível do score",
      col = "seagreen3", border = NA
    )
    
    abline(h = mean(preds_diag$acc_0_5, na.rm = TRUE), lty = 2, col = "red")

    text(bp, perf_by_level$pct_0_5,
         labels = paste0("n=", perf_by_level$n),
         pos = 3, cex = 0.85)
    
    par(op)
  })
  
  opt_files <- list(
    lasso = file.path(out_cap5, "lasso_coeficientes_lambda1se.csv"),
    ridge = file.path(out_cap5, "ridge_coeficientes_lambda1se.csv"),
    tree  = file.path(out_cap5, "tree_importancia.csv"),
    rf    = file.path(out_cap5, "rf_importancia.csv"),
    gbm   = file.path(out_cap5, "gbm_rel_influence.csv")
  )
  
  top_rows <- data.frame()
  
  if (file.exists(opt_files$lasso)) {
    lasso_df <- read.csv(opt_files$lasso, check.names = FALSE)
    if (ncol(lasso_df) >= 2) {
      term_col <- names(lasso_df)[1]
      coef_col <- names(lasso_df)[2]
      tmp <- data.frame(variavel = lasso_df[[term_col]],
                        score = abs(as.numeric(lasso_df[[coef_col]])),
                        stringsAsFactors = FALSE)
      tmp <- tmp[tmp$variavel != "(Intercept)", , drop = FALSE]
      tmp <- tmp |> dplyr::arrange(dplyr::desc(score)) |> dplyr::slice(1:15)
      tmp$modelo <- "lasso"
      top_rows <- dplyr::bind_rows(top_rows, tmp)
    }
  }
  
  if (file.exists(opt_files$ridge)) {
    ridge_df <- read.csv(opt_files$ridge, check.names = FALSE)
    if (ncol(ridge_df) >= 2) {
      term_col <- names(ridge_df)[1]
      coef_col <- names(ridge_df)[2]
      tmp <- data.frame(variavel = ridge_df[[term_col]],
                        score = abs(as.numeric(ridge_df[[coef_col]])),
                        stringsAsFactors = FALSE)
      tmp <- tmp[tmp$variavel != "(Intercept)", , drop = FALSE]
      tmp <- tmp |> dplyr::arrange(dplyr::desc(score)) |> dplyr::slice(1:15)
      tmp$modelo <- "ridge"
      top_rows <- dplyr::bind_rows(top_rows, tmp)
    }
  }
  
  if (file.exists(opt_files$tree)) {
    tree_imp <- read.csv(opt_files$tree, stringsAsFactors = FALSE)
    if (all(c("variavel","importancia") %in% names(tree_imp))) {
      tmp <- tree_imp |> dplyr::arrange(dplyr::desc(importancia)) |> dplyr::slice(1:15)
      tmp <- tmp |> dplyr::transmute(modelo = "rpart", variavel = variavel, score = importancia)
      top_rows <- dplyr::bind_rows(top_rows, tmp)
    }
  }
  
  if (file.exists(opt_files$rf)) {
    rf_imp <- read.csv(opt_files$rf, check.names = FALSE)
    var_col <- names(rf_imp)[1]
    num_cols <- names(rf_imp)[sapply(rf_imp, is.numeric)]
    if (length(num_cols) > 0) {
      imp_col <- num_cols[1]
      tmp <- data.frame(variavel = rf_imp[[var_col]],
                        score = as.numeric(rf_imp[[imp_col]]),
                        stringsAsFactors = FALSE)
      tmp <- tmp |> dplyr::arrange(dplyr::desc(score)) |> dplyr::slice(1:15)
      tmp$modelo <- "rf"
      top_rows <- dplyr::bind_rows(top_rows, tmp)
    }
  }
  
  if (file.exists(opt_files$gbm)) {
    gbm_rel <- read.csv(opt_files$gbm, stringsAsFactors = FALSE)
    if (all(c("var","rel.inf") %in% names(gbm_rel))) {
      tmp <- gbm_rel |> dplyr::arrange(dplyr::desc(rel.inf)) |> dplyr::slice(1:15)
      tmp <- tmp |> dplyr::transmute(modelo = "gbm", variavel = var, score = rel.inf)
      top_rows <- dplyr::bind_rows(top_rows, tmp)
    }
  }
  
  if (nrow(top_rows) > 0) {
    write.csv(top_rows, file.path(out_dir, "cap6_top_importancias_por_modelo.csv"), row.names = FALSE)
    
  } 
  
  invisible(list(metrics_cv = metrics_cv_ord,
                 metrics_test = metrics_test,
                 preds_diag = preds_diag,
                 top_vars = top_rows))
}

run_cap6(out_cap5, out_cap6, tol = 0.5)
