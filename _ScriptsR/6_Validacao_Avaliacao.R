###############################################################################
# 6_Validacao_Avaliacao.R
###############################################################################

run_cap6 <- function(cap5_dir, out_dir) {
  
  ensure_dir(out_dir)
  
  # Inputs vindos do Cap5
  cv_path         <- file.path(cap5_dir, "metricas_cv.csv")
  best_path       <- file.path(cap5_dir, "best_model_by_cv.txt")
  test_final_path <- file.path(cap5_dir, "metricas_teste_final.csv")
  pred_final_path <- file.path(cap5_dir, "previsoes_teste_final.csv")
  
  # ---- CV metrics (ordenar + guardar)
  metrics_cv <- read.csv(cv_path, stringsAsFactors = FALSE) |>
    dplyr::arrange(.data$RMSE)
  
  write.csv(metrics_cv, file.path(out_dir, "metricas_cv_final.csv"), row.names = FALSE)
  
  best_id   <- metrics_cv$model_id[1]
  best_desc <- metrics_cv$modelo[1]
  
  save_lines(
    c("Melhor modelo (criterio: menor RMSE por CV no treino):",
      paste(best_id, "-", best_desc)),
    file.path(out_dir, "melhor_modelo_por_cv.txt")
  )
  
  # ---- Test final metrics (só 1 modelo, com/sem clipping)
  metrics_test_final <- read.csv(test_final_path, stringsAsFactors = FALSE)
  write.csv(metrics_test_final, file.path(out_dir, "metricas_teste_final.csv"), row.names = FALSE)
  
  test_row_unc  <- metrics_test_final |> dplyr::slice(1)
  test_row_clip <- if (nrow(metrics_test_final) >= 2) metrics_test_final |> dplyr::slice(2) else NULL
  
  # ---- Previsões finais + diagnosticos
  pred_best <- read.csv(pred_final_path, stringsAsFactors = FALSE) |>
    add_pred_diagnostics(tol = 0.5)
  
  write.csv(pred_best, file.path(out_dir, "diagnosticos_previsoes_teste_final.csv"), row.names = FALSE)
  
  # ---- Graficos
  save_plot(
    plot_obs_vs_pred(
      pred_best, y_pred_col = "y_pred",
      title = paste("Observado vs Previsto (Teste final) -", best_id),
      subtitle = best_desc
    ),
    file.path(out_dir, "fig_obs_vs_prev_teste_final.png"),
    width = 7, height = 5
  )
  
  save_plot(
    plot_obs_vs_pred(
      pred_best, y_pred_col = "y_pred_clipped",
      title = paste("Observado vs Previsto (Teste final, clipped[1,5]) -", best_id),
      subtitle = best_desc
    ),
    file.path(out_dir, "fig_obs_vs_prev_teste_final_clipped.png"),
    width = 7, height = 5
  )
  
  save_plot(
    plot_resid_vs_pred(
      pred_best, y_pred_col = "y_pred", resid_col = "residuo",
      title = paste("Residuos vs Previsto (Teste final) -", best_id),
      subtitle = best_desc
    ),
    file.path(out_dir, "fig_residuos_vs_prev_teste_final.png"),
    width = 7, height = 5
  )
  
  save_plot(
    plot_hist(
      pred_best, x_col = "residuo", bins = 30,
      title = paste("Distribuicao dos residuos (Teste final) -", best_id),
      subtitle = best_desc,
      xlab = "Residuo (y_true - y_pred)"
    ),
    file.path(out_dir, "fig_hist_residuos_teste_final.png"),
    width = 7, height = 5
  )
  
  save_plot(
    plot_hist(
      pred_best, x_col = "residuo_abs", bins = 30,
      title = paste("Distribuicao do erro absoluto (Teste final) -", best_id),
      subtitle = best_desc,
      xlab = "|Residuo|"
    ),
    file.path(out_dir, "fig_erro_absoluto_teste_final.png"),
    width = 7, height = 5
  )
  
  save_plot(
    plot_rmse_bar_cv(metrics_cv),
    file.path(out_dir, "fig_rmse_por_modelo_CV.png"),
    width = 8, height = 5
  )
  
  # ---- Acc dentro de ±0.5
  acc05      <- mean(pred_best$acc_0_5, na.rm = TRUE)
  acc05_clip <- mean(pred_best$acc_0_5_clip, na.rm = TRUE)
  
  save_lines(
    c(
      paste("Percentagem dentro de ±0.5 (teste final, sem clipping):", round(100 * acc05, 2), "%"),
      paste("Percentagem dentro de ±0.5 (teste final, clipped[1,5]):", round(100 * acc05_clip, 2), "%")
    ),
    file.path(out_dir, "acc_within_0_5_teste_final.txt")
  )
  
  # ---- Resumo final
  summary_lines <- c(
    "CAPITULO 6 - VALIDACAO E AVALIACAO concluido.",
    paste("Melhor modelo (por CV treino):", best_id),
    paste("Descricao:", best_desc),
    paste("TESTE FINAL - MAE:",  round(test_row_unc$MAE,  4)),
    paste("TESTE FINAL - RMSE:", round(test_row_unc$RMSE, 4)),
    paste("TESTE FINAL - R2:",   round(test_row_unc$R2,   4)),
    paste("TESTE FINAL - % dentro ±0.5:", round(100 * test_row_unc$PCT_0_5, 2), "%"),
    if (!is.null(test_row_clip)) paste("TESTE FINAL (clipped) - RMSE:", round(test_row_clip$RMSE, 4)) else NULL,
    paste("Outputs em:", out_dir)
  )
  summary_lines <- summary_lines[!is.na(summary_lines) & nzchar(summary_lines)]
  save_lines(summary_lines, file.path(out_dir, "cap6_resumo_final.txt"))
  
  print(metrics_cv)
  print(metrics_test_final)
  print(summary_lines)
  
  invisible(list(best_model_id = best_id, metrics_cv = metrics_cv, metrics_test_final = metrics_test_final))
}
