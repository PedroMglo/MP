###############################################################################
# 6_Validacao_Avaliacao.R
# Capitulo 6: Validacao/Avaliacao (usa melhor por CV, teste só no fim)
###############################################################################

run_cap6 <- function(cap5_dir, out_dir) {
  
  cv_path   <- file.path(cap5_dir, "metricas_cv.csv")
  test_path <- file.path(cap5_dir, "metricas_test.csv")
  stopifnot(file.exists(cv_path), file.exists(test_path))
  
  metrics_cv   <- read.csv(cv_path, stringsAsFactors = FALSE)
  metrics_test <- read.csv(test_path, stringsAsFactors = FALSE)
  
  # Melhor modelo escolhido por CV (treino)
  metrics_cv <- metrics_cv |> dplyr::arrange(.data$RMSE)
  best_id <- metrics_cv$model_id[1]
  best_desc <- metrics_cv$modelo[1]
  
  save_lines(c("Melhor modelo (criterio: menor RMSE por CV no treino):",
               paste(best_id, "-", best_desc)),
             file.path(out_dir, "melhor_modelo_por_cv.txt"))
  
  # Carregar previsoes do teste do melhor (já treinado no cap5)
  pred_path <- file.path(cap5_dir, paste0("previsoes_", best_id, ".csv"))
  stopifnot(file.exists(pred_path))
  
  pred_best <- read.csv(pred_path, stringsAsFactors = FALSE) |>
    dplyr::mutate(
      residuo = .data$y_true - .data$y_pred,
      residuo_abs = abs(.data$residuo),
      acc_0_5 = abs(.data$y_true - .data$y_pred) <= 0.5
    )
  
  write.csv(pred_best, file.path(out_dir, "diagnosticos_melhor_modelo.csv"), row.names = FALSE)
  
  # Métricas finais (teste) do melhor
  best_test_row <- metrics_test |>
    dplyr::filter(.data$model_id == best_id) |>
    dplyr::slice(1)
  
  # Guardar métricas consolidadas
  write.csv(metrics_cv,   file.path(out_dir, "metricas_cv_final.csv"), row.names = FALSE)
  write.csv(metrics_test, file.path(out_dir, "metricas_test_final.csv"), row.names = FALSE)
  
  # Graficos: Observado vs Previsto
  p_obs_pred <- ggplot2::ggplot(pred_best, ggplot2::aes(x = .data$y_true, y = .data$y_pred)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::labs(title = paste("Observado vs Previsto (Teste) -", best_id),
                  subtitle = best_desc,
                  x = "Score observado (y_true)", y = "Score previsto (y_pred)")
  ggplot2::ggsave(file.path(out_dir, "fig_obs_vs_prev_melhor_modelo.png"), p_obs_pred, width = 7, height = 5, dpi = 150)
  
  # Residuos vs Previsto
  p_resid <- ggplot2::ggplot(pred_best, ggplot2::aes(x = .data$y_pred, y = .data$residuo)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(title = paste("Residuos vs Previsto (Teste) -", best_id),
                  subtitle = best_desc,
                  x = "Score previsto (y_pred)", y = "Residuo (y_true - y_pred)")
  ggplot2::ggsave(file.path(out_dir, "fig_residuos_vs_prev_melhor_modelo.png"), p_resid, width = 7, height = 5, dpi = 150)
  
  # Hist residuos
  p_hist_resid <- ggplot2::ggplot(pred_best, ggplot2::aes(x = .data$residuo)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::labs(title = paste("Distribuicao dos residuos (Teste) -", best_id),
                  subtitle = best_desc,
                  x = "Residuo (y_true - y_pred)", y = "Frequencia")
  ggplot2::ggsave(file.path(out_dir, "fig_hist_residuos_melhor_modelo.png"), p_hist_resid, width = 7, height = 5, dpi = 150)
  
  # Erro absoluto
  p_abs <- ggplot2::ggplot(pred_best, ggplot2::aes(x = .data$residuo_abs)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::labs(title = paste("Distribuicao do erro absoluto (Teste) -", best_id),
                  subtitle = best_desc,
                  x = "|Residuo|", y = "Frequencia")
  ggplot2::ggsave(file.path(out_dir, "fig_erro_absoluto_melhor_modelo.png"), p_abs, width = 7, height = 5, dpi = 150)
  
  # Comparacao RMSE (CV e Test)
  p_rmse_cv <- ggplot2::ggplot(metrics_cv, ggplot2::aes(x = reorder(.data$modelo, .data$RMSE), y = .data$RMSE)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "RMSE por modelo (CV no treino)", x = "Modelo", y = "RMSE (CV)")
  ggplot2::ggsave(file.path(out_dir, "fig_rmse_por_modelo_CV.png"), p_rmse_cv, width = 8, height = 5, dpi = 150)
  
  p_rmse_test <- ggplot2::ggplot(metrics_test, ggplot2::aes(x = reorder(.data$modelo, .data$RMSE), y = .data$RMSE)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "RMSE por modelo (Teste)", x = "Modelo", y = "RMSE (Teste)")
  ggplot2::ggsave(file.path(out_dir, "fig_rmse_por_modelo_TESTE.png"), p_rmse_test, width = 8, height = 5, dpi = 150)
  
  # % dentro de ±0.5 (teste) do melhor
  acc05 <- mean(pred_best$acc_0_5, na.rm = TRUE)
  save_lines(paste("Percentagem dentro de ±0.5 (teste) do melhor modelo:", round(100*acc05, 2), "%"),
             file.path(out_dir, "acc_within_0_5_best_model.txt"))
  
  summary_lines <- c(
    "CAPITULO 6 - VALIDACAO E AVALIACAO concluido.",
    paste("Melhor modelo (por CV treino):", best_id),
    paste("Descricao:", best_desc),
    paste("TESTE - MAE:",  round(best_test_row$MAE,  4)),
    paste("TESTE - RMSE:", round(best_test_row$RMSE, 4)),
    paste("TESTE - R2:",   round(best_test_row$R2,   4)),
    paste("TESTE - % dentro ±0.5:", round(100*best_test_row$PCT_0_5, 2), "%"),
    paste("Outputs em:", out_dir)
  )
  save_lines(summary_lines, file.path(out_dir, "cap6_resumo_final.txt"))
  
  print(metrics_cv |> dplyr::arrange(.data$RMSE))
  print(metrics_test |> dplyr::arrange(.data$RMSE))
  print(summary_lines)
  
  invisible(list(best_model_id = best_id, metrics_cv = metrics_cv, metrics_test = metrics_test))
}
