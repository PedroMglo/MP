run_cap3 <- function(df, out_tables, out_figs) {
  
  if (!dir.exists(out_tables)) dir.create(out_tables, recursive = TRUE)
  if (!dir.exists(out_figs))   dir.create(out_figs,   recursive = TRUE)
  
  # ------------------------------------------------------------
  # 3.0 Snapshot inicial
  # ------------------------------------------------------------
  write_text_snapshot(df, file.path(out_tables, "cap3_estrutura_dataset.txt"))
  
  # ------------------------------------------------------------
  # 3.1 Duplicados
  # ------------------------------------------------------------
  n_dup <- sum(duplicated(df))
  save_lines(paste("Numero de linhas duplicadas:", n_dup),
             file.path(out_tables, "cap3_duplicados.txt"))
  
  # ------------------------------------------------------------
  # 3.2 Missing values
  # ------------------------------------------------------------
  missing_summary <- missing_summary_df(df)
  write.csv(missing_summary, file.path(out_tables, "cap3_missing_summary.csv"), row.names = FALSE)
  
  # ------------------------------------------------------------
  # 3.3 Coerencia basica
  # ------------------------------------------------------------
  msgs <- coherence_messages(df, target = "score_review", target_min = 1, target_max = 5, numeric_var = "logavaliacoes")
  save_lines(msgs, file.path(out_tables, "cap3_coerencia_basica.txt"))
  
  # ------------------------------------------------------------
  # 3.4 Distintos por coluna
  # ------------------------------------------------------------
  uniq_df <- unique_counts_df(df)
  write.csv(uniq_df, file.path(out_tables, "cap3_unique_counts.csv"), row.names = FALSE)
  
  # ------------------------------------------------------------
  # 3.5 Summary geral
  # ------------------------------------------------------------
  write_summary_txt(df, file.path(out_tables, "cap3_summary_geral.txt"))
  
  # ------------------------------------------------------------
  # 3.6 Grafico target + frequencias
  # ------------------------------------------------------------
  if ("score_review" %in% names(df)) {
    score_freq <- as.data.frame(table(df$score_review, useNA = "ifany"))
    names(score_freq) <- c("score_review", "freq")
    write.csv(score_freq, file.path(out_tables, "cap3_score_review_frequencias.csv"), row.names = FALSE)
    
    p_score <- plot_target_bar(df, target = "score_review")
    ggplot2::ggsave(file.path(out_figs, "cap3_dist_score_review.png"),
                    p_score, width = 7, height = 4, dpi = 150)
  } else {
    save_lines("AVISO: variavel 'score_review' nao encontrada. Grafico do target nao gerado.",
               file.path(out_tables, "cap3_target_ausente.txt"))
  }
  
  # ------------------------------------------------------------
  # 3.7  hist + box
  # ------------------------------------------------------------
  if ("logavaliacoes" %in% names(df)) {
    p_hist <- plot_hist_numeric(df, var = "logavaliacoes", bins = 30)
    ggplot2::ggsave(file.path(out_figs, "cap3_hist_logavaliacoes.png"),
                    p_hist, width = 7, height = 4, dpi = 150)
    
    p_box <- plot_box_numeric(df, var = "logavaliacoes")
    ggplot2::ggsave(file.path(out_figs, "cap3_box_logavaliacoes.png"),
                    p_box, width = 5, height = 4, dpi = 150)
  } else {
    save_lines("AVISO: variavel 'logavaliacoes' nao encontrada. Hist/box nao gerados.",
               file.path(out_tables, "cap3_logavaliacoes_ausente.txt"))
  }
  
  # ------------------------------------------------------------
  # 3.8 Binarias 0/1 (raridade + NZV simples)
  # ------------------------------------------------------------
  df_num <- coerce_all_to_numeric_safely(df)
  
  bin_info <- binary_props_df(df_num, thr_nzv = 0.02)
  bin_cols <- bin_info$bin_cols
  
  save_lines(bin_cols, file.path(out_tables, "cap3_colunas_binarias_01.txt"))
  
  if (length(bin_cols) > 0) {
    write.csv(bin_info$props_df, file.path(out_tables, "cap3_proporcoes_binarias.csv"), row.names = FALSE)
    write.csv(bin_info$nzv_df,   file.path(out_tables, "cap3_binarias_raras_NZV.csv"), row.names = FALSE)
    
    p_props <- plot_binary_props(bin_info$props_df)
    ggplot2::ggsave(file.path(out_figs, "cap3_proporcoes_binarias.png"),
                    p_props, width = 9, height = 7, dpi = 150)
  } else {
    save_lines("Nao foram encontradas colunas binarias 0/1 apos coerção numerica.",
               file.path(out_tables, "cap3_binarias_01_vazias.txt"))
  }
  
  # ------------------------------------------------------------
  # 3.9 Relacao score vs logavaliacoes
  # ------------------------------------------------------------
  if (all(c("score_review", "logavaliacoes") %in% names(df))) {
    p_scatter <- plot_scatter(df, xvar = "logavaliacoes", yvar = "score_review", alpha = 0.5)
    ggplot2::ggsave(file.path(out_figs, "cap3_scatter_score_vs_logavaliacoes.png"),
                    p_scatter, width = 7, height = 4, dpi = 150)
  }
  
  # ------------------------------------------------------------
  # 3.10 Correlacoes (numericas validas)
  # ------------------------------------------------------------
  df_num_only <- valid_numeric_cols(df_num)
  cor_out <- correlation_outputs(df_num_only, target = "score_review")
  
  if (!is.null(cor_out$cor_mat)) {
    write.csv(cor_out$cor_mat, file.path(out_tables, "cap3_matriz_correlacao.csv"), row.names = TRUE)
    
    if (!is.null(cor_out$cor_target)) {
      write.csv(cor_out$cor_target, file.path(out_tables, "cap3_correlacao_com_score_review.csv"), row.names = FALSE)
    } else {
      save_lines("AVISO: 'score_review' nao esta na matriz de correlacao (ou nao foi possivel calcular).",
                 file.path(out_tables, "cap3_correlacao_target_indisponivel.txt"))
    }
  } else {
    save_lines(cor_out$msg, file.path(out_tables, "cap3_correlacao_erro.txt"))
  }
  
  save_lines(c("Script Cap.3 executado com sucesso.",
               paste("Tabelas em:", out_tables),
               paste("Figuras em:", out_figs)),
             file.path(out_tables, "cap3_execucao_ok.txt"))
  
  sink(file.path(out_tables, "cap3_sessionInfo.txt"))
  sessionInfo()
  sink()
  
  invisible(list(
    n_dup = n_dup,
    missing_summary = missing_summary,
    bin_cols = bin_cols
  ))
}

run_cap3(df_raw, out_cap3_tables, out_cap3_figs)
