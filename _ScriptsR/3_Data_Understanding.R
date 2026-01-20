###############################################################################
# 3_Data_Understanding.R
# Capitulo 3: Data Understanding (com raridade/NZV report)
###############################################################################

run_cap3 <- function(df, out_tables, out_figs) {
  
  # Snapshot estrutura
  sink(file.path(out_tables, "cap3_estrutura_dataset.txt"))
  cat("=== DIMENSOES ===\n"); print(dim(df))
  cat("\n=== NOMES DAS VARIAVEIS ===\n"); print(names(df))
  cat("\n=== STR(df) ===\n"); str(df)
  sink()
  
  # Duplicados
  n_dup <- sum(duplicated(df))
  save_lines(paste("Numero de linhas duplicadas:", n_dup),
             file.path(out_tables, "cap3_duplicados.txt"))
  
  # Missing values
  na_count <- sapply(df, function(x) sum(is.na(x)))
  na_pct   <- round(100 * na_count / nrow(df), 2)
  missing_summary <- data.frame(
    variavel    = names(df),
    n_missing   = as.integer(na_count),
    pct_missing = as.numeric(na_pct)
  ) |>
    dplyr::arrange(dplyr::desc(.data$n_missing))
  write.csv(missing_summary, file.path(out_tables, "cap3_missing_summary.csv"), row.names = FALSE)
  
  # Coerencia basica
  msgs <- c()
  if ("score_review" %in% names(df)) {
    score_rng <- range(df$score_review, na.rm = TRUE)
    n_outside <- sum(df$score_review < 1 | df$score_review > 5, na.rm = TRUE)
    msgs <- c(msgs,
              paste("Range score_review:", paste(score_rng, collapse = " a ")),
              paste("N fora de [1,5]:", n_outside))
  } else {
    msgs <- c(msgs, "AVISO: variavel 'score_review' nao encontrada.")
  }
  
  if ("logavaliacoes" %in% names(df)) {
    msgs <- c(msgs, "Resumo logavaliacoes:", capture.output(summary(df$logavaliacoes)))
  } else {
    msgs <- c(msgs, "AVISO: variavel 'logavaliacoes' nao encontrada.")
  }
  save_lines(msgs, file.path(out_tables, "cap3_coerencia_basica.txt"))
  
  # Distintos por coluna
  unique_counts <- sapply(df, function(x) length(unique(x[!is.na(x)])))
  unique_counts_df <- data.frame(
    variavel = names(unique_counts),
    n_unicos = as.integer(unique_counts)
  ) |>
    dplyr::arrange(.data$n_unicos)
  write.csv(unique_counts_df, file.path(out_tables, "cap3_unique_counts.csv"), row.names = FALSE)
  
  # Summary geral
  sink(file.path(out_tables, "cap3_summary_geral.txt"))
  summary(df)
  sink()
  
  # Grafico target
  if ("score_review" %in% names(df)) {
    score_freq <- as.data.frame(table(df$score_review, useNA = "ifany"))
    names(score_freq) <- c("score_review", "freq")
    write.csv(score_freq, file.path(out_tables, "cap3_score_review_frequencias.csv"), row.names = FALSE)
    
    p_score <- ggplot2::ggplot(df, ggplot2::aes(x = factor(.data$score_review))) +
      ggplot2::geom_bar() +
      ggplot2::labs(x = "score_review (1 a 5)", y = "N de hoteis", title = "Distribuicao do score_review")
    ggplot2::ggsave(file.path(out_figs, "cap3_dist_score_review.png"), p_score, width = 7, height = 4, dpi = 150)
  }
  
  # logavaliacoes: hist + box
  if ("logavaliacoes" %in% names(df)) {
    p_hist <- ggplot2::ggplot(df, ggplot2::aes(x = .data$logavaliacoes)) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::labs(x = "logavaliacoes", y = "Frequencia", title = "Distribuicao de logavaliacoes")
    ggplot2::ggsave(file.path(out_figs, "cap3_hist_logavaliacoes.png"), p_hist, width = 7, height = 4, dpi = 150)
    
    p_box <- ggplot2::ggplot(df, ggplot2::aes(y = .data$logavaliacoes)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(y = "logavaliacoes", title = "Boxplot de logavaliacoes")
    ggplot2::ggsave(file.path(out_figs, "cap3_box_logavaliacoes.png"), p_box, width = 5, height = 4, dpi = 150)
  }
  
  # ---- Binarias 0/1 (conversao tentativa e raridade)
  df_num <- df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x))))
  
  bin_cols <- names(df_num)[sapply(df_num, is_binary_01)]
  save_lines(bin_cols, file.path(out_tables, "cap3_colunas_binarias_01.txt"))
  
  if (length(bin_cols) > 0) {
    bin_props <- sapply(df_num[bin_cols], function(x) mean(x == 1, na.rm = TRUE))
    bin_props_df <- data.frame(variavel = names(bin_props), prop_1 = as.numeric(bin_props)) |>
      dplyr::arrange(dplyr::desc(.data$prop_1))
    write.csv(bin_props_df, file.path(out_tables, "cap3_proporcoes_binarias.csv"), row.names = FALSE)
    
    # NZV simples: muito perto de 0% ou 100%
    thr <- 0.02
    nzv <- bin_props_df |>
      dplyr::mutate(min_prop = pmin(.data$prop_1, 1 - .data$prop_1)) |>
      dplyr::filter(.data$min_prop < thr) |>
      dplyr::arrange(.data$min_prop)
    write.csv(nzv, file.path(out_tables, "cap3_binarias_raras_NZV.csv"), row.names = FALSE)
    
    p_props <- ggplot2::ggplot(bin_props_df, ggplot2::aes(x = reorder(.data$variavel, .data$prop_1), y = .data$prop_1)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Proporcao de 1 nas variaveis binarias", x = "Variavel", y = "Prop(1)")
    ggplot2::ggsave(file.path(out_figs, "cap3_proporcoes_binarias.png"), p_props, width = 9, height = 7, dpi = 150)
  }
  
  # Relacao score vs logavaliacoes
  if (all(c("score_review", "logavaliacoes") %in% names(df))) {
    p_scatter <- ggplot2::ggplot(df, ggplot2::aes(x = .data$logavaliacoes, y = .data$score_review)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::labs(x = "logavaliacoes", y = "score_review", title = "score_review vs logavaliacoes")
    ggplot2::ggsave(file.path(out_figs, "cap3_scatter_score_vs_logavaliacoes.png"),
                    p_scatter, width = 7, height = 4, dpi = 150)
  }
  
  # Correlacoes (apenas numericas coeridas)
  num_cols <- names(df_num)[sapply(df_num, is.numeric)]
  df_num_only <- df_num[, num_cols, drop = FALSE]
  
  keep_cols <- sapply(df_num_only, function(x) {
    x2 <- x[!is.na(x)]
    length(x2) > 1 && stats::sd(x2) > 0
  })
  df_num_only2 <- df_num_only[, keep_cols, drop = FALSE]
  
  if (ncol(df_num_only2) >= 2) {
    cor_mat <- stats::cor(df_num_only2, use = "pairwise.complete.obs")
    write.csv(cor_mat, file.path(out_tables, "cap3_matriz_correlacao.csv"), row.names = TRUE)
    
    if ("score_review" %in% colnames(cor_mat)) {
      cor_with_target <- sort(cor_mat["score_review", ], decreasing = TRUE)
      cor_with_target_df <- data.frame(variavel = names(cor_with_target), cor_pearson = as.numeric(cor_with_target))
      write.csv(cor_with_target_df, file.path(out_tables, "cap3_correlacao_com_score_review.csv"), row.names = FALSE)
    }
  } else {
    save_lines("Nao foi possivel calcular correlacoes: poucas colunas numericas validas.",
               file.path(out_tables, "cap3_correlacao_erro.txt"))
  }
  
  # Exec ok + sessionInfo
  save_lines(c("Script Cap.3 executado com sucesso.",
               paste("Tabelas em:", out_tables),
               paste("Figuras em:", out_figs)),
             file.path(out_tables, "cap3_execucao_ok.txt"))
  
  sink(file.path(out_tables, "cap3_sessionInfo.txt"))
  sessionInfo()
  sink()
}
