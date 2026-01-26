run_cap3 <- function(df, out_tables, out_figs) {
  
  if (!dir.exists(out_tables)) dir.create(out_tables, recursive = TRUE)
  if (!dir.exists(out_figs))   dir.create(out_figs,   recursive = TRUE)
  

  n_dup <- sum(duplicated(df))
  

  missing_summary <- missing_summary_df(df)
  write.csv(missing_summary, file.path(out_tables, "cap3_missing_summary.csv"), row.names = FALSE)
  

  uniq_df <- unique_counts_df(df)
  write.csv(uniq_df, file.path(out_tables, "cap3_unique_counts.csv"), row.names = FALSE)

  if ("score_review" %in% names(df)) {
    score_freq <- as.data.frame(table(df$score_review, useNA = "ifany"))
    names(score_freq) <- c("score_review", "freq")
    write.csv(score_freq, file.path(out_tables, "cap3_score_review_frequencias.csv"), row.names = FALSE)
    
    p_score <- plot_target_bar(df, target = "score_review")
    ggplot2::ggsave(
      file.path(out_figs, "cap3_dist_score_review.png"),
      p_score, width = 7, height = 4, dpi = 150
    )
  }
  

  if ("logavaliacoes" %in% names(df)) {
    p_hist <- plot_hist_numeric(df, var = "logavaliacoes", bins = 30)
    ggplot2::ggsave(
      file.path(out_figs, "cap3_hist_logavaliacoes.png"),
      p_hist, width = 7, height = 4, dpi = 150
    )
    
    p_box <- plot_box_numeric(df, var = "logavaliacoes")
    ggplot2::ggsave(
      file.path(out_figs, "cap3_box_logavaliacoes.png"),
      p_box, width = 5, height = 4, dpi = 150
    )
  }
  

  df_num <- coerce_all_to_numeric_safely(df)
  
  bin_info <- binary_props_df(df_num, thr_nzv = 0.02)
  bin_cols <- bin_info$bin_cols
  
  if (length(bin_cols) > 0) {
    write.csv(bin_info$props_df, file.path(out_tables, "cap3_proporcoes_binarias.csv"), row.names = FALSE)
    write.csv(bin_info$nzv_df,   file.path(out_tables, "cap3_binarias_raras_NZV.csv"), row.names = FALSE)
    
    p_props <- plot_binary_props(bin_info$props_df)
    ggplot2::ggsave(
      file.path(out_figs, "cap3_proporcoes_binarias.png"),
      p_props, width = 9, height = 7, dpi = 150
    )
  }
  
  # ------------------------------------------------------------
  # 3.9 Relacao score vs logavaliacoes
  #     + LOESS por CarimboTripAdvisor (se existir)
  #     + boxplot logavaliacoes por score_review
  # ------------------------------------------------------------
  if (all(c("score_review", "logavaliacoes") %in% names(df))) {
    
    df_39 <- df |>
      dplyr::filter(!is.na(score_review), !is.na(logavaliacoes))
    
    # LOESS por CarimboTripAdvisor
    if ("CarimboTripAdvisor" %in% names(df_39)) {
      
      df_39g <- df_39 |>
        dplyr::filter(!is.na(CarimboTripAdvisor)) |>
        dplyr::mutate(
          CarimboTripAdvisor = as.integer(CarimboTripAdvisor),
          CarimboTripAdvisor = factor(
            CarimboTripAdvisor,
            levels = c(0, 1),
            labels = c("0 (Não)", "1 (Sim)")
          )
        )
      
      p_loess_grp <- ggplot2::ggplot(
        df_39g,
        ggplot2::aes(x = logavaliacoes, y = score_review, color = CarimboTripAdvisor)
      ) +
        ggplot2::geom_point(
          alpha = 0.20,
          position = ggplot2::position_jitter(width = 0, height = 0.05)
        ) +
        ggplot2::geom_smooth(
          method = "loess",
          formula = y ~ x,
          se = FALSE,
          span = 0.8
        ) +
        ggplot2::labs(
          x = "logavaliacoes",
          y = "score_review",
          title = "score_review vs logavaliacoes (tendência LOESS por CarimboTripAdvisor)",
          color = "CarimboTripAdvisor"
        )
      
      ggplot2::ggsave(
        file.path(out_figs, "cap3_scatter_score_vs_logavaliacoes_loess_por_carimbo.png"),
        p_loess_grp, width = 7, height = 4, dpi = 150
      )
    }
    
    # Boxplot logavaliacoes por score_review
    df_biv <- df_39
    df_biv$score_review <- as.numeric(df_biv$score_review)
    df_biv$score_review_f <- factor(
      df_biv$score_review,
      levels = sort(unique(df_biv$score_review))
    )
    
    p_box_by_score <- ggplot2::ggplot(
      df_biv,
      ggplot2::aes(x = score_review_f, y = logavaliacoes)
    ) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(
        title = "logavaliacoes por nivel de score_review",
        x = "score_review",
        y = "logavaliacoes"
      )
    
    ggplot2::ggsave(
      file.path(out_figs, "cap3_box_logavaliacoes_por_score.png"),
      p_box_by_score, width = 7, height = 4, dpi = 150
    )
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
    }
  }
  
  invisible(list(
    n_dup = n_dup,
    missing_summary = missing_summary,
    bin_cols = bin_cols,
    unique_counts = uniq_df
  ))
}

# chamada (mantém como tinhas)
run_cap3(df_raw, out_cap3_tables, out_cap3_figs)
