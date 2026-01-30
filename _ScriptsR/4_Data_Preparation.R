prep_basic <- function(df) {
  stopifnot("score_review" %in% names(df))
  
  qc <- list(
    n_raw = nrow(df),
    n_dup_raw = sum(duplicated(df)),
    n_missing_target_raw = sum(is.na(df$score_review))
  )
  
  df <- df |> dplyr::filter(!is.na(.data$score_review))
  qc$n_after_drop_missing_target <- nrow(df)
  
  df <- df[!duplicated(df), , drop = FALSE]
  qc$n_after_dedup <- nrow(df)
  
  df$score_review <- suppressWarnings(as.numeric(df$score_review))
  if ("logavaliacoes" %in% names(df)) df$logavaliacoes <- suppressWarnings(as.numeric(df$logavaliacoes))

  n_na_after_numeric <- sum(is.na(df$score_review))
  qc$n_target_na_after_numeric <- n_na_after_numeric
  if (n_na_after_numeric > 0) {
    df <- df |> dplyr::filter(!is.na(.data$score_review))
  }
  qc$n_after_drop_non_numeric_target <- nrow(df)
 
  outside <- sum(df$score_review < 1 | df$score_review > 5, na.rm = TRUE)
  qc$n_target_outside_1_5 <- outside
  if (outside > 0) {
    df$score_review <- pmin(5, pmax(1, df$score_review))
  }

  cols_except <- c("score_review", "logavaliacoes")
  for (col in setdiff(names(df), cols_except)) {
    df[[col]] <- to_binary01(df[[col]])
  }

  attr(df, "cap4_qc") <- qc
  
  df
}



preprocess_fit <- function(train, target = "score_review", rare_thr = 0.02) {
  
  bin_cols <- names(train)[sapply(train, is_binary_01)]
  bin_cols <- setdiff(bin_cols, target)
  
  num_cols  <- names(train)[sapply(train, is.numeric)]
  char_cols <- names(train)[sapply(train, is.character)]
  
  mode_map <- list()
  for (col in c(bin_cols, char_cols)) mode_map[[col]] <- mode_value(train[[col]])
  
  median_map <- list()
  num_nonbin <- setdiff(num_cols, c(target, bin_cols))
  for (col in num_nonbin) median_map[[col]] <- stats::median(train[[col]], na.rm = TRUE)
  
  
  winsor_limits <- NULL
  if ("logavaliacoes" %in% names(train) && is.numeric(train$logavaliacoes)) {
    winsor_limits <- winsorize_iqr_fit(train$logavaliacoes, k = 1.5)
  }
  
  drop_cols <- character(0)
  if (length(bin_cols) > 0) {
    props <- sapply(train[bin_cols], function(x) mean(x == 1, na.rm = TRUE))
    min_prop <- pmin(props, 1 - props)
    drop_cols <- names(min_prop[min_prop < rare_thr])
  }
  
  list(
    target = target,
    bin_cols = bin_cols,
    num_cols = num_cols,
    char_cols = char_cols,
    mode_map = mode_map,
    median_map = median_map,
    winsor_limits = winsor_limits,
    drop_cols = drop_cols,
    rare_thr = rare_thr
  )
}

add_features <- function(df, params) {
  # soma de TRUE/1 nas binárias disponíveis (após dropar)
  bin_cols <- intersect(params$bin_cols, names(df))
  if (length(bin_cols) > 0) {
    df$n_bin_true <- rowSums(df[, bin_cols, drop = FALSE] == 1, na.rm = TRUE)
  } else {
    df$n_bin_true <- 0
  }
  
  if (all(c("CarimboTripAdvisor", "logavaliacoes") %in% names(df)) &&
      is.numeric(df$CarimboTripAdvisor) && is.numeric(df$logavaliacoes)) {
    df$CarimboTripAdvisor_x_logavaliacoes <- df$CarimboTripAdvisor * df$logavaliacoes
  }
  
  df
}

preprocess_apply <- function(df, params) {
  
  if (length(params$drop_cols) > 0) {
    keep <- setdiff(names(df), params$drop_cols)
    df <- df[, keep, drop = FALSE]
  }
  
  for (col in names(params$mode_map)) {
    if (!col %in% names(df)) next
    m <- params$mode_map[[col]]
    if (is.na(m)) next
    if (is.numeric(df[[col]])) m <- suppressWarnings(as.numeric(m))
    df[[col]][is.na(df[[col]])] <- m
  }
  
  for (col in names(params$median_map)) {
    if (!col %in% names(df)) next
    med <- params$median_map[[col]]
    df[[col]][is.na(df[[col]])] <- med
  }
  
  if ("logavaliacoes" %in% names(df)) {
    df$logavaliacoes <- winsorize_apply(df$logavaliacoes, params$winsor_limits)
  }
  
  df <- add_features(df, params)
  
  df
}

scale_fit <- function(train, target, bin_cols) {
  predictors_num <- names(train)[sapply(train, is.numeric)]
  predictors_num <- setdiff(predictors_num, c(target, bin_cols))
  
  mu <- sapply(train[predictors_num], mean, na.rm = TRUE)
  sdv <- sapply(train[predictors_num], sd, na.rm = TRUE)
  sdv[sdv == 0] <- 1
  
  list(predictors_num = predictors_num, mu = mu, sd = sdv)
}

scale_apply <- function(df, scaler) {
  for (col in scaler$predictors_num) {
    if (!col %in% names(df)) next
    df[[col]] <- (df[[col]] - scaler$mu[[col]]) / scaler$sd[[col]]
  }
  df
}

run_cap4 <- function(df_raw,
                     out_dir,
                     seed = 1,
                     train_frac = 0.7,
                     val_frac = 0.1,
                     rare_thr = 0.02) {
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  df <- prep_basic(df_raw)
  
  qc <- attr(df, "cap4_qc")
  if (is.null(qc)) qc <- list()
  
  miss_df <- missing_summary_df(df)
  write.csv(miss_df, file.path(out_dir, "cap4_missing_after_basic.csv"), row.names = FALSE)
  
  qc_df <- data.frame(
    metric = names(qc),
    value  = unlist(qc, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  write.csv(qc_df, file.path(out_dir, "cap4_qc_basic.csv"), row.names = FALSE)

  
  
  split3 <- stratified_split_3way(
    df, target = "score_review",
    train_frac = train_frac, val_frac = val_frac,
    seed = seed
  )
  train <- split3$train
  val   <- split3$val
  test  <- split3$test
  
  
  params <- preprocess_fit(train, target = "score_review", rare_thr = rare_thr)
  train2 <- preprocess_apply(train, params)
  val2   <- preprocess_apply(val,   params)
  test2  <- preprocess_apply(test,  params)
  
  if ("score_review" %in% names(train) && "score_review" %in% names(val) && "score_review" %in% names(test)) {
    
    split_summary <- data.frame(
      split = c("train", "val", "test"),
      n = c(nrow(train), nrow(val), nrow(test)),
      mean_score = c(mean(train$score_review), mean(val$score_review), mean(test$score_review)),
      sd_score   = c(stats::sd(train$score_review), stats::sd(val$score_review), stats::sd(test$score_review))
    )
    write.csv(split_summary, file.path(out_dir, "cap4_split_summary.csv"), row.names = FALSE)
    
    df_dist <- dplyr::bind_rows(
      dplyr::mutate(train, split = "train"),
      dplyr::mutate(val,   split = "val"),
      dplyr::mutate(test,  split = "test")
    ) |>
      dplyr::filter(!is.na(score_review)) |>
      dplyr::mutate(
        score_f = factor(score_review, levels = sort(unique(df$score_review)), ordered = TRUE)
      ) |>
      dplyr::count(split, score_f, name = "n_level") |>
      dplyr::group_by(split) |>
      dplyr::mutate(prop = n_level / sum(n_level)) |>
      dplyr::ungroup()
    
    write.csv(df_dist, file.path(out_dir, "cap4_target_dist_by_split.csv"), row.names = FALSE)
    
    p_split <- ggplot2::ggplot(df_dist, ggplot2::aes(x = score_f, y = prop, fill = score_f)) +
      ggplot2::geom_col(alpha = 0.85) +
      ggplot2::facet_wrap(~ split, nrow = 1) +
      ggplot2::scale_fill_brewer(palette = "Blues") +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        title = "Distribuição do score_review por split (estratificação)",
        x = "score_review",
        y = "Proporção"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")
    
    
    save_plot(p_split, file.path(out_dir, "cap4_target_dist_by_split.png"),
              width = 9, height = 3.6, dpi = 150)
  }
  
  if (!is.null(params$winsor_limits) &&
      "logavaliacoes" %in% names(train) &&
      "logavaliacoes" %in% names(train2)) {
    
    lo <- params$winsor_limits$lo
    hi <- params$winsor_limits$hi
    
    x_before <- train$logavaliacoes
    if (!is.null(params$median_map) && "logavaliacoes" %in% names(params$median_map)) {
      x_before[is.na(x_before)] <- params$median_map[["logavaliacoes"]]
    }

    x_after <- winsorize_apply(x_before, params$winsor_limits)
    
    n_total <- sum(!is.na(x_before))
    n_lo <- sum(!is.na(x_before) & x_before < lo)
    n_hi <- sum(!is.na(x_before) & x_before > hi)
    
    df_ba <- data.frame(
      estado = factor(rep(c("Antes", "Depois"), each = length(x_before)),
                      levels = c("Antes", "Depois")),
      logavaliacoes = c(x_before, x_after)
    )
    df_ba <- df_ba[!is.na(df_ba$logavaliacoes), , drop = FALSE]
    
    p_winsor_ba <- ggplot2::ggplot(df_ba, ggplot2::aes(x = estado, y = logavaliacoes)) +
      ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.65, outlier.alpha = 0.35) +
      ggplot2::labs(
        title = "logavaliacoes — antes vs depois da winsorização (treino)",
        subtitle = sprintf("Limites fit no treino: lo=%.3f | hi=%.3f | ajustados: abaixo_lo=%d, acima_hi=%d (n=%d)",
                           lo, hi, n_lo, n_hi, n_total),
        x = NULL,
        y = "logavaliacoes"
      )
    
    save_plot(
      p_winsor_ba,
      file.path(out_dir, "cap4_box_logavaliacoes_before_after_winsor.png"),
      width = 7, height = 4, dpi = 150
    )
  }
  
  

  if ("n_bin_true" %in% names(train2)) {
    p1 <- plot_hist_numeric(train2, var = "n_bin_true", bins = 20) +
      ggplot2::labs(title = "Distribuição de n_bin_true (treino)")
    save_plot(p1, file.path(out_dir, "cap4_hist_n_bin_true.png"), width = 7, height = 4, dpi = 150)
  }
  

  if (all(c("score_review", "n_bin_true") %in% names(train2))) {
    p2 <- ggplot2::ggplot(train2, ggplot2::aes(x = n_bin_true, y = score_review)) +
      ggplot2::geom_point(alpha = 0.5, color = "darkorange") +
      ggplot2::labs(
        title = "score_review vs n_bin_true (treino)",
        x = "n_bin_true (nº amenities/flags=1)",
        y = "score_review"
      )
    save_plot(p2, file.path(out_dir, "cap4_score_vs_n_bin_true.png"), width = 7, height = 4, dpi = 150)
  }
  
  if (all(c("score_review", "logavaliacoes", "CarimboTripAdvisor") %in% names(train2))) {
    p3 <- ggplot2::ggplot(train2, ggplot2::aes(x = logavaliacoes, y = score_review)) +
      ggplot2::geom_point(ggplot2::aes(color = factor(CarimboTripAdvisor)), alpha = 0.5, color = "darkorange") +
      ggplot2::labs(
        title = "score_review vs logavaliacoes (treino), por CarimboTripAdvisor",
        x = "logavaliacoes",
        y = "score_review",
        color = "CarimboTripAdvisor"
      )
    save_plot(p3, file.path(out_dir, "cap4_scatter_score_vs_log_by_carimbo.png"), width = 7, height = 4, dpi = 150)
  }
  
  
  bin_cols2 <- names(train2)[sapply(train2, is_binary_01)]
  bin_cols2 <- setdiff(bin_cols2, "score_review")
  
  scaler  <- scale_fit(train2, target = "score_review", bin_cols = bin_cols2)
  train_s <- scale_apply(train2, scaler)
  val_s   <- scale_apply(val2,   scaler)
  test_s  <- scale_apply(test2,  scaler)
  
  scale_check <- data.frame(
    variavel = scaler$predictors_num,
    mean_train_scaled = sapply(train_s[scaler$predictors_num], mean, na.rm = TRUE),
    sd_train_scaled   = sapply(train_s[scaler$predictors_num], sd, na.rm = TRUE)
  )
  
  write.csv(scale_check, file.path(out_dir, "cap4_scaling_check_train.csv"), row.names = FALSE)
  
  
  write.csv(train2,  file.path(out_dir, "train.csv"), row.names = FALSE)
  write.csv(val2,    file.path(out_dir, "val.csv"),   row.names = FALSE)
  write.csv(test2,   file.path(out_dir, "test.csv"),  row.names = FALSE)
  
  write.csv(train_s, file.path(out_dir, "train_scaled.csv"), row.names = FALSE)
  write.csv(val_s,   file.path(out_dir, "val_scaled.csv"),   row.names = FALSE)
  write.csv(test_s,  file.path(out_dir, "test_scaled.csv"),  row.names = FALSE)
  
  write.csv(data.frame(drop_cols = params$drop_cols),
            file.path(out_dir, "cap4_drop_cols_raras.csv"), row.names = FALSE)

  drop_cols_raras <- params$drop_cols
  if (length(drop_cols_raras) > 0) {

    is_binary01 <- function(x) {
      ux <- unique(x[!is.na(x)])
      length(ux) > 0 && all(ux %in% c(0, 1))
    }
    
    bin_cols <- names(train)[vapply(train, is_binary01, logical(1))]
    bin_cols <- setdiff(bin_cols, "score_review")
    
    prev <- vapply(train[bin_cols], function(x) mean(x == 1, na.rm = TRUE), numeric(1))
    
    df_prev <- data.frame(
      var    = bin_cols,
      prop_1 = prev,
      drop   = bin_cols %in% drop_cols_raras
    )

    df_prev <- df_prev[order(df_prev$prop_1), ]
    df_prev_min <- head(df_prev, 12)
    
    p_nzv <- ggplot2::ggplot(
      df_prev_min,
      ggplot2::aes(x = stats::reorder(var, prop_1), y = prop_1, fill = drop)
    ) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::geom_hline(yintercept = 0.02, linetype = "dashed") +
      ggplot2::labs(
        title = "Prevalência das binárias mais raras (treino) e limiar NZV=2%",
        x = NULL,
        y = "prop_1 (prevalência de 1)",
        fill = "Removida (NZV)"
      )
    
    ggplot2::ggsave(file.path(out_dir, "cap4_prev_binarias_nzv_2pct.png"),
                    p_nzv, width = 7, height = 4, dpi = 150)
  
    
  }
  
  invisible(list(
    train = train2, val = val2, test = test2,
    train_s = train_s, val_s = val_s, test_s = test_s,
    preprocess_params = params, scaler = scaler
  ))
}


cap4_obj <- run_cap4(df_raw, out_cap4, seed = seed)

train   <- cap4_obj$train
val     <- cap4_obj$val
test    <- cap4_obj$test

train_s <- cap4_obj$train_s
val_s   <- cap4_obj$val_s
test_s  <- cap4_obj$test_s

