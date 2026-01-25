.get_this_file <- function() {
  f <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
                error = function(e) NA_character_)
  if (is.na(f)) {
    f <- tryCatch(normalizePath(attr(sys.frames()[[1]], "ofile"), winslash = "/", mustWork = TRUE),
                  error = function(e) NA_character_)
  }
  f
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

read_dataset_auto <- function(path) {
  stopifnot(file.exists(path))
  first_line <- readLines(path, n = 1, warn = FALSE)
  sep_guess <- if (grepl(";", first_line)) ";" else ","
  if (sep_guess == ";") read.csv2(path, stringsAsFactors = FALSE) else read.csv(path, stringsAsFactors = FALSE)
}

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
mae  <- function(y, yhat) mean(abs(y - yhat), na.rm = TRUE)
r2   <- function(y, yhat) {
  ss_res <- sum((y - yhat)^2, na.rm = TRUE)
  ss_tot <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  1 - ss_res / ss_tot
}
clip_1_5 <- function(x) pmin(5, pmax(1, x))

within_tolerance <- function(y, yhat, tol = 0.5) {
  mean(abs(y - yhat) <= tol, na.rm = TRUE)
}

save_lines <- function(lines, path) writeLines(as.character(lines), con = path)

mode_value <- function(x) {
  ux <- x[!is.na(x)]
  if (length(ux) == 0) return(NA)
  tab <- sort(table(ux), decreasing = TRUE)
  names(tab)[1]
}

to_binary01 <- function(x) {
  if (is.logical(x)) return(ifelse(is.na(x), NA, ifelse(x, 1, 0)))
  
  if (is.numeric(x)) {
    ux <- unique(x[!is.na(x)])
    if (length(ux) <= 2 && all(ux %in% c(0, 1))) return(x)
    return(x)
  }
  
  if (is.character(x)) {
    x_trim <- trimws(tolower(x))
    map1 <- c("TRUE")
    map0 <- c("FALSE")
    
    out <- rep(NA_real_, length(x_trim))
    out[x_trim %in% map1] <- 1
    out[x_trim %in% map0] <- 0
    
    ok_rate <- mean(!is.na(out) | is.na(x_trim))
    if (ok_rate > 0.7) return(out)
    return(x)
  }
  
  x
}

is_binary_01 <- function(x) {
  if (!is.numeric(x)) return(FALSE)
  ux <- unique(x[!is.na(x)])
  length(ux) <= 2 && all(ux %in% c(0, 1))
}

winsorize_iqr_fit <- function(x, k = 1.5) {
  if (!is.numeric(x)) return(NULL)
  q1 <- as.numeric(quantile(x, 0.25, na.rm = TRUE))
  q3 <- as.numeric(quantile(x, 0.75, na.rm = TRUE))
  iqr <- q3 - q1
  list(lo = q1 - k * iqr, hi = q3 + k * iqr, k = k)
}
winsorize_apply <- function(x, limits) {
  if (is.null(limits) || !is.numeric(x)) return(x)
  x2 <- x
  x2[!is.na(x2) & x2 < limits$lo] <- limits$lo
  x2[!is.na(x2) & x2 > limits$hi] <- limits$hi
  x2
}

make_folds_stratified <- function(y, k = 5, seed = 1) {
  set.seed(seed)
  yfac <- as.factor(y)
  folds <- vector("list", k)
  for (lvl in levels(yfac)) {
    idx <- which(yfac == lvl)
    idx <- sample(idx)
    parts <- split(idx, rep(1:k, length.out = length(idx)))
    for (i in 1:k) folds[[i]] <- c(folds[[i]], parts[[i]])
  }
  folds <- lapply(folds, sort)
  folds
}

mmatrix <- function(df, target = "score_review") {
  f <- stats::as.formula(paste(target, "~ ."))
  x <- stats::model.matrix(f, df)[, -1, drop = FALSE]
  y <- df[[target]]
  list(x = x, y = y)
}

write_text_snapshot <- function(df, path) {
  sink(path)
  cat("=== DIMENSOES ===\n"); print(dim(df))
  cat("\n=== NOMES DAS VARIAVEIS ===\n"); print(names(df))
  cat("\n=== STR(df) ===\n"); str(df)
  sink()
}

missing_summary_df <- function(df) {
  na_count <- sapply(df, function(x) sum(is.na(x)))
  na_pct   <- round(100 * na_count / nrow(df), 2)
  data.frame(
    variavel    = names(df),
    n_missing   = as.integer(na_count),
    pct_missing = as.numeric(na_pct),
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(dplyr::desc(.data$n_missing))
}
coherence_messages <- function(df,
                               target = "score_review",
                               target_min = 1,
                               target_max = 5,
                               numeric_var = "logavaliacoes") {
  msgs <- c()
  
  if (target %in% names(df)) {
    rng <- range(df[[target]], na.rm = TRUE)
    n_outside <- sum(df[[target]] < target_min | df[[target]] > target_max, na.rm = TRUE)
    msgs <- c(
      msgs,
      paste("Range", target, ":", paste(rng, collapse = " a ")),
      paste("N fora de [", target_min, ",", target_max, "]:", n_outside)
    )
  } else {
    msgs <- c(msgs, paste("AVISO: variavel", shQuote(target), "nao encontrada."))
  }
  
  if (numeric_var %in% names(df)) {
    msgs <- c(msgs, paste("Resumo", numeric_var, ":", capture.output(summary(df[[numeric_var]]))))
  } else {
    msgs <- c(msgs, paste("AVISO: variavel", shQuote(numeric_var), "nao encontrada."))
  }
  
  msgs
}

unique_counts_df <- function(df) {
  unique_counts <- sapply(df, function(x) length(unique(x[!is.na(x)])))
  data.frame(
    variavel = names(unique_counts),
    n_unicos = as.integer(unique_counts),
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(.data$n_unicos)
}

# write_summary_txt <- function(df, path) {
#   sink(path)
#   summary(df)
#   sink()
# }
write_summary_txt <- function(df, path) {
  out <- capture.output(summary(df))
  writeLines(out, path, useBytes = TRUE)
}



plot_target_bar <- function(df, target = "score_review") {
  ggplot2::ggplot(df, ggplot2::aes(x = factor(.data[[target]]))) +
    ggplot2::geom_bar() +
    ggplot2::labs(
      x = paste0(target, " (1 a 5)"),
      y = "N",
      title = paste0("Distribuicao do ", target)
    )
}

plot_hist_numeric <- function(df, var, bins = 30) {
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[var]])) +
    ggplot2::geom_histogram(bins = bins) +
    ggplot2::labs(x = var, y = "Frequencia", title = paste0("Distribuicao de ", var))
}

plot_box_numeric <- function(df, var) {
  ggplot2::ggplot(df, ggplot2::aes(y = .data[[var]])) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(y = var, title = paste0("Boxplot de ", var))
}

plot_scatter <- function(df, xvar, yvar, alpha = 0.5) {
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]])) +
    ggplot2::geom_point(alpha = alpha) +
    ggplot2::labs(x = xvar, y = yvar, title = paste0(yvar, " vs ", xvar))
}

plot_binary_props <- function(props_df) {
  ggplot2::ggplot(props_df, ggplot2::aes(x = reorder(.data$variavel, .data$prop_1), y = .data$prop_1)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Proporcao de 1 nas variaveis binarias", x = "Variavel", y = "Prop(1)")
}

coerce_all_to_numeric_safely <- function(df) {
  df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x))))
}

binary_props_df <- function(df_num, thr_nzv = 0.02) {
  bin_cols <- names(df_num)[sapply(df_num, is_binary_01)]
  if (length(bin_cols) == 0) {
    return(list(
      bin_cols = character(0),
      props_df = data.frame(),
      nzv_df   = data.frame()
    ))
  }
  
  bin_props <- sapply(df_num[bin_cols], function(x) mean(x == 1, na.rm = TRUE))
  props_df <- data.frame(
    variavel = names(bin_props),
    prop_1 = as.numeric(bin_props),
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(dplyr::desc(.data$prop_1))
  
  nzv_df <- props_df |>
    dplyr::mutate(min_prop = pmin(.data$prop_1, 1 - .data$prop_1)) |>
    dplyr::filter(.data$min_prop < thr_nzv) |>
    dplyr::arrange(.data$min_prop)
  
  list(bin_cols = bin_cols, props_df = props_df, nzv_df = nzv_df)
}

valid_numeric_cols <- function(df_num) {
  num_cols <- names(df_num)[sapply(df_num, is.numeric)]
  df_num_only <- df_num[, num_cols, drop = FALSE]
  
  keep <- sapply(df_num_only, function(x) {
    x2 <- x[!is.na(x)]
    length(x2) > 1 && stats::sd(x2) > 0
  })
  
  df_num_only[, keep, drop = FALSE]
}

correlation_outputs <- function(df_num_only, target = "score_review") {
  if (ncol(df_num_only) < 2) {
    return(list(
      cor_mat = NULL,
      cor_target = NULL,
      msg = "Nao foi possivel calcular correlacoes: poucas colunas numericas validas."
    ))
  }
  
  cor_mat <- stats::cor(df_num_only, use = "pairwise.complete.obs")
  
  cor_target_df <- NULL
  if (target %in% colnames(cor_mat)) {
    cor_with_target <- sort(cor_mat[target, ], decreasing = TRUE)
    cor_target_df <- data.frame(
      variavel = names(cor_with_target),
      cor_pearson = as.numeric(cor_with_target),
      stringsAsFactors = FALSE
    )
  }
  
  list(cor_mat = cor_mat, cor_target = cor_target_df, msg = NULL)
}

stratified_split_3way <- function(df,
                                  target = "score_review",
                                  train_frac = 0.7,
                                  val_frac = 0.1,
                                  seed = 1) {
  stopifnot(train_frac > 0, val_frac >= 0, train_frac + val_frac < 1)
  set.seed(seed)
  strata <- as.factor(df[[target]])
  train_idx <- integer(0)
  val_idx   <- integer(0)
  test_idx  <- integer(0)
  
  for (lvl in levels(strata)) {
    idx_lvl <- which(strata == lvl)
    idx_lvl <- sample(idx_lvl)
    
    n_lvl <- length(idx_lvl)
    n_train <- floor(train_frac * n_lvl)
    n_val   <- floor(val_frac * n_lvl)
    if (n_train < 1 && n_lvl > 0) n_train <- 1
    
    tr <- idx_lvl[seq_len(min(n_train, n_lvl))]
    rest <- setdiff(idx_lvl, tr)
    
    va <- integer(0)
    if (length(rest) > 0 && n_val > 0) {
      va <- rest[seq_len(min(n_val, length(rest)))]
    }
    te <- setdiff(rest, va)
    
    train_idx <- c(train_idx, tr)
    val_idx   <- c(val_idx, va)
    test_idx  <- c(test_idx, te)
  }
  
  train_idx <- sort(unique(train_idx))
  val_idx   <- sort(unique(val_idx))
  test_idx  <- sort(unique(test_idx))
  val_idx  <- setdiff(val_idx, train_idx)
  test_idx <- setdiff(test_idx, union(train_idx, val_idx))
  
  list(
    train = df[train_idx, , drop = FALSE],
    val   = df[val_idx,   , drop = FALSE],
    test  = df[test_idx,  , drop = FALSE]
  )
}


add_pred_diagnostics <- function(df_preds,
                                 y_true_col = "y_true",
                                 y_pred_col = "y_pred",
                                 y_pred_clip_col = "y_pred_clipped",
                                 tol = 0.5) {
  y_true <- df_preds[[y_true_col]]
  y_pred <- df_preds[[y_pred_col]]
  y_clip <- df_preds[[y_pred_clip_col]]
  
  dplyr::mutate(
    df_preds,
    residuo       = y_true - y_pred,
    residuo_abs   = abs(.data$residuo),
    residuo_clip  = y_true - y_clip,
    acc_0_5       = abs(y_true - y_pred) <= tol,
    acc_0_5_clip  = abs(y_true - y_clip) <= tol
  )
}


save_plot <- function(plot_obj, path, width = 7, height = 5, dpi = 150) {
  ggplot2::ggsave(path, plot_obj, width = width, height = height, dpi = dpi)
  invisible(path)
}

plot_obs_vs_pred <- function(df, y_pred_col = "y_pred", title, subtitle) {
  ggplot2::ggplot(df, ggplot2::aes(x = .data$y_true, y = .data[[y_pred_col]])) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "Score observado (y_true)", y = paste0("Score previsto (", y_pred_col, ")"))
}

plot_resid_vs_pred <- function(df, y_pred_col = "y_pred", resid_col = "residuo", title, subtitle) {
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[y_pred_col]], y = .data[[resid_col]])) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(title = title, subtitle = subtitle, x = paste0("Score previsto (", y_pred_col, ")"), y = paste0("Residuo (", resid_col, ")"))
}

plot_hist <- function(df, x_col, bins = 30, title, subtitle, xlab) {
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_col]])) +
    ggplot2::geom_histogram(bins = bins) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = "Frequencia")
}

plot_rmse_bar_cv <- function(metrics_cv) {
  ggplot2::ggplot(metrics_cv, ggplot2::aes(x = reorder(.data$modelo, .data$RMSE), y = .data$RMSE)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "RMSE por modelo (CV no treino)", x = "Modelo", y = "RMSE (CV)")
}


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

baseline_predict <- function(y_train, n) rep(mean(y_train, na.rm = TRUE), n)

save_preds_generic <- function(filename, y_true, y_pred, out_dir) {
  dfp <- data.frame(
    y_true = as.numeric(y_true),
    y_pred = as.numeric(y_pred),
    y_pred_clipped = clip_1_5(as.numeric(y_pred))
  )
  write.csv(dfp, file.path(out_dir, filename), row.names = FALSE)
}
