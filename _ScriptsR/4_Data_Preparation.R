###############################################################################
# 4_Data_Preparation.R
# Capitulo 4: Data Preparation + Split (train/val/test) + Scaling (SEM leakage)
###############################################################################

# =============================
# 4.1 Preparacao basica
# =============================
prep_basic <- function(df) {
  stopifnot("score_review" %in% names(df))
  
  df <- df |> dplyr::filter(!is.na(.data$score_review))
  
  # Converter potenciais binarias para 0/1
  cols_except <- c("score_review", "logavaliacoes")
  for (col in setdiff(names(df), cols_except)) {
    df[[col]] <- to_binary01(df[[col]])
  }
  
  # Garantir numeric em target/logavaliacoes
  df$score_review <- suppressWarnings(as.numeric(df$score_review))
  if ("logavaliacoes" %in% names(df)) df$logavaliacoes <- suppressWarnings(as.numeric(df$logavaliacoes))
  
  df
}

# =============================
# 4.2 Preprocess fit/apply (SEM leakage)
# =============================
preprocess_fit <- function(train, target = "score_review", rare_thr = 0.02) {
  
  # Binarias apos conversoes
  bin_cols <- names(train)[sapply(train, is_binary_01)]
  bin_cols <- setdiff(bin_cols, target)
  
  num_cols  <- names(train)[sapply(train, is.numeric)]
  char_cols <- names(train)[sapply(train, is.character)]
  
  # Imputacao (fit no treino)
  mode_map <- list()
  for (col in c(bin_cols, char_cols)) mode_map[[col]] <- mode_value(train[[col]])
  
  median_map <- list()
  num_nonbin <- setdiff(num_cols, c(target, bin_cols))
  for (col in num_nonbin) median_map[[col]] <- stats::median(train[[col]], na.rm = TRUE)
  
  # Winsor limits (fit no treino) - logavaliacoes
  winsor_limits <- NULL
  if ("logavaliacoes" %in% names(train) && is.numeric(train$logavaliacoes)) {
    winsor_limits <- winsorize_iqr_fit(train$logavaliacoes, k = 1.5)
  }
  
  # Remover binarias raras/NZV (fit no treino)
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
  
  # interação: CarimboTripAdvisor x logavaliacoes (se existir)
  if (all(c("CarimboTripAdvisor", "logavaliacoes") %in% names(df)) &&
      is.numeric(df$CarimboTripAdvisor) && is.numeric(df$logavaliacoes)) {
    df$CarimboTripAdvisor_x_logavaliacoes <- df$CarimboTripAdvisor * df$logavaliacoes
  }
  
  df
}

preprocess_apply <- function(df, params) {
  
  # Drop colunas raras (fit no treino)
  if (length(params$drop_cols) > 0) {
    keep <- setdiff(names(df), params$drop_cols)
    df <- df[, keep, drop = FALSE]
  }
  
  # Imputar binarias + characters pela moda
  for (col in names(params$mode_map)) {
    if (!col %in% names(df)) next
    m <- params$mode_map[[col]]
    if (is.na(m)) next
    if (is.numeric(df[[col]])) m <- suppressWarnings(as.numeric(m))
    df[[col]][is.na(df[[col]])] <- m
  }
  
  # Imputar numericas nao-binarias pela mediana
  for (col in names(params$median_map)) {
    if (!col %in% names(df)) next
    med <- params$median_map[[col]]
    df[[col]][is.na(df[[col]])] <- med
  }
  
  # Winsor (aplicar limites fit do treino)
  if ("logavaliacoes" %in% names(df)) {
    df$logavaliacoes <- winsorize_apply(df$logavaliacoes, params$winsor_limits)
  }
  
  # Features derivadas
  df <- add_features(df, params)
  
  df
}

# =============================
# 4.3 Scaling (SEM leakage)
# =============================
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

# =============================
# 4.4 Runner
# =============================
run_cap4 <- function(df_raw,
                     out_dir,
                     seed = 1,
                     train_frac = 0.7,
                     val_frac = 0.1,
                     rare_thr = 0.02) {
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  df <- prep_basic(df_raw)
  
  # Split estratificado (train/val/test) - SEM leakage
  split3 <- stratified_split_3way(
    df, target = "score_review",
    train_frac = train_frac, val_frac = val_frac,
    seed = seed
  )
  train <- split3$train
  val   <- split3$val
  test  <- split3$test
  
  # Fit preprocess no treino e aplicar em train/val/test
  params <- preprocess_fit(train, target = "score_review", rare_thr = rare_thr)
  train2 <- preprocess_apply(train, params)
  val2   <- preprocess_apply(val,   params)
  test2  <- preprocess_apply(test,  params)
  
  # Recalcular bin_cols depois de drop (para scaling correto)
  bin_cols2 <- names(train2)[sapply(train2, is_binary_01)]
  bin_cols2 <- setdiff(bin_cols2, "score_review")
  
  # Scaling (SEM leakage): fit no treino e aplica a tudo
  scaler  <- scale_fit(train2, target = "score_review", bin_cols = bin_cols2)
  train_s <- scale_apply(train2, scaler)
  val_s   <- scale_apply(val2,   scaler)
  test_s  <- scale_apply(test2,  scaler)
  
  # Guardar datasets
  write.csv(train2,  file.path(out_dir, "train.csv"), row.names = FALSE)
  write.csv(val2,    file.path(out_dir, "val.csv"),   row.names = FALSE)
  write.csv(test2,   file.path(out_dir, "test.csv"),  row.names = FALSE)
  
  write.csv(train_s, file.path(out_dir, "train_scaled.csv"), row.names = FALSE)
  write.csv(val_s,   file.path(out_dir, "val_scaled.csv"),   row.names = FALSE)
  write.csv(test_s,  file.path(out_dir, "test_scaled.csv"),  row.names = FALSE)
  
  # Guardar info do preprocess (para relatório)
  write.csv(data.frame(drop_cols = params$drop_cols),
            file.path(out_dir, "cap4_drop_cols_raras.csv"), row.names = FALSE)
  
  save_lines(c(
    "CAPITULO 4 - DATA PREPARATION concluido.",
    paste("Seed:", seed),
    paste("train_frac:", train_frac, "| val_frac:", val_frac, "| test_frac:", round(1 - train_frac - val_frac, 3)),
    paste("rare_thr:", params$rare_thr),
    paste("Drop cols (raras/NZV):", if (length(params$drop_cols) == 0) "nenhuma" else paste(params$drop_cols, collapse = ", ")),
    paste("Train:", nrow(train2), "| Val:", nrow(val2), "| Test:", nrow(test2)),
    paste("Outputs em:", out_dir)
  ), file.path(out_dir, "cap4_resumo_final.txt"))
  
  invisible(list(
    train = train2, val = val2, test = test2,
    train_s = train_s, val_s = val_s, test_s = test_s,
    preprocess_params = params, scaler = scaler
  ))
}
