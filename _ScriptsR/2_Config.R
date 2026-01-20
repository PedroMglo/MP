###############################################################################
# 2_Config.R
# Modelos de Previsao (2025/26) - Trabalho Individual (2a Epoca)
# CRISP-DM - Config + utilitarios + paths robustos (sem hardcode)
###############################################################################

# =============================
# 0) Paths robustos (auto)
# =============================
.get_this_file <- function() {
  # Funciona quando chamado via source()
  f <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
                error = function(e) NA_character_)
  if (is.na(f)) {
    f <- tryCatch(normalizePath(attr(sys.frames()[[1]], "ofile"), winslash = "/", mustWork = TRUE),
                  error = function(e) NA_character_)
  }
  f
}

.this_file  <- .get_this_file()
scripts_dir <- if (!is.na(.this_file)) dirname(.this_file) else getwd()
project_dir <- normalizePath(file.path(scripts_dir, ".."), winslash = "/", mustWork = FALSE)

# Dataset (mantem nome do teu ficheiro)
csv_file_name <- "Dubai_data.csv"
csv_file <- file.path(project_dir, csv_file_name)

# Resultados
result_dir <- file.path(project_dir, "result_dir")

# Seed global
seed <- 20260119

# Pastas de output (por capitulo)
out_cap3_tables <- file.path(result_dir, "outputs_tabelas_cap3")
out_cap3_figs   <- file.path(result_dir, "outputs_figuras_cap3")
out_cap4        <- file.path(result_dir, "outputs_cap4")
out_cap5        <- file.path(result_dir, "outputs_cap5")
out_cap6        <- file.path(result_dir, "outputs_cap6")

dir.create(out_cap3_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(out_cap3_figs,   recursive = TRUE, showWarnings = FALSE)
dir.create(out_cap4,        recursive = TRUE, showWarnings = FALSE)
dir.create(out_cap5,        recursive = TRUE, showWarnings = FALSE)
dir.create(out_cap6,        recursive = TRUE, showWarnings = FALSE)

# Controlos (liga/desliga capitulos)
RUN_CAP3 <- TRUE
RUN_CAP4 <- TRUE
RUN_CAP5 <- TRUE
RUN_CAP6 <- TRUE

# =============================
# 1) Pacotes (core + opcionais)
# =============================
require_pkgs <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) {
    message("Pacotes em falta: ", paste(missing, collapse = ", "))
    message('Instala com: install.packages(c(', paste0('"', missing, '"', collapse = ", "), '))')
    stop("Instala os pacotes em falta e volta a correr o script.")
  }
  invisible(lapply(pkgs, require, character.only = TRUE))
}

require_optional <- function(pkgs) {
  ok <- pkgs[pkgs %in% rownames(installed.packages())]
  if (length(ok) > 0) invisible(lapply(ok, require, character.only = TRUE))
  invisible(ok)
}

core_pkgs <- c("dplyr", "ggplot2", "glmnet", "rpart", "rpart.plot", "randomForest")
require_pkgs(core_pkgs)

# opcionais (se tiveres instalado, o pipeline aproveita)
optional_loaded <- require_optional(c("e1071", "gbm", "FNN"))

# =============================
# 2) Funcoes utilitarias
# =============================
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
    map1 <- c("1","sim","s","yes","y","true","t")
    map0 <- c("0","nao","não","n","no","false","f")
    
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

# Fold estratificado (target discreto)
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

# Modelo matrix helper (para glmnet/svm/knn)
mmatrix <- function(df, target = "score_review") {
  f <- stats::as.formula(paste(target, "~ ."))
  x <- stats::model.matrix(f, df)[, -1, drop = FALSE]
  y <- df[[target]]
  list(x = x, y = y)
}
