
# -----------------------------
# 2) Validacoes minimas (evitar falhas silenciosas)
# -----------------------------
needed <- c("csv_file", "seed",
            "RUN_CAP3", "RUN_CAP4", "RUN_CAP5", "RUN_CAP6",
            "out_cap3_tables", "out_cap3_figs", "out_cap4", "out_cap5", "out_cap6",
            "read_dataset_auto")
missing <- needed[!vapply(needed, exists, logical(1), inherits = TRUE)]
if (length(missing) > 0) stop("Faltam objetos no ambiente (Config/source?): ", paste(missing, collapse = ", "))

# -----------------------------
# 3) Run pipeline
# -----------------------------
df_raw <- read_dataset_auto(csv_file)

if (RUN_CAP3) {
  run_cap3(df_raw, out_cap3_tables, out_cap3_figs)
}

cap4_obj <- NULL
if (RUN_CAP4) {
  cap4_obj <- run_cap4(df_raw, out_cap4, seed = seed)
}

if (RUN_CAP5) {
  # Se Cap4 nao correu agora, tenta ler do disco (outputs_cap4)
  if (is.null(cap4_obj)) {
    message("RUN_CAP4=FALSE ou cap4_obj NULL: a ler train/val/test do disco...")
    
    train   <- read.csv(file.path(out_cap4, "train.csv"), stringsAsFactors = FALSE)
    val     <- read.csv(file.path(out_cap4, "val.csv"), stringsAsFactors = FALSE)
    test    <- read.csv(file.path(out_cap4, "test.csv"), stringsAsFactors = FALSE)
    
    train_s <- read.csv(file.path(out_cap4, "train_scaled.csv"), stringsAsFactors = FALSE)
    val_s   <- read.csv(file.path(out_cap4, "val_scaled.csv"), stringsAsFactors = FALSE)
    test_s  <- read.csv(file.path(out_cap4, "test_scaled.csv"), stringsAsFactors = FALSE)
  } else {
    train   <- cap4_obj$train
    val     <- cap4_obj$val
    test    <- cap4_obj$test
    
    train_s <- cap4_obj$train_s
    val_s   <- cap4_obj$val_s
    test_s  <- cap4_obj$test_s
  }
  
  run_cap5(train, val, test, train_s, val_s, test_s, out_cap5, seed = seed)
}

if (RUN_CAP6) {
  run_cap6(out_cap5, out_cap6)
}
