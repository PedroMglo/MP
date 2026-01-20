###############################################################################
# main.R — LAUNCHER / RUNNER (sem hardcode e robusto a setwd)
# - Descobre project_dir automaticamente pela localizacao deste ficheiro
# - Faz source() por caminho absoluto
# - Corre pipeline (Cap.3 a Cap.6) e gera outputs
###############################################################################

# -----------------------------
# 0) Descobrir project_dir e scripts_dir
# -----------------------------
.get_this_file <- function() {
  f <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
                error = function(e) NA_character_)
  if (is.na(f)) {
    # fallback: se correr no RStudio com Source
    f <- tryCatch(normalizePath(attr(sys.frames()[[1]], "ofile"), winslash = "/", mustWork = TRUE),
                  error = function(e) NA_character_)
  }
  f
}

.this_file  <- .get_this_file()

if (is.na(.this_file)) {

  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
} else {
  project_dir <- normalizePath(dirname(.this_file), winslash = "/", mustWork = FALSE)
}

scripts_dir <- file.path(project_dir, "_ScriptsR")

message("Project dir: ", project_dir)
message("Scripts dir: ", scripts_dir)

if (!dir.exists(scripts_dir)) stop("scripts_dir nao existe: ", scripts_dir)

# -----------------------------
# 1) Source scripts (ordem)
# -----------------------------
scripts <- c(
  "2_Config.R",
  "3_Data_Understanding.R",
  "4_Data_Preparation.R",
  "5_Modelacao.R",
  "6_Validacao_Avaliacao.R"
)

for (s in scripts) {
  f <- file.path(scripts_dir, s)
  message("A correr: ", f)
  if (!file.exists(f)) stop("Ficheiro nao encontrado: ", f)
  source(f, encoding = "UTF-8")
}


df_raw <- read_dataset_auto(csv_file)

if (RUN_CAP3) run_cap3(df_raw, out_cap3_tables, out_cap3_figs)

cap4_obj <- NULL
if (RUN_CAP4) cap4_obj <- run_cap4(df_raw, out_cap4, seed = seed)

if (RUN_CAP5) {

  train   <- cap4_obj$train
  test    <- cap4_obj$test
  train_s <- cap4_obj$train_s
  test_s  <- cap4_obj$test_s
  
  run_cap5(train, test, train_s, test_s, out_cap5, seed = seed)
}

if (RUN_CAP6) run_cap6(out_cap5, out_cap6)
