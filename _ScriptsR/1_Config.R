###############################################################################
# 2_Config.R
###############################################################################

# Dataset
csv_file_name <- "Dubai_data.csv"
csv_file <- file.path(project_dir, csv_file_name)
df_raw <- read_dataset_auto(csv_file)

# Resultados
result_dir <- file.path(project_dir, "result_dir")

# Seed global
seed <- 20260119

# Pastas de output 
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

# Controlos 
RUN_CAP3 <- TRUE
RUN_CAP4 <- TRUE
RUN_CAP5 <- TRUE
RUN_CAP6 <- TRUE

