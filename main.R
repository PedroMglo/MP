project_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)


scripts_dir <- file.path(project_dir, "_ScriptsR")

message("Project dir: ", project_dir)
message("Scripts dir: ", scripts_dir)

scripts <- c(
  "0_Utils.R"
  ,"1_Config.R"
  ,"2_Business_Understanding.R"
  ,"3_Data_Understanding.R"
  ,"4_Data_Preparation.R"
  ,"5_Modelacao.R"
  ,"6_Validacao_Avaliacao.R"
)

for (s in scripts) {
  f <- file.path(scripts_dir, s)
  message("A correr: ", f)
  source(f, encoding = "UTF-8")
}










