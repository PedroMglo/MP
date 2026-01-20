###############################################################################
# 1_packages.R — carregar pacotes (simples e reprodutivel)
###############################################################################

required_pkgs <- c("dplyr","ggplot2","glmnet","rpart","rpart.plot","randomForest")
optional_pkgs <- c("e1071","gbm","FNN")

missing <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing) > 0) {
  stop("Pacotes em falta: ", paste(missing, collapse = ", "),
       "\nInstala com: install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))")
}

invisible(lapply(required_pkgs, library, character.only = TRUE))

optional_ok <- intersect(optional_pkgs, rownames(installed.packages()))
if (length(optional_ok) > 0) invisible(lapply(optional_ok, library, character.only = TRUE))
