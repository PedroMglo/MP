run_cap1 <- function(df,
                     out_dir,
                     target = "score_review",
                     tol_within = 0.5) {

  ensure_dir(out_dir)


  n_obs  <- nrow(df)
  n_vars <- ncol(df)


  predictors <- setdiff(names(df), target)

  var_type <- sapply(df, function(x) {
    if (is.numeric(x)) "numerica"
    else if (is.logical(x)) "logica"
    else if (is.character(x)) "categorica_texto"
    else class(x)[1]
  })

  is_bin <- sapply(df[predictors], function(x) {
    if (is.logical(x)) return(TRUE)
    if (is.numeric(x)) {
      ux <- unique(x[!is.na(x)])
      return(length(ux) <= 2 && all(ux %in% c(0, 1)))
    }
    if (is.character(x)) {
      ux <- unique(tolower(trimws(x[!is.na(x)])))
      return(length(ux) <= 2 && all(ux %in% c("true","false")))
    }
    FALSE
  })

  hypotheses <- data.frame(
    variavel = predictors,
    hipotese = NA_character_,
    stringsAsFactors = FALSE
  )

  hypotheses$hipotese[is.na(hypotheses$hipotese)] <- "Hipotese a discutir no relatorio (sinal esperado nao definido a priori)."

  n_dup <- sum(duplicated(df))
  miss  <- missing_summary_df(df)

  miss_nonzero <- miss[miss$n_missing > 0, , drop = FALSE]
  if (nrow(miss_nonzero) == 0) {
    miss_note <- "Nao foram detetados missing values."
  } else {
    miss_note <- paste0("Existem missing values em ", nrow(miss_nonzero),
                        " variaveis (ver ficheiro cap1_qualidade_dados_resumo.csv).")
  }

  target_rng     <- range(df[[target]], na.rm = TRUE)
  target_outside <- sum(df[[target]] < 1 | df[[target]] > 5, na.rm = TRUE)

  bin_flag <- is_bin[match(names(df), names(is_bin))]  
  dict <- data.frame(
    variavel = names(df),
    tipo     = unname(var_type),
    papel    = ifelse(names(df) == target, "target", "preditor"),
    binaria  = ifelse(names(df) %in% predictors, ifelse(!is.na(bin_flag) & bin_flag, "sim", "nao"), NA_character_),
    hipotese = ifelse(names(df) %in% predictors,
                      hypotheses$hipotese[match(names(df), hypotheses$variavel)],
                      NA_character_),
    stringsAsFactors = FALSE
  )
  write.csv(dict, file.path(out_dir, "dicionario_variaveis.csv"), row.names = FALSE)

  tgt_tab <- as.data.frame(table(df[[target]], useNA = "ifany"))
  names(tgt_tab) <- c(target, "freq")
  write.csv(tgt_tab, file.path(out_dir, "distribuicao_target.csv"), row.names = FALSE)

  quality_df <- miss
  quality_df$duplicados_total_dataset <- n_dup
  write.csv(quality_df, file.path(out_dir, "qualidade_dados_resumo.csv"), row.names = FALSE)

  invisible(list(
    out_dir = out_dir,
    dict = dict,
    target_dist = tgt_tab,
    missing = miss,
    duplicated_rows = n_dup
  ))
}


out_cap1 <- file.path(result_dir, "outputs_cap1")
if (exists("df_raw")) {
  run_cap1(df_raw, out_dir = out_cap1, target = "score_review", tol_within = 0.5)
} else {
  warning("df_raw nao existe no ambiente. Certifica-te que 2_Config.R correu antes deste script.")
}
