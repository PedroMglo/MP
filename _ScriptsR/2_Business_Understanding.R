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

  set_hyp <- function(v, txt) {
    if (v %in% hypotheses$variavel) hypotheses$hipotese[hypotheses$variavel == v] <<- txt
  }

  set_hyp("CarimboTripAdvisor", "Espera-se associacao positiva (selo/credibilidade pode sinalizar qualidade).")
  set_hyp("Patrocinado", "Efeito incerto: pode refletir marketing (sem relacao direta com qualidade) ou segmentacao de hoteis.")
  set_hyp("Tomar_medidas_seguranca", "Espera-se associacao positiva (confianca/qualidade do servico).")
  set_hyp("Visitar_website_hotel", "Efeito possivelmente positivo (hotéis mais estruturados/atrativos), mas pode ser proxy de procura.")
  set_hyp("logavaliacoes", "Efeito incerto: mais avaliacoes pode estabilizar rating; pode tambem refletir maior heterogeneidade.")

  amenity_vars <- c("Breakfast","WiFi_gratuito","Estacionamento_gratuito","Piscina",
                    "Restaurante","Servico_quartos","Praia","Bar_lounge")
  for (v in amenity_vars) {
    set_hyp(v, "Espera-se associacao positiva (amenity/servico adicional tende a melhorar experiencia).")
  }

  hypotheses$hipotese[is.na(hypotheses$hipotese)] <- "Hipotese a discutir no relatorio (sinal esperado nao definido a priori)."

  success_lines <- c(
    "Metricas principais: RMSE, MAE e R2 (no conjunto de teste).",
    paste0("Metrica complementar: percentagem de previsoes dentro de ±", tol_within,
           " pontos do score real (within_tolerance)."),
    "Comparacao com baseline: prever a media do score no treino."
  )

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

  quality_lines <- c(
    paste0("Numero de observacoes: ", n_obs),
    paste0("Numero de variaveis: ", n_vars, " (", length(predictors), " preditoras + 1 target)"),
    paste0("Linhas duplicadas (brutas): ", n_dup),
    miss_note,
    paste0("Range observado do target '", target, "': ", paste(target_rng, collapse = " a ")),
    paste0("Observacoes com '", target, "' fora de [1,5]: ", target_outside),
    "Nota: o pipeline posterior faz pre-processamento (imputacao, binarizacao, winsorizacao e escalamento) e avalia modelos em train/val/test."
  )


  crisp_lines <- c(
    "CRISP-DM (resumo):",
    "  1) Business Understanding: este ficheiro (objetivo, hipoteses, criterios de sucesso).",
    "  2) Data Understanding: Capitulo 3 (snapshot, missing, duplicados, distribuicoes, correlacoes).",
    "  3) Data Preparation: Capitulo 4 (limpeza, imputacao, features, split e escalamento).",
    "  4) Modeling: Capitulo 5 (baseline, lm, ridge, lasso, arvore, RF, GBM + CV/tuning).",
    "  5) Evaluation: Capitulo 6 (avaliacao final no teste, comparacao e interpretacao).",
    "  6) Deployment: fora do ambito (entrega do relatorio e codigo reproduzivel)."
  )


  lines <- c(
    "=== CAPITULO 1: BUSINESS UNDERSTANDING ===",
    "",
    "1) Objetivo do trabalho",
    paste0("- Prever o score de avaliacao ('", target, "') de hoteis (escala 1 a 5) usando variaveis explicativas do dataset."),
    "- O problema e formulado como regressao (target quantitativa).",
    "",
    "2) Questoes de negocio / analiticas (exemplos)",
    "- Quais as caracteristicas (amenities/indicadores) mais associadas a melhores scores?",
    "- E possivel melhorar significativamente a baseline (media) com modelos mais flexiveis (arvores/RF/GBM)?",
    "- Quais variaveis parecem ter maior impacto e com que direcao?",
    "",
    "3) Variavel target e preditoras",
    paste0("- Target: ", target, " (1 a 5)"),
    paste0("- Preditoras (", length(predictors), "): ", paste(predictors, collapse = ", ")),
    "",
    "4) Hipoteses iniciais (a validar na modelacao)",
    paste0("- ", paste0(hypotheses$variavel, ": ", hypotheses$hipotese, collapse = "\n- ")),
    "",
    "5) Criterios de sucesso",
    paste0("- ", paste(success_lines, collapse = "\n- ")),
    "",
    "6) Restrições e qualidade de dados (alto nível)",
    paste0("- ", paste(quality_lines, collapse = "\n- ")),
    "",
    paste(crisp_lines, collapse = "\n")
  )

  save_lines(lines, file.path(out_dir, "business_understanding.txt"))

  # Dicionário de variáveis
  bin_flag <- is_bin[match(names(df), names(is_bin))]  # NA para variaveis nao preditivas
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

  # Distribuicao
  tgt_tab <- as.data.frame(table(df[[target]], useNA = "ifany"))
  names(tgt_tab) <- c(target, "freq")
  write.csv(tgt_tab, file.path(out_dir, "distribuicao_target.csv"), row.names = FALSE)

  # Resumo de qualidade (missing + duplicados)
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
