### Training model
genomic_prediction_rrblup <- function(
    snp_data,
    pheno_data,
    trait = NULL,
    cv_type = "kfold",
    k = 5,
    n_reps = 5,
    seed = 123
) {
  trait_idx <- which(colnames(pheno_data) == trait)
  pheno_data <- pheno_data[, c(1, trait_idx)]
  colnames(pheno_data) <- c("genotype", "phenotype")
  cv_type <- match.arg(cv_type)

  snp_data <- snp_data |>
    tibble::column_to_rownames(var = colnames(snp_data)[1]) |>
    as.matrix()

  ## -----------------------------
  ## Input checks
  ## -----------------------------
  if (is.null(rownames(snp_data)))
    stop("snp_data must have rownames = genotype IDs")

  if (!all(c("genotype", "phenotype") %in% colnames(pheno_data)))
    stop("pheno_data must contain columns: genotype, phenotype")
  #
  if (cv_type == "kfold" && (k < 2 || k > 10))
    stop("k must be between 2 and 10")

  ## -----------------------------
  ## Align genotypes
  ## -----------------------------
  if(length(which(is.na(snp_data))) > 0){
    snp_data <- (rrBLUP::A.mat(snp_data, max.missing=0.5, impute.method = "mean", return.imputed = TRUE))[[2]]
  }
  common_ids <- intersect(rownames(snp_data), pheno_data$genotype)
  paste0("The number of common genotypes between SNP and phenotype data is: ", length(common_ids)) |> message()

  snp <- snp_data[common_ids, , drop = FALSE]

  pheno <- pheno_data |>
    dplyr::filter(genotype %in% common_ids) |>
    dplyr::arrange(match(genotype, common_ids))

  y <- pheno$phenotype
  # y_c <- as.character(y)
  names(y) <- pheno$genotype

  n <- length(y)

  ## -----------------------------
  ## Cross-validation
  ## -----------------------------
  all_results <- list()
  start_total <- Sys.time()

  for (rep in seq_len(n_reps)) {

    set.seed(seed + rep)

    ## caret fold control
    if (cv_type == "kfold") {

      folds <- caret::createFolds(
        y = as.character(y),
        k = k,
        list = TRUE,
        returnTrain = FALSE
      )

    } else {

      ## LOGO using genotype as grouping factor
      folds <- caret::groupKFold(
        group = names(y),
        k = n
      )
    }

    for (i in seq_along(folds)) {

      test_idx  <- folds[[i]]
      train_idx <- setdiff(seq_len(n), test_idx)

      y_train <- y
      y_train[test_idx] <- NA

      model <- rrBLUP::mixed.solve(
        y = y_train,
        Z = snp
      )

      gebv <- as.vector(snp %*% model$u) + model$beta
      names(gebv) <- names(y)

      all_results[[length(all_results) + 1]] <- data.frame(
        repetition = as.factor(rep),
        genotype   = names(y)[test_idx],
        observed   = y[test_idx],
        predicted  = round(gebv[test_idx],2),
        stringsAsFactors = FALSE
      )
    }
  }

  prediction_df <- dplyr::bind_rows(all_results)
  metrics <- prediction_df |>
    dplyr::group_by(repetition) |>
    dplyr::summarize(correlation = stats::cor(observed, predicted),
                     rmse = sqrt(mean((observed - predicted)^2)))

  ## -----------------------------
  ## Final model on full data
  ## -----------------------------
  final_model <- rrBLUP::mixed.solve(
    y = y,
    Z = snp
  )

  total_time <- round(difftime(Sys.time(), start_total, units = "secs"), 1)

  ## -----------------------------
  ## Output
  ## -----------------------------
  list(
    model = final_model,
    predictions = prediction_df,
    model_metric = metrics,
    metadata = list(
      cv_type = cv_type,
      k = ifelse(cv_type == "kfold", k, NA),
      n_reps = n_reps,
      n_genotypes = n,
      n_markers = ncol(snp),
      runtime_sec = total_time
    )
  )
}


### Actual prediction ++++++++++++++++++++++++++++++++++++++++++ ####

predict_gebv_rrblup <- function(
    final_model,
    snp_new,
    verbose = TRUE
) {
  snp_new <- tibble::column_to_rownames(snp_new, var = colnames(snp_new)[1]) |>
    as.matrix()

  if(length(which(is.na(snp_new))) > 0){
    snp_new <- (rrBLUP::A.mat(snp_new, max.missing=0.5, impute.method = "mean", return.imputed = TRUE))[[2]]
  }
  ## -----------------------------
  ## Input checks
  ## -----------------------------
  if (is.null(final_model$u))
    stop("final_model must be an rrBLUP model object with marker effects (u)")

  if (is.null(colnames(snp_new)))
    stop("snp_new must have column names matching training markers")

  if (is.null(rownames(snp_new)))
    stop("snp_new must have rownames = genotype IDs")

  marker_effects <- final_model$u
  marker_ids <- names(marker_effects)

  ## -----------------------------
  ## Marker alignment
  ## -----------------------------
  common_markers <- intersect(colnames(snp_new), marker_ids)

  if (length(common_markers) < length(marker_ids)) {
    missing <- length(setdiff(marker_ids, colnames(snp_new)))
    warning(paste("Missing", missing, "markers in new SNP data. Prediction will only use",
                  common_markers, "markers."))
  }

  snp_aligned <- snp_new[, marker_ids, drop = FALSE]

  ## -----------------------------
  ## Prediction
  ## -----------------------------
  gebv <- as.vector(snp_aligned %*% marker_effects)
  names(gebv) <- rownames(snp_aligned)

  gebv_df <- data.frame(
    genotype = names(gebv),
    genetic_merit = gebv,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  gebv_df <- stats::na.omit(gebv_df)

  return(gebv_df)
}

### Scatterplot for cross-validation results ++++++++++++++++++++++++++++++++++++++++++ ####
plot_prediction <- function(cross_vd_dt){

  # Compute correlation
  cor_test <- stats::cor.test(cross_vd_dt$observed, cross_vd_dt$predicted)
  cor_val  <- round(cor_test$estimate, 3)
  p_val    <- signif(cor_test$p.value, 3)

  # Axis limits
  mini <- min(c(cross_vd_dt$observed, cross_vd_dt$predicted), na.rm = TRUE)
  maxi <- max(c(cross_vd_dt$observed, cross_vd_dt$predicted), na.rm = TRUE)

  # Create annotation text
  label_text <- paste0("r = ", cor_val, "\n", "p = ", p_val)

  p <- ggplot2::ggplot(cross_vd_dt, ggplot2::aes(x = observed, y = predicted, color = repetition)) +
    ggplot2::geom_point(alpha = 0.6, ) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed", color = "blue") +
    ggplot2::annotate("text", x = mini, y = maxi,
                      label = label_text, hjust = 0, vjust = 1, size = 4) +
    ggplot2::labs(x = "Observed phenotype", y = "Predicted phenotype") +
    ggplot2::scale_x_continuous(limits = c(mini, maxi)) +
    ggplot2::scale_y_continuous(limits = c(mini, maxi)) +
    boris_theme()

  print(p)
}

### Violin plot for summarized cross-validation +++++++++++++++++++++++++ ####
plot_violin <- function(y_vector) {
  # Build plot
  p <- ggplot2::ggplot(data = data.frame(yvar = y_vector), mapping = ggplot2::aes(y = yvar, x = " ")) +
    ggplot2::geom_violin(color = "darkgreen", fill = "lightgreen") +
    ggplot2::geom_point(color = "black", alpha = 0.8,
                        position = ggplot2::position_jitter(width = 0.1)) +
    ggplot2::labs(y = "Predictive ability",x = "") +
    boris_theme()

  print(p)
}

### Histogram for Actual prediction results ++++++++++++++++++++++++++++++++++++++++++ ####
plot_histogram <- function(y_vector, percent) {

  # Compute quantile
  q_val <- stats::quantile(y_vector, probs = 1-percent/100, na.rm = TRUE)
  nbins <- max(10, 100/percent)
  # Build plot
  p <- ggplot2::ggplot(data = data.frame(y = y_vector), mapping = ggplot2::aes(x = y)) +
    ggplot2::geom_histogram(bins = nbins, fill = "lightblue", color = "white") +
    ggplot2::geom_vline(xintercept = q_val, color = "red",linetype = "dashed",linewidth = 1) +
    ggplot2::labs(y = "Number of genotypes",x = "Genetic merit") +
    boris_theme()

  print(p)
}

### This function extract the data needed genomic prediction cross-validation
## the multi-generation and selection type simulation
sim_data_gp <- function(mega_list, generation, sel_type, SP_object){
  # Simulate data for GWAS
  # mega_list: list of lists containing the data for each generation and selection type
  # generation: the generation to simulate data for
  # sel_type: the selection type to simulate data for
  names(mega_list) <- c("Directional_higher", "Directional_lower",
                        "Disruptive", "Stabilizing", "Random_drift")
  SP <- SP_object

  # Extract the relevant data from the mega_list
  generation = generation+1 # Adjust for 1-based indexing in R
  obj_list <- mega_list[[sel_type]][[generation]]

  genomic_data <- AlphaSimR::pullSegSiteGeno(obj_list, simParam = SP) |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "ID") |> as.data.frame()
  pheno_data <- as.data.frame(AlphaSimR::pheno(obj_list))
  colnames(pheno_data) <- "Trait1"
  pheno_data <- data.frame(ID = genomic_data$ID, pheno_data)
  if(length(intersect(genomic_data$ID, pheno_data$ID)) == 0){
    stop("Genomic data and phenotype data do not have any common IDs.")
  }
  gp_dt <- list(
    snp_data = genomic_data,
    pheno_data = pheno_data
  )
  return(gp_dt)
}

### A function to plot selection on test set
test_gp_selection <- function(data_frame, selection_type, percent_selected) {

  # ---- Checks ----
  if (!"genetic_merit" %in% names(data_frame)) {
    stop("Column 'genetic_merit' not found in data_frame.")
  }

  if (!is.numeric(data_frame$genetic_merit)) {
    stop("'genetic_merit' must be numeric.")
  }

  # Convert percent to proportion
  p <- percent_selected / 100
  gm <- data_frame$genetic_merit

  # ---- Determine thresholds and selection ----
  if (selection_type == "Directional_higher") {

    threshold <- stats::quantile(gm, probs = 1 - p, na.rm = TRUE)
    selected <- gm >= threshold

    vlines <- threshold

  } else if (selection_type == "Directional_lower") {

    threshold <- stats::quantile(gm, probs = p, na.rm = TRUE)
    selected <- gm <= threshold

    vlines <- threshold

  } else if (selection_type == "Stabilizing") {

    lower <- stats::quantile(gm, probs = (1 - p)/2, na.rm = TRUE)
    upper <- stats::quantile(gm, probs = 1 - (1 - p)/2, na.rm = TRUE)
    selected <- gm >= lower & gm <= upper

    vlines <- c(lower, upper)

  } else if (selection_type == "Disruptive") {

    lower <- stats::quantile(gm, probs = p/2, na.rm = TRUE)
    upper <- stats::quantile(gm, probs = 1 - p/2, na.rm = TRUE)
    selected <- gm <= lower | gm >= upper

    vlines <- c(lower, upper)

  } else {
    stop("Invalid selection_type. Choose one of:
         'Directional_higher',
         'Directional_lower',
         'stabilizing',
         'Disruptive'")
  }

  # ---- Filtered data ----
  selected_df <- data_frame[selected, ]

  # ---- Add selection flag for plotting ----
  plot_df <- data_frame
  plot_df$Selected <- ifelse(selected, "Selected", "Not Selected")

  # ---- Histogram ----
  p_hist <- ggplot2::ggplot(plot_df, ggplot2::aes(x = genetic_merit, fill = Selected)) +
    ggplot2::geom_histogram(
      bins = 20,
      color = "white",
      alpha = 0.9,
      position = "identity"
    ) +
    ggplot2::scale_fill_manual(
      values = c("Not Selected" = "steelblue",
                 "Selected" = "darkred")
    ) +
    ggplot2::geom_vline(
      xintercept = vlines,
      linetype = "dashed",
      linewidth = 1
    ) +
    boris_theme() +
    ggplot2::labs(
      title = paste("Selection Type:", selection_type),
      x = "Genetic Merit",
      y = "Number of individuals"
    )

  return(list(
    selected_data = selected_df,
    histogram = p_hist
  ))
}
