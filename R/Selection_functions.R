# Description: This function performs selection across multiple generations using different selection strategies.
# Uses functions and objects available in AlphaSimR
# initial_generation, this is the base population, an object of Pop-class from AlphaSimR. The default is NULL.
# SP_object This is the simulation parameter object from AlphaSimR. The default is NULL.
# selectionType This is a string argument to specify the type of selection to be carried out across generations.
# There are five possible choices, "Random", "Directional_high", "Directional_low", "Disruptive", and "Stabilizing". The default is "Random".
# selectPercent  This is a numeric argument specifying the percent of individuals to be selected in each generation. The default is NULL.
# nCross  This is a numeric argument specifying the number of crosses to be made in each generation. The default is NULL.
# nGenerations This is a numeric argument specifying the number of generations to simulate. The default is 1.
# nProgenyPerCross This is a numeric argument specifying the number progenies to advance per crosses

multi_generations_selection <- function(initial_generation = NULL,
                                        SP_object = NULL,
                                        selectionType = "Random",
                                        tSel = 1,
                                        tHet = NULL,
                                        selectPercent = NULL,
                                        nCross = NULL,
                                        nProgenyPerCross = 10,
                                        nGenerations = 1){

  if(is.null(initial_generation)){
    stop("initial_generation is NULL. Please provide a valid Pop-class object from AlphaSimR.")
  }
  if(is.null(selectPercent)){
    stop("selectPercent is NULL. Please provide a valid numeric value for the percent of individuals to select.")
  }
  if(is.null(nCross)){
    stop("nCross is NULL. Please provide a valid numeric value for the number of crosses to make.")
  }
  if(!(selectionType %in% c("Random_drift", "Directional_higher", "Directional_lower", "Disruptive", "Stabilizing"))){
    stop("The selectionType you provided is invalid. Please choose from 'Random_drift',
         'Directional_higher', 'Directional_lower', 'Disruptive', or 'Stabilizing'.")
  }

  if(all(is.na(AlphaSimR::pheno(initial_generation)))){
    stop("There is no phenotype. Use, setPheno function to set phenotype before selection.")
  }

  nSelect = round((AlphaSimR::nInd(initial_generation) * selectPercent/100), 0)
  generation_data_list <- list(initial_generation)

  ###Loop through the n generations
  for(i in 1:nGenerations){
    # het <- ifelse(tHet == 0, (cor(gv(initial_generation), pheno(initial_generation)))^2, tHet)

    print(paste("---------------Initiating generation ", i, " of ",
                selectionType, " selection------------", sep = ""))

    #Store the current generation as the "previous generation". This
    # will allow us to use info of the previous generation in GWAS and GS
    previous.generation = initial_generation

    #Breeding decision for ith breeding program...select individuals
    if(selectionType == "Random_drift"){
      selected_inds = AlphaSimR::selectInd(pop = initial_generation, use = "rand", trait = tSel,
                                           nInd = nSelect, simParam = SP_object, selectTop = FALSE)
    }else if(selectionType == "Directional_higher"){
      selected_inds = AlphaSimR::selectInd(pop = initial_generation, use = "pheno", trait = tSel,
                                           nInd = nSelect, simParam = SP_object, selectTop = TRUE)
    }else if(selectionType == "Directional_lower"){
      selected_inds = AlphaSimR::selectInd(pop = initial_generation, use = "pheno", trait = tSel,
                                           nInd = nSelect, simParam = SP_object, selectTop = FALSE)
    }else if(selectionType == "Disruptive"){
      #This code is from https://cran.r-project.org/web/packages/AlphaSimR/AlphaSimR.pdf, Boris edited it
      squaredDeviation = function(x){optima = mean(x);final_val <- (x - optima)^2; return(final_val)}
      selected_inds = AlphaSimR::selectInd(pop = initial_generation, use = "pheno",
                                           nInd = nSelect, simParam = SP_object,
                                           trait = squaredDeviation, selectTop = TRUE)
    }else if(selectionType == "Stabilizing"){
      #This code is from https://cran.r-project.org/web/packages/AlphaSimR/AlphaSimR.pdf
      squaredDeviation = function(x){optima = mean(x);final_val <- (x - optima)^2; return(final_val)}
      selected_inds = AlphaSimR::selectInd(pop = initial_generation, use = "pheno", nInd = nSelect,
                                           simParam = SP_object, trait = squaredDeviation, selectTop = FALSE)
    }

    #Make crosses for the next generation
    next_generation = AlphaSimR::randCross(pop = selected_inds, nCrosses = nCross, nProgeny = nProgenyPerCross, simParam = SP_object)
    next_generation = AlphaSimR::setPheno(next_generation, H2 = tHet, simParam = SP_object)
    #Add the new generation to the generation list
    generation_data_list <- c(generation_data_list, next_generation)

    #Set the initial generation to be the next generation for the next loop
    initial_generation = next_generation

    #update nselect
    nSelect = round((AlphaSimR::nInd(next_generation) * selectPercent/100), 0)

  } #end for loop through generations

  return(generation_data_list)
} #end cross.stuff.for.a.whole.bunch.of.generations

multi_generations_selection_sp <- function(initial_generation = NULL,
                                        SP_object = NULL,
                                        selectionType = "Random",
                                        QTN_list = NULL,
                                        tSel = 1, arch = NULL,
                                        tHet = NULL,
                                        selectPercent = NULL,
                                        nCross = NULL,
                                        nProgenyPerCross = 10,
                                        nGenerations = 1,
                                        a_QTNs= NULL, d_QTNs = NULL, e_QTNs = NULL,
                                        big_a_eff = NULL, a_eff = NULL, d_eff = NULL, e_eff = NULL){

  if(is.null(initial_generation)){
    stop("initial_generation is NULL. Please provide a valid Pop-class object from AlphaSimR.")
  }
  if(is.null(selectPercent)){
    stop("selectPercent is NULL. Please provide a valid numeric value for the percent of individuals to select.")
  }
  if(is.null(nCross)){
    stop("nCross is NULL. Please provide a valid numeric value for the number of crosses to make.")
  }
  if(!(selectionType %in% c("Random_drift", "Directional_higher", "Directional_lower", "Disruptive", "Stabilizing"))){
    stop("The selectionType you provided is invalid. Please choose from 'Random_drift',
         'Directional_higher', 'Directional_lower', 'Disruptive', or 'Stabilizing'.")
  }

  if(all(is.na(AlphaSimR::pheno(initial_generation)))){
    stop("There is no phenotype. Use, setPheno function to set phenotype before selection.")
  }

  nSelect = round((AlphaSimR::nInd(initial_generation) * selectPercent/100), 0)
  generation_data_list <- list(initial_generation)

  ###Loop through the n generations
  for(i in 1:nGenerations){
    # het <- ifelse(tHet == 0, (cor(gv(initial_generation), pheno(initial_generation)))^2, tHet)

    print(paste("---------------Initiating generation ", i, " of ",
                selectionType, " selection------------", sep = ""))

    # #Store the current generation as the "previous generation". This
    # # will allow us to use info of the previous generation in GWAS and GS
    # previous.generation = initial_generation

    #Breeding decision for ith breeding program...select individuals
    if(selectionType == "Random_drift"){
      selected_inds = AlphaSimR::selectInd(pop = initial_generation, use = "rand", trait = tSel,
                                           nInd = nSelect, simParam = SP_object, selectTop = FALSE)
    }else if(selectionType == "Directional_higher"){
      selected_inds = AlphaSimR::selectInd(pop = initial_generation, use = "pheno", trait = tSel,
                                           nInd = nSelect, simParam = SP_object, selectTop = TRUE)
    }else if(selectionType == "Directional_lower"){
      selected_inds = AlphaSimR::selectInd(pop = initial_generation, use = "pheno", trait = tSel,
                                           nInd = nSelect, simParam = SP_object, selectTop = FALSE)
    }else if(selectionType == "Disruptive"){
      #This code is from https://cran.r-project.org/web/packages/AlphaSimR/AlphaSimR.pdf, Boris edited it
      squaredDeviation = function(x){optima = mean(x);final_val <- (x - optima)^2; return(final_val)}
      selected_inds = AlphaSimR::selectInd(pop = initial_generation, use = "pheno",
                                           nInd = nSelect, simParam = SP_object,
                                           trait = squaredDeviation, selectTop = TRUE)
    }else if(selectionType == "Stabilizing"){
      #This code is from https://cran.r-project.org/web/packages/AlphaSimR/AlphaSimR.pdf
      squaredDeviation = function(x){optima = mean(x);final_val <- (x - optima)^2; return(final_val)}
      selected_inds = AlphaSimR::selectInd(pop = initial_generation, use = "pheno", nInd = nSelect,
                                           simParam = SP_object, trait = squaredDeviation, selectTop = FALSE)
    }

    ### Need to update the trait mean
    new_tMean <- mean(AlphaSimR::gv(selected_inds))

    #Make crosses for the next generation
    next_generation = AlphaSimR::randCross(pop = selected_inds, nCrosses = nCross, nProgeny = nProgenyPerCross, simParam = SP_object)

    ## Set the phenotype for the base population
    ### Pulling out the genotypic/genomic data. This Does not work unless you have added a trait.
    ngen_map <- AlphaSimR::getQtlMap(trait = 1, simParam = SP_object)
    ngen_qtls <- AlphaSimR::pullQtlGeno(next_generation, trait = 1, simParam = SP_object)

    #The function by Alex transforms the snp data into hapmap format needed for simplePHENOTYPES
    ngen_qtls_hapmap <- get.me.my.SNPs.in.hapmap.format(these.SNPs = ngen_qtls, this.physical.map = ngen_map)

    ### Use simplePHENOTYPES to create the Genetic values and phenotype values for the base population.

    pheno.value <- simplePHENOTYPES::create_phenotypes(geno_obj = ngen_qtls_hapmap,
                                                       ntraits = 2,
                                                       h2 = c(tHet, 1),
                                                       mean = rep(new_tMean,2),
                                                       rep = 1,
                                                       #Select QTNs manually
                                                       QTN_list = QTN_list,
                                                       #Additive
                                                       add_QTN_num = a_QTNs[1],
                                                       big_add_QTN_effect = rep(big_a_eff[1],2),
                                                       add_effect = rep(a_eff[1],2),
                                                       #Dominance
                                                       dom_QTN_num = d_QTNs[1],
                                                       dom_effect = rep(d_eff[1],2),
                                                       #Epistasis
                                                       epi_QTN_num = e_QTNs[1],
                                                       epi_effect = rep(e_eff[1],2),
                                                       # sim_method = "geometric",
                                                       #specify trait architecture
                                                       model = arch,
                                                       home_dir = tempdir(),
                                                       to_r = TRUE,
                                                       quiet = T,
                                                       seed = sample(1:100,1))

    # gen.value <- simplePHENOTYPES::create_phenotypes(geno_obj = ngen_qtls_hapmap,
    #                                                  ntraits = 1,
    #                                                  h2 = 1,
    #                                                  mean = new_tMean[1],
    #                                                  rep = 1,
    #                                                  #Additive
    #                                                  add_QTN_num = a_QTNs[1],
    #                                                  big_add_QTN_effect = big_a_eff[1],
    #                                                  add_effect = a_eff[1],
    #                                                  #Dominance
    #                                                  dom_QTN_num = d_QTNs[1],
    #                                                  dom_effect = d_eff[1],
    #                                                  #Epistasis
    #                                                  epi_QTN_num = e_QTNs[1],
    #                                                  epi_effect = e_eff[1],
    #
    #                                                  sim_method = "geometric",
    #                                                  model = arch,
    #                                                  home_dir = tempdir(),
    #                                                  to_r = TRUE,
    #                                                  quiet = TRUE,
    #                                                  seed = 4000)

    ## Import the genetic and phenotypic values back into the population object

    # next_generation@gv <- as.matrix(gen.value[,2])
    next_generation@gv <- as.matrix(pheno.value[,3])
    next_generation@pheno <- as.matrix(pheno.value[,2])


    #Add the new generation to the generation list
    generation_data_list <- c(generation_data_list, next_generation)

    #Set the initial generation to be the next generation for the next loop
    initial_generation = next_generation

    #update nselect
    nSelect = round((AlphaSimR::nInd(next_generation) * selectPercent/100), 0)

  } #end for loop through generations

  return(generation_data_list)
} #end cross.stuff.for.a.whole.bunch.of.generations


## This function "extract_data", computes pheno, genetic values, and breeding values across all individuals and
### generations simulation above. It returns a data frame.
# generation_list,  This is the list of all generations simulated above. Each gene is a pop object of AlphaSimR.
# SP_object This is the simulation parameter object from AlphaSimR. The default is SP.
extract_data <- function(generation_list, SP_object = SP, nTrait =1){

  if(length(generation_list) == 0){
    return(NULL)
  }else{
    nGen = length(generation_list)
    genZeroSize = AlphaSimR::nInd(generation_list[[1]])
    genSize = AlphaSimR::nInd(generation_list[[2]])

    tmp_list <- function(x){
      one_gen <- data.frame(phenotype = as.numeric(AlphaSimR::pheno(x)),
                            genetic_value = as.numeric(AlphaSimR::gv(x)),
                            breeding_value = as.numeric(AlphaSimR::bv(x, simParam = SP_object))
      )
      return(one_gen)
    }

    pheno <- lapply(generation_list, tmp_list) |>
      data.table::rbindlist() |> as.data.frame()
    # |>
    #   dplyr::mutate(Generation = c(rep(0,genZeroSize), rep(1:(nGen-1), each = genSize)),
    #                 Trait = "Trait1")
    pheno <- within(pheno,{
      Generation <- c(rep(0,genZeroSize), rep(1:(nGen-1), each = genSize))
      Trait <- rep("Trait1", nrow(pheno))
    })

    return(pheno)


  }

}

add_select_type <- function(df_list, labels) {
  lapply(seq_along(df_list), function(i) {
    df <- df_list[[i]]
    if (is.null(df)) return(NULL)

    dplyr::mutate(as.data.frame(df), Selection_type = labels[i])
  })
}


## This function "genetic_gain", generates a genetic gain plot across generations
### The arguments are data frame returned from the extract_data function, a factor for fill color, and the y variable to plot
# It returns a ggplot object made of boxplots and and maybe density plot

genetic_gain <- function(dt, fill_factor1, fill_factor2, y_variable, trait_name = NULL){
  # dt <- dplyr::filter(Trait == trait_name)
  y_label <- ifelse(y_variable == "genetic_value", "Genetic value",
                    ifelse(y_variable == "breeding_value", "Breeding value", "Phenotype"))
  custom_theme <- ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(color = "black", size = 12, angle = 45),
                   axis.text.y = ggplot2::element_text(color = "black", size = 12),
                   axis.title = ggplot2::element_text(colour = "black", face = "bold", size = 13),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(linewidth = 1.2, color = "black"),
                   axis.ticks = ggplot2::element_line(linewidth = 1, colour = "black"),
                   legend.text = ggplot2::element_text(colour = "black", size = 12),
                   text = ggplot2::element_text(colour = "black", size = 12))
  dt_plot <- dt |>
    dplyr::mutate(!!rlang::sym(fill_factor1) := as.factor(!!rlang::sym(fill_factor1)),
                  !!rlang::sym(fill_factor2) := as.factor(!!rlang::sym(fill_factor2)))

  dt_summary <- dt |>
    dplyr::mutate(!!rlang::sym(fill_factor1) := as.factor(!!rlang::sym(fill_factor1)),
                  !!rlang::sym(fill_factor2) := as.factor(!!rlang::sym(fill_factor2)),
                  mean_pheno = !!rlang::sym(y_variable)) |>
    dplyr::group_by(!!rlang::sym(fill_factor1), !!rlang::sym(fill_factor2)) |>
    dplyr::summarise_at("mean_pheno", mean)

  cols <- c("#4477AA", "#228833", "#AA3377", "grey30", "#CCBB14")
  names(cols) <- c("Directional_higher", "Directional_lower", "Disruptive",
                   "Stabilizing", "Random_drift")
  plt <- ggplot2::ggplot(data = dt_plot, ggplot2::aes(x = !!rlang::sym(fill_factor1), y = !!rlang::sym(y_variable))) +
    ggplot2::geom_boxplot(ggplot2::aes(color = !!rlang::sym(fill_factor2))) +
    ggplot2::geom_line(data = dt_summary, ggplot2::aes(x = !!rlang::sym(fill_factor1), y = mean_pheno, group = !!rlang::sym(fill_factor2),
                                                       color = !!rlang::sym(fill_factor2)), linewidth=1) +
    ggplot2::geom_point(data = dt_summary,
                        ggplot2::aes(x = !!rlang::sym(fill_factor1), y = mean_pheno,
                                     color = !!rlang::sym(fill_factor2)), size=2,
                        position = ggplot2::position_dodge(width = 0.75)) +
    ggplot2::labs(y = y_label, x = fill_factor1) +
    ggplot2::scale_color_manual(values = cols) +
    custom_theme
  # "#4477AA" "#228833" "#AA3377" "#BBBBBB" "#66CCEE" "#CCBB44" "#EE6677"
  return(plt)
}


#' Compute and plot variance components across generations
#'
#' @param gen_list
#' @param SP_object
#'
#' @returns
#' @export
#'
#' @examples
computeVariancePlot <- function(gen_list, SP_object) {
  # if(length(gen_list) == 0) {
  #   stop("gen_list is empty. Please provide a list of population objects from AlphaSimR.")
  # }
  # 1. Extract variance components for each generation
  variance_df <- base::lapply(seq_along(gen_list), function(i) {
    pop <- gen_list[[i]]
    # SP_object <- SP_object

    tibble::tibble(
      generation = i,
      Additive = AlphaSimR::varA(pop, simParam = SP_object),
      Dominance = AlphaSimR::varD(pop, simParam = SP_object),
      AA_Epistasis = AlphaSimR::varAA(pop, simParam = SP_object)
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(generation = generation -1) |>
    as.data.frame()

  # 2. Convert to long format and compute proportions
  variance_long <- variance_df |>
    tidyr::pivot_longer(
      cols = c("Additive", "Dominance", "AA_Epistasis"),
      names_to = "component",
      values_to = "value"
    ) |>
    dplyr::group_by(.data$generation) |>
    dplyr::mutate(proportion = .data$value / sum(.data$value)) |>
    as.data.frame()

  # 3. Create stacked barplot
  custom_theme <- ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(color = "black", size = 12, angle = 45),
                   axis.title = ggplot2::element_text(colour = "black", face = "bold", size = 13),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(linewidth = 1.2, color = "black"),
                   axis.ticks = ggplot2::element_line(linewidth = 1, colour = "black"),
                   legend.text = ggplot2::element_text(colour = "black", size = 12),
                   text = ggplot2::element_text(colour = "black", size = 12))
  fill_col <- c("Additive" = "#1b9e77", "Dominance" = "#d95f02",
                "AA_Epistasis" = "#7570b3")

  p <- ggplot2::ggplot(
    variance_long,
    ggplot2::aes(
      x = base::factor(.data$generation),
      y = .data$proportion,
      fill = .data$component
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = "Generation", y = "Prop. of Variance",
                  title = " ") +
    ggplot2::scale_fill_manual(
      values = fill_col) +
    custom_theme

  # # Return both data and plot
  # list(
  #   data = variance_long,
  #   plot = p
  # )

  return(p)
}

computeVariancePlot_SP <- function(gen_list, SP_object) {
  # if(length(gen_list) == 0) {
  #   stop("gen_list is empty. Please provide a list of population objects from AlphaSimR.")
  # }
  # 1. Create list of merged geno and pheno dts
  compute_variances <- function(pop, SP_object) {

    geno_dt <- AlphaSimR::pullQtlGeno(pop, trait = 1, simParam = SP_object)
    pheno_dt <- as.data.frame(AlphaSimR::gv(pop))
    geno_df <- data.frame(ID = paste0("id",1:nrow(geno_dt)), geno_dt)
    pheno_df <- data.frame(ID = paste0("id",1:nrow(geno_dt)), pheno_dt)
    trait <- colnames(pheno_df)[2]

    # -------------------------
    # Input validation
    # -------------------------
    if (!"ID" %in% colnames(geno_df)) {
      stop("geno_df must contain an 'ID' column.")
    }

    if (!"ID" %in% colnames(pheno_df)) {
      stop("pheno_df must contain an 'ID' column.")
    }

    if (ncol(pheno_df) < 2) {
      stop("pheno_df must contain at least one trait column.")
    }

    # -------------------------
    # Merge data
    # -------------------------
    merged_df <- dplyr::inner_join(geno_df, pheno_df, by = "ID")

    trait_name <- colnames(pheno_df)[2]
    markers <- colnames(geno_df)[-1]
    nloci = ncol(geno_df)-1
    nindiv = nrow(geno_df)

    # -------------------------
    # Convert to long format
    # -------------------------
    long_df <- tidyr::pivot_longer(
      data = merged_df,
      cols = dplyr::all_of(markers),
      names_to = "SNP",
      values_to = "Dosage"
    )

    # -------------------------
    # Genotype means and counts
    # -------------------------
    genotype_stats <- dplyr::group_by(long_df, SNP, Dosage) |>
      dplyr::summarise(
        mean_trait = mean(.data[[trait_name]], na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )

    # -------------------------
    # Allele frequency
    # -------------------------
    allele_freq <- tidyr::pivot_wider(
      dplyr::select(genotype_stats, -mean_trait),
      names_from = Dosage,
      values_from = n, names_prefix = "freq_") |>
      dplyr::mutate(p = (2*freq_0 + freq_1)/(2*nindiv),
                    q = 1-p)

    # -------------------------
    # Gene action effect: a & d
    # -------------------------
    a_d_effect <- tidyr::pivot_wider(
      dplyr::select(genotype_stats, -n),
      names_from = Dosage,
      values_from = mean_trait,
      names_prefix = "mu_") |>
      dplyr::mutate(a = (mu_0 - mu_2)/2,
                    d = mu_1 - (mu_0 + mu_2)/2)

    # -------------------------
    # Compute genetic effects
    # -------------------------
    eff_and_var <- dplyr::full_join(a_d_effect, allele_freq) |>
      dplyr::mutate(
        # Average effect of allele substitution
        alpha = a + d * (q - p),

        # Variance components
        additive_variance = 2 * p * q * (alpha^2),
        dominance_variance = (2 * p * q * d)^2)

    varAdd <- sum(eff_and_var$additive_variance)
    vardom <- sum(eff_and_var$dominance_variance)
    vargen <- var(pheno_df[[trait]])
    varepi <- vargen - (varAdd + vardom)
    # list(eff_and_var = eff_and_var, vargen = vargen,
    #      varAdd = varAdd, vardom = vardom, varepi = varepi)
    df = data.frame(Additive = varAdd, Dominance = vardom,
                    AA_Epistasis = varepi, Genetic = vargen)
    return(df)
  }

  # 1. Extract variance components for each generation
  variance_df <- base::lapply(gen_list, compute_variances, SP_object = SP_object) |>
    data.table::rbindlist() |>
    dplyr::mutate(generation = 0:(length(gen_list)-1)) |>
    as.data.frame()

  # 2. Convert to long format and compute proportions
  variance_long <- variance_df |>
    tidyr::pivot_longer(
      cols = c("Additive", "Dominance", "AA_Epistasis"),
      names_to = "component",
      values_to = "value"
    ) |>
    dplyr::group_by(.data$generation) |>
    dplyr::mutate(proportion = .data$value / sum(.data$value)) |>
    as.data.frame()

  # 3. Create stacked barplot
  custom_theme <- ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(color = "black", size = 12, angle = 45),
                   axis.text.y =  ggplot2::element_text(color = "black", size = 12),
                   axis.title = ggplot2::element_text(colour = "black", face = "bold", size = 13),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(linewidth = 1.2, color = "black"),
                   axis.ticks = ggplot2::element_line(linewidth = 1, colour = "black"),
                   legend.text = ggplot2::element_text(colour = "black", size = 12),
                   text = ggplot2::element_text(colour = "black", size = 12))
  fill_col <- c("Additive" = "#1b9e77", "Dominance" = "#d95f02",
                "AA_Epistasis" = "#7570b3")

  p <- ggplot2::ggplot(
    variance_long,
    ggplot2::aes(
      x = base::factor(.data$generation),
      y = .data$proportion,
      fill = .data$component
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = "Generation", y = "Prop. of Variance",
                  title = " ") +
    ggplot2::scale_fill_manual(
      values = fill_col) +
    custom_theme

  # # Return both data and plot
  # list(
  #   data = variance_long,
  #   plot = p
  # )

  return(p)
}
