
#' An internal customized ggplot2 theme for ViSAGE
#'
#' @noRd
boris_theme <- function(){
  ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text       = ggplot2::element_text(color = "black", size = 16),
      axis.title      = ggplot2::element_text(colour = "black", face = "bold", size = 16),
      legend.text     = ggplot2::element_text(colour = "black", size = 16),
      legend.position = "none",
      legend.title    = ggplot2::element_blank(),
      panel.grid      = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border    = ggplot2::element_rect(colour = "black"),
      text            = ggplot2::element_text(colour = "black", size = 16)
    )
}


####A function to create the founders +++++++++++++++++++++++++ ####
#' Create a founder population using AlphaSimR.
#' @description
#' This function simulates a founder population with specified parameters such
#' as the number of founders, number of chromosomes, and number of segregating sites.
#'
#' @param nfounders an integer representing the number of founders to be simulated.
#' @param nChrom an integer representing the number of chromosomes in the simulated genome.
#' @param nSites an integer representing the total number of segregating sites across the genome.
#' The function will distribute these sites evenly across the specified number of chromosomes.
#'
#' @returns An AlphaSimR founder population object.
#' @noRd
create_founders <- function(nfounders = 100, nChrom = 10, nSites = 200){
  founders <- AlphaSimR::quickHaplo(nInd = nfounders, nChr = nChrom,
                                 segSites = ceiling(2*nSites/nChrom),
                                 inbred = FALSE)
  return(founders)
}


#' Convert to HapMap format
#' @description
#' The function below was written by Alex Lipka on February 20, 2024. It is designed
## to take SNPs simulated from AlphaSimR and create a
# "hapmap-formatted" data that can be read into simplePHENOTYPES
#'
#' @param these.SNPs a matrix of snp maker genotypes
#' @param this.physical.map a data frame containing the physical map information for snp markers.
#'
#' @returns A data frame in HapMap format that can be used as input for simplePHENOTYPES.
#' @noRd
get.me.my.SNPs.in.hapmap.format <- function(these.SNPs = NA,
                                            this.physical.map = NA){
  # Subtract the numeric genotypes so that they range from -1,0,1
  these.SNPs.minus.one <- these.SNPs-1

  #Arrange the SNPs and genetic map  so that they are in a HapMap-like format
  # for simplePHENOTYPES
  hapmap.file.of.these.SNPs <- data.frame(
    snp = colnames(these.SNPs.minus.one),
    allele = rep("A/G", ncol(these.SNPs.minus.one)),
    chr = this.physical.map$chr,
    pos = this.physical.map$pos,
    cm = rep(NA, ncol(these.SNPs.minus.one)),
    t(these.SNPs.minus.one)
  )

  return(hapmap.file.of.these.SNPs)
} #end get.me.my.SNPs.in.hapmap.format

### An updated function to create the base population (AlphaSimR)
#' Create a base population using AlphaSimR.
#' @description
#' This function simulates a base population based on a founder population and
#' specified parameters by the user.
#'
#' @param founders an AlphaSimR founder population object.
#' @param nQTN an integer representing the number of quantitative trait nucleotides (QTNs).
#' @param tMean a integer representing the population mean value for the trait.
#' @param tVA a numeric value representing the additive genetic variance for the trait.
#' @param tVD a numeric value representing the dominance genetic variance for the trait.
#' @param tVE a numeric value representing the epistatic genetic variance for the trait.
#' @param tdomDeg a numeric value representing the mean dominance degree for the trait.
#' @param tVDomDeg a numeric value representing the variance of dominance degree for the trait.
#' @param tHet a numeric value representing the broad-senseheritability of the trait.
#'
#' @returns A list containing the genetic architecture 'arch' of the trait and the simulated base population object.
#' @noRd
create_base_pop <- function(founders, sp_object, nQTN = NULL, tMean = NULL,
                            tVA=25, tVD=0, tVE=0, tdomDeg= 0, tVDomDeg = 0, tHet=NULL){
  nChrom <- founders@nChr
  SP <- sp_object
  SP$resetPed()

  ##If trait is pure Additive
  if(tVD == 0 & tVE == 0){
    SP$addTraitA(nQtlPerChr = ceiling(nQTN[1]/nChrom), name = "Trait1", mean = tMean[1], var = tVA[1])

    ##If trait is Additive + Dominance
  }else if(tVD != 0 & tVE == 0){
    SP$addTraitAD(nQtlPerChr = ceiling(nQTN[1]/nChrom), name = "Trait1", mean = tMean[1], var = tVA[1] + tVD[1],
                  meanDD = tdomDeg[1], varDD = tVDomDeg[1])

    ##If trait is Additive + Epistasis
  }else if(tVD == 0 & tVE != 0){
    SP$addTraitAE(nQtl = ceiling(nQTN[1]/nChrom), mean = tMean[1], var = tVA[1]+tVE[1],
                  name = "Trait1", relAA = tVE[1]/tVA[1])
    #relAA = traitVE/traitVA

    ##If trait is Additive + Dominance + Epistasis
  }else if(tVD != 0 & tVE != 0){
    SP$addTraitADE(nQtl = ceiling(nQTN[1]/nChrom), mean = tMean[1], var = tVA[1]+tVE[1]+tVD[1],
                   name = "Trait1", relAA = tVE[1]/tVA[1],
                   meanDD = tdomDeg[1], varDD = tVDomDeg[1])
  }

  ## Create base population and set heritablity to get pheno
  pop <- AlphaSimR::newPop(founders, simParam = SP)
  pop <- AlphaSimR::setPheno(pop, H2 = tHet, simParam = SP)
  out <- list(arch = " ", pop = pop, QTN_list = NULL)
  return(out)
}


local_sp <- function(f_pop){
  SP <- AlphaSimR::SimParam$new(f_pop)
  return(SP)
}

### An updated function to create the base population (simplePHENOTYPES)
#' Create a base population using AlphaSimR and simplePHENOTYPES.
#'
#' @param founders an AlphaSimR founder population object.
#' @param tMean a numeric value representing the population mean value for the trait.
#' @param a_QTNs an integer representing the number of additive QTNs for the trait.
#' @param d_QTNs an integer representing the number of dominance QTNs for the trait.
#' @param e_QTNs an integer representing the number of epistatic QTNs for the trait.
#' @param big_a_eff a numeric value representing the effect size of the major additive QTNs for the trait.
#' @param a_eff a numeric value representing the effect size of the minor additive QTNs for the trait.
#' @param d_eff a numeric value representing the effect size of the dominance QTNs for the trait.
#' @param e_eff a numeric value representing the effect size of the epistatic QTNs for the trait.
#' @param tHet a numeric value representing the broad-sense heritability of the trait.
#'
#' @returns A list containing the genetic architecture 'arch' of the trait and the simulated base population object.
#' @noRd
create_base_pop_sp <- function(founders, sp_object, tMean = NULL,
                            a_QTNs = 0, d_QTNs = 0, e_QTNs = 0,
                            big_a_eff = 0, a_eff = 0, d_eff = 0, e_eff = 0, tHet=NULL){

  ## Required conditions for the simulations
  if(identical(c(a_QTNs,d_QTNs, e_QTNs), rep(0,3))){
    stop("At least one of the QTN types must be provided.")
  }
  if(identical(c(big_a_eff, a_eff, d_eff, e_eff), rep(0,4))){
    stop("At least one of the effect types must be provided.")
  }

  nChrom <- founders@nChr
  nQTN <- sum(a_QTNs, d_QTNs, 2*e_QTNs, na.rm = T)

  SP <- sp_object
  SP$resetPed()
  ##This is just a dummy trait
  SP$addTraitA(nQtlPerChr = ceiling(nQTN[1]/nChrom), name = "Trait1", mean = 0, var = 64)



  ## Create base population. Note, pop cannot be created unless trait is added to SP.
  pop <- AlphaSimR::newPop(founders, simParam = SP)

  ## Set the phenotype for the base population
  ### Pulling out the genotypic/genomic data. This Does not work unless you have added a trait.
  bp_qtl_map <- AlphaSimR::getQtlMap(trait = 1, simParam = SP)
  bp_qtls <- AlphaSimR::pullQtlGeno(pop, trait = 1, simParam = SP)

  #The function by Alex transforms the snp data into hapmap format needed for simplePHENOTYPES
  bp_qtls_hapmap <- get.me.my.SNPs.in.hapmap.format(these.SNPs = bp_qtls, this.physical.map = bp_qtl_map)

  ### a nested function to manually select QTNs
  select_qtns <- function(hapmap_obj, a_qtns, d_qtns, e_qtns, seed_num){
    if(identical(c(a_qtns, d_qtns, e_qtns), rep(0,3))){
      stop("")
    }else{
      set.seed(seed = seed_num)
      a_list <- sample(hapmap_obj$snp, a_qtns)
      d_list <- sample(setdiff(hapmap_obj$snp, a_list), d_qtns)
      e_list <- sample(setdiff(hapmap_obj$snp, c(a_list, d_list)), 2*e_qtns)

      QTN_list <- list()
      QTN_list$add[[1]] <- a_list
      QTN_list$dom[[1]] <- d_list
      QTN_list$epi[[1]] <- e_list
      return(QTN_list)
    }

  }

  QTN_list <- select_qtns(hapmap_obj = bp_qtls_hapmap, a_qtns = a_QTNs,
                          d_qtns = d_QTNs, e_qtns = e_QTNs, seed_num = 1989)

  ## Determine the genetic architecture of the trait
  arch <- ""
  if (a_QTNs != 0 & !identical(c(a_eff, big_a_eff), rep(0,2))) arch <- paste0(arch, "A")
  if (d_QTNs != 0 & d_eff != 0) arch <- paste0(arch, "D")
  if (e_QTNs != 0 & e_eff != 0) arch <- paste0(arch, "E")


  a_QTNs   <- if (a_QTNs   == 0) NULL else a_QTNs
  big_a_eff <- if (big_a_eff == 0) NULL else big_a_eff
  a_eff    <- if (a_eff    == 0) NULL else a_eff

  d_QTNs   <- if (d_QTNs   == 0) NULL else d_QTNs
  d_eff    <- if (d_eff    == 0) NULL else d_eff

  e_QTNs   <- if (e_QTNs   == 0) NULL else e_QTNs
  e_eff    <- if (e_eff    == 0) NULL else e_eff

  # a_QTNs <<- a_QTNs; big_a_eff <<- big_a_eff; a_eff <<- a_eff;
  # d_QTNs <<- d_QTNs; d_eff <<- d_eff; e_QTNs <<- e_QTNs; e_eff <<- e_eff

  ### Use simplePHENOTYPES to create the Genetic values and phenotype values for the base population.
  pheno.value <- simplePHENOTYPES::create_phenotypes(geno_obj = bp_qtls_hapmap,
                                            ntraits = 2,
                                            h2 = c(tHet, 1),
                                            mean = rep(tMean,2),
                                            rep = 1,
                                            #Select QTNs manually
                                            QTN_list = QTN_list,
                                            #Additive
                                            add_QTN_num = a_QTNs,
                                            big_add_QTN_effect = rep(big_a_eff, 2),
                                            add_effect = list(a_eff, a_eff),
                                            #Dominance
                                            dom_QTN_num = d_QTNs,
                                            dom_effect = list(d_eff, d_eff),
                                            #Epistasis
                                            epi_QTN_num = e_QTNs,
                                            epi_effect = list(e_eff, e_eff),
                                            # sim_method = "geometric",
                                            #specify trait architecture
                                            model = arch,
                                            home_dir = tempdir(),
                                            to_r = TRUE,
                                            quiet = T,
                                            seed = 4000)

  ## Import the genetic and phenotypic values back into the population object

  # pop@gv <- as.matrix(gen.value[,2])
  pop@pheno <- as.matrix(pheno.value[,2])
  pop@gv <- as.matrix(pheno.value[,3])

  out <- list(arch = arch, pop = pop, QTN_list = QTN_list, hapmap = bp_qtls_hapmap)

  return(out)
}


#### A function to plot PCA biplot of snp data +++++++++++++++++++++++++ #######
plot_pca_biplot <- function(pop){
  geno_mat <- AlphaSimR::pullSegSiteGeno(pop)
  pca_res <- stats::prcomp(geno_mat, center = TRUE, scale. = TRUE)
  pca_var <- (summary(pca_res)$importance)[2,1:2]
  pca_df <- as.data.frame(pca_res$x[,1:5])

  plt <- ggplot2::ggplot(pca_df, ggplot2::aes(x = PC1, y = PC2)) +
    ggplot2::geom_point(alpha = 0.7, color = "darkblue") +
    ggplot2::labs(x = paste0("PC1 ", round(100*pca_var[1],2), "%"),
                  y = paste0("PC2 ", round(100*pca_var[2],2), "%")) +
    boris_theme()
  return(plt)
}
