### Customized theme
boris_theme <- ggplot2::theme_bw() +
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

####A function to create the founders +++++++++++++++++++++++++ ####
create_founders <- function(nfounders = 100, nChrom = 10, nSites = 200){
  founders <- AlphaSimR::runMacs(nInd = nfounders, nChr = nChrom, segSites = ceiling(nSites/nChrom), species = "GENERIC", inbred = T)
  return(founders)
}

### The function below was written by Alex Lipka on February 20, 2024. It is designed
## to take SNPs simulated from AlphaSimR and create a
# "hapmap-formatted" data that can be read into simplePHENOTYPES

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
create_base_pop <- function(founders, nQTN = NULL, tMean = NULL,
                            tVA=25, tVD=0, tVE=0, tdomDeg= 0, tVDomDeg = 0, tHet=NULL){
  nChrom = founders@nChr
  SP <<- AlphaSimR::SimParam$new(founders)

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
  out <- list(arch = " ", pop = pop)
  return(out)
}

### An updated function to create the base population (simplePHENOTYPES)
create_base_pop_sp <- function(founders, tMean = NULL,
                            a_QTNs= NULL, d_QTNs = NULL, e_QTNs = NULL,
                            big_a_eff = NULL, a_eff = NULL, d_eff = NULL, e_eff = NULL, tHet=NULL){

  ## Required conditions for the simulations
  if(identical(c(a_QTNs,d_QTNs, e_QTNs), rep(0,3))){
    stop("At least one of the QTN types must be provided.")
  }
  if(identical(c(big_a_eff, a_eff, d_eff, e_eff), rep(0,4))){
    stop("At least one of the effect types must be provided.")
  }

  nChrom = founders@nChr
  nQTN <- 2*sum(a_QTNs, d_QTNs, e_QTNs, na.rm = T)
  SP <<- AlphaSimR::SimParam$new(founders)
  SP$addTraitA(nQtlPerChr = ceiling(nQTN[1]/nChrom), name = "Trait1", mean = tMean[1], var = 125)



  ## Create base population and set heritablity to get pheno
  pop <- AlphaSimR::newPop(founders, simParam = SP)

  ## Set the phenotype for the base population
  ### Pulling out the genotypic/genomic data. This Does not work unless you have added a trait.
  bp_qtl_map <- AlphaSimR::getQtlMap(trait = 1, simParam = SP)
  bp_qtls <- AlphaSimR::pullQtlGeno(pop, trait = 1, simParam = SP)

  #The function by Alex transforms the snp data into hapmap format needed for simplePHENOTYPES
  bp_qtls_hapmap <- get.me.my.SNPs.in.hapmap.format(these.SNPs = bp_qtls, this.physical.map = bp_qtl_map)

  ## Determine the genetic architecture of the trait
  arch <- ""
  if (a_QTNs != 0 & !identical(c(a_eff, big_a_eff), rep(0,2))) arch <- paste0(arch, "A")
  if (d_QTNs != 0 & d_eff != 0) arch <- paste0(arch, "D")
  if (e_QTNs != 0 & e_eff != 0) arch <- paste0(arch, "E")


  ### Use simplePHENOTYPES to create the Genetic values and phenotype values for the base population.
  pheno.value <- simplePHENOTYPES::create_phenotypes(geno_obj = bp_qtls_hapmap,
                                            ntraits = 1,
                                            h2 = tHet[1],
                                            mean = tMean[1],
                                            rep = 1,
                                            #Additive
                                            add_QTN_num = a_QTNs[1],
                                            big_add_QTN_effect = big_a_eff[1],
                                            add_effect = a_eff[1],
                                            #Dominance
                                            dom_QTN_num = d_QTNs[1],
                                            dom_effect = d_eff[1],
                                            #Epistasis
                                            epi_QTN_num = e_QTNs[1],
                                            epi_effect = e_eff[1],

                                            sim_method = "geometric",
                                            model = arch,
                                            home_dir = tempdir(),
                                            to_r = TRUE,
                                            quiet = T,
                                            seed = 4000)

### After simulating the phenotype up, need to use the same QTNs for simulating the genetic values.
### This is because the same QTNs should be used for both the genetic and phenotypic values.
  if (a_QTNs != 0) {
    a_QTNs_list <- tryCatch(
      {
        read.table(file.path(tempdir(), "Additive_Selected_QTNs.txt"),
                   header = TRUE) |> dplyr::pull(snp)
      },
      error = function(e) {
        message("Additive_Selected_QTNs.txt not found — skipping.")
        NULL
      }
    )
  }

  if (d_QTNs != 0) {
    d_QTNs_list <- tryCatch(
      {
        read.table(file.path(tempdir(), "Dominance_Selected_QTNs.txt"),
                   header = TRUE) |> dplyr::pull(snp)
      },
      error = function(e) {
        message("Dominance_Selected_QTNs.txt not found — skipping.")
        NULL
      }
    )
  }

  if (e_QTNs != 0) {
    e_QTNs_list <- tryCatch(
      {
        read.table(file.path(tempdir(), "Epistatic_Selected_QTNs.txt"),
                   header = TRUE) |> dplyr::pull(snp)
      },
      error = function(e) {
        message("Epistatic_Selected_QTNs.txt not found — skipping.")
        NULL
      }
    )
  }

  QTN_list <- list()
  if(a_QTNs != 0){QTN_list$add[[1]] <- a_QTNs_list}
  if(d_QTNs != 0){QTN_list$dom[[1]] <- d_QTNs_list}
  if(e_QTNs != 0){QTN_list$epi[[1]] <- e_QTNs_list}

  # ## Now genetic values is simulated using that QTN list.
  # gen.value <- simplePHENOTYPES::create_phenotypes(geno_obj = bp_qtls_hapmap,
  #                                                  ntraits = 1,
  #                                                  h2 = 1,
  #                                                  mean = tMean[1],
  #                                                  rep = 1,
  #                                                  QTN_list = QTN_list,
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
  #                                                  quiet = T,
  #                                                  seed = 4000)

  gen.value <- simplePHENOTYPES::create_phenotypes(geno_obj = bp_qtls_hapmap,
                                            ntraits = 1,
                                            h2 = 1,
                                            mean = tMean[1],
                                            rep = 1,
                                            #Additive
                                            add_QTN_num = a_QTNs[1],
                                            big_add_QTN_effect = big_a_eff[1],
                                            add_effect = a_eff[1],
                                            #Dominance
                                            dom_QTN_num = d_QTNs[1],
                                            dom_effect = d_eff[1],
                                            #Epistasis
                                            epi_QTN_num = e_QTNs[1],
                                            epi_effect = e_eff[1],

                                            sim_method = "geometric",
                                            model = arch,
                                            home_dir = tempdir(),
                                            to_r = TRUE,
                                            quiet = T,
                                            seed = 4000)

  ## Import the genetic and phenotypic values back into the population object

  pop@gv <- as.matrix(gen.value[,2])
  pop@pheno <- as.matrix(pheno.value[,2])
  out <- list(arch = arch, pop = pop)

  return(out)
}

# #### A function to create the base population +++++++++++++++++++++++++ ####
# create_base_pop <- function(founders, nChrom = 10, nQTN = NULL, tMean = NULL, tVar = NULL,
#                             tCor = 0, tHet=NULL){
#   SP <<- AlphaSimR::SimParam$new(founders)
#   ##If trait correlation is zero
#   if(tCor == 0){
#     SP$addTraitA(nQtlPerChr = ceiling(nQTN[1]/nChrom), name = "Trait1", mean = tMean[1], var = tVar[1])
#
#     if(nQTN[2] > 0 & tVar[2] > 0){
#       qtl.map <- getQtlMap(trait = 1)
#       SP$restrSegSites(excludeQtl = qtl.map$id)
#       ### Add the 1st trait, oligogenic and non correlated to other traits
#       SP$addTraitA(nQtlPerChr = ceiling(nQTN[2]/nChrom), name = "Trait2", mean = tMean[2], var = tVar[2])
#     }else{
#       tHet = tHet[1]
#     }
#
#     ##If trait correlation is nonzero
#   }else if(tCor != 0){
#     traitcor <- matrix(c(1,tCor,tCor, 1), ncol = 2, byrow = T)
#     SP$addTraitA(nQtlPerChr = ceiling(max(nQTN)/nChrom), name = c("Trait1", "Trait2"),
#                  mean = tMean, var = tVar, corA = traitcor)
#   }
#
#   ## Create base population and set heritablity to get pheno
#   pop <- AlphaSimR::newPop(founders, simParam = SP)
#   pop <- AlphaSimR::setPheno(pop, h2 = tHet, simParam = SP)
#   return(pop)
# }

#### A function to plot PCA biplot of snp data +++++++++++++++++++++++++ #######
plot_pca_biplot <- function(pop){
  geno_mat <- AlphaSimR::pullSegSiteGeno(pop)
  pca_res <- prcomp(geno_mat, center = TRUE, scale. = TRUE)
  pca_var <- (summary(pca_res)$importance)[2,1:2]
  pca_df <- as.data.frame(pca_res$x[,1:5])

  plt <- ggplot2::ggplot(pca_df, ggplot2::aes(x = PC1, y = PC2)) +
    ggplot2::geom_point(alpha = 0.7, color = "darkblue") +
    ggplot2::labs(x = paste0("PC1 ", round(100*pca_var[1],2), "%"),
                  y = paste0("PC2 ", round(100*pca_var[2],2), "%")) +
    boris_theme
  return(plt)
}

# pheatmap(pullQtlGeno(parents), cluster_rows = F, cluster_cols = F,
#          show_rownames = T, show_colnames = T,
#          fontsize_row = 5, fontsize_col = 5)
