sim_data_gwas <- function(mega_list, generation, sel_type){
  # Simulate data for GWAS
  # mega_list: list of lists containing the data for each generation and selection type
  # generation: the generation to simulate data for
  # sel_type: the selection type to simulate data for
  names(mega_list) <- c("Directional_higher", "Directional_lower",
                                   "Disruptive", "Stabilizing", "Random_drift")

  # Extract the relevant data from the mega_list
  generation = generation+1 # Adjust for 1-based indexing in R
  obj_list <- mega_list[[sel_type]][[generation]]

  genomic_data <- AlphaSimR::pullSegSiteGeno(obj_list) |>
    tibble::rownames_to_column(var = "ID")
  pheno_data <- as.data.frame(AlphaSimR::pheno(obj_list))|>
    tibble::rownames_to_column(var = "ID")

  ### Pulling out the genotypic/genomic data. This Does not work unless you have added a trait.
  obj_map <- AlphaSimR::getQtlMap(trait = 1, simParam = SP)
  obj_qtls <- AlphaSimR::pullQtlGeno(obj_list, trait = 1, simParam = SP)

  #The function by Alex transforms the snp data into hapmap format needed for simplePHENOTYPES
  obj_qtls_hapmap <- get.me.my.SNPs.in.hapmap.format(these.SNPs = ngen_qtls, this.physical.map = ngen_map)


  return(gwas_data)
}


#' Format GAPIT results for plotting packages
#'
#' @description
#' Standardizes GAPIT output for use with packages such as **qqman** or **ggman**.
#'
#' @param data A data.frame containing GAPIT results.
#' @param package specify the choice between qqman and ggman
#'
#' @returns A cleaned and standardized data.frame.
#' @noRd
format_gapit_results <- function(data, package) {

  # common_headers <- base::intersect(
  #   c("SNP", "Chromosome", "Chr", "Position", "Pos", "P.value"),
  #   base::colnames(data)
  # )
  #
  # data <- data[, common_headers]
  #
  # base::colnames(data) <- c("SNP", "Chromosome", "Position", "P.value")

  data <- data[,1:4]
  base::colnames(data) <- c("SNP", "Chromosome", "Position", "P.value")

  if (package == "qqman") {

    data <- data |>
      dplyr::rename(CHR = Chromosome, BP = Position, P = P.value) |>
      dplyr::mutate(
        CHR = as.numeric(stringr::str_sub(stringr::word(SNP, 1, sep = "_")), 2)
        )

  } else if (package == "ggman") {

    data <- data |>
      dplyr::rename(
        chrom = Chromosome,
        bp = Position,
        snp = SNP,
        pvalue = P.value) |>
      dplyr::mutate(
        chrom = base::as.numeric(stringr::str_sub(stringr::word(snp, 1, sep = "_"), 2)))
  }

  data <- stats::na.omit(data)

  return(data)
} ### Use function qqman::manhattan() for plotting


### The function below was obtained from:
### https://github.com/cran/multtest/tree/master/R
mt.rawp2adjp<-function(rawp,proc=c("Bonferroni","Holm","Hochberg","SidakSS","SidakSD","BH","BY","ABH",
                                   "TSBH"), alpha=0.05, na.rm=FALSE)
{

  m<-length(rawp)
  if(na.rm){
    mgood<-sum(!is.na(rawp))
  }else{
    mgood<-m
  }
  n<-length(proc)
  a<-length(alpha)
  index<-order(rawp)
  h0.ABH<-NULL
  h0.TSBH<-NULL
  spval<-rawp[index]

  adjp<-matrix(0,m,n+1)
  dimnames(adjp)<-list(NULL,c("rawp",proc))
  adjp[,1]<-spval

  if(is.element("TSBH",proc))
  {
    #N.B.: This method performed first in order to handle a potential $adjp
    #dimension change in the case that length(alpha)>1.
    #Could also be possibly done using more append() functions, should more
    #alpha-dependent procedures be developed/included later.
    TS.spot <- which(proc=="TSBH")
    TSBHs<-paste("TSBH",alpha,sep="_")
    newprocs<-append(proc,TSBHs,after=TS.spot)
    newprocs<-newprocs[newprocs!="TSBH"]
    adjp<-matrix(0,m,n+a)
    dimnames(adjp)<-list(NULL,c("rawp",newprocs))
    adjp[,1]<-spval

    # Apply first-pass BH.
    tmp<-spval
    for(i in (m-1):1){
      tmp[i]<-min(tmp[i+1],min((mgood/i)*spval[i],1,na.rm=TRUE),na.rm=TRUE)
      if(is.na(spval[i])) tmp[i]<-NA
    }
    # Now use first-pass results to estimate h_0, the number of true nulls.
    # These results depend on the nominal testing level, alpha.
    h0.TSBH <- rep(0,length(alpha))
    names(h0.TSBH) <- paste("h0.TSBH",alpha,sep="_")
    for(i in 1:length(alpha)){
      h0.TSBH[i] <- mgood - sum(tmp < alpha[i]/(1+alpha[i]),na.rm=TRUE)
      adjp[,TS.spot+i]<-tmp*h0.TSBH[i]/mgood
    }
  }

  if(is.element("Bonferroni",proc))
  {
    tmp<-mgood*spval
    tmp[tmp>1]<-1
    adjp[,"Bonferroni"]<-tmp
  }

  if(is.element("Holm",proc))
  {
    tmp<-spval
    tmp[1]<-min(mgood*spval[1],1)
    for(i in 2:m)
      tmp[i]<-max(tmp[i-1],min((mgood-i+1)*spval[i],1))
    adjp[,"Holm"]<-tmp
  }

  if(is.element("Hochberg",proc))
  {
    tmp<-spval
    for(i in (m-1):1){
      tmp[i]<-min(tmp[i+1],min((mgood-i+1)*spval[i],1,na.rm=TRUE),na.rm=TRUE)
      if(is.na(spval[i])) tmp[i]<-NA
    }
    adjp[,"Hochberg"]<-tmp
  }

  if(is.element("SidakSS",proc))
    adjp[,"SidakSS"]<-1-(1-spval)^mgood

  if(is.element("SidakSD",proc))
  {
    tmp<-spval
    tmp[1]<-1-(1-spval[1])^mgood
    for(i in 2:m)
      tmp[i]<-max(tmp[i-1],1-(1-spval[i])^(mgood-i+1))
    adjp[,"SidakSD"]<-tmp
  }

  if(is.element("BH",proc))
  {
    tmp<-spval
    for(i in (m-1):1){
      tmp[i]<-min(tmp[i+1],min((mgood/i)*spval[i],1,na.rm=TRUE),na.rm=TRUE)
      if(is.na(spval[i])) tmp[i]<-NA
    }
    adjp[,"BH"]<-tmp
  }

  if(is.element("BY",proc))
  {
    tmp<-spval
    a<-sum(1/(1:mgood))
    tmp[m]<-min(a*spval[m], 1)
    for(i in (m-1):1){
      tmp[i]<-min(tmp[i+1],min((mgood*a/i)*spval[i],1,na.rm=TRUE),na.rm=TRUE)
      if(is.na(spval[i])) tmp[i]<-NA
    }
    adjp[,"BY"]<-tmp
  }

  if(is.element("ABH",proc))
  {
    ## First obtain estimate of h_0, the number of true null hypotheses.
    tmp<-spval
    h0.m <- rep(0,mgood)
    for(k in 1:mgood){
      h0.m[k] <- (mgood+1-k)/(1-spval[k])
    }
    grab <- min(which(diff(h0.m,na.rm=TRUE)>0),na.rm=TRUE)
    h0.ABH <- ceiling(min(h0.m[grab],mgood))
    ## Now apply BH procedure with adaptive correction.
    for(i in (m-1):1){
      tmp[i]<-min(tmp[i+1],min((mgood/i)*spval[i],1,na.rm=TRUE),na.rm=TRUE)
      if(is.na(spval[i])) tmp[i]<-NA
    }
    adjp[,"ABH"]<-tmp*h0.ABH/mgood
  }

  list(adjp=adjp,index=index,h0.ABH=h0.ABH[1],h0.TSBH=h0.TSBH[1:length(alpha)])
}

###########################################################################
### A function for selection of GLM or MLM in classical GAPIT code
model_selection <- function(data, model = "GLM"){
  sample_size = nrow(data)
  grp_from = 1
  grp_to = 1
  grp_by = 1

  if(model == "MLM"){
    grp_from = sample_size
    grp_to = sample_size
    grp_by = 1
  }
  out <- list(grp_from = grp_from, grp_to = grp_to, grp_by = grp_by)
  return(out)
}
