#####################################################################################################################
## The function below runs the multi-generation future simulation of a population
### It makes selection and crosses for 20 generations and returns a list of 3: pop.list, a GVs df, and Pheno df
multi.gener.sim <- function(pop = pop, intensity = 20, direction = "Higher values", 
                            trait, crop = "Maize"){
  ####### Embed a simulation --------------- ##
  set.seed(900)
  pop.size = pop@nInd
  col.names <- colnames(pop@gv)
  trait.num = which(col.names == trait)
  direct = ifelse(direction == "Higher values", TRUE, FALSE)
  # h2.list <- list(Maize = c(0.9, 0.7, 0.8), 
  #                 Avocado = c(0.8, 0.8, 0.9), 
  #                 Strawberry = c(0.8, 0.8, 0.7))
  h2.list <- switch(crop,
                    "Maize" = c(0.9, 0.7, 0.8),
                    "Avocado" = c(0.8, 0.8, 0.9),
                    "Strawberry" = c(0.8, 0.8, 0.9),
                    NULL)
  SP.crop <- switch(crop,
                    "Maize" = SP,
                    "Avocado" = SP.avocado,
                    "Strawberry" = SP.strawberry,
                    SP)
  # pop.dt <- data.frame(col.names[1] = NA, col.names[2] = NA, col.names[3] = NA)
  pop.list <- list()
  pop <- randCross(pop, nCrosses = 100, nProgeny = 10, simParam = SP.crop)
  pop <- setPheno(pop, h2 = h2.list, simParam = SP.crop)
  pop.gv.dt <- data.frame(Generation = rep(0, nrow(pop@gv)), as.data.frame(pop@gv))
  pop.pv.dt <- data.frame(Generation = rep(0, nrow(pop@pheno)), as.data.frame(pop@pheno))
  for(generation in 1:20){
    pop = selectCross(pop=pop, use="pheno", nInd = intensity*pop.size/100,
                      nCrosses=100, nProgeny = 10, trait = trait.num, selectTop = direct, 
                      simParam = SP.crop)
    pop = setPheno(pop, h2 = h2.list, simParam = SP.crop)
    
    pop.gv.dt.temp <- data.frame(Generation = rep(generation, nrow(pop@gv)),
                              as.data.frame(pop@gv))
    pop.pv.dt.temp <- data.frame(Generation = rep(generation, nrow(pop@pheno)),
                              as.data.frame(pop@pheno))
    pop.gv.dt <- rbind(pop.gv.dt, pop.gv.dt.temp)
    pop.pv.dt <- rbind(pop.pv.dt, pop.pv.dt.temp)
    pop.list[[generation]] <- pop
  }
  combined.dt <- rbind(mutate(pop.gv.dt, Type = "Genetic"), mutate(pop.pv.dt, Type = "Phenotypic"))
  out.list <- list(popList = pop.list, gv = pop.gv.dt, pheno = pop.pv.dt, comb = combined.dt)
  return(out.list)
} # End of function multi.gener.sim
#####################################################################################################################


#####################################################################################################################
## This function summarizes the simulation results by the computing the quantile
## chosen by the user for each generation.
summary.simulation <- function(data, quartile =50, pivot = F){
  require(dplyr)
  summary.dt <- data %>% group_by(., Generation) %>%
    summarise_at(., names(data[,-1]), ~quantile(., probs = quartile/100)) %>%
    mutate_at(., names(data[,-1]), ~ ifelse(. > 0, ., 2)) %>% 
    as.data.frame()
  # pivot_longer(., 2:ncol(data), names_to = "variate", values_to = "values") %>%
  # mutate(., values = ifelse(values > 0, values, 2))
  if(pivot == T){
    summary.dt <- summary.dt %>%
    tidyr::pivot_longer(., 2:ncol(data), names_to = "variate", values_to = "values") %>% 
    as.data.frame()
    # mutate(., values = ifelse(values > 0, values, 2))
  }
  return(summary.dt)
} # End of function summary.simulation
#####################################################################################################################


#####################################################################################################################
## Function : Draws dot plot to depict the temporal trend of the population quantile over 20 generations. This function is used in Fun panel
### The first argument is the output from the summary.simulation function
draw.genetic.gain.plot <- function(dt, trait, col = "black", annotate.bottom, annotate.top, draw.arrow = F){
  tmp_dt <- filter(dt, variate == trait)
  trait2 <- switch(trait,
                   "StemDiameter" = "Stem diameter (mm)",
                   "TasselLength" = "Tassel length (cm)",
                   "FruitLength" = "Fruit length (cm)",
                   "FruitWidth"  = "Fruit width (cm)",
                   "PlantHeight" = "Plant height (cm)",
                   paste0(trait, " (cm)"))
  
  label.size <- 5
  y.max <- 1.5*max(tmp_dt$values)
  y.min <- 0.5*min(tmp_dt$values)
  p <- ggplot(data = tmp_dt, aes(x = Generation, y = values)) +
    geom_line(linewidth = 1) + geom_point(size = 4, color = col) +
    boris_theme + labs(y = trait2, x = "Generation") +
    annotate(geom = "text", x = 2, y = 1, label = annotate.bottom, size =label.size, color = col, fontface = "bold") +
    annotate(geom = "text", x = 2, y = y.max-2, label = annotate.top, size =label.size, color = col, fontface = "bold")
  
  if(draw.arrow == TRUE){
    p <- p + coord_cartesian(ylim = c(-10, y.max), xlim = c(0,20)) +
      annotate("segment", x = 3, y = -8, xend = 17, yend = -8, linewidth = 2, color = "black", arrow = arrow()) +
      annotate(geom="text", x= 10, y = -5, label = "Time", size = label.size, color = "black", fontface = "bold")
  }else{
    p <- p + coord_cartesian(ylim = c(0, y.max), xlim = c(0,20))
  }
  return(p)
} # End of function draw.genetic.gain.plot
#####################################################################################################################
