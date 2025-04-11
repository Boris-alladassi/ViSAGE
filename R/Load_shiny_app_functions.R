## The function below runs the multi-generation future simulation of a population
### It makes selection and crosses for 20 generations and returns a list of 3: pop.list, a GVs df, and Pheno df
multi.gener.sim <- function(pop = pop, intensity = 20, direction = "Higher values", 
                            trait, crop = "Corn"){
  ####### Embed a simulation --------------- ##
  set.seed(900)
  pop.size = pop@nInd
  col.names <- colnames(pop@gv)
  trait.num = which(col.names == trait)
  direct = ifelse(direction == "Higher values", TRUE, FALSE)
  # h2.list <- list(Corn = c(0.9, 0.7, 0.8), 
  #                 Avocado = c(0.8, 0.8, 0.9), 
  #                 Strawberry = c(0.8, 0.8, 0.7))
  h2.list <- switch(crop,
                    "Corn" = c(0.9, 0.7, 0.8),
                    "Avocado" = c(0.8, 0.8, 0.9),
                    "Strawberry" = c(0.8, 0.8, 0.9),
                    NULL)
  SP.crop <- switch(crop,
                    "Corn" = SP,
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

# Testing the simulation for avocado #####################################################
# pop = pop.avocado
# SP.crop = SimParam$new(founder.pop.avocado)
# trait = "Height"
# trait.num = 1
# direction = "Higer values"
# intensity = 20
# pop.size = pop@nInd
# pop <- setPheno(pop, h2 = h2.list, simParam = SP.crop)
# pop = selectCross(pop=pop, use="pheno", nInd = intensity*pop.size/100,
#                   nCrosses=100, nProgeny = 10, trait = trait.num, selectTop = direct,
#                   simParam = SP.crop)
# sim_res <- multi.gener.sim(pop = pop.avocado, trait = "Height", crop = "Avocado")
# saveRDS(sim_res, "../Test_files/sim_res_avocado.rds")
##########################################################################################

## This function summarizes the simulation results by the computing the quantile
## chosen by the user for each generation.
summary.simulation <- function(data, quartile =50, pivot = F){
  require(tidyverse)
  summary.dt <- data %>% group_by(., Generation) %>%
    summarise_at(., names(data[,-1]), ~quantile(., probs = quartile/100)) %>%
    mutate_at(., names(data[,-1]), ~ ifelse(. > 0, ., 2)) %>% 
    as.data.frame()
  # pivot_longer(., 2:ncol(data), names_to = "variate", values_to = "values") %>%
  # mutate(., values = ifelse(values > 0, values, 2))
  if(pivot == T){
    summary.dt <- summary.dt %>%
    pivot_longer(., 2:ncol(data), names_to = "variate", values_to = "values") %>% 
    as.data.frame()
    # mutate(., values = ifelse(values > 0, values, 2))
  }
  return(summary.dt)
} # End of function summary.simulation

## Function : Draws dot plot to depict the temporal trend of the population quartile over 20 generations
### The first argument is the output from the summarry.simulation function
draw.genetic.gain.plot <- function(dt, trait, col = "black", annotate.bottom, annotate.top, draw.arrow = F){
  tmp_dt <- filter(dt, variate == trait)
  label.size <- 5
  y.max <- 1.5*max(tmp_dt$values)
  y.min <- 0.5*min(tmp_dt$values)
  p <- ggplot(data = tmp_dt, aes(x = Generation, y = values)) +
    geom_line(linewidth = 1) + geom_point(size = 4, color = col) +
    boris_theme + labs(y = trait, x = "Generation") + theme(legend.position = "none") +
    annotate(geom = "text", x = 2, y = 1, label = annotate.bottom, size =label.size, color = col, fontface = "bold") +
    annotate(geom = "text", x = 2, y = y.max-2, label = annotate.top, size =label.size, color = col, fontface = "bold")
  
  if(draw.arrow == TRUE){
    p <- p + coord_cartesian(ylim = c(-10, y.max), xlim = c(0,20)) +
      geom_segment(aes(x = 3, y = -8, xend = 17, yend = -8), linewidth = 2, color = "black", arrow = arrow()) +
      annotate(geom="text", x= 10, y = -5, label = "Time", size = label.size, color = "black", fontface = "bold")
  }else{
    p <- p + coord_cartesian(ylim = c(0, y.max), xlim = c(0,20))
  }
  return(p)
} # End of function draw.genetic.gain.plot

## Creating two functions for generating a GIf for correlation heatmap across generations
### The first function computes the correlation matrix for each generation
compute.cor.dt <- function(dt){
  final_dt <- data.frame(Var1 = NA, Var2 = NA, value = NA, Generation = NA)
  for (g in unique(dt[["Generation"]])) {
    ## Filter only 1 generation
    temp.dt <- filter(dt, Generation == g)
    ## compute the correlation matrix and melt it.
    cor.dt <- reshape2::melt(as.matrix(cor(temp.dt[,-1]))) %>%
      as.data.frame() %>% mutate(., Generation = g)
    final_dt <- rbind(final_dt, cor.dt)
  } # End of for loop

  final_dt2 <- final_dt[-1,]
  return(final_dt2)
} # End of function compute.cor.dt

### The second function generates the animation GIF file
generate.animation <- function(dt, filename = "www/animationCorr.gif"){
  require(ggplot2)
  if (!dir.exists("www")) dir.create("www")
  ## Create the heatmap using geom_tile
  anim <- ggplot(data = dt, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() + boris_theme + labs(x = "", y = "", title = "Generation {current_frame}")+
    theme(axis.ticks = element_blank(), panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
    scale_fill_gradient2(low = "navy", mid = "white", high = "darkred", midpoint = 0, limits = c(-1, 1))+
    transition_manual(Generation)
  # transition_states(Generation, transition_length = 5, state_length = 3)
  # return(anim)# + enter_fade() + exit_fade())
  animate(anim, renderer = gifski_renderer(filename))
  return(filename)
} # End of function generate.animation


# ## Function to plot the scatterplot for the genetic and phenotypic correlation
# geno.pheno.cor.plot <- function(dt, col.names, filename = "www/animationCorr2.gif"){
#   require(ggplot2)
#   require(ggpubr)
#   if (!dir.exists("www")) dir.create("www")
#   ## Create the heatmap using geom_tile
#   ggarrange(ggplot(data = dt, aes_string(x = col.names[1], y = col.names[2], color = Type)) +
#     geom_point(size = 1, shape = 15) + geom_line(linewidth = 1)+
#     boris_theme + labs(x = "", y = "", title = "Generation {current_frame}")+
#     theme(axis.ticks = element_blank(), panel.border = element_blank(),
#           plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
#     transition_manual(Generation)
#   ggplot(data = dt, aes_string(x = col.names[1], y = col.names[2], color = Type)) +
#     geom_point(size = 1, shape = 16) + geom_line(linewidth = 1)+
#     boris_theme + labs(x = "", y = "", title = "Generation {current_frame}")+
#     theme(axis.ticks = element_blank(), panel.border = element_blank(),
#           plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
#     transition_manual(Generation)
#   anim1 <- ggplot(data = dt, aes_string(x = col.names[1], y = col.names[2], color = Type)) +
#     geom_point(size = 1, shape = 17) + geom_line(linewidth = 1)+
#     boris_theme + labs(x = "", y = "", title = "Generation {current_frame}")+
#     theme(axis.ticks = element_blank(), panel.border = element_blank(),
#           plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
#     transition_manual(Generation)
#   animate(anim, renderer = gifski_renderer(filename)), nco = 3, nrow = 1)
#   return(filename)
# }
# 
# 
# 
# 
# (pg = ggplot(both_long, aes(x = UAV, y = Manual)) + geom_point(size = 3, aes(color = Stage)) +
#     geom_abline(intercept = 0, slope = 1, linewidth =1, color = "black", linetype = "dashed") + 
#     stat_cor(size = 8, cor.coef.name = "r", 
#              aes(color = factor(Stage, levels = level),
#                  label = paste(after_stat(r.label), cut(..p.., breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
#                                                         labels = c("'****'", "'***'", "'**'", "'*'", "'ns'")),sep = "~")), 
#              show.legend = F,label.y = c(maxi, maxi-20, maxi-40),label.x = mini +20) +
#     scale_y_continuous(limits = c(20,400), breaks = seq(100,400,100)) + 
#     scale_x_continuous(limits = c(20,400), breaks = seq(100,400,100)) +boris_theme +
#     labs(y = "Manual plant height (cm)", x = "UAV plant height (cm)") + 
#     scale_color_manual(values = col_18) + 
#     theme(axis.title = element_text(size = 20)))

