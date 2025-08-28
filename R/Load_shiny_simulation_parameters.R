### Set working directory if needed
# setwd("C:/Users/aboris/Box/Postdoc_UIUC/Omnigenic_Project/code/shiny_app2/app_row_column_design")

###### Simulation in AlphaSimR ####################################################
########### Maize #######################################################################
### Create, save, and preload a founder population for maize
# founder.pop <- runMacs(nInd = 100, nChr = 10, segSites = 10,
#                        species = "MAIZE", inbred = T)
# saveRDS(founder.pop, "R/Maize.founder.population.preloaded.rds")
founder.pop.maize <- readRDS("R/Maize.founder.population.preloaded.rds")

### Creates a container for storing simulation parameters for the founder population
SP <<- SimParam$new(founder.pop.maize)

### Adding two negatively correlated traits
traitcor <- matrix(c(1,-0.5,-0.5, 1), ncol = 2, byrow = T)
SP$addTraitA(nQtlPerChr = 5, name = c("PlantHeight", "StemDiameter"),
             mean = c(150, 30), var = c(80, 10), corA = traitcor)
qtl.map <- getQtlMap(trait = 1)
SP$restrSegSites(excludeQtl = qtl.map$id)
### Add the 1st trait, oligogenic and non correlated to other traits
SP$addTraitA(nQtlPerChr = 2, name = "TasselLength",
             mean = 15, var = 5)

### Create the base population
pop.maize <- newPop(founder.pop.maize)
pop.maize.fun <- pop.maize


### Set phenotypes by defining heritabilities hence, error Var(e)
# pop <- setPheno(pop, h2 = c(0.9, 0.7, 0.8)) # Tassel length, height, and StemDiameter
###########################################################################################


####### Avocado #########################################################################
# ## Create, save, and preload a founder population for Avocado
# founder.pop <- runMacs(nInd = 100, nChr = 12, segSites = 10,
#                        species = "GENERIC", inbred = T)
# saveRDS(founder.pop, "R/Avocado.founder.population.preloaded.rds")
founder.pop.avocado <- readRDS("R/Avocado.founder.population.preloaded.rds")

### Creates a container for storing simulation parameters for the founder population
SP.avocado <<- SimParam$new(founder.pop.avocado)

### Add the 1st trait, oligogenic and non correlated to other traits
SP.avocado$addTraitA(nQtlPerChr = 2, name = "FruitWidth",
                     mean = 8, var = 2)
qtl.map.avocado <- getQtlMap(trait = 1, simParam = SP.avocado)
SP.avocado$restrSegSites(excludeQtl = qtl.map.avocado$id)

### Adding two correlated traits
traitcor.avocado <- matrix(c(1,0.3,0.3,1), ncol = 2, byrow = T)
SP.avocado$addTraitA(nQtlPerChr = 2, name = c("FruitLength", "PitSize"),
                     mean = c(12, 3), var = c(3, 2), corA = traitcor.avocado)

### Create the base population
pop.avocado <- newPop(founder.pop.avocado, simParam = SP.avocado)
### Set phenotypes by defining heritabilities hence, error Var(e)
# pop <- setPheno(pop, h2 = c(0.9, 0.7, 0.8)) # Tassel length, height, and StemDiameter
###########################################################################################


####### Strawberry #########################################################################
# ## Create, save, and preload a founder population for Strawberry
# founder.pop <- runMacs(nInd = 100, nChr = 28, segSites = 5,
#                        species = "GENERIC", inbred = F)
# saveRDS(founder.pop, "R/Strawberry.founder.population.preloaded.rds")
founder.pop.strawberry <- readRDS("R/Strawberry.founder.population.preloaded.rds")

### Creates a container for storing simulation parameters for the founder population
SP.strawberry <<- SimParam$new(founder.pop.strawberry)

### Add the 1st trait, oligogenic and non correlated to other traits
traitcor.strawberry <- matrix(c(1,0.5,0.5, 1), ncol = 2, byrow = T)
SP.strawberry$addTraitA(nQtlPerChr = 2, name = c("FruitLength", "FruitWidth"),
                        mean = c(8, 6), var = c(3, 2), corA = traitcor.strawberry)
qtl.map.strawberry <- getQtlMap(trait = 1, simParam = SP.strawberry)
SP.strawberry$restrSegSites(excludeQtl = qtl.map.strawberry$id)

### Adding two negatively correlated traits
SP.strawberry$addTraitA(nQtlPerChr = 1, name = "SeedSize", mean = 3, var = 1.5)

### Create the base population
pop.strawberry <- newPop(founder.pop.strawberry, simParam = SP.strawberry)
### Set phenotypes by defining heritabilities hence, error Var(e)
# pop <- setPheno(pop, h2 = c(0.9, 0.7, 0.8)) # Tassel length, height, and StemDiameter
###########################################################################################
#Free some memory
rm(list = c("qtl.map", "qtl.map.avocado", "qtl.map.strawberry"))
gc()
