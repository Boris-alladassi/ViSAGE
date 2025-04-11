### Set working directory if needed
# setwd("C:/Users/aboris/Box/Postdoc_UIUC/Omnigenic_Project/code/shiny_app2/app_row_column_design")

###### Simulation in AlphaSimR ####################################################
### Create, save, and preload a founder population for maize
# founder.pop <- runMacs(nInd = 100, nChr = 10, segSites = 10,
#                        species = "MAIZE", inbred = T)
# saveRDS(founder.pop, "R/Founder.population.preloaded.rds")

### Create, save, and preload a founder population for Avocado
# founder.pop <- runMacs(nInd = 100, nChr = 24, segSites = 10,
#                        species = "GENERIC", inbred = T)
# saveRDS(founder.pop, "R/Avocado.founder.population.preloaded.rds")

### Create, save, and preload a founder population for Strawberry
# founder.pop <- runMacs(nInd = 100, nChr = 56, segSites = 10,
#                        species = "GENERIC", inbred = T)
# saveRDS(founder.pop, "R/Strawberry.founder.population.preloaded.rds")


founder.pop <- readRDS("R/Founder.population.preloaded.rds")
founder.pop.avocado <- readRDS("R/Avocado.founder.population.preloaded.rds")
founder.pop.strawberry <- readRDS("R/Strawberry.founder.population.preloaded.rds")

########### Maize #######################################################################
### Creates a container for storing simulation parameters for the founder population
SP <<- SimParam$new(founder.pop)


### Adding two negatively correlated traits
traitcor <- matrix(c(1,-0.5,-0.5, 1), ncol = 2, byrow = T)
SP$addTraitA(nQtlPerChr = 5, name = c("Height", "Girth"),
             mean = c(175, 10), var = c(60, 2), corA = traitcor)
qtl.map <- getQtlMap(trait = 1)
SP$restrSegSites(excludeQtl = qtl.map$id)
### Add the 1st trait, oligogenic and non correlated to other traits
SP$addTraitA(nQtlPerChr = 1, name = "TasselLength",
             mean = 12, var = 5)

### Create the base population
pop <- newPop(founder.pop)
popFun <- pop
### Set phenotypes by defining heritabilities hence, error Var(e)
# pop <- setPheno(pop, h2 = c(0.9, 0.7, 0.8)) # Tassel length, height, and girth

####### Avocado #########################################################################
### Creates a container for storing simulation parameters for the founder population
SP.avocado <<- SimParam$new(founder.pop.avocado)

### Add the 1st trait, oligogenic and non correlated to other traits
SP.avocado$addTraitA(nQtlPerChr = 1, name = "Height",
                     mean = 10, var = 3)
qtl.map.avocado <- getQtlMap(trait = 1, simParam = SP.avocado)
SP.avocado$restrSegSites(excludeQtl = qtl.map.avocado$id)

### Adding two negatively correlated traits
traitcor.avocado <- matrix(c(1,0.5,0.5, 1), ncol = 2, byrow = T)
SP.avocado$addTraitA(nQtlPerChr = 2, name = c("Width", "PitSize"),
                     mean = c(8, 5), var = c(2, 2), corA = traitcor.avocado)

### Create the base population
pop.avocado <- newPop(founder.pop.avocado, simParam = SP.avocado)
### Set phenotypes by defining heritabilities hence, error Var(e)
# pop <- setPheno(pop, h2 = c(0.9, 0.7, 0.8)) # Tassel length, height, and girth

####### Strawberry #########################################################################
### Creates a container for storing simulation parameters for the founder population
SP.strawberry <<- SimParam$new(founder.pop.strawberry)

### Add the 1st trait, oligogenic and non correlated to other traits
traitcor.strawberry <- matrix(c(1,0.5,0.5, 1), ncol = 2, byrow = T)
SP.strawberry$addTraitA(nQtlPerChr = 2, name = c("Height", "Width"),
                        mean = c(10, 8), var = c(3, 2), corA = traitcor.strawberry)
qtl.map.strawberry <- getQtlMap(trait = 1, simParam = SP.strawberry)
SP.strawberry$restrSegSites(excludeQtl = qtl.map.strawberry$id)

### Adding two negatively correlated traits
SP.strawberry$addTraitA(nQtlPerChr = 1, name = "SeedSize",
                        mean = 5, var = 1.5)

### Create the base population
pop.strawberry <- newPop(founder.pop.strawberry, simParam = SP.strawberry)
### Set phenotypes by defining heritabilities hence, error Var(e)
# pop <- setPheno(pop, h2 = c(0.9, 0.7, 0.8)) # Tassel length, height, and girth
