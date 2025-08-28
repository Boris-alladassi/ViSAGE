# First, let us load the necessary packages, functions, and simulation parameters
source("R/Load_shiny_app_packages.R")
source("R/Load_shiny_app_functions.R")
source("R/Load_shiny_simulation_parameters.R")

#### Set some parameters
canvas.width.gbl = 1650
canvas.height.gbl =700
Illini.blue <- "#13294B"
Illini.orange <- "#FF5F05"
Royal.blue <- "#001489"

boris_theme <<- theme_bw() + theme(axis.text = element_text(color = "black", size = 16),
                                   axis.title = element_text(colour = "black", face = "bold", size = 16),
                                   legend.text = element_text(colour = "black", size = 16),
                                   legend.position = "none",
                                   legend.title = element_blank(),
                                   panel.grid = element_blank(),
                                   panel.background = element_rect(fill = "White", colour = NA),
                                   panel.border = element_rect(colour = "black"),
                                   text = element_text(colour = "black", size = 16))

########################################################################################################################
######################------- User Interface of the app ---------- #####################################################
########################################################################################################################
ui <- fluidPage(
  theme = bs_theme(bootswatch = "united"),
  
  tags$head(
    tags$style(HTML("
      body, html {
        margin: 0;
        padding: 0;
        overflow: auto;
      }

      .vh-95  { height: 95vh; overflow-y: auto;}
      .vh-40  { height: 40vh; overflow-y: hidden;}
      .vh-35  { height: 35vh; overflow-y: hidden;}
      .vh-25  { height: 25vh; overflow-y: hidden;}
      .vh-20  { height: 19vh; overflow-y: hidden;}
      .vh-10  { height: 10vh; overflow-y: hidden;}

      .left-scrollable {
        height: 95vh;
        overflow: auto;
      }

      .right-scrollable {
        height: 95vh;
        overflow: auto;
      }
    "))
  ),
  
  tabsetPanel(
    #### Introduction panel ###################
    tabPanel("Introduction", includeMarkdown("Introduction.md")
             ),# End of Introduction panel
    
    ######### Standard panel ####################======================= 
    tabPanel("Standard", 
             fluidRow(
               class = "vh-95",
               
               # LEFT COLUMN: Fixed layout
               column(
                 width = 2,
                 div(class = "left-scrollable",
                     card(class = "height: 35vh; overflow-y: hidden;", card_header("Selection decisions"),
                          selectInput("choosetrait", label = "Select a trait", choices = unique(SP$traitNames)),
                          selectInput(inputId = "direction", label = "How do you want to select?",
                                      choices = c("Higher values", "Lower values")),
                          sliderInput(inputId = "intensity", "What percentage would you like to select?",
                                      min = 0, max = 100, step = 5, value = 10),
                          actionButton(inputId = "simulate", "Simulate", class = "btn btn-success")),
                     card(class = "vh-25", card_header("Recycle a generation for new simulations"),
                          sliderInput(inputId = "genNumber", "Select the generation",
                                      min = 1, max = 20, step = 1, value = 4),
                          actionButton(inputId = "recycle", "Recycle"),
                          progressBar(id = "pb", value = 0, display_pct = T, title = "Ready to recycle")),
                     card(class = "vh-20", card_header("Genetic gain metric"),
                          sliderInput(inputId = "quantile", "Choose a quartile for comparing generations",
                                      min = 0, max = 100, step = 25, value = 50),
                          actionButton(inputId = "metric", "Update metric")),
                     card(class = "vh-10", card_header("Reset the app"), 
                          actionButton(inputId = "reset", "Reset", class = "btn btn-warning"))
                 )
               ),
               
               # RIGHT COLUMN: Scrollable with nested grid
               column(
                 width = 10,
                 div(class = "right-scrollable",
                     
                     fluidRow(
                       column(12,
                              card(class = "height: 40vh; overflow-y: hidden;", card_header("Genetic gain plot"),
                                   plotOutput(outputId = "dotplot"))
                       )
                     ),
                     
                     fluidRow(
                       column(8,
                              card(class = "height: 40vh; overflow-y: hidden;", card_header("Phenotypic changes"),
                                   useShinyjs(),
                                   tags$head(tags$script(src = "maize.js")),
                                   tags$canvas(id = "myCanvas", width = canvas.width.gbl,
                                               height = canvas.height.gbl, style = "border:1px solid black;"))
                       ),
                       column(4,
                              card(class = "height: 40vh; overflow-y: hidden;", card_header("Phenotypic distribution"),
                                   selectInput(inputId = "selectGen", label = "Select a generation", choices = c(1:20)),
                                   selectInput(inputId = "selectTrait", label = "Select a trait", choices = unique(SP$traitNames)),
                                   actionButton(inputId = "plotHist", "Plot histogram"),
                                   plotOutput(outputId = "histplot"))
                       )
                     ),
                     
                     fluidRow( #Download buttons
                       column(3,
                              card(class = "vh-10", card_header("Download simulated phenotypes"),
                                   downloadButton("downloadData", "Download CSV"))
                       ),
                       column(3,
                              card(class = "vh-10", card_header("Download genetic gain plot"),
                                   downloadButton("downloadGainPlot", "Download JPEG"))
                       ),
                       column(3,
                              card(class = "vh-10", card_header("Download phenotypic changes plot"),
                                   downloadButton("downloadPhenoPlot", "Download JPEG"))
                       ),
                       column(3,
                              card(class = "vh-10", card_header("Download phenotypic distribution plot"),
                                   downloadButton("downloadPhenoDistPlot", "Download JPEG"))
                       )
                     ) # End of fluidrow download buttons
                     
                 )
               )
             )),
    ######################-------------------  End of STANDARD Panel--------------------################################
    
    
    ############################################------- Beginning of Fun Panel ------------##########################################
    tabPanel("Fun", 
             fluidRow(
               class = "vh-95",
               
               ### LEFT COLUMN: Fixed layout
               column(
                 width = 2,
                 div(class = "left-scrollable",
                     card(class = "height: 37vh; overflow-y: hidden;", card_header("Selection decisions"),
                          selectInput("crop", label = "Select a crop", choices = c("Maize" , "Avocado" , "Strawberry")),
                          uiOutput("choosetrait2"),
                          selectInput(inputId = "direction2", label = "How do you want to select?",
                                      choices = c("Higher values", "Lower values")),
                          sliderInput(inputId = "intensity2", "What percentage would you like to select?",
                                      min = 0, max = 100, step = 5, value = 10),
                          actionButton(inputId = "simulate2", "Simulate", class = "btn btn-success")),
                     card(class = "height: 24vh;", card_header("Recycle a generation for new simulation"),
                          sliderInput(inputId = "genNumber2", "Select the generation",
                                      min = 1, max = 20, step = 1, value = 4),
                          actionButton(inputId = "recycle2", "Recycle"),
                          progressBar(id = "pb2", value = 0, display_pct = T, title = "Ready to recycle")),
                     card(class = "height: 19vh;", card_header("Genetic gain metric"),
                          sliderInput(inputId = "quantile2", "Choose a quartile for comparing generations",
                                      min = 0, max = 100, step = 25, value = 50),
                          actionButton(inputId = "metric2", "Update metric")),
                     card(class = "vh-10", card_header("Reset the app"),
                          actionButton(inputId = "reset2", "Reset", class = "btn btn-warning"))
                 )
               ),
               
               # RIGHT COLUMN: Scrollable with nested grid
               column(
                 width = 10,
                 div(class = "right-scrollable",
                     
                     fluidRow(
                       column(12,
                              card(class = "height: 40vh; overflow-y: hidden;", card_header("Genetic gain plot"),
                                   plotOutput(outputId = "dotplot2"))
                       )
                     ),
                     
                     fluidRow(
                       column(8,
                              card(class = "height: 40vh; overflow-y: hidden;", card_header("Phenotypic changes"),
                                   useShinyjs(),
                                   tags$head(tags$script(src = "fun.js")),
                                   tags$canvas(id = "myCanvas2", width = canvas.width.gbl,
                                               height = canvas.height.gbl, style = "border:1px solid black;"))
                       ),
                       column(4,
                              card(class = "height: 40vh; overflow-y: hidden;", card_header("Phenotypic distribution"),
                                   selectInput(inputId = "selectGen2", label = "Select a generation", choices = c(1:20)),
                                   uiOutput("selectTrait2"),
                                   actionButton(inputId = "plotHist2", "Plot histogram"),
                                   plotOutput(outputId = "histplot2"))
                       )
                     ),
                     
                     fluidRow( #Download buttons
                       column(3,
                              card(class = "vh-10", card_header("Download simulated phenotypes"),
                                   downloadButton("downloadData2", "Download CSV"))
                       ),
                       column(3,
                              card(class = "vh-10", card_header("Download genetic gain plot"),
                                   downloadButton("downloadGainPlot2", "Download JPEG"))
                       ),
                       column(3,
                              card(class = "vh-10", card_header("Download phenotypic changes plot"),
                                   downloadButton("downloadPhenoPlot2", "Download JPEG"))
                       ),
                       column(3,
                              card(class = "vh-10", card_header("Download phenotypic distribution plot"),
                                   downloadButton("downloadPhenoDistPlot2", "Download JPEG"))
                       )
                     ) # End of fluidrow download buttons FUN
                     
                 )
               )# End of Right column of Fun
               
             ))# End of FUN panel
  )#End of TabSetPanel
  
)#End of the overall fluidPage.

########################################################################################################################
######################------- Server function of the app ---------- ####################################################
########################################################################################################################
server <- function(input, output, session) {
  ## Run the simulation function based on user-defined parameters and the click of the simulate button
  simulation.results <- eventReactive(input$simulate, {
    withProgress(message = "Simulation in progress ...", value = 0, {
      multi.gener.sim(pop = pop.maize, trait = input$choosetrait, direction = input$direction, 
                      intensity = input$intensity, crop = "Maize")
    })
  })## Output a list of 4 elements, including GVs and phenotype
  
  ##### Download simulation results
  output$downloadStandard <- downloadHandler(
    filename = function(){
      paste0("simulation_data_standard_maize_", Sys.Date(), ".csv")
    },
    content = function(file){
      req(simulation.results())
      write.csv(simulation.results()[["pheno"]], file, row.names = FALSE)
    }
  ) # For the Standard panel
  
  
  ## List of choices for crops. For the Fun panel, user first selects the crop of choice
  output$choosetrait2 <- renderUI({
    req(input$crop)  # Ensure input$crop is available before execution
    
    trait_choices <- switch(input$crop,
                            "Maize" = unique(SP$traitNames),
                            "Avocado" = unique(SP.avocado$traitNames),
                            "Strawberry" = unique(SP.strawberry$traitNames),
                            character(0))  # Default empty vector if no match
    
    selectInput("choosetrait2", label = "Select a trait", choices = trait_choices)
  })
  
  
  
  ## Now the Fun panel can run the simulation function for the selected crop
  simulation.results2 <- eventReactive(input$simulate2, {
    req(input$crop, input$choosetrait2, input$direction2, input$intensity2)  # Ensure inputs exist
    withProgress(message = "Simulation in progress ...", value = 0, {
      pop_data <- switch(input$crop,
                         "Maize" = pop.maize.fun,
                         "Avocado" = pop.avocado,
                         "Strawberry" = pop.strawberry,
                         NULL)
      
      req(pop_data)  # Ensure population data exists
      multi.gener.sim(pop = pop_data, trait = input$choosetrait2,  # Use choosetrait2 instead of dynamic_input
                      direction = input$direction2, intensity = input$intensity2, crop = input$crop)
    })
  })## Output a list of 4 elements, including GVs and phenotype
  
  observeEvent(input$simulate, {
    updateProgressBar(session, id = "pb", value = 0, title = "Ready to recycle")
  })
  
  observeEvent(input$simulate2, {
    updateProgressBar(session, id = "pb2", value = 0, title = "Ready to recycle")
  }) # For the fun panel
  
  ###################################################################################################
  ## Plot the phenotypic distribution as a histogram
  # For the standard panel--------------------------------------------------------------------
  histogram <- eventReactive(input$plotHist, {
    req(input$selectGen)  # Ensure input$selectGen is available before execution
    req(input$selectTrait)  # Ensure input$selectTrait is available before execution
    req(simulation.results())  # Ensure simulation.results() is available before execution
    
    ggplot(data = filter(simulation.results()[["pheno"]], Generation == input$selectGen), aes(x = .data[[input$selectTrait]])) +
      geom_histogram(color = "white", fill = "steelblue", bins = 10) +
      labs(x = paste0(input$selectTrait, " (cm)"), y = "Count",
           title = paste0("Histogram for ", input$selectTrait, " at generation ", input$selectGen)) +
      boris_theme + theme(panel.grid = element_line())
  }) # For the Standard panel
  
  output$histplot <- renderPlot({
    req(histogram())  # Ensure input$selectGen is available before execution
    print(histogram())
  }) # For the Standard panel
  
  # For the Fun panel -----------------------------------------------------------------------
  ## List of choices for traits for the histogram
  output$selectTrait2 <- renderUI({
    req(input$crop)  # Ensure input$crop is available before execution
    req(simulation.results2())  # Ensure simulation.results2() is available before execution
    
    trait_choices <- switch(input$crop,
                            "Maize" = unique(SP$traitNames),
                            "Avocado" = unique(SP.avocado$traitNames),
                            "Strawberry" = unique(SP.strawberry$traitNames),
                            character(0))  # Default empty vector if no match
    
    selectInput("selectTrait3", label = "Select a trait", choices = trait_choices)
  })
  histogram2 <- eventReactive(input$plotHist2, {
    req(input$selectGen2)
    req(input$selectTrait3)
    req(simulation.results2())  
    
    ggplot(data = filter(simulation.results2()[["pheno"]], Generation == input$selectGen2), aes(x = .data[[input$selectTrait3]])) +
      geom_histogram(color = "white", fill = "steelblue", bins = 10) +
      labs(x = paste0(input$selectTrait3, " (cm)"), y = "Count",
           title = paste0("Histogram for ", input$selectTrait3, " at generation ", input$selectGen2)) +
      boris_theme + theme(panel.grid = element_line())
  }) # For the Fun panel
  
  output$histplot2 <- renderPlot({
    req(histogram2()) 
    print(histogram2())
  }) # For the Fun panel
  
  ####################################################################################################
  # Create a summarized table using user-defined percentile
  summary.sim.results <<- eventReactive(c(simulation.results(), input$metric), {
    summary.simulation(data = simulation.results()[["pheno"]], 
                       quartile = input$quantile, pivot = T)
  })
  summary.sim.results2 <- eventReactive(c(simulation.results2(), input$metric2),{
    summary.simulation(data = simulation.results2()[["pheno"]], 
                       quartile = input$quantile2, pivot = T)
  })
  
  # #################################################################################################################
  ## ###### (Top panel: Create a multi-generation dot and line plots showing genetic progress #####################
  dotplot_temp <- eventReactive(c(simulation.results(), input$metric), {
    ggpubr::ggarrange(
      ggplot(data = filter(summary.sim.results(), variate == "PlantHeight"), aes(x = Generation, y = values)) +
        geom_line(linewidth = 1, color = "#001489") + geom_point(size =4, color = "#001489") +
        coord_cartesian(ylim = c(-10, 300), xlim = c(0,20)) +
        boris_theme + labs(y = "Plant height (cm)", x = "Generation")+
        annotate(geom ="segment",x = 3, y = -8, xend = 17, yend = -8, linewidth = 2, color = "black", arrow = arrow())+
        annotate(geom = "text", x= 10, y = 5, label = "Time", size = 8, color = "black", fontface = "bold")+
        annotate(geom = "text", x = 2, y = 15, label = "Short", size =8, color = "#001489", fontface = "bold")+
        annotate(geom = "text", x = 2, y = 295, label = "Tall", size =9, color = "#001489", fontface = "bold"),
      
      ggplot(data = filter(summary.sim.results(), variate == "StemDiameter"), aes(x = Generation, y = values)) +
        geom_line(linewidth = 1, color = "#FF5F05") + geom_point(size =4, color = "#FF5F05") +
        coord_cartesian(ylim = c(0, 60), xlim = c(0,20)) +
        boris_theme + labs(y = "Stem diameter (mm)", x = "Generation") +
        annotate(geom = "text", x = 2, y = 1, label = "Thin", size =8, color = "#FF5F05", fontface = "bold")+
        annotate(geom = "text", x = 2, y = 58, label = "Thick", size =8, color = "#FF5F05", fontface = "bold"),
      
      ggplot(data = filter(summary.sim.results(), variate == "TasselLength"), aes(x = Generation, y = values)) +
        geom_line(linewidth = 1) + geom_point(size = 4) +
        coord_cartesian(ylim = c(0, 30), xlim = c(0,20)) +
        boris_theme + labs(y = "Tassel length (cm)", x = "Generation")+
        annotate(geom = "text", x = 2, y = 1, label = "Short", size =8, color = "black", fontface = "bold") +
        annotate(geom = "text", x = 2, y = 29, label = "Long", size =9, color = "black", fontface = "bold")
      
      ,ncol = 3, nrow = 1)
  })
  
  output$dotplot <- renderPlot({
    req(dotplot_temp())  
    print(dotplot_temp())
  }) # For the Standard tab
  
  dotplot_temp2 <- eventReactive(c(summary.sim.results2(), input$metric2),{
    trait_choices <- switch(input$crop,
                            "Avocado" = unique(SP.avocado$traitNames),
                            "Maize" = unique(SP$traitNames),
                            "Strawberry" = unique(SP.strawberry$traitNames),
                            character(0))
    annot <- switch(input$crop,
                    "Avocado" = c("Narrow", "Wide", "Short", "Tall", "Small", "Big"),
                    "Maize" = c("Short", "Tall", "Thin", "Thick", "Short", "Long"),
                    "Strawberry" = c("Short", "Tall", "Thin", "Thick", "Small", "Big"),
                    character(0))
    ggpubr::ggarrange(
      draw.genetic.gain.plot(dt = summary.sim.results2(), trait = trait_choices[1], col = "#001489",
                             annotate.bottom = annot[1], annotate.top = annot[2], draw.arrow = T),
      draw.genetic.gain.plot(summary.sim.results2(), trait = trait_choices[2], col = "black",
                             annotate.bottom = annot[3], annotate.top = annot[4], draw.arrow = F),
      draw.genetic.gain.plot(summary.sim.results2(), trait = trait_choices[3], col = "#FF5F05",
                             annotate.bottom = annot[5], annotate.top = annot[6], draw.arrow = F)
      
      , ncol = 3, nrow = 1)
  })
  output$dotplot2 <- renderPlot({
    req(dotplot_temp2())
    print(dotplot_temp2())
  }) # For the fun tab
  
  
  ############### Integration of JavaScript code ###########################################################
  ### 1. Prepare and format simulation data for JavaScript
  plot_values <- eventReactive(c(simulation.results(), input$metric),{
    dt <- summary.simulation(data = simulation.results()[["pheno"]], 
                             quartile = input$quantile, pivot = F)
    dt[seq(1,21,5),]
  })
  
  
  plot_values2 <- eventReactive(c(simulation.results2(), input$metric2), {
    dt2 <- summary.simulation(data = simulation.results2()[["pheno"]], 
                              quartile = input$quantile2, pivot = F)
    dt2[seq(1,21,5),]
  })#For fun Panel
  
  ### 2. Input the data into the javascript file
  observeEvent(plot_values(), {
    js_code <- sprintf("
      const canvas = document.getElementById('myCanvas');
      const ctx = canvas.getContext('2d');
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      drawBackground(ctx, canvas.width, canvas.height);
      drawMultipleMaizePlants(ctx, 50 + canvas.width/5, canvas.height, [%s], [%s], [%s])
      ", paste(2.5*plot_values()[[2]], collapse = ","), paste(3*plot_values()[[4]], collapse = ","),
                       paste(.4*plot_values()[[3]], collapse = ","))
    # drawMultipleMaizePlants(ctx, x, y, heightArray, tasselLengthArray, StemDiameterArray)
    runjs(js_code)
  }) # For the Standard panel
  
  
  ### Use if else statement to draw the three alternative crops from the fun.js JavaScript file
  observeEvent(plot_values2(), {
    if(input$crop == "Maize"){
      js_code2 <- sprintf("
      const canvas2 = document.getElementById('myCanvas2');
      const ctx2 = canvas2.getContext('2d');
      ctx2.clearRect(0, 0, canvas2.width, canvas2.height);
      drawBackground(ctx2, canvas2.width, canvas2.height);
      drawThreshold(ctx=ctx2, xthresh=canvas2.width, ythresh=canvas2.height, margin = 100);
      drawMultipleFunCorn(ctx2, 50 + canvas2.width/5, canvas2.height, [%s], [%s], [%s])
      ", paste(2*plot_values2()[[2]], collapse = ","), paste(1.5*plot_values2()[[4]], collapse = ","),
                          paste(0.4*plot_values2()[[3]], collapse = ","))
      # drawMultipleFunCorn(ctx, x, y, heightArray, tasselLengthArray, StemDiameterArray)
    }else if (input$crop == "Avocado") {
      js_code2 <- sprintf("
      const canvas2 = document.getElementById('myCanvas2');
      const ctx2 = canvas2.getContext('2d');
      ctx2.clearRect(0, 0, canvas2.width, canvas2.height);
      drawBackground(ctx2, canvas2.width, canvas2.height);
      drawSun(ctx = ctx2, xsun = canvas2.width, ysun =canvas2.height);
      drawMultipleAvocados(ctx2, 50 + canvas2.width/5, canvas2.height, [%s], [%s], [%s])
      ", paste(18*plot_values2()[[3]], collapse = ","), paste(10*plot_values2()[[2]], collapse = ","),
                          paste(5*plot_values2()[[4]], collapse = ","))
      # drawMultipleAvocados(ctx, x, y, heightArray, widthArray, pitArray)
      
    } else if(input$crop == "Strawberry"){
      js_code2 <- sprintf("
      const canvas2 = document.getElementById('myCanvas2');
      const ctx2 = canvas2.getContext('2d');
      ctx2.clearRect(0, 0, canvas2.width, canvas2.height);
      drawBackground(ctx2, canvas2.width, canvas2.height);
      drawMultipleStrawberries(ctx2, 50 + canvas2.width/5, canvas2.height, [%s], [%s], [%s])
      ", paste(25*plot_values2()[[2]], collapse = ","), paste(12*plot_values2()[[3]], collapse = ","),
                          paste(2*plot_values2()[[4]], collapse = ","))
      # drawMultipleStrawberries(ctx, x, y, heightArray, widthArray, seedSizeArray)
    }
    runjs(js_code2)
  }) # For the Fun panel
  
  ##########################---------- DOWNLOAD Buttons--------##################################
  ####------ Simulation data
  output$downloadData <- downloadHandler(
    filename = function(){
      paste0("Simulation_Data_Standard_maize_", Sys.Date(), ".csv")
    },
    content = function(file){
      req(simulation.results())  # Ensure simulation.results() is available before execution
      write.csv(simulation.results()[["pheno"]], file, row.names = FALSE)
    }
  ) # For the Standard panel
  
  output$downloadData2 <- downloadHandler(
    filename = function(){
      paste0("Simulation_Data_Fun_", input$crop,"_", Sys.Date(), ".csv")
    },
    content = function(file){
      req(simulation.results2())  # Ensure simulation.results2() is available before execution
      write.csv(simulation.results2()[["pheno"]], file, row.names = FALSE)
    }
  ) # For the fun panel
  
  ####------ Genetic gain
  output$downloadGainPlot <- downloadHandler(
    filename = function() {
      paste0("Genetic_Gain_Plot_Standard_", Sys.Date(), ".jpeg")
    },
    content = function(file) {
      req(dotplot_temp())
      ggsave(file, plot = dotplot_temp(), device = "jpeg", width = 12, height = 4, dpi = 300)
    }
  ) # For the Standard panel
  
  output$downloadGainPlot2 <- downloadHandler(
    filename = function() {
      paste0("Genetic_Gain_Plot_Fun_", input$crop, "_", Sys.Date(), ".jpeg")
    },
    content = function(file) {
      req(dotplot_temp2())
      ggsave(file, plot = dotplot_temp2(), device = "jpeg", width = 12, height = 4, dpi = 300)
    }
  ) # For the Fun panel
  
  ####------ Pheno Distribution
  output$downloadPhenoDistPlot <- downloadHandler(
    filename = function(){
      paste0("Histogram_Standard_", input$selectTrait, "_Generation_", input$selectGen, "_", Sys.Date(), ".jpeg")
    },
    content = function(file){
      req(histogram())
      ggsave(file, plot = histogram(), device = "jpeg", width = 6, height = 4, dpi = 300)
    }
  )# For the Standard panel
  
  output$downloadPhenoDistPlot2 <- downloadHandler(
    filename = function(){
      paste0("Histogram_Fun_", input$selectTrait3,"_Generation_", input$selectGen2, "_", Sys.Date(), ".jpeg")
    },
    content = function(file){
      req(histogram2())
      ggsave(file, plot = histogram(), device = "jpeg", width = 6, height = 4, dpi = 300)
    }
  )# For the Fun panel
  
  ####------ Pheno changes
  
  
  ##########################---------- RECYCLE GENERATION---------##################################
  ## Recycle the a user-defined generation for a new simulation cycle #################
  observeEvent(input$recycle, {
    pop.maize <<- simulation.results()[["popList"]][[input$genNumber]]
  }) # For the Standard panel
  
  observeEvent(input$recycle2, {
    if(input$crop == "Maize"){
      pop.maize.fun <<- simulation.results2()[["popList"]][[input$genNumber2]]
    }else if(input$crop == "Avocado"){
      pop.avocado <<- simulation.results2()[["popList"]][[input$genNumber2]]
    }else if(input$crop == "Strawberry"){
      pop.strawberry <<- simulation.results2()[["popList"]][[input$genNumber2]]
    }
  }) # For the Fun panel
  
  observeEvent(input$recycle,{
    updateProgressBar(session = session, id = "pb", value = 100, title ="Reclycling done!")
  })
  observeEvent(input$recycle2,{
    updateProgressBar(session = session, id = "pb2", value = 100, title ="Reclycling done!")
  })
  ##########################---------- RESET APP---------##################################
  observeEvent(input$reset, {
    source("R/Load_shiny_simulation_parameters.R")
    session$reload()
  }) # For the Standard panel
  observeEvent(input$reset2, {
    source("R/Load_shiny_simulation_parameters.R")
    session$reload()
  }) # For the Fun panel
  
}

########################################################################################################################
######################### ---------- The End! Let us run the app! --------- ############################################
# Run the application
shinyApp(ui = ui, server = server)
