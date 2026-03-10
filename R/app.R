
#' Launch the ViSAGE R Shiny app
#'
#' @description The `run_visage()` function launches the ViSAGE R Shiny app.
#' The app is designed to simulate and visualize various genetic
#' architectures and selection schemes in breeding populations.
#' The app provides an interactive interface for users to
#' explore and compare different selection strategies.
#' It also allows the user to run GWAS and genomic prediction analyses using either
#' simulated data or user-inputed data.
#' @usage run_visage()
#'
#' @returns A Shiny app object that launches the ViSAGE application.
#' @export
run_visage <- function() {
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file("app/www", package = "ViSAGE")
  )


  ########################################################################################################################
  ######################------- User Interface of the app ---------- #####################################################
  ########################################################################################################################

  ui <- shiny::fluidPage(
    # shiny::tags$head(
    #   shiny::tags$link(
    #     rel = "stylesheet",
    #     type = "text/css",
    #     href = "www/custom_modals.css"
    #   )
    # ),
    shiny::tags$head(
      shiny::includeCSS(system.file("app/www/custom_modals.css", package = "ViSAGE"))
    ),

    theme = bslib::bs_theme(bootswatch = "united"),

    shiny::tabsetPanel(
      ######### Beginning of POPULATION panel ####################=======================
      shiny::tabPanel("Population",
                      shiny::fluidRow(
                        # LEFT shiny::column: Fixed layout
                        shiny::column(width = 3,
                                      shiny::div(class = "height: 90vh; overflow: auto;",
                                                 bslib::card(class = "height: 20vh; overflow-y: hidden;", bslib::card_header("Founders"),
                                                             shiny::numericInput("numFounder", "Number of founders", value = 100, min = 1),
                                                             shiny::numericInput("numChr", "Number of chromosomes", value = 1, min = 1),
                                                             shiny::numericInput("totalSeg", "Number of segregation sites", value = 1, min = 1),
                                                             shiny::actionButton(inputId = "createfounder", "Create founders", class = "btn btn-success")),

                                                 bslib::card(class = "height: 30vh; overflow-y: hidden;", bslib::card_header("Base population"),
                                                             shiny::selectInput(inputId = "basepopchoice", label = "How would you like to simulate your trait?",
                                                                                choices = c("Using effect sizes", "Using variances"),selected = "Using effect sizes"),
                                                             shiny::conditionalPanel(condition = "input.basepopchoice == 'Using effect sizes'",
                                                                                     shiny::numericInput("tMean1_sp", "Population mean", value = 0, min = 0),
                                                                                     shiny::numericInput("bsh1_sp", "Broad-sense heritability", value = 0, min = 0, max = 1),
                                                                                     shiny::numericInput("numadd", "Enter number of additive QTNs", value = 0, min = 0),
                                                                                     shiny::numericInput("numdom", "Enter number of dominance QTNs", value = 0, min = 0),
                                                                                     shiny::numericInput("numepi", "Enter number of pairwise additive x additive epistasis interactions", value = 0, min = 0),
                                                                                     shiny::numericInput("Baddeff", "Enter one large additive effect size", value = 0),
                                                                                     shiny::textOutput(outputId = "describeffect"),
                                                                                     shiny::numericInput("addeff", "Enter first additive effect size", value = 0, min = -1, max = 1),
                                                                                     shiny::numericInput("domeff", "Enter first dominance effect size", value = 0, min = -1, max = 1),
                                                                                     shiny::numericInput("epieff", "Enter first addxadd epistasis effect size", value = 0, min = -1, max = 1),
                                                                                     shiny::actionButton(inputId = "createbasepop", "Create base population", class = "btn btn-success")
                                                             ), #End of effect sizes conditional panel
                                                             shiny::conditionalPanel(condition = "input.basepopchoice == 'Using variances'",
                                                                                     shiny::numericInput("tMean1", "Population mean", value = 0),
                                                                                     shiny::numericInput("bsh1", "Broad-sense heritability", value = 0, min = 0, max = 1),
                                                                                     shiny::numericInput("totalNqtn1", "Number of QTNs", value = 0, min = 0),
                                                                                     shiny::numericInput("VA1", "Additive variance", value = 0, min = 0),
                                                                                     shiny::numericInput("VD1", "Dominance variance", value = 0, min = 0),
                                                                                     shiny::numericInput("VEpi1", "Epistasis variance", value = 0, min = 0),
                                                                                     shiny::numericInput("ddeg", "Mean dominance degree", value = 0, min = 0),
                                                                                     shiny::numericInput("vddeg", "Variance of dominance degree", value = 0, min = 0),
                                                                                     shiny::actionButton(inputId = "createbasepop", "Create base population", class = "btn btn-success")
                                                             )#End of variance conditional panel

                                                 ),#End of Base population card
                                                 bslib::card(class = "height: 10vh; overflow-y: hidden;",
                                                             shiny::actionButton(inputId = "reset", "Reset the app", class = "btn btn-warning"))
                                      )# End of shiny::div
                        ), #End of LEFT shiny::column

                        # RIGHT shiny::column: Scrollable with nested grid
                        shiny::column(width = 9,
                                      shiny::div(class = "height: 90vh; overflow: auto;",
                                                 shiny::fluidRow(class = "height: 30vh; overflow-y: hidden;",
                                                                 shiny::column(width = 12,
                                                                               bslib::card( bslib::card_header("Genomic PCA for founders"),
                                                                                            shiny::plotOutput(outputId = "founderpca"))
                                                                 )),#End of shiny::column and shiny::fluidRow

                                                 shiny::fluidRow(class = "height: 30vh; overflow-y: hidden;",
                                                                 shiny::column(width = 6,
                                                                               bslib::card( bslib::card_header("Genomic heatmap for base population"),
                                                                                            shiny::plotOutput(outputId = "basepopheatmap"))
                                                                 ),
                                                                 shiny::column(width = 6,
                                                                               bslib::card( bslib::card_header("Phenotypic distribution"),
                                                                                            # shiny::uiOutput("choosetrait_bp"),
                                                                                            # shiny::actionButton(inputId = "plotHistbp", "Plot histogram"),
                                                                                            shiny::plotOutput(outputId = "histplotbp"))
                                                                 )
                                                 ),

                                                 bslib::card(bslib::card_header("Download outputs"),
                                                             shiny::fluidRow(class = "height: 6vh;", #Download buttons
                                                                             shiny::column(3,
                                                                                           shiny::downloadButton("downloadGenobp", "Download base populatin genotypes")
                                                                             ),
                                                                             shiny::column(3,
                                                                                           shiny::downloadButton("downloadPhenobp", "Download base population phenotypes")
                                                                             ),
                                                                             shiny::column(3,
                                                                                           shiny::downloadButton("downloadheatf", "Download founders heatmap")
                                                                             ),
                                                                             shiny::column(3,
                                                                                           shiny::downloadButton("downloadheatbp", "Download base population heatmap")
                                                                             )
                                                             ))#End of card and shiny::fluidRow
                                      )
                        )
                      )),
      ######################-------------------  End of POPULATION Panel--------------------################################

      ######### Beginning of SELECTION panel ####################=======================
      shiny::tabPanel("Selection",
                      shiny::fluidRow(class = "height: 90vh; overflow-y: auto;",
                                      # class = "height: 95vh; overflow-y: auto;",

                                      # LEFT shiny::column: Fixed layout
                                      shiny::column(width = 3,
                                                    shiny::div(class = "height: 90vh; overflow: auto;",
                                                               bslib::card(class = "height: 55vh; overflow-y: hidden;", bslib::card_header("Selection decisions"),
                                                                           shiny::uiOutput("multichoosetrait"),
                                                                           # shiny::selectInput("multichoosetrait", label = "Select a trait", choices = c("Trait 1", "Trait 2")),
                                                                           # shiny::selectInput(inputId = "selType", label = "Choose three types of selection to compare",
                                                                           #             choices = c("Directional_higher", "Directional_lower",
                                                                           #                         "Disruptive", "Stabilizing", "Random_drift"), multiple = T),
                                                                           shiny::checkboxGroupInput(inputId = "selType", label = "Choose the types of selection  you want to compare",
                                                                                                     choices = c("Directional_higher", "Directional_lower",
                                                                                                                 "Disruptive", "Stabilizing", "Random_drift"),selected = "Directional_higher"),
                                                                           shiny::sliderInput(inputId = "intensity", "What percentage would you like to select?",
                                                                                              min = 0, max = 100, step = 5, value = 10),
                                                                           shiny::numericInput("NumGener", "Number of Generations", value = 20, min = 2),
                                                                           shiny::numericInput("NumCross", "Number of crosses per generation", value = 10, min = 1),
                                                                           shiny::numericInput("NumProgeny", "Number of progeny per cross", value = 10, min = 1),
                                                                           shiny::actionButton(inputId = "multisimulate", "Perform selection", class = "btn btn-success")),

                                                               bslib::card(class = "height: 6vh",
                                                                           shiny::actionButton(inputId = "reset", "Reset the app", class = "btn btn-warning"))
                                                    )#End of shiny::div
                                      ),# End of LEFT shiny::column

                                      # RIGHT shiny::column: Scrollable with nested grid
                                      shiny::column(width = 9,
                                                    shiny::div(class = "height: 90vh; overflow: auto;",

                                                               shiny::fluidRow(
                                                                 shiny::column(12,
                                                                               bslib::card(class = "height: 25vh; overflow-y: hidden;", bslib::card_header("Changes in phenotypic values"),
                                                                                           shiny::plotOutput(outputId = "phenGainplot"))
                                                                 )
                                                               ),
                                                               shiny::fluidRow(
                                                                 shiny::column(12,
                                                                               bslib::card(class = "height: 25vh; overflow-y: hidden;", bslib::card_header("Changes in genetic values"),
                                                                                           shiny::plotOutput(outputId = "genGainplot"))
                                                                 )
                                                               ),
                                                               shiny::fluidRow(class = "height: 25vh; overflow-y: hidden;",
                                                                               shiny::column(12, bslib::card(bslib::card_header("Changes in genetic variance"),
                                                                                                             shiny::plotOutput(outputId = "genvarplot")
                                                                               )
                                                                               )
                                                                               # shiny::column(6,
                                                                               #        bslib::card(bslib::card_header("Changes in allele frequencies"),
                                                                               #             shiny::plotOutput(outputId = "allelefreqplot"))
                                                                               # )
                                                               ),

                                                               bslib::card(bslib::card_header("Download results"),
                                                                           shiny::fluidRow(class = "height: 10vh", #Download buttons
                                                                                           shiny::column(4,
                                                                                                         shiny::downloadButton("downloadMultigeno", "Download multi-generation genotypes")
                                                                                           ),
                                                                                           shiny::column(4,
                                                                                                         shiny::downloadButton("downloadMultipheno", "Download multi-generation phenotypes")
                                                                                           ),
                                                                                           shiny::column(4,
                                                                                                         shiny::downloadButton("downloadGainPlot", "Download genetic gain plot")
                                                                                           )
                                                                           )) # End of card and shiny::fluidRow download buttons

                                                    )
                                      )
                      )),
      ######################-------------------  End of SELECTION Panel--------------------################################

      ######### Beginning of GWAS panel ####################=======================
      shiny::tabPanel("GWAS",
                      shiny::fluidRow(class = "height: 95vh; overflow-y: auto;",

                                      # LEFT shiny::column: Fixed layout
                                      shiny::column(width = 3,
                                                    shiny::div(class = "height: 90vh; overflow: auto;",
                                                               bslib::card(class = "height: 80vh; overflow-y: hidden;", bslib::card_header("GWAS Analysis"),
                                                                           shiny::selectInput("gwasdtchoice", label = "How would you like to run GWAS?",
                                                                                              choices = c("Using simulated data", "Using my own data"),
                                                                                              selected = "Using simulated data"),
                                                                           shiny::conditionalPanel(
                                                                             condition = "input.gwasdtchoice == 'Using simulated data'",
                                                                             shiny::uiOutput("gwasseltype"),
                                                                             shiny::uiOutput("gwasgeneration")
                                                                           ), #End of simulated data conditional panel
                                                                           shiny::conditionalPanel(
                                                                             condition = "input.gwasdtchoice == 'Using my own data'",
                                                                             shiny::fileInput("phenodtgwas", "Upload phenotypic data"),
                                                                             shiny::fileInput("genodtgwas", "Upload genomic data"),
                                                                             shiny::fileInput("qmatrix", "Upload population structure Q matrix"),
                                                                             shiny::icon("info-circle", class = "text-info", id = "run_info"),
                                                                             shinyBS::bsTooltip(
                                                                               id = "run_info",
                                                                               title = "This button fits the model using all current inputs.",
                                                                               placement = "right"
                                                                             ),
                                                                             bslib::tooltip(shiny::icon("info"), "The assumption of the presence of population structure implies
                                      the presence of subsets of individuals more closely related to each other than
                                      to individuals in other subsets.The Q matrix can be obtained from PCA or STRUCTURE analysis."),
                                                                             shiny::fileInput("kmatrix", "Upload kinship K matrix"),
                                                                             shiny::selectInput("alltrait", label = "Analyze all the traits?", choices = c("No", "Yes")),
                                                                             shiny::uiOutput("gwaschoosetrait", label = "Select the trait shiny::column"),
                                                                           ), #End of conditional panel for using own data

                                                                           shiny::selectInput("gwasmodel", label = "Select one GWAS model", choices = c("GLM", "MLM")),
                                                                           shiny::numericInput("numpcs", "Enter number of PCs", value = 3, min = 1),
                                                                           shiny::actionButton(inputId = "rungwas", "Run GWAS", class = "btn btn-success")),
                                                               bslib::card(class = "height: 5vh",
                                                                           shiny::actionButton(inputId = "reset", "Reset the app", class = "btn btn-warning")),
                                                               bslib::card(class = "height: 5vh",
                                                                           shiny::tags$img(src = "www/Gapit_Logo_draft4.jpg",
                                                                                           height = "100%", width = "100%"))
                                                    )
                                      ), #End of LEFT shiny::column

                                      # RIGHT shiny::column: Scrollable with nested grid
                                      shiny::column(width = 9,
                                                    shiny::div(class = "height: 90vh; overflow: auto;",

                                                               shiny::fluidRow(class = "vh-70",#GWAS outputs
                                                                               shiny::column(8,
                                                                                             bslib::card(bslib::card_header("Manhathan plot"), shiny::plotOutput(outputId = "manhathanplot"))
                                                                               ),
                                                                               shiny::column(4,
                                                                                             bslib::card(bslib::card_header("Q-Q plot"), shiny::plotOutput(outputId = "qqplot"))
                                                                               )
                                                               ),#End of shiny::fluidRow GWAS outputs
                                                               shiny::fluidRow(class = "height: 10vh", #GWAS outputs control

                                                                               shiny::column(6,
                                                                                             shiny::actionButton(inputId = "plotGWAS", "Create Manhattan plot")
                                                                               ),
                                                                               shiny::column(6,
                                                                                             shiny::actionButton("plotQQ", "Create Q-Q plot")
                                                                               )
                                                               ), #End of shiny::fluidRow GWAS outputs control

                                                               shiny::fluidRow(class = "height: 10vh",#Download buttons
                                                                               shiny::column(4,
                                                                                             shiny::downloadButton("downloadGWASresults", "Download GWAS results")
                                                                               ),
                                                                               shiny::column(4,
                                                                                             shiny::downloadButton("downloadGWASP", "Download Manhattan plot")
                                                                               ),
                                                                               shiny::column(4,
                                                                                             shiny::downloadButton("downloadQQ", "Download Q-Q plot")
                                                                               )
                                                               ) # End of shiny::fluidRow download buttons

                                                    )#End of shiny::div
                                      )# End of RIGHT shiny::column:
                      )),
      ######################-------------------  End of GWAS Panel--------------------################################

      ######### Beginning of GENOMIC PREDICTION panel ####################=======================
      shiny::tabPanel("Genomic Prediction",
                      shiny::fluidRow(class = "height: 90vh; overflow-y: auto;",
                                      # class = "height: 95vh; overflow-y: auto;",

                                      # LEFT shiny::column: Fixed layout
                                      shiny::column(width = 3,
                                                    shiny::div(class = "height: 90vh; overflow: auto;",
                                                               bslib::card(class = "height: 60; overflow-y: hidden;", bslib::card_header("Training set"),
                                                                           shiny::selectInput("gpdtchoice", label = "How would you like to run genomic prediction?",
                                                                                              choices = c("Using simulated data", "Using my own data"),
                                                                                              selected = "Using simulated data"),
                                                                           shiny::conditionalPanel(condition = "input.gpdtchoice == 'Using simulated data'",
                                                                                                   shiny::uiOutput("gpseltype"),
                                                                                                   shiny::uiOutput("gpgeneration"),
                                                                           ), #End of simulated data conditional panel
                                                                           shiny::conditionalPanel(condition = "input.gpdtchoice == 'Using my own data'",
                                                                                                   shiny::fileInput("phenodtgp", "Upload phenotypic data"),
                                                                                                   shiny::fileInput("traingenodtgp", "Upload genomic data")
                                                                           ), #End of conditional panel for using own data

                                                                           shiny::uiOutput("choosetrait"),
                                                                           shiny::numericInput("numfolds", "Enter number of k folds", value = 5, min = 2),
                                                                           shiny::numericInput("numreps", "Enter number of repetitions", value = 1, min = 1),
                                                                           shiny::actionButton(inputId = "runcv", "Run cross-validation", class = "btn btn-success")),
                                                               bslib::card(class = "height: 10vh",
                                                                           shiny::radioButtons(inputId = "gpchoice", "Would you like to predict a test set of unphenotyped breeding lines",
                                                                                               choices = c("No", "Yes"), selected = "No")
                                                               ),
                                                               shiny::conditionalPanel(condition = "input.gpchoice == 'Yes'",
                                                                                       bslib::card(class = "height: 20; overflow-y: hidden;", bslib::card_header("Prediction set"),
                                                                                                   shiny::conditionalPanel(condition = "input.gpdtchoice == 'Using simulated data'",
                                                                                                                           shiny::textOutput(outputId = "describedata"),
                                                                                                                           shiny::uiOutput("gpseltype2"),
                                                                                                                           shiny::uiOutput("gpgeneration2")),

                                                                                                   shiny::conditionalPanel(condition = "input.gpdtchoice == 'Using my own data'",
                                                                                                                           shiny::fileInput("testgenodtgp", "Upload genomic data")),

                                                                                                   shiny::selectInput(inputId = "GSselType", label = "Choose a selection type you wish to apply to the GEBVs",
                                                                                                                      choices = c("Directional_higher", "Directional_lower",
                                                                                                                                  "Disruptive", "Stabilizing")),
                                                                                                   shiny::sliderInput("selectpct", "What is your selection intensity (%)?",
                                                                                                                      min = 1, max = 100, step = 1, value = 10),
                                                                                                   shiny::actionButton(inputId = "rungp", "Run prediction model", class = "btn btn-success")

                                                                                                   ) ## End card for prediction set

                                                               ), #End of conditional panel for prediction set
                                                               bslib::card(class = "height: 10vh",
                                                                           shiny::actionButton(inputId = "reset", "Reset the app", class = "btn btn-warning"))
                                                    )
                                      ),

                                      # RIGHT shiny::column: Scrollable with nested grid
                                      shiny::column(width = 9,
                                                    shiny::div(class = "height: 90vh; overflow: auto;",
                                                               shiny::fluidRow(class = "height:25vh; overflow-y:auto;",#Data quality control
                                                                               shiny::column(6,
                                                                                             bslib::card(
                                                                                               full_screen = TRUE, height = 450,
                                                                                               bslib::card_header("Phenotypic data"),
                                                                                                         shiny::plotOutput(outputId = "gphistplot"))
                                                                               ),
                                                                               shiny::column(6,
                                                                                             bslib::card(
                                                                                               full_screen = TRUE, height = 450,
                                                                                               bslib::card_header("SNP data"),
                                                                                                         DT::DTOutput(outputId = "gpsnpdata"))
                                                                               )
                                                               ),#End of shiny::fluidRow GS CV outputs
                                                               shiny::fluidRow(class = "height: 35vh; overflow-y: auto;",#GS CV outputs
                                                                               shiny::column(6,
                                                                                             bslib::card(
                                                                                               full_screen = TRUE, height = 450,
                                                                                               bslib::card_header("Cross-validation Scatter plot"),
                                                                                                         shiny::plotOutput(outputId = "gpscatterplot"))
                                                                               ),
                                                                               shiny::column(6,
                                                                                             bslib::card(full_screen = TRUE, height = 450,
                                                                                                         bslib::card_header("Cross-validation Violin plot"),
                                                                                                         shiny::plotOutput(outputId = "gpviolin"))
                                                                               )
                                                               ),#End of shiny::fluidRow GS CV outputs

                                                               # shiny::fluidRow(class = "height: 10vh",#GS outputs control
                                                               #                 shiny::column(6,
                                                               #                               shiny::actionButton(inputId = "scatterplotGP", "Create scatter plot")
                                                               #                 ),
                                                               #                 shiny::column(6,
                                                               #                               shiny::actionButton("violinplotGS", "Create violin plot")
                                                               #                 )
                                                               # ),# End of shiny::fluidRow GS outputs control

                                                               shiny::fluidRow(class = "height: 35vh; overflow-y: auto;",#Gp outputs
                                                                               shiny::conditionalPanel(condition = "input.gpdtchoice == 'Using simulated data' & input.gpchoice == 'Yes'",
                                                                                                       shiny::column(4,
                                                                                                                     bslib::card(full_screen = TRUE, height = 450,
                                                                                                                                 bslib::card_header("Prediction accuracy"),
                                                                                                                                 shiny::plotOutput(outputId = "gp_accuracy1"))
                                                                                                       ),
                                                                                                       shiny::column(4,
                                                                                                                     bslib::card(full_screen = TRUE, height = 450,
                                                                                                                                 bslib::card_header("Prediction Histogram"),
                                                                                                                                 shiny::plotOutput(outputId = "gphist1"))
                                                                                                       ),
                                                                                                       shiny::column(4,
                                                                                                                     bslib::card(full_screen = TRUE, height = 450,
                                                                                                                                 bslib::card_header("Selected individuals"),
                                                                                                                                 DT::DTOutput(outputId = "summary_pred1"))
                                                                                                       )

                                                                                                ),
                                                                               shiny::conditionalPanel(condition = "input.gpdtchoice == 'Using my own data' & input.gpchoice == 'Yes'",
                                                                                                       shiny::column(6,
                                                                                                                     bslib::card(
                                                                                                                       full_screen = TRUE, height = 450,
                                                                                                                       bslib::card_header("Prediction Histogram"),
                                                                                                                       shiny::plotOutput(outputId = "gphist2"))
                                                                                                       ),
                                                                                                       shiny::column(6,
                                                                                                                     bslib::card(full_screen = TRUE, height = 450,
                                                                                                                                 bslib::card_header("Selected individuals"),
                                                                                                                                 DT::DTOutput(outputId = "summary_pred2"))
                                                                                                       )
                                                                               )#End of condiction panel own data

                                                               ),#End of shiny::fluidRow GP outputs

                                                               shiny::fluidRow(class = "height: 10vh", #Download buttons
                                                                               shiny::column(3,
                                                                                             shiny::downloadButton("downloadCVScatter", "Download GP scatterplot")
                                                                               ),
                                                                               shiny::column(3,
                                                                                             shiny::downloadButton("downloadCVViolin", "Download CV violin plot")
                                                                               ),
                                                                               shiny::column(3,
                                                                                             shiny::downloadButton("downloadGPHist", "Download GP histogram")
                                                                               ),
                                                                               shiny::column(3,
                                                                                             shiny::downloadButton("downloadGP", "Download GP results")
                                                                               ),
                                                               )# End of shiny::fluidRow download buttons

                                                    )#End of shiny::div
                                      )
                      ))
      ######################-------------------  End of GP Panel UI--------------------################################

    )#End of TabSetPanel

  )#End of the overall UI and fluidPage.

  ########################################################################################################################
  ######################------- Server function of the app ---------- ####################################################
  ########################################################################################################################
  server <- function(input, output, session) {

    #### Beginning of server for POPULATION ++++++++++++++++++++++++++####
    founders <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$createfounder, {
      shiny::req(input$numFounder, input$numChr, input$totalSeg)
      shiny::withProgress(message = "Creating founders ...", value = 0, {
        new_f <- create_founders(nfounders = input$numFounder, nChrom = input$numChr, nSites = input$totalSeg)
        founders(new_f)
        })
    })

    simparms <- shiny::eventReactive(founders(),{
      local_sp(f_pop = founders())
    })

    output$describeffect <- shiny::renderText({"The simulation based on effect sizes uses an underlying
    geometric series of QTN effects where a few QTNs have large effects
    and many QTNs have small effects. The acceptable values for QTN effects should be between -1 and 1."
    })

    shiny::observeEvent(input$createbasepop, {
      if (!exists("founders") || is.null(founders()) || length(founders()) == 0) {
        show_error_modal("Please first create the founders population before creating the base population.")
      }
    })

    #### This exception makes sure the number of simulated QTNs is less than the number of loci in the founder population.
    # shiny::observeEvent(input$createbasepop, {
    #
    #   total_loci <- sum(
    #     as.numeric(input$numadd),
    #     as.numeric(input$numdom),
    #     2 * as.numeric(input$numepi)
    #   )
    #
    #   if (founders()@nLoci < total_loci) {
    #     show_error_modal(paste0("You are simulating ", total_loci,
    #                             " QTNs but the founders population has only ",
    #                             founders()@nLoci, " polymorphisms. Please adjust accordingly. Please note that the number of epistasis QTNs ",
    #                             "doubles (i.e. 2 QTNs = > 2 pairwise interactions.)"))}
    # })


    base_pop <- shiny::eventReactive(input$createbasepop, {
      shiny::withProgress(message = "Creating base population ...", value = 0, {
        if(input$basepopchoice == "Using effect sizes"){
          # totnumQTNs <- sum(input$numadd, input$numdom, 2*input$numepi)
          # if(founders()@nLoci < totnumQTNs){
          #   stop(paste0("You are simulating more QTNs than segrating sites in the founders.
          #        Please resimulate founders with at least,", totnumQTNs, "Segregation sites"))
          # }
          shiny::req(founders(), input$bsh1_sp, input$tMean1_sp, simparms())

          create_base_pop_sp(founders = founders(),
                             sp_object = simparms(),
                             tMean = input$tMean1_sp,
                             a_QTNs = input$numadd,
                             d_QTNs = input$numdom,
                             e_QTNs = input$numepi,
                             big_a_eff = input$Baddeff,
                             a_eff = input$addeff,
                             d_eff = input$domeff,
                             e_eff = input$epieff,
                             tHet = input$bsh1_sp)
        }else if(input$basepopchoice == "Using variances"){
          shiny::req(founders(), input$bsh1, input$tMean1, simparms())

          create_base_pop(founders = founders(),
                          sp_object = simparms(),
                          nQTN = input$totalNqtn1,
                          tMean = input$tMean1,
                          tVA = input$VA1,
                          tVD = input$VD1,
                          tVE = input$VEpi1,
                          tHet = input$bsh1,
                          tdomDeg = input$ddeg, tVDomDeg = input$vddeg)
        }# End of if-else for base population creation
      })
    })


    output$founderpca <- shiny::renderPlot({
      shiny::req(founders())
      plot_pca_biplot(founders())
    })


    output$basepopheatmap <- shiny::renderPlot({
      shiny::req(base_pop(), simparms())
      pheatmap::pheatmap(AlphaSimR::pullSegSiteGeno(base_pop()[[2]], simParam = simparms()),
                         cluster_rows = F, cluster_cols = F,
                         show_rownames = T, show_colnames = T,
                         fontsize_row = 5, fontsize_col = 5)
    })

    # Select the trait base population
    # output$choosetrait_bp <- shiny::renderUI({
    #   shiny::req(base_pop())
    #   tchoices <- simparms()$traitNames
    #   shiny::selectInput("choosetraitbp", label = "Select a trait", choices = tchoices)
    # })
    # pheno_bp <- shiny::reactive({
    #   SP <- simparms()
    #   shiny::req(base_pop())
    #   b_pop <- base_pop()[[2]]
    #   dt_trt <- as.data.frame(AlphaSimR::pheno(b_pop))
    #   colnames(dt_trt) <- SP$traitNames
    #   data.frame(ID = b_pop@id, dt_trt)
    #
    # })
    #
    # histbp_reactive <- shiny::eventReactive(input$plotHistbp, {
    #   # shiny::req(base_pop(), input$choosetraitbp)
    #   # pheno_dtbp <- pheno_bp()[input$choosetraitbp]
    #   # pheno_dtbp <- as.data.frame(AlphaSimR::pheno(base_pop()))[input$choosetraitbp]
    #   shiny::req(base_pop())
    #   b_pop <- base_pop()[[2]]
    #   dt_trt <- as.data.frame(AlphaSimR::pheno(b_pop))
    #   colnames(dt_trt) <- "phenotype"
    #   ggplot2::ggplot(data = pheno_dtbp, ggplot2::aes(x = phenotype)) +
    #     ggplot2::geom_histogram(color = "white", fill = "#FF5F0F", bins = 10) +
    #     ggplot2::labs(x = input$choosetraitbp, y = "Count") +
    #     boris_theme()
    #
    # })

    output$histplotbp <- shiny::renderPlot({
      # shiny::req(histbp_reactive())
      # print(histbp_reactive())
      shiny::req(base_pop())
      b_pop <- base_pop()[[2]]
      dt_trt <- as.data.frame(AlphaSimR::pheno(b_pop))
      colnames(dt_trt) <- "phenotype"
      ggplot2::ggplot(data = dt_trt, ggplot2::aes(x = phenotype)) +
        ggplot2::geom_histogram(color = "white", fill = "#FF5F0F", bins = 10) +
        ggplot2::labs(x = "Phenotypic values", y = "Number of individuals") +
        boris_theme()
    })


    ## Download base population genotypes
    output$downloadGenobp <- shiny::downloadHandler(
      filename = function(){
        paste0("Base_population_genotypes_", Sys.Date(), ".csv")
      },
      content = function(file){
        shiny::req(base_pop())
        bp <- base_pop()[[2]]
        geno_dt <- data.frame(ID = bp@id,
                              as.data.frame(AlphaSimR::pullSegSiteGeno(bp, simParam = simparms())))
        utils::write.csv(geno_dt, file, row.names = FALSE)
      }
    )

    ## Download base population phenotypes
    output$downloadPhenobp <- shiny::downloadHandler(
      filename = function(){
        paste0("Base_population_phenotypes_", Sys.Date(), ".csv")
      },
      content = function(file){
        shiny::req(base_pop())
        bp <- base_pop()[[2]]
        pheno_dt <- data.frame(ID = bp@id,
                               as.data.frame(AlphaSimR::pheno(bp)))
        utils::write.csv(pheno_dt, file, row.names = FALSE)
      }
    )

    ## Download founders heatmap
    output$downloadheatf <- shiny::downloadHandler(
      filename = function(){
        paste0("Founders_heatmap_", Sys.Date(), ".jpeg")
      },
      content = function(file){
        shiny::req(heatmapf())
        ggplot2::ggsave(file, plot = heatmapf(), device = "jpeg", width = 6, height = 6, dpi = 300)
      }
    )

    ## Download base population heatmap
    output$downloadheatbp <- shiny::downloadHandler(
      filename = function(){
        paste0("Base_population_heatmap_", Sys.Date(), ".jpeg")
      },
      content = function(file){
        shiny::req(heatmapbp())
        ggplot2::ggsave(file, plot = heatmapbp(), device = "jpeg", width = 6, height = 6, dpi = 300)
      }
    )

    #### Beginning server for Selection ++++++++++++++++++++++++++####
    ## Select the trait for multi-generation selection
    output$multichoosetrait <- shiny::renderUI({
      shiny::req(base_pop())
      multi_trait_choices <- simparms()$traitNames
      shiny::selectInput("multichoosetrait2", label = "Select a trait", choices = multi_trait_choices)
    })
    ## Run multi-geration simulation based on user-defined parameters and the click of the simulate button
    gen_simulation <- shiny::eventReactive(input$multisimulate, {
      shiny::req(base_pop(), input$multichoosetrait2, input$selType, input$intensity, input$NumGener)
      dh <- list(); dl <- list(); dr <- list(); st <- list(); rd <- list(); bp <- base_pop()
      SP <- simparms();
      shiny::withProgress(message = "Selection simulation in progress ...", value = 0, {
        if("Directional_higher" %in% input$selType){
          if(input$basepopchoice == "Using variances"){
            dh <- multi_generations_selection(initial_generation = bp[[2]],
                                              SP_object = SP,
                                              selectionType = "Directional_higher",
                                              tSel = input$multichoosetrait2,
                                              selectPercent = input$intensity,
                                              tHet = input$bsh1,
                                              nCross = input$NumCross,
                                              nProgenyPerCross = input$NumProgeny,
                                              nGenerations = input$NumGener)
          }else if(input$basepopchoice == "Using effect sizes"){
            dh <- multi_generations_selection_sp(initial_generation = bp[[2]],
                                                 SP_object = SP,
                                                 QTN_list = bp[[3]],
                                                 selectionType = "Directional_higher",
                                                 tSel = input$multichoosetrait2,
                                                 selectPercent = input$intensity,
                                                 tHet = input$bsh1,
                                                 nCross = input$NumCross,
                                                 nProgenyPerCross = input$NumProgeny,
                                                 nGenerations = input$NumGener,
                                                 a_QTNs = input$numadd,
                                                 d_QTNs = input$numdom,
                                                 e_QTNs = input$numepi,
                                                 big_a_eff = input$Baddeff,
                                                 a_eff = input$addeff,
                                                 d_eff = input$domeff,
                                                 e_eff = input$epieff,
                                                 arch = bp[[1]])
          }

        }
        if("Directional_lower" %in% input$selType){
          if(input$basepopchoice == "Using variances"){
            dl <- multi_generations_selection(initial_generation = bp[[2]],
                                              SP_object = SP,
                                              selectionType = "Directional_lower",
                                              tSel = input$multichoosetrait2,
                                              selectPercent = input$intensity,
                                              tHet = input$bsh1,
                                              nCross = input$NumCross,
                                              nProgenyPerCross = input$NumProgeny,
                                              nGenerations = input$NumGener)
          }else if(input$basepopchoice == "Using effect sizes"){
            dl <- multi_generations_selection_sp(initial_generation = bp[[2]],
                                                 SP_object = SP,
                                                 QTN_list = bp[[3]],
                                                 selectionType = "Directional_lower",
                                                 tSel = input$multichoosetrait2,
                                                 selectPercent = input$intensity,
                                                 tHet = input$bsh1,
                                                 nCross = input$NumCross,
                                                 nProgenyPerCross = input$NumProgeny,
                                                 nGenerations = input$NumGener,
                                                 a_QTNs = input$numadd,
                                                 d_QTNs = input$numdom,
                                                 e_QTNs = input$numepi,
                                                 big_a_eff = input$Baddeff,
                                                 a_eff = input$addeff,
                                                 d_eff = input$domeff,
                                                 e_eff = input$epieff,
                                                 arch = bp[[1]])
          }

        }
        if("Disruptive" %in% input$selType){
          if(input$basepopchoice == "Using variances"){
            dr <- multi_generations_selection(initial_generation = bp[[2]],
                                              SP_object = SP,
                                              selectionType = "Disruptive",
                                              tSel = input$multichoosetrait2,
                                              selectPercent = input$intensity,
                                              tHet = input$bsh1,
                                              nCross = input$NumCross,
                                              nProgenyPerCross = input$NumProgeny,
                                              nGenerations = input$NumGener)
          }else if(input$basepopchoice == "Using effect sizes"){
            dr <- multi_generations_selection_sp(initial_generation = bp[[2]],
                                                 SP_object = SP,
                                                 QTN_list = bp[[3]],
                                                 selectionType = "Disruptive",
                                                 tSel = input$multichoosetrait2,
                                                 selectPercent = input$intensity,
                                                 tHet = input$bsh1,
                                                 nCross = input$NumCross,
                                                 nProgenyPerCross = input$NumProgeny,
                                                 nGenerations = input$NumGener,
                                                 a_QTNs = input$numadd,
                                                 d_QTNs = input$numdom,
                                                 e_QTNs = input$numepi,
                                                 big_a_eff = input$Baddeff,
                                                 a_eff = input$addeff,
                                                 d_eff = input$domeff,
                                                 e_eff = input$epieff,
                                                 arch = bp[[1]])
          }
          # dr <- multi_generations_selection(initial_generation = base_pop(),
          #                                   SP_object = SP,
          #                                   selectionType = "Disruptive",
          #                                   tSel = input$multichoosetrait2,
          #                                   selectPercent = input$intensity,
          #                                   tHet = input$bsh1,
          #                                   nCross = input$NumCross,
          #                                   nProgenyPerCross = input$NumProgeny,
          #                                   nGenerations = input$NumGener)
        }
        if("Stabilizing" %in% input$selType){
          if(input$basepopchoice == "Using variances"){
            st <- multi_generations_selection(initial_generation = bp[[2]],
                                              SP_object = SP,
                                              selectionType = "Stabilizing",
                                              tSel = input$multichoosetrait2,
                                              selectPercent = input$intensity,
                                              tHet = input$bsh1,
                                              nCross = input$NumCross,
                                              nProgenyPerCross = input$NumProgeny,
                                              nGenerations = input$NumGener)
          }else if(input$basepopchoice == "Using effect sizes"){
            st <- multi_generations_selection_sp(initial_generation = bp[[2]],
                                                 SP_object = SP,
                                                 QTN_list = bp[[3]],
                                                 selectionType = "Stabilizing",
                                                 tSel = input$multichoosetrait2,
                                                 selectPercent = input$intensity,
                                                 tHet = input$bsh1,
                                                 nCross = input$NumCross,
                                                 nProgenyPerCross = input$NumProgeny,
                                                 nGenerations = input$NumGener,
                                                 a_QTNs = input$numadd,
                                                 d_QTNs = input$numdom,
                                                 e_QTNs = input$numepi,
                                                 big_a_eff = input$Baddeff,
                                                 a_eff = input$addeff,
                                                 d_eff = input$domeff,
                                                 e_eff = input$epieff,
                                                 arch = bp[[1]])
          }
          # st <- multi_generations_selection(initial_generation = base_pop(),
          #                                   SP_object = SP,
          #                                   selectionType = "Stabilizing",
          #                                   tSel = input$multichoosetrait2,
          #                                   selectPercent = input$intensity,
          #                                   tHet = input$bsh1,
          #                                   nCross = input$NumCross,
          #                                   nProgenyPerCross = input$NumProgeny,
          #                                   nGenerations = input$NumGener)
        }
        if("Random_drift" %in% input$selType){
          if(input$basepopchoice == "Using variances"){
            rd <- multi_generations_selection(initial_generation = bp[[2]],
                                              SP_object = SP,
                                              selectionType = "Random_drift",
                                              tSel = input$multichoosetrait2,
                                              selectPercent = input$intensity,
                                              tHet = input$bsh1,
                                              nCross = input$NumCross,
                                              nProgenyPerCross = input$NumProgeny,
                                              nGenerations = input$NumGener)
          }else if(input$basepopchoice == "Using effect sizes"){
            rd <- multi_generations_selection_sp(initial_generation = bp[[2]],
                                                 SP_object = SP,
                                                 QTN_list = bp[[3]],
                                                 selectionType = "Random_drift",
                                                 tSel = input$multichoosetrait2,
                                                 selectPercent = input$intensity,
                                                 tHet = input$bsh1,
                                                 nCross = input$NumCross,
                                                 nProgenyPerCross = input$NumProgeny,
                                                 nGenerations = input$NumGener,
                                                 a_QTNs = input$numadd,
                                                 d_QTNs = input$numdom,
                                                 e_QTNs = input$numepi,
                                                 big_a_eff = input$Baddeff,
                                                 a_eff = input$addeff,
                                                 d_eff = input$domeff,
                                                 e_eff = input$epieff,
                                                 arch = bp[[1]])
          }

        }

        list(dh = dh, dl = dl, dr = dr, st = st, rd = rd)
        # multi_generations_selection(initial_generation = base_pop(),
        #                             SP_object = SP,
        #                             selectionType = input$selType,
        #                             tSel = input$multichoosetrait2,
        #                             selectPercent = input$intensity,
        #                             tHet = input$bsh1,
        #                             nCross = input$NumCross,
        #                             nProgenyPerCross = input$NumProgeny,
        #                             nGenerations = input$NumGener)
      })
    })## Output a list of 4 elements, including GVs and phenotype

    ## Extract data
    gaindt_reactive <- shiny::eventReactive(gen_simulation(), {
      # extract_data(generation_list = gen_simulation(), SP_object = SP,
      #              nTrait = length(SP$traitNames))
      SP <- simparms()
      options <- c("Directional_higher", "Directional_lower", "Disruptive", "Stabilizing", "Random_drift")
      extracted <- lapply(gen_simulation(), extract_data, SP_object = SP,
                          nTrait = length(SP$traitNames))

      mutated <- add_select_type(extracted, labels = options)
      data.table::rbindlist(mutated) |> as.data.frame()
    })
    ## create genetic gain plot
    output$phenGainplot <- shiny::renderPlot({
      shiny::req(gaindt_reactive())
      genetic_gain(dt = gaindt_reactive(), fill_factor1 = "Generation",
                   fill_factor2 = "Selection_type", y_variable = "phenotype")
    })
    output$genGainplot <- shiny::renderPlot({
      shiny::req(gaindt_reactive())
      genetic_gain(dt = gaindt_reactive(), fill_factor1 = "Generation",
                   fill_factor2 = "Selection_type", y_variable = "genetic_value")
    })

    var_plot <- shiny::eventReactive(gen_simulation(),{
      sims <- gen_simulation()
      names(sims) <- c("Directional_higher", "Directional_lower", "Disruptive",
                       "Stabilizing", "Random_drift")
      indices <- which(sapply(sims, length) != 0)
      # computeVariancePlot(gen_simulation()[[indices[1]]])

      plts <- lapply(sims[indices], computeVariancePlot, SP_object = simparms())
      ggpubr::ggarrange(plotlist = plts,
                        ncol = ifelse(length(plts) < 3, 1, 2),
                        nrow = ifelse(length(plts) < 4, 2, 3),
                        common.legend = TRUE, legend = "right",
                        labels = names(sims)[indices], vjust = 1)

    })

    output$genvarplot <- shiny::renderPlot({
      print(var_plot())
    })
    #### End of server for Selection ++++++++++++++++++++++++++####
    #### Server for GWAS ++++++++++++++++++++++++++####
    ## Select the trait
    output$gwasseltype <- shiny::renderUI({
      shiny::req(input$selType)
      shiny::selectInput("gwaseltype", label = "Select the selection type", choices = input$selType)
    })

    output$gwasgeneration <- shiny::renderUI({
      shiny::req(input$NumGener)
      shiny::selectInput("gwasgener", label = "Select the generation",
                         choices = c(0:input$NumGener))
    })

    #### End of server for GWAS ++++++++++++++++++++++++++####

    #### Server for Genomic prediction ++++++++++++++++++++++++++####

    ########## Cross-validation portion ----------------------------
    output$gpseltype <- shiny::renderUI({
      shiny::req(input$selType)
      shiny::selectInput("gpseltype", label = "Select a simulated selection type", choices = input$selType)
    })
    output$gpgeneration <- shiny::renderUI({
      shiny::req(input$NumGener)
      shiny::selectInput("gpgeneration", label = "Select a simulated generation",
                         choices = c(0:input$NumGener))
    })
    # gp_sim_dt <- shiny::eventReactive(gen_simulation(), {
    #   shiny::req(gen_simulation(), input$gpseltype, input$gpgeneration)
    #   sim_data_gp(mega_list = gen_simulation(), generation = as.numeric(input$gpgeneration),
    #               sel_type = input$gpseltype, SP_object = simparms())
    # })

    ## Read in training set data
    tgeno_reactive <- shiny::reactive({
      if(input$gpdtchoice == "Using simulated data"){
        shiny::req(input$gpseltype, input$gpgeneration, gen_simulation())
        (sim_data_gp(mega_list = gen_simulation(), generation = as.numeric(input$gpgeneration),
                    sel_type = input$gpseltype, SP_object = simparms()))$snp_data
        # shiny::req(gp_sim_dt())
        # gp_sim_dt()$snp_data
      }else if(input$gpdtchoice == "Using my own data"){
        shiny::req(input$traingenodtgp)
        utils::read.csv(input$traingenodtgp$datapath, header = TRUE)}
    })

    tpheno_reactive <- shiny::reactive({
      if(input$gpdtchoice == "Using simulated data"){
        shiny::req(input$gpseltype, input$gpgeneration, gen_simulation())
        (sim_data_gp(mega_list = gen_simulation(), generation = as.numeric(input$gpgeneration),
                    sel_type = input$gpseltype, SP_object = simparms()))$pheno_data
        # shiny::req(gp_sim_dt())
        # gp_sim_dt()$pheno_data
      }else if(input$gpdtchoice == "Using my own data"){
        shiny::req(input$phenodtgp)
        utils::read.csv(input$phenodtgp$datapath, header = TRUE)
      }

    })

    ##### preview of cross-valition data sets for genomic prediction ------------
    output$choosetrait <- shiny::renderUI({
      shiny::req(tpheno_reactive())
      trait_choices <- names(tpheno_reactive())[-1]
      shiny::selectInput("choosetraitgp", label = "Select a trait", choices = trait_choices)
    })

    output$gpsnpdata <- DT::renderDT({
      shiny::req(tgeno_reactive())
      if(nrow(tgeno_reactive()) > 50){
        tgeno_reactive()[1:50, 1:10]
      }else{
        tgeno_reactive()[, 1:10]
      }

    })
    output$gphistplot<- shiny::renderPlot({
      shiny::req(tpheno_reactive())
      graphics::hist(tpheno_reactive()[[input$choosetraitgp]], main = "",
           xlab = input$choosetraitgp, col = "#0455A4", border = "white")
    })

    ## Run cross-validation
    cv_results <- shiny::eventReactive(input$runcv,{
      print("Button runcv clicked")
      shiny::req(tgeno_reactive(), tpheno_reactive(), input$choosetraitgp, input$numfolds, input$numreps)
      shiny::withProgress(message = "Cross-validation in progress ...", value = 0, {
        genomic_prediction_rrblup(snp_data = tgeno_reactive(),
                                  pheno_data = tpheno_reactive(),
                                  trait = input$choosetraitgp,
                                  cv_type = "kfold",
                                  k = input$numfolds,
                                  n_reps = input$numreps)
      })
    })
    ## Output a list of 3 elements: 1. model, 2. cross-validation dt, 3. y vector

    ### Output cross-validation results as two plots
    ## 1. Scatter plot
    scatter <- shiny::reactiveVal(NULL) # Create a reactive value to store the scatter plot.
    output$gpscatterplot <- shiny::renderPlot({
      shiny::req(cv_results())
      p <- plot_prediction(cross_vd_dt = cv_results()[[2]])
      scatter(p) #Store plot p in the reactive value scatter, so that it can be accessed by the download handler
      print(p)
    })

    output$downloadCVScatter <- shiny::downloadHandler(
      filename = function() {
        paste0("Cross_validation_scatter_",input$numreps, "_reps_", Sys.Date(), ".jpg")
      },
      content = function(file) {
        shiny::req(scatter())
        ggplot2::ggsave(file, plot = scatter(), device = "jpeg",
                        width = 4, height = 4, dpi = 300)
      }
    )

    ## 2. Violin plot
    violin <- shiny::reactiveVal(NULL) # Create a reactive value to store the violin plot.
    output$gpviolin <- shiny::renderPlot({
      shiny::req(cv_results())
      set.seed(50)
      v <- plot_violin(y_vector = cv_results()[[3]][[2]])
      violin(v) #Store plot v in the reactive value violin
      print(v)
    })

    output$downloadCVViolin <- shiny::downloadHandler(
      filename = function() {
        paste0("Cross_validation_violin_",input$numreps, "_reps_", Sys.Date(), ".jpg")
      },
      content = function(file) {
        shiny::req(violin())
        ggplot2::ggsave(file, plot = violin(), device = "jpeg",
                        width = 4, height = 4, dpi = 300)
      }
    )

    #### Actual prediction on the unobserved individuals --------------------

    ### If using simulated data
    output$describedata <- shiny::renderText({
      "Since the training set for cross-validation was a simulated data set, the prediction set
      will also be a simulated data set. Select below, any simulated data of your choice based on
      the selection type (s) and generation (s) you simulated in the Selection panel."
    })
    output$gpseltype2 <- shiny::renderUI({
      shiny::req(input$selType)
      shiny::selectInput("gpseltype2", label = "Select a simulated selection type", choices = input$selType)
    })
    output$gpgeneration2 <- shiny::renderUI({
      shiny::req(input$NumGener)
      shiny::selectInput("gpgeneration2", label = "Select a simulated generation",
                         choices = c(0:input$NumGener))
    })

    testgeno_reactive <- shiny::reactive({
      if(input$gpdtchoice == "Using simulated data"){
        shiny::req(input$gpseltype2, input$gpgeneration2, gen_simulation())
        (sim_data_gp(mega_list = gen_simulation(), generation = as.numeric(input$gpgeneration2),
                     sel_type = input$gpseltype2, SP_object = simparms()))$snp_data

      }else if(input$gpdtchoice == "Using my own data"){
        shiny::req(input$testgenodtgp)
        utils::read.csv(input$testgenodtgp$datapath, header = TRUE)
      }

    })

    testpheno_reactive <- shiny::reactive({
      if(input$gpdtchoice == "Using simulated data"){
        shiny::req(input$gpseltype2, input$gpgeneration2, gen_simulation())
        (sim_data_gp(mega_list = gen_simulation(), generation = as.numeric(input$gpgeneration2),
                     sel_type = input$gpseltype2, SP_object = simparms()))$pheno_data
        }else{
          NULL
        }

    })

    pred <- shiny::eventReactive(input$rungp, {
      shiny::req(cv_results(), testgeno_reactive())
      shiny::withProgress(message = "Genomic prediction in progress ...", value = 0, {
        predict_gebv_rrblup(final_model = cv_results()[[1]],
                            snp_new = testgeno_reactive())
      })
    })

    #### Selection on test set
    test_selected <- shiny::eventReactive(list(pred(), input$GSselType, input$selectpct),{
      shiny::req(pred(), input$GSselType, input$selectpct)
      test_gp_selection(data_frame = pred(), selection_type = input$GSselType,
                        percent_selected = input$selectpct)
    })

    #### genomic prediction quantile coincidence
    coin_plt <- shiny::eventReactive(list(pred(),input$selectpct, testpheno_reactive()),{
        shiny::req(pred(), input$selectpct, testpheno_reactive())
        data = as.data.frame(cbind(pred()$genetic_merit, testpheno_reactive()[,2]))
        gp_coincidence_plot(df = data, var1 = colnames(data)[1],
                            var2 = colnames(data)[2],
                            q1 = 0.01*(input$selectpct))
        })#End of coin_plt

    output$gp_accuracy1 <- shiny::renderPlot({
      print(coin_plt())
    })

    ## Create and download histogram
    output$gphist1 <- shiny::renderPlot({
      # shiny::req(pred(), input$selectpct)
      # plot_histogram(y_vector = pred()[,2], percent = input$selectpct)
      print(test_selected()[[2]])
    })

    output$gphist2 <- shiny::renderPlot({
      # shiny::req(pred(), input$selectpct)
      # plot_histogram(y_vector = pred()[,2], percent = input$selectpct)
      print(test_selected()[[2]])
    })

    output$downloadGPHist <- shiny::downloadHandler(
      filename = function() {
        paste0("Genomic_prediction_histogram_", Sys.Date(), ".jpeg")
      },
      content = function(file) {
        shiny::req(pred(), input$selectpct)
        ggplot2::ggsave(file, plot = test_selected()[[2]],
                        device = "jpeg", width = 4, height = 4, dpi = 300)
        # ggplot2::ggsave(file, plot = plot_histogram(y_vector = pred()[,2], percent = input$selectpct),
        #                 device = "jpeg", width = 4, height = 4, dpi = 300)
      }
    )

    ## print selected individuals based on user-defined selection intensity
    output$summary_pred1 <- DT::renderDT({ #Using simulated data
      shiny::req(pred(), input$selectpct)
      test_selected()[[1]]
    })

    output$summary_pred2 <- DT::renderDT({ #Using own data
      shiny::req(pred(), input$selectpct)
      test_selected()[[1]]
    })

    ## Download genomic prediction results as a CSV file.
    output$downloadGP <- shiny::downloadHandler(
      filename = function() {
        paste0("Genomic_prediction_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        shiny::req(pred())
        utils::write.csv(pred(), file, row.names = FALSE)
      }
    )
    ###### End of server for Genomic Prediction ++++++ ######



    ##########################---------- RESET APP---------##################################
    shiny::observeEvent(input$reset, {
      rm(list = ls())
      session$reload()
    }) # For the Standard panel

  }

  ########################################################################################################################
  ######################### ---------- The End! Let us run the app! --------- ############################################
  shiny::shinyApp(ui = ui, server = server)
}
