library(shiny)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(tibble)
library(magrittr)
library(ggrepel)
library(scales)
library(stringr)
library(shinycssloaders)
options(dplyr.summarise.inform = FALSE)

load("shiny_input.RData")

# Define UI for random distribution app ----
shinyUI(fluidPage(
    
    # App title ----
    div(
        titlePanel("MetaboExtract"),
        style = "
        position:fixed;
        width:100%;
        "
    ),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            style = "
            overflow-y: scroll;
            max-height: 90%;
            position: fixed;
            width: 23%;
            margin-top: 60px;
            ",
            
            # Inlcude help text
            checkboxInput(inputId = "help",
                          label = HTML("<b>Show help text</b>")),
            
            # Input: Select tissues
            uiOutput("tissuesInput"),
            br(),
            
            # Input: Select methods
            uiOutput("methodsInput"),
            uiOutput("bestSelection"),
            br(),
            
            # Input: Select metabolite class
            selectizeInput("class", 
                           label = h3("Select class"), 
                           choices = unique(input_df$Class),
                           selected = unique(input_df$Class), 
                           multiple = TRUE, 
                           options = NULL),
            actionButton(inputId = "kit_overview",
                         label = tags$strong("View classes"),
                         width = "100%",
                         style = "color: #fff;
                         background-color: #337ab7;
                         border-color: #2e6da4"),
            br(),
            # Select CV
            sliderInput("cv",
                        h3("Select CV limit"),
                        value = 0.5,
                        min = 0,
                        max = signif(max(input_df$CV, na.rm = TRUE), 3) + 0.02),
            br(),
            # Select LOD
            uiOutput("selectLOD"),
            # Activation button
            br(),
            uiOutput("calcInput"),
            width = 3
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            style = "
            overflow-y: scroll;
            max-height: 90%;
            position: fixed;
            width: 75%;
            margin-left: 25%;
            margin-top: 60px;
            ",
            
            h5("This is a resource to compare different 
                                    extraction methods among four tissues for
                                    intracellular metabolic measurements."),
            h5("Reference: xxx et al. (2021) Comparison of 
                                    extraction methods for intracellular 
                                    metabolomics"),
            br(),
            HTML("<b>Click Show help text for more information.</b><br>"),
            br(),
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        id = "tabs",
                        tabPanel("Statistics",
                                 br(),
                                 uiOutput("help.text_met"),
                                 h2("Number of detectable metabolites"),
                                 h4("Number of metabolites across extraction 
                                    methods and stratified by class"), 
                                 uiOutput("help.text_stat_1"),
                                 plotlyOutput("plot2") %>%
                                     withSpinner(color="#428bca"), # loading spinner
                                 h4("Number of metabolites across extraction 
                                    methods and stratified by tissue"), 
                                 uiOutput("help.text_stat_2"),
                                 plotlyOutput("plot5", width = "70%") %>%
                                     withSpinner(color="#428bca"),
                                 h2("Coefficient of Variation (CV)"),
                                 h4("Distribution of CVs from technical 
                                    triplicates."),
                                 plotlyOutput("plot3", width = "70%", 
                                              height = "120%") %>%
                                     withSpinner(color="#428bca"),
                                 h4("Variability of CVs"),
                                 plotlyOutput("plot6", width = "70%") %>%
                                     withSpinner(color="#428bca")),
                        tabPanel("Concentration",
                                 br(),
                                 h2("Concentration comparison between methods"),
                                 uiOutput("help.text_conc"),
                                 br(),
                                 column(width = 12, tabsetPanel(
                                     type = "tabs",
                                     id = "sub_tabs",
                                     tabPanel("Overview", fluidRow(
                                         h4("Number of optimal extracted metabolites
                                            across extraction methods and stratified
                                            by class."),
                                         plotlyOutput("plot2b") %>%
                                             withSpinner(color="#428bca")
                                     )),
                                     tabPanel("Single view", fluidRow(
                                         h4("Concentrations of single metabolites
                                            measured across extraction methods."),
                                         column(
                                             width = 3,
                                             style = "margin-top: 10px;",
                                             actionButton(inputId = "previous6",
                                                          label = tags$strong("Previous 6"),
                                                          width = "100%",
                                                          style = "color: #fff;
                                                  background-color: #337ab7;
                                                  border-color: #2e6da4")
                                         ),
                                         column(
                                             width = 3,
                                             style = "margin-top: 10px;",
                                             actionButton(inputId = "next6",
                                                          label = tags$strong("Next 6"),
                                                          width = "100%",
                                                          style = "color: #fff;
                                                  background-color: #337ab7;
                                                  border-color: #2e6da4")
                                         ),
                                         column(
                                             width = 6,
                                             uiOutput("plot_numbers")
                                         ),
                                         column(
                                             width = 12,
                                             br()
                                         ),
                                         column(
                                             width = 12,
                                             uiOutput("plotlyUI")
                                         )
                                     ))
                                 ))),
                        tabPanel("Spectra", 
                                 h2("Spectra of concentrations"),
                                 h4("Concentrations measured across tissue types
                                    and methods"),
                                 uiOutput("help.text_spec_1"),
                                 plotOutput("plot4", height = "1000px") %>%
                                     withSpinner(color="#428bca")),
                        tabPanel("Replicates", 
                                 h2("Comparison of Replicates"),
                                 h4("Sum of concentrations show global differences 
                                    between replicates"),
                                 uiOutput("help.text_rep_1"),
                                 plotOutput("plot7", width = "60%") %>%
                                     withSpinner(color="#428bca"),
                                 h2("Table"),
                                 dataTableOutput("rep_table") %>%
                                     withSpinner(color="#428bca")),
                        tabPanel("Table", 
                                 HTML("<h4>Metabolite concentrations are either
                                      given as pmol/10<sup>6</sup> cells (HEK, 
                                      HL-60 and bone marrow) or pmol/mg (liver).
                                      </h4>"),
                                 uiOutput("help.text_table_1"),
                                 br(),
                                 dataTableOutput("table") %>%
                                     withSpinner(color="#428bca")),
                        tabPanel("Kit Overview", 
                                 h2("Classes of Metabolites in Kit"),
                                 h4("The Biocrates MxP 500 Quant Kit can quantify 
                                    up to 630 metabolites of different classes."),
                                 plotOutput("plot") %>%
                                     withSpinner(color="#428bca"),
                                 h2("Table"),
                                 h4("List of all metabolites covered."),
                                 dataTableOutput("metaboliteclasses") %>%
                                     withSpinner(color="#428bca"))
            )
        )
    )
))

