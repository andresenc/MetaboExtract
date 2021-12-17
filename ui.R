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
            
            # Input: Select data set
            radioButtons("data_set", 
                         label = h3("Select data set"),
                         choiceNames = list(HTML("Andresen <i>et al.</i>"),
                                            HTML("Gegner <i>et al.</i>")),
                         choiceValues = list("cells", "organisms"),
                         selected = c("cells")),
            br(),
            
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
                           choices = unique(input_df_cells$Class),
                           selected = unique(input_df_cells$Class), 
                           multiple = TRUE, 
                           options = NULL),
            actionButton(inputId = "kit_overview",
                         label = "View classes",
                         width = "100%",
                         style = "color: #333333;
                         background-color: #E2E3E5;
                         border-color: #CCCBCD"),
            br(),
            # Select CV
            uiOutput("selectCV"),
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
            HTML("<h5>Reference: Andresen <i>et al.</i> (2021) Comparison of 
                                    extraction methods for intracellular 
                                    metabolomics</h5>"),
            br(),
            HTML("<b>Click \"Show help text\" for more information.</b><br>"),
            br(),
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        id = "tabs",
                        # Tab: Statistics ----
                        tabPanel("Statistics",
                                 br(),
                                 uiOutput("help.text_met"),
                                 h2("Number of detectable metabolites"),
                                 h4("Number of metabolites across extraction 
                                    methods and stratified by class"),
                                 uiOutput("help.text_stat_1"),
                                 plotlyOutput("detec_metabo") %>%
                                     withSpinner(color="#428bca"), # loading spinner
                                 h4("Number of metabolites across extraction
                                    methods and stratified by tissue"),
                                 uiOutput("help.text_stat_2"),
                                 plotlyOutput("sum_detec_metabo", width = "70%") %>%
                                     withSpinner(color="#428bca"),
                                 h2("Coefficient of Variation (CV)"),
                                 h4("Distribution of CVs from technical
                                    triplicates."),
                                 plotlyOutput("CV_dist", width = "70%",
                                              height = "120%") %>%
                                     withSpinner(color="#428bca"),
                                 h4("Variability of CVs"),
                                 plotlyOutput("var_CV", width = "70%") %>%
                                     withSpinner(color="#428bca")),
                        # Tab: Concentration ----
                        tabPanel("Concentration",
                                 br(),
                                 h2("Concentration comparison between methods"),
                                 uiOutput("help.text_conc"),
                                 br(),
                                 column(width = 12, tabsetPanel(
                                     type = "tabs",
                                     id = "sub_tabs",
                                     tabPanel("Overview", fluidRow(
                                         h4("Number of metabolites with the highest yield
                                            across extraction methods and stratified
                                            by class."),
                                         plotlyOutput("bar_high_yield") %>%
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
                                             uiOutput("box_plots_UI")
                                         )
                                     ))
                                 ))),
                        # Tab: Spectra ----
                        tabPanel("Spectra",
                                 h2("Spectra of concentrations"),
                                 h4("Concentrations measured across tissue types
                                    and methods"),
                                 uiOutput("help.text_spec_1"),
                                 plotOutput("spectra", height = "1000px") %>%
                                     withSpinner(color="#428bca")),
                        # Tab: Replicates ----
                        tabPanel("Replicates",
                                 h2("Comparison of Replicates"),
                                 h4("Sum of concentrations show global differences
                                    between replicates"),
                                 uiOutput("help.text_rep_1"),
                                 plotOutput("bar_replicates", width = "60%") %>%
                                     withSpinner(color="#428bca"),
                                 h2("Table"),
                                 dataTableOutput("rep_table") %>%
                                     withSpinner(color="#428bca")),
                        # Tab: Table ----
                        tabPanel("Table",
                                 uiOutput("table_title"),
                                 uiOutput("help.text_table_1"),
                                 br(),
                                 dataTableOutput("data_sum_table") %>%
                                     withSpinner(color="#428bca")),
                        # Tab: Kit Overview ----
                        tabPanel("Kit Overview",
                                 h2("Classes of Metabolites in Kit"),
                                 h4("The Biocrates MxP 500 Quant Kit can quantify
                                    up to 630 metabolites of different classes."),
                                 plotOutput("pie_chart") %>%
                                     withSpinner(color="#428bca"),
                                 h2("Table"),
                                 h4("List of all metabolites covered."),
                                 dataTableOutput("metabolite_classes") %>%
                                     withSpinner(color="#428bca"))
            )
        )
    )
))

