load("shiny_input.RData")

shinyServer(function(input, output, session) {
    # define default selection
    rv <- reactiveValues(
        # for tab "Metabolites" and others
        go1 = 0,
        # for tab "Kit Overview"
        go2 = 0,
        selected.performance = FALSE
    )


# Data & UI: Switch between data sets -------------------------------------

# Reactive:
#   input$data_set
#
# Input:
#   input_df_cells
#
#   input_df_organisms
#  
# Output:
#   rv$input_df
#   rv$table_df
#
#   rv$selected.methods
#   rv$selected.tissues
#   rv$selected.method
#   rv$selected.tissue 
    
    observeEvent(input$data_set, {
        if (input$data_set == "cells") {
            ## input_df
            rv$input_df <- input_df_cells %>%
                ungroup() %>%
                group_by(Tissue, Class, Metabolite, Method, Methods, Mean, CV,
                         Group, LOD) %>%
                dplyr::summarise()
            ## table_df
            rv$table_df <- input_df_cells %>%
                ungroup() %>%
                select(Metabolite, Methods, Tissue, Class, Replicate, Concentration,
                       LOD, CV, Sum) %>%
                rename(Method = Methods)
            ## conc_df
            rv$conc_df <- input_df_cells %>%
                ungroup %>%
                select(Tissue, Class, Metabolite, Method, Methods, Concentration,
                       Group, CV)
            
        } else if (input$data_set == "organisms") {
            ## input_df
            rv$input_df <- input_df_organisms %>%
                ungroup() %>%
                group_by(Tissue, Class, Metabolite, Method, Methods, Mean, CV,
                         Group, LOD) %>%
                dplyr::summarise()
            ## table_df
            rv$table_df <- input_df_organisms %>%
                ungroup() %>%
                select(Metabolite, Methods, Tissue, Class, Replicate, Concentration,
                       LOD, CV, Sum) %>%
                rename(Method = Methods)
            ## conc_df
            rv$conc_df <- input_df_organisms %>%
                ungroup %>%
                select(Tissue, Class, Metabolite, Method, Methods, Concentration,
                       Group, CV)
        }
        
        ## default selection
        rv$selected.methods <- unique(rv$input_df$Methods) # for input$methods
        rv$selected.tissues <- unique(rv$input_df$Tissue) # for input$tissues
        rv$selected.method <- unique(rv$input_df$Methods)[1] # for input$method
        rv$selected.tissue <- unique(rv$input_df$Tissue)[1] # for input$tissue
    })
    
    
# Reactive:
#   rv$initial.calc1
#   rv$go1
#
# Input:
#   col_vector_methods_cells
#   col_vector_tissues_cells
#
#   col_vector_methods_organisms
#   col_vector_tissues_organisms
#  
# Output:
#  
#   rv$col_vector_methods
#   rv$col_vector_tissue

    observeEvent(c(rv$initial.calc1, rv$go1), {
        if (input$data_set == "cells") {
            ## color vectors
            rv$col_vector_methods <- col_vector_methods_cells
            rv$col_vector_tissue <- col_vector_tissues_cells
            
        } else if (input$data_set == "organisms") {
            ## color vectors
            rv$col_vector_methods <- col_vector_methods_organisms
            rv$col_vector_tissue <-col_vector_tissues_organisms
        }
    })


# UI: Sidebar input depending on tab --------------------------------------

# Reactive:
#   input$tabs
#   input$sub_tabs
    
    observeEvent(c(input$tabs, input$sub_tabs), {
        if (input$tabs == "Concentration") {
            # Select LOD is disabled for tab "Concentration"
            output$selectLOD <- NULL
        } else {
            # Select LOD
            output$selectLOD <- renderUI({
                radioButtons("lod", 
                             label = h3("Filter based on LOD"), 
                             choices = list("Yes" = 0, "No" = "dummy"),
                             selected = c(0))
            })
        }
        if (input$tabs == "Concentration" & input$sub_tabs == "Single view") {
            # Output:
            #   input$tissue
            #   input$method
            #   input$bestPerformance
            #   input$applyFilters2
            
            # Single tissue
            output$tissuesInput <- renderUI({
                radioButtons("tissue",
                             label = h3("Select tissue"),
                             choices = unique(rv$input_df$Tissue),
                             selected = rv$selected.tissue)
            })
            # Single method
            output$methodsInput <- renderUI({
                selectizeInput("method",
                               label = h3("Select method"),
                               choices = unique(rv$input_df$Methods),
                               selected = rv$selected.method, 
                               multiple = FALSE, 
                               options = NULL)
            })
            # Performance selection
            output$bestSelection <- renderUI({
                radioButtons("bestPerformance",
                             label = "",
                             choices = list("Selected method performs best" = TRUE,
                                            "Selected method performs not optimal" = FALSE),
                             selected = rv$selected.performance)
            })
            # Action button ID
            actionID <- "applyFilters2"
        } else {
            # Output:
            #   input$tissues
            #   input$methods
            #   input$applyFilters
            
            # Multiple tissues
            output$tissuesInput <- renderUI({
                checkboxGroupInput("tissues",
                                   label = h3("Select tissues"),
                                   choices = as.list(as.character(unique(rv$input_df$Tissue))),
                                   selected = rv$selected.tissues)
            })
            # Multiple methods
            output$methodsInput <- renderUI({
                selectizeInput("methods",
                               label = h3("Select methods"),
                               choices = unique(rv$input_df$Methods),
                               selected = rv$selected.methods, 
                               multiple = TRUE, 
                               options = NULL)
            })
            # Performance selection
            output$bestSelection <- NULL
            # Action button ID
            actionID <- "applyFilters1"
        }
        # Render action button
        output$calcInput <- renderUI({
            actionButton(inputId = actionID,
                         label = tags$strong("Apply filters"),
                         width = "100%", 
                         style="color: #fff;
                         background-color: #337ab7; 
                         border-color: #2e6da4")
        })
    })
    

# UI: Sidebar input depending on dataset ----------------------------------

# Reactive:
#   rv$input_df
    
    observeEvent(rv$input_df, {
        req(rv$input_df)
        output$selectCV <- renderUI({
            sliderInput("cv",
                        h3("Select CV limit"),
                        value = 0.5,
                        min = 0,
                        max = signif(max(rv$input_df$CV, na.rm = TRUE), 3) + 0.02)
        })
    })

    
# UI: Show Kit Overview ---------------------------------------------------

# Reactive:
#   input$kit_overview
    
    observeEvent(input$kit_overview, {
        updateTabsetPanel(session, "tabs", selected = "Kit Overview")
    })
    

# Reactive: Trigger reactive calculation ----------------------------------

    # Action buttons counter will be reset to 0 each time they are newly rendered 
    # (when changing the tab). An extra counter won't be reset and only triggers
    # new calculation when really needed
    observeEvent(input$applyFilters1, {
        rv$go1 <- rv$go1 + 1
    })
    observeEvent(input$applyFilters2, {
        rv$go2 <- rv$go2 + 1
    })
    # trigger initial calculation after reactive UI has been rendered
    observe({
        req(input$tissues, input$methods)
        rv$initial.calc1 <- 1
    })
    observe({
        req(input$tissue, input$method, input$bestPerformance)
        rv$initial.calc2 <- 1
    })
    

# UI: Save selection for different tabs -----------------------------------
    
    observeEvent(rv$go1, {
        req(input$tissues, input$methods)
        rv$selected.methods <- input$methods
        rv$selected.tissues <- input$tissues
    })
    observeEvent(rv$go2, {
        req(input$tissue, input$method, input$bestPerformance)
        rv$selected.method <- input$method
        rv$selected.tissue <- input$tissue
        rv$selected.performance <- input$bestPerformance
    })
    

# UI: Help text -----------------------------------------------------------
    
    observeEvent(input$help, {
        output$help.text_met <- renderUI({
            if (input$help) {
                return(
                    fluidRow(
                        column(
                            width = 12,
                            HTML("This ShinyApp allows to explore ten extraction 
                                 methods for metabolic measurements of four 
                                 human tissues or cell lines (liver, bone 
                                 marrow, HEK and HL-60) using the Biocrates MxP 
                                 500 Quant Kit.<br><br>
                                 Check out the reference publication for detailed 
                                 information and extraction protocols.<br><br>
                                 Tissues, methods and classes of metabolites can 
                                 be (de)selected to focus on the data of interest 
                                 and the maximal coefficient of variation (CV) 
                                 between triplicates can be chosen. 
                                 Additionally, values below the limit of 
                                 detection (LOD) can be included. Though it is 
                                 highly recommended to use the LOD for 
                                 filtering.<br><br>"),
                            # HTML("<p><img src="Extraction.png"/></p><br><br>"),
                            HTML("<b>Overview:</b><br><br>"),
                            renderTable(data.frame("Tab" = c("Statistics", 
                                                             "Concentration", 
                                                             "Spectra", 
                                                             "Replicates", 
                                                             "Table",
                                                             "Kit Overview"),
                                                   "Content" = c("explore the number of detectable metabolites
                                                                 and the variablity between triplicates", 
                                                                 "explore which extraction methods allows to detect the 
                                                                 highest concentrations", 
                                                                 "explore the metabolite concentrations across 
                                                                 methods and tissue types", 
                                                                 "explore global variation between technical 
                                                                 replicates", 
                                                                 "explore and download raw data",
                                                                 "explore the metabolites and classes of metabolites 
                                                                 which are covered by the Biocrates MxP 500 Quant Kit")))
                        )
                    )
                )
            }
        })
        output$help.text_stat_1 <- renderUI({
            if (input$help) {
                return(
                    fluidRow(
                        column(
                            width = 12,
                            HTML("Note: the number depends on CV and LOD 
                            settings. Some metabolites are not detected 
                            at all.")
                        )
                    )
                )
            }
        })
        output$help.text_stat_2 <- renderUI({
            if (input$help) {
                return(
                    fluidRow(
                        column(
                            width = 12,
                            HTML("Note: the number depends on CV and LOD 
                            settings. Some metabolites are not detected 
                            all.")
                        )
                    )
                )
            }
        })
        output$help.text_spec_1 <- renderUI({
            if (input$help) {
                return(
                    fluidRow(
                        column(
                            width = 12,
                            HTML("Note: concentrations are displayed in log 
                                 scale. Concentrations are either
                                 given as pmol/10<sup>6</sup> cells (HEK, 
                                 HL-60 and bone marrow) or pmol/mg (liver).")
                        )
                    )
                )
            }
        })
        output$help.text_rep_1 <- renderUI({
            if (input$help) {
                return(
                    fluidRow(
                        column(
                            width = 12,
                            HTML("Note: MeOH/ChCl2-HEK was measured in 
                            duplicates due to quality-based filtering.")
                        )
                    )
                )
            }
        })
        output$help.text_table_1 <- renderUI({
            if (input$help) {
                return(
                    fluidRow(
                        column(
                            width = 12,
                            HTML("Choose option to download table.")
                        )
                    )
                )
            }
        })
        output$help.text_conc <- renderUI({
            if (input$help) {
                return(
                    fluidRow(
                        column(
                            width = 12,
                            HTML("Optimal extraction methods were determined for
                                 each metabolite based on the measured concentration.
                                 The method with the highest median yield was
                                 considered optimal. This method is labeled with an
                                 &quot;A&quot; (refer to &quot;Single view&quot;).
                                 Other methods with non-significantly lower
                                 concentrations were also taken into account and
                                 given an &quot;A&quot; in their labeling among
                                 other letters. All other methods were considered
                                 non-optimal and labeled with consecutive letters.
                                 The dotted line shows the number of detectable
                                 metabolites for each sample type.
                                 <br><br>Note: LOD filtering is disabled for this
                                 tab since it is already included in the
                                 statistical analysis. Metabolites where values of
                                 all methods are below LOD were therefore excluded.<br>
                                 CV filtering in \"Single view\" is only applied to
                                 the selected method.")
                        )
                    )
                )
            }
        })
    })
    
    
# Data: Filter input_df ---------------------------------------------------

# Reactive:
#   rv$initial.calc1
#   rv$go1
    
    prefiltered_input_df <- eventReactive(c(rv$initial.calc1, rv$go1), {
        req(input$tissues, input$methods)
        temp_input_df <- rv$input_df %>%
            filter(CV <= input$cv,
                   Tissue %in% input$tissues,
                   Methods %in% input$methods,
                   Class %in% input$class) %>%
            droplevels()
        temp_input_df
    })
    
    filtered_input_df <- reactive({
        filter(prefiltered_input_df(), LOD != input$lod)
    })


# Tab: Statistics ---------------------------------------------------------

    # Barplot showing number of metabolites above LOD
    bar_input_df <- reactive({
        group_by(filtered_input_df(), Methods, Class, Tissue) %>%
            dplyr::summarise(Number = n()) %>%
            data.frame()
        })
    
    output$detec_metabo <- renderPlotly({
        print(ggplotly(ggplot(bar_input_df(),
                              aes(x = Methods, y = Number, fill = Class)) +
                           geom_bar(stat = "identity") +
                           scale_fill_manual(values = col_vector_classes) +
                           theme_light() +
                           theme(legend.position = "bottom",
                                 axis.text.x = element_text(angle = 45,
                                                            hjust = 1,
                                                            size = 8)) +
                           xlab("") +
                           facet_grid(~ Tissue)))
    })
    
    # Summary plot
    sum_input_df <- reactive({
        tmp_df <- filtered_input_df() %>%
            group_by(Tissue, Methods) %>%
            dplyr::summarise(Number_Metabolites = n(),
                             Median_CV = median(CV, na.rm = TRUE),
                             MAD_CV = mad(CV, na.rm = TRUE)) %>%
            data.frame()
        tmp_df
    })

    output$sum_detec_metabo <- renderPlotly({
        print(ggplotly(ggplot(sum_input_df(), aes(Methods, Number_Metabolites)) +
                           geom_bar(aes(fill = Tissue), position = "dodge",
                                    stat="identity") +
                           xlab("") +
                           ylab("Number") +
                           theme_light() +
                           theme(legend.position = "bottom",
                                 axis.text.x = element_text(angle = 45,
                                                            hjust = 1)) +
                           scale_fill_manual(values = rv$col_vector_tissue)
                           # ylim(0, 500)
                       ))
    })
    
    # CV plot
    output$CV_dist <- renderPlotly({
        print(ggplotly(ggplot(filtered_input_df(), aes(CV)) +
                           geom_histogram(aes(fill = Tissue), alpha = 0.5,
                                          position = "identity", binwidth = 0.05) +
                           facet_wrap(~ Methods, ncol = 2, strip.position = "right") +
                           # ggtitle("All metabolites") +
                           theme_bw() +
                           theme(strip.text.y = element_text(angle = 270, size = 6)) +
                           scale_fill_manual(values = rv$col_vector_tissue) +
                           ylab("")))
    })
    
    output$var_CV <- renderPlotly({
        print(ggplotly(ggplot(sum_input_df(), aes(Methods, Median_CV)) +
                           geom_bar(aes(fill = Tissue),
                                    position = "dodge",
                                    stat = "identity") +
                           geom_errorbar(aes(ymin = Median_CV,
                                             ymax = Median_CV+MAD_CV,
                                             group = Tissue),
                                         width = .2,
                                         position = position_dodge(.9),
                                         col = "black") +
                           xlab("") +
                           ylab("Median CV + MAD") +
                           theme_light() +
                           theme(legend.position = "bottom",
                                 axis.text.x = element_text(angle = 45,
                                                            hjust = 1)) +
                           scale_fill_manual(values = rv$col_vector_tissue)))
    })


# Tab: Concentration ------------------------------------------------------

    # Barplot showing number of metabolites for best extraction method
    high_yield_df <- reactive({
        temp_input_df <- prefiltered_input_df() %>%
            filter(!is.na(Group)) %>%
            droplevels() %>%
            group_by(Methods, Class, Tissue) %>%
            dplyr::summarise(Number = length(grep("A", Group))) %>%
            data.frame()
        temp_input_df
    })
    
    max_metabolites <- reactive({
        prefiltered_input_df() %>%
            filter(!is.na(Group)) %>%
            group_by(Tissue) %>%
            summarise(max = length(unique(Metabolite)))
    })

    output$bar_high_yield <- renderPlotly({
        print(ggplotly(ggplot(high_yield_df(),
                              aes(x = Methods, y = Number, fill = Class)) +
                           geom_bar(stat = "identity") +
                           geom_hline(data=max_metabolites(),
                                      aes(yintercept=max), linetype = 2) +
                           scale_fill_manual(values = col_vector_classes) +
                           theme_light() +
                           theme(legend.position = "bottom",
                                 axis.text.x = element_text(angle = 45,
                                                            hjust = 1,
                                                            size = 8)) +
                           xlab("") +
                           facet_grid(~ Tissue)))
    })
    
    # Concentration boxplots
    observeEvent(c(rv$initial.calc2, rv$go2), {
        req(input$tissue, input$method, input$bestPerformance)
        rv$plot_index <- 1 # refresh index for new input
        rv$ylab <- ifelse(input$tissue %in% c("HEK", "HL60", "Bone Marrow"),
                          "Concentration [pmol/10^6 cells]",
                          "Concentration [pmol/mg tissue]")
        temp_conc_df <- rv$conc_df %>%
            filter(Tissue == input$tissue,
                   Class %in% input$class,
                   !is.na(Group)) %>%
            droplevels()
        
        tmp_df <- temp_conc_df %>%
            filter(CV <= input$cv,
                   Methods == input$method) %>%
            select(-c(Concentration)) %>%
            droplevels() %>%
            distinct()
            
        metabolites <- tmp_df$Metabolite[grep("A", tmp_df$Group, invert = !as.logical(input$bestPerformance))]

        if (length(metabolites) == 0) {
            rv$s <- 0
            rv$e <- 0
            rv$max_len <- 0
            rv$box_input_df <- NULL
        } else {
            plot_data <- temp_conc_df %>%
                filter(Metabolite %in% metabolites) %>%
                group_by(Metabolite, Method) %>%
                mutate(LabelPos = max(Concentration)) %>%
                group_by(Metabolite) %>%
                mutate(LabelPos = LabelPos + 0.1*max(Concentration)) %>%
                select(Metabolite, Method, Methods, Concentration, Group, LabelPos)
            
            rv$max_len <- length(unique(plot_data$Metabolite))
            rv$box_input_df <- plot_data
        }
        # update colors
        if (input$data_set == "cells") {
            ## color vectors
            rv$col_vector_methods <- col_vector_methods_cells
            rv$col_vector_tissue <- col_vector_tissues_cells
            
        } else if (input$data_set == "organisms") {
            ## color vectors
            rv$col_vector_methods <- col_vector_methods_organisms
            rv$col_vector_tissue <-col_vector_tissues_organisms
        }
    })

    observeEvent(input$next6, {
        max_index <- ceiling(rv$max_len / 6)
        if (rv$plot_index < max_index) {
            rv$plot_index <- rv$plot_index + 1
        }
    })
    observeEvent(input$previous6, {
        if (rv$plot_index > 1) {
            rv$plot_index <- rv$plot_index - 1
        }
    })

    box_input_sub <- eventReactive(c(rv$plot_index, rv$go2), {
        req(rv$box_input_df)
        rv$s <- (rv$plot_index - 1) * 6 + 1
        rv$e <- rv$plot_index * 6
        if (rv$e > rv$max_len) {
            rv$e <- rv$max_len
        }
        if (rv$e - rv$s == 0) {
            # only 1 plot
            rv$width <- "60%"
            rv$ncol <- 1
        } else {
            rv$width <- "100%"
            rv$ncol <- 2
        }
        rv$nrow <- ceiling((rv$e-rv$s+1) / 2)
        rv$height <- paste0(160 + rv$nrow * 180, "px")

        out <- filter(rv$box_input_df,
                      Metabolite %in% unique(rv$box_input_df$Metabolite)[rv$s:rv$e])
        return(out)
    })

    output$plot_numbers <- renderUI({
        req(rv$s, rv$e, rv$max_len)
        if (rv$s < rv$e) {
            return(HTML(paste("<h5>Showing plots<b>", rv$s, "</b>to<b>", rv$e,
                              "</b><br>The total number of plots is<b>",
                              rv$max_len, "</b></h5>")))
        } else {
            return(HTML(paste("<h5>Showing plot<b>", rv$s,
                              "</b><br>The total number of plots is<b>",
                              rv$max_len, "</b></h5>")))
        }

    })

    output$box_plots <- renderPlotly({
        print(ggplotly(ggplot(box_input_sub(),
                              aes(x = Methods, y = Concentration, fill = Methods)) +
                           geom_boxplot() +
                           scale_fill_manual(values = rv$col_vector_methods) +
                           geom_text(data = box_input_sub() %>%
                                         group_by(Metabolite, Methods) %>%
                                         dplyr::summarise(Group = unique(Group),
                                                   LabelPos = unique(LabelPos)),
                                     aes(x = Methods, y = LabelPos, label = Group),
                                     # position = position_dodge(width = 0.9),
                           ) +
                           theme_light() +
                           theme(legend.position = "bottom",
                                 axis.text.x = element_text(angle = 45,
                                                            hjust = 1,
                                                            size = 8),
                                 plot.margin = unit(c(0, 5, 0, 20), "pt")) +
                           xlab("") +
                           ylab(rv$ylab) +
                           facet_wrap(~ Metabolite, scales="free_y",
                                      nrow = rv$nrow, ncol = rv$ncol),
                       tooltip = "text") %>%
                  style(hoverinfo = "none", traces = 1))
    })

    output$box_plots_UI <- renderUI({
        plotlyOutput("box_plots", height = rv$height, width = rv$width) %>%
            withSpinner(color="#428bca")
    })

    
# Tab: Spectra ------------------------------------------------------------

    y_lab <- eventReactive(c(rv$initial.calc1, rv$go1), {
        ifelse(input$data_set == "cells",
               expression(paste("Mean Concentration [pmol/mg] or [pmol/", 10^6, " cells]")),
               "Mean Concentration [pmol/mg]")
    })
    
    output$spectra <- renderPlot({
        ggplot(filtered_input_df(), aes(x = Metabolite, y = Mean, fill = Class)) +
            geom_bar(stat = "identity") +
            scale_fill_manual(values = col_vector_classes) +
            facet_grid(Methods ~ Tissue) +
            theme_bw() +
            scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) +
            ylab(y_lab()) +
            theme(axis.text.x = element_blank(),
                  axis.ticks.length.x = unit(0, "cm"),
                  panel.grid.major= element_line(color = "white"),
                  panel.grid.minor.y = element_blank(),
                  strip.text.y = element_text(angle = 270, size = 8.5),
                  strip.text.x = element_text(size = 10),
                  legend.position = "bottom")
    })

    
# Tab: Replicates ---------------------------------------------------------
    rep_input_df <- reactive({
        temp_input_df <- filtered_table_df() %>%
            ungroup() %>%
            select(c(Tissue, Method, Replicate, Sum)) %>%
            distinct() %>%
            mutate(Replicate = paste0("R", Replicate)) %>%
            arrange(Tissue, Method, Replicate)
        temp_input_df
    })

    output$bar_replicates <- renderPlot({
        suppressWarnings(print(ggplot(rep_input_df(), aes(x = Tissue,
                                                          y = Sum,
                                                          fill = Replicate)) +
                                   geom_bar(position = "dodge",
                                            stat = "identity") +
                                   facet_wrap(~ Method, ncol = 2) +
                                   theme_bw() +
                                   ylab("Sum of concentrations") +
                                   xlab("") +
                                   scale_fill_manual(values = c("grey40",
                                                                "grey60",
                                                                "grey80"))))
    })
    
    # Replicate table
    output$rep_table <- DT::renderDataTable(
        rep_input_df(),
        rownames= FALSE,
        server = FALSE,
        selection = "none",
        options = list(dom = "Blfrtip",
                       buttons = c("copy", "excel", "pdf")),
        extensions = "Buttons"
    )

    
# Tab: Tables -------------------------------------------------------------

    observeEvent(c(rv$initial.calc1, rv$go1), {
        if (input$data_set == "cells") {
            output$table_title <- renderUI({
                HTML("<h4>Metabolite concentrations are either given as
                pmol/10<sup>6</sup> cells (HEK, HL-60 and bone marrow) or
                pmol/mg (liver).</h4>")
            })
        } else {
            output$table_title <- renderUI({
                HTML("<h4>Metabolite concentrations are given as pmol/mg.</h4>")
            })
            
        }
            
    })
    
    # Prep raw data table
    filtered_table_df <- eventReactive(c(rv$initial.calc1, rv$go1), {
        req(input$tissues, input$methods)
        if (input$cv == signif(max(rv$input_df$CV, na.rm = TRUE), 3) + 0.02) {
            temp_input_df <- rv$table_df %>%
                filter(LOD != input$lod,
                       CV <= input$cv,
                       Tissue %in% input$tissues,
                       Method %in% input$methods,
                       Class %in% input$class) %>%
                droplevels()
        } else {
            temp_input_df <- rv$table_df %>%
                filter(LOD != input$lod,
                       CV <= input$cv,
                       Tissue %in% input$tissues,
                       Method %in% input$methods,
                       Class %in% input$class) %>%
                droplevels()
        }
        temp_input_df <- temp_input_df %>%
            dplyr::select(c(Tissue, Method, Class, Metabolite,
                            Replicate, Concentration, Sum)) %>%
            arrange(Tissue, Method, Class, Metabolite, Replicate)

        temp_input_df})
    
    # Generate a summary of the data
    output$data_sum_table <- DT::renderDataTable(
        select(filtered_table_df(), -Sum),
        rownames= FALSE,
        server = FALSE,
        selection = "none",
        options = list(pageLength = 25,
                       dom = "Blfrtip",
                       buttons = c("copy", "excel", "pdf")),
        extensions = "Buttons"
    )

    
# Tab: Kit Overview -------------------------------------------------------
    
    ## Pie chart metabolite overview
    pie_table_df <- eventReactive(c(rv$go1, input$kit_overview[1]), {
        temp_pie_df <- anno_row_df[anno_row_df$Class %in% input$class, ] %>%
            droplevels() %>%
            dplyr::select(c(Metabolite, Name, Class))
        rownames(temp_pie_df) <- NULL
        temp_pie_df})

    pie_input_df <- reactive({
        pie_df <- data.frame(Weight = 1, Class = pie_table_df()$Class) %>%
            group_by(Class) %>%
            dplyr::summarise(Weight = n()) %>%
            arrange(desc(Class)) %>%
            mutate(text_temp = cumsum(Weight),
                   text_y = cumsum(Weight) - Weight/2) %>%
            arrange(Class) %>%
            data.frame
        pie_df})

    output$pie_chart <- renderPlot({
        ggplot(pie_input_df(), aes(x="", y=Weight, fill=Class)) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start=0) +
            scale_fill_manual(values = col_vector_classes) + theme_void() +
            theme(axis.text.x=element_blank()) +
            geom_label_repel(aes(label = Weight, y = text_y), size=4,
                             show.legend = F, nudge_x = 0.3)
    })
    
    ## Metabolites in classes
    output$metabolite_classes <- DT::renderDataTable(
        pie_table_df(),
        server = FALSE,
        selection = "none",
        options = list(dom = "Blfrtip",
                       buttons = c("copy", "excel", "pdf")),
        extensions = "Buttons"
    )
    
})