load("shiny_input.RData")

shinyServer(function(input, output, session) {
    # define default selection
    rv <- reactiveValues(
        # for tab "Metabolites" and others
        go1 = 0,
        selected.methods = unique(input_df$Methods),
        selected.tissues = c("Liver", "HEK", "HL60", "Bone Marrow"),
        # for tab "Kit Overview"
        go2 = 0,
        selected.method = unique(input_df$Methods)[1],
        selected.tissue = "Liver",
        selected.performance = FALSE 
    )
    
# Sidebar input
################################################################################
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
            ## input: tissue, method, bestPerformance, applyFilters2 ##
            
            # Single tissue
            output$tissuesInput <- renderUI({
                radioButtons("tissue",
                             label = h3("Select tissue"),
                             choices = list("Liver",
                                            "HEK",
                                            "HL60",
                                            "Bone Marrow"),
                             selected = rv$selected.tissue)
            })
            # Single method
            output$methodsInput <- renderUI({
                selectizeInput("method",
                               label = h3("Select method"),
                               choices = unique(input_df$Methods),
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
            ## input: tissues, methods, applyFilters ##
            
            # Multiple tissues
            output$tissuesInput <- renderUI({
                checkboxGroupInput("tissues",
                                   label = h3("Select tissues"),
                                   choices = list("Liver" = "Liver", 
                                                  "HEK" = "HEK", 
                                                  "HL60" = "HL60", 
                                                  "Bone Marrow" = "Bone Marrow"),
                                   selected = rv$selected.tissues)
            })
            # Multiple methods
            output$methodsInput <- renderUI({
                selectizeInput("methods",
                               label = h3("Select methods"),
                               choices = unique(input_df$Methods),
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
    
    # Show Kit Overview
    observeEvent(input$kit_overview, {
        updateTabsetPanel(session, "tabs", selected = "Kit Overview")
    })
    
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
    
    # save selection for different tabs
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
    
# Help text
################################################################################
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
                                 <br><br>Note: LOD filtering is disabled for this
                                 tab since statistical analysis could not be
                                 performed for metabolites where all methods were
                                 in the LOD range.")
                        )
                    )
                )
            }
        })
    })
    
    

# Pie chart
################################################################################
    
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
    
    output$plot <- renderPlot({
        ggplot(pie_input_df(), aes(x="", y=Weight, fill=Class)) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start=0) +
            scale_fill_manual(values = col_vector) + theme_void() + 
            theme(axis.text.x=element_blank()) +
            geom_label_repel(aes(label = Weight, y = text_y), size=4, 
                             show.legend = F, nudge_x = 0.3)
    })
    
# Filter input 
################################################################################
    filtered_input_df <- eventReactive(c(rv$initial.calc1, rv$go1), {
        req(input$tissues, input$methods)
        temp_input_df <- input_df %>%
            filter(LOD != input$lod,
                   CV <= input$cv,
                   Tissue %in% input$tissues,
                   Methods %in% input$methods,
                   Class %in% input$class) %>% 
            droplevels()
        temp_input_df})
    
# Prep raw data table
################################################################################
    filtered_table_df <- eventReactive(c(rv$initial.calc1, rv$go1), {
        req(input$tissues, input$methods)
        if(input$cv == 1.75){
            temp_input_df <- table_df %>%
                filter(LOD != input$lod,
                       Tissue %in% input$tissues,
                       Method %in% input$methods,
                       Class %in% input$class) %>% 
                droplevels()
        }else{
            temp_input_df <- table_df %>%
                filter(LOD != input$lod,
                       CV <= input$cv,
                       Tissue %in% input$tissues,
                       Method %in% input$methods,
                       Class %in% input$class) %>% 
                droplevels()
        }
        temp_input_df <- dplyr::select(temp_input_df, c(Metabolite, Method,
                                                        Tissue, Class, Replicate, 
                                                        Concentration))
        
        temp_input_df})
    
# Barplot showing number of metabolites above LOD
################################################################################
    bar_input_df <- reactive({
        temp_input_df <- group_by(filtered_input_df(), Methods, Class, Tissue) %>%
            dplyr::summarise(Number = n()) %>%
            # summarise(Number = floor(sum(LOD))) %>%
            data.frame()
        temp_input_df})
    
    output$plot2 <- renderPlotly({
        print(ggplotly(ggplot(bar_input_df(), 
                              aes(x = Methods, y = Number, fill = Class)) +
                           geom_bar(stat = "identity") +
                           scale_fill_manual(values = col_vector) +
                           theme_light() +
                           theme(legend.position = "bottom", 
                                 axis.text.x = element_text(angle = 45, 
                                                            hjust = 1, 
                                                            size = 8)) +
                           # ylim(c(0, 520)) +
                           xlab("") +
                           facet_grid(~ Tissue)))
    })
    
# Barplot showing number of metabolites for best extraction method
################################################################################
    bar2_input_df <- reactive({
        temp_input_df <- filtered_input_df() %>%
            filter(LOD != 0,
                   !is.na(Group)) %>%
            droplevels() %>%
            group_by(Methods, Class, Tissue) %>%
            dplyr::summarise(Number = length(grep("A", Group))) %>%
            data.frame()
        temp_input_df
    })
    
    output$plot2b <- renderPlotly({
        print(ggplotly(ggplot(bar2_input_df(), 
                              aes(x = Methods, y = Number, fill = Class)) +
                           geom_bar(stat = "identity") +
                           scale_fill_manual(values = col_vector) +
                           theme_light() +
                           theme(legend.position = "bottom", 
                                 axis.text.x = element_text(angle = 45, 
                                                            hjust = 1, 
                                                            size = 8)) +
                           xlab("") +
                           facet_grid(~ Tissue)))
    })
    
# Concentration Boxplots
################################################################################
    observeEvent(c(rv$initial.calc2, rv$go2), {
        req(input$tissue, input$method, input$bestPerformance)
        rv$plot_index <- 1 # refresh index for new input
        rv$ylab <- ifelse(input$tissue == "Liver",
                          "Concentration [pmol/mg tissue]",
                          "Concentration [pmol/10^6 cells]")
        temp_input_df <- input_df %>%
            filter(LOD != input$lod,
                   CV <= input$cv,
                   Tissue == input$tissue,
                   Methods == input$method,
                   Class %in% input$class,
                   !is.na(Group)) %>%
            droplevels()
        all.metabolites <- as.character(temp_input_df$Metabolite)
        if (input$bestPerformance == TRUE) {
            selected.metabolites <- all.metabolites[grepl("A", temp_input_df$Group)]
        } else {
            selected.metabolites <- all.metabolites[!grepl("A", temp_input_df$Group)]
        }
        if (length(selected.metabolites) == 0) {
            rv$s <- 0
            rv$e <- 0
            rv$max_len <- 0
            rv$box_input_df <- NULL
        } else {
            tmp_raw_df <- raw_df[grep(input$tissue, rownames(raw_df)), ]
            methods <- unique(sapply(str_split(rownames(tmp_raw_df), "\\."), "[", 1))
            plot_data <- lapply(selected.metabolites, function(metabolite) {
                out_df <- lapply(methods, function(m) {
                    values <- tmp_raw_df %>%
                        filter(grepl(m, rownames(tmp_raw_df))) %>%
                        select(all_of(metabolite)) %>%
                        unlist()
                    groups <- input_df %>%
                        filter(Tissue == input$tissue,
                               Methods == m,
                               Metabolite == metabolite) %>%
                        select(Group) %>%
                        unlist()
                    tmp_df <- data.frame(
                        Metabolite = factor(metabolite, levels = selected.metabolites),
                        Method = factor(m, levels = methods),
                        Concentration = values,
                        Group = groups,
                        LabelPos = max(values))
                    return(tmp_df)
                }) %>%
                    bind_rows()
                out_df <- mutate(out_df, LabelPos = LabelPos + 0.1*max(LabelPos))
                return(out_df)
            }) %>%
                bind_rows()
            rv$max_len <- length(unique(plot_data$Metabolite))
            rv$box_input_df <- plot_data
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
    
    output$plot8 <- renderPlotly({
        print(ggplotly(ggplot(box_input_sub(),
                              aes(x = Method, y = Concentration, fill = Method)) +
                           geom_boxplot() +
                           scale_fill_manual(values = col_vector_methods) +
                           geom_text(data = box_input_sub() %>%
                                         group_by(Metabolite, Method) %>%
                                         summarise(Group = unique(Group),
                                                   LabelPos = unique(LabelPos)),
                                     aes(x = Method, y = LabelPos, label = Group),
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
    
    output$plotlyUI <- renderUI({
        plotlyOutput("plot8", height = rv$height, width = rv$width) %>%
            withSpinner(color="#428bca")
    })
    
# CV plot
################################################################################
    output$plot3 <- renderPlotly({
        print(ggplotly(ggplot(filtered_input_df(), aes(CV)) +
                           geom_histogram(aes(fill = Tissue), alpha = 0.5, 
                                          position = "identity", binwidth = 0.05) +
                           facet_wrap(~ Methods, ncol = 2, strip.position = "right") +
                           # ggtitle("All metabolites") +
                           theme_bw() +
                           theme(strip.text.y = element_text(angle = 270, size = 6)) +
                           scale_fill_manual(values = col_vector_tissue) +
                           ylab("")))
    })
    
# Spectra
################################################################################
    output$plot4 <- renderPlot({
        ggplot(filtered_input_df(), aes(x = Metabolite, y = Mean, fill = Class)) +
            geom_bar(stat = "identity") +
            scale_fill_manual(values = col_vector) +
            facet_grid(Methods ~ Tissue) +
            theme_bw() +
            scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) +
            ylab(expression(paste("Mean Concentration [pmol/mg] or [pmol/", 10^6, " cells]"))) +
            theme(axis.text.x = element_blank(), 
                  axis.ticks.length.x = unit(0, "cm"),
                  panel.grid.major= element_line(color = "white"), 
                  panel.grid.minor.y = element_blank(),
                  strip.text.y = element_text(angle = 270, size = 8.5),
                  strip.text.x = element_text(size = 10),
                  legend.position = "bottom")
    })
    
# Summary plot
################################################################################
    sum_input_df <- reactive({
        sum_temp_df <- group_by(filtered_input_df(), Tissue, Methods) %>% 
            dplyr::summarise(Number = sum(LOD, na.rm = TRUE))
        median_temp_df <- group_by(filtered_input_df(), Tissue, Methods) %>% 
            dplyr::summarise(Median = median(CV, na.rm = TRUE),
                             MAD = mad(CV, na.rm = TRUE)) %>% 
            data.frame()
        temp_output_df <- data.frame(Tissue = sum_temp_df$Tissue,
                                     Methods = sum_temp_df$Methods,
                                     Number_Metabolites = sum_temp_df$Number,
                                     Median_CV = median_temp_df$Median,
                                     MAD_CV = median_temp_df$MAD)
    })
    
    output$plot5 <- renderPlotly({
        print(ggplotly(ggplot(sum_input_df(), aes(Methods, Number_Metabolites)) +
                           geom_bar(aes(fill = Tissue), position = "dodge", 
                                    stat="identity") +
                           xlab("") +
                           ylab("#") +
                           theme_light() +
                           theme(legend.position = "bottom", 
                                 axis.text.x = element_text(angle = 45, 
                                                            hjust = 1)) +
                           scale_fill_manual(values = col_vector_tissue) +
                           ylim(0, 500)))
    })
    
    output$plot6 <- renderPlotly({
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
                           scale_fill_manual(values = col_vector_tissue)))
    })
    
# Concentrations over replicates
################################################################################
    rep_input_df <- reactive({
        temp_input_df <- dplyr::select(filtered_input_df(), 
                                       c(Methods, SizeA, SizeB, SizeC, Tissue)) %>%
            distinct() %>%
            group_by(Methods, Tissue) %>%
            gather(Replicate, Sum, -c(Methods, Tissue))
        temp_input_df$Replicate <- ifelse(temp_input_df$Replicate == "SizeA", "R1",
                                          ifelse(temp_input_df$Replicate == "SizeB", 
                                                 "R2", "R3"))
        temp_input_df})
    
    output$plot7 <- renderPlot({
        suppressWarnings(print(ggplot(rep_input_df(), aes(x = Tissue, 
                                                          y = Sum, 
                                                          fill = Replicate)) +
                                   geom_bar(position = "dodge", 
                                            stat = "identity") +
                                   facet_wrap(~ Methods, ncol = 2) +
                                   theme_bw() +
                                   ylab("Sum of concentrations") +
                                   xlab("") +
                                   scale_fill_manual(values = c("grey40", 
                                                                "grey60", 
                                                                "grey80"))))
    })
    
    # Metabolites in classes
    output$metaboliteclasses <- DT::renderDataTable(
        pie_table_df(),
        server = FALSE,
        selection = "none",
        options = list(dom = "Blfrtip",
                       buttons = c("copy", "excel", "pdf")),
        extensions = "Buttons"
    )
    
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
    
    # Generate a summary of the data ----
    output$table <- DT::renderDataTable(
        filtered_table_df(),
        rownames= FALSE,
        server = FALSE,
        selection = "none",
        options = list(pageLength = 25,
                       dom = "Blfrtip",
                       buttons = c("copy", "excel", "pdf")),
        extensions = "Buttons"
    )
})