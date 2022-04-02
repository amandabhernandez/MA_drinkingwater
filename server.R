### Author: AHz
### Date published: 

library(shiny)



# run shiny server
shinyServer(function(input, output) {
    
    #filter water data based on user input
    dat <- reactive(
        ma_dw_clean %>% 
            filter(year %in% c(input$year)) %>% 
            filter(`Chemical Name` %in% c(input$chemicals)) %>% 
            mutate(town_select = case_when(Town == input$town ~ "highlight",
                                           TRUE ~ "fade")
            )
    )
    
    output$town <- renderUI({
        pickerInput("town", "TOWN:", choices = as.list(unique(levels(ma_dw_clean$Town))), 
                    #selected = "CAMBRIDGE",
                    options = list(
                        `actions-box` = TRUE,
                        title = "Select town"), 
                    multiple = FALSE)
    })
    
    
    output$year <- renderUI({
        pickerInput("year", "YEAR:", choices = as.list(unique(levels(ma_dw_clean$year))), 
                    #selected = "2021",
                    options = list(
                        `actions-box` = TRUE,
                        title = "Select year(s)"), 
                    multiple = TRUE)
    })
    
    output$chemicals <- renderUI({
        pickerInput("chemicals", "CHEMICALS TO INCLUDE:", 
                    #choices = as.list(unique(levels(ma_dw_clean$`Chemical Name`))),
                    choices = list("Disinfection By-Products" = c("TOTAL TRIHALOMETHANES",
                                                                  "HALOACETIC ACIDS"),
                                   "Synthetic Organic Chemicals" = c("ALACHLOR", "BENZO(A)PYRENE", "CHLORDANE", "DI(2-ETHYLHEXYL)PHTHALATE",
                                                                     "ETHYLENE DIBROMIDE (EDB)", "HEPTACHLOR", "HEPTACHLOR EPOXIDE",
                                                                     "HEXACHLOROBENZENE", "PENTACHLOROPHENOL", "TOXAPHENE"),
                                   "Volatile Organic Chemicals" = c("1,2-DICHLOROETHANE", "1,2-DICHLOROPROPANE",
                                                                    "BENZENE", "CARBON TETRACHLORIDE", "DICHLOROMETHANE",
                                                                    "TETRACHLOROETHYLENE", "TRICHLOROETHYLENE", "VINYL CHLORIDE"),
                                   " " = c("ARSENIC", "URANIUM")),
                    #selected = c("TOTAL TRIHALOMETHANES"),
                    options = list(
                        `actions-box` = TRUE,
                        title = "Select  chemical(s)"), 
                    multiple = TRUE)
    })
    instruct <- reactive(
        if(!isTruthy(input$town)|!isTruthy(input$year)|!isTruthy(input$chemicals)){
           paste0("<br> Use the dropdown boxes to the right to select your town, the testing year,
               and chemicals of interest.") 
        })
    output$instructions <- renderText({
        instruct()
    })
    

    output$summary <- renderText({
        
        req(input$town)
        req(input$year)
        req(input$chemicals)
        

        
        text <- list()
        for(i in unique(input$chemicals)){

            # get town-specific data: most recent test and highest level in that year
            summary_dat <- ma_dw_clean %>%
                filter(Town %in% c(input$town)) %>%
                filter(`Chemical Name` == i) %>%
                mutate(year = as.numeric(as.character(year))) %>%
                group_by(Town, year, `Chemical Name`, MCL, `Maximum Contaminant Level (MCL)`, UOM) %>%
                summarize(max_result = max(result))
            
            snake_chem <- str_to_title(i)
            
            if(length(summary_dat$Town) == 0){
                text[[i]] <- (paste0("<br><b>", snake_chem, "</b><br>",   "The public water system in ", unique(str_to_title(input$town)), 
                                     " did not report testing results for ", snake_chem, ". <br>"))
            }
            
            #start rules for summary text -- will indicate if highest level is above/below MCL
            else if(max(summary_dat$max_result[which(summary_dat$`Chemical Name` == i &
                                                summary_dat$year == max(summary_dat$year))]) > summary_dat$MCL)
            {
                text[[i]] <- (paste0("<b>", i, "</b><br>", "The most recent testing for ",
                                     snake_chem ," in ", unique(str_to_title(summary_dat$Town)),
                              " was conducted in ", max(summary_dat$year), ". ",
                              "The federal standard for ", snake_chem, " is ", unique(summary_dat$`Maximum Contaminant Level (MCL)`), 
                              ". The highest level reported in ", max(summary_dat$year), 
                              " in your town for this chemical is: ", "<font color=\"#6A5ACD\"><b>",
                              max(summary_dat$max_result[which(summary_dat$year ==
                                                                   max(summary_dat$year) &
                                                                   summary_dat$`Chemical Name` == i)])," ", 
                              unique(summary_dat$UOM), "</b></font>",". This value <b>exceeds </b>the
                          federal standard for ", snake_chem, ". <br>"))
            }
            
            #else if it was detected but below MCL
            else if(max(summary_dat$max_result[which(summary_dat$year == max(summary_dat$year))]) < summary_dat$MCL &
                    max(summary_dat$max_result[which(summary_dat$year == max(summary_dat$year))]) > 0
            )
            {
                text[[i]] <- paste0("<br><b>", i, "</b><br>", "The most recent testing in ", 
                                    unique(str_to_title(summary_dat$Town)),
                                    " was conducted in ", max(summary_dat$year), ". ",
                                    "The federal standard for ", snake_chem ," is ", 
                                    unique(summary_dat$`Maximum Contaminant Level (MCL)`),
                                    ". The highest level reported in ", max(summary_dat$year), 
                                    " in your town for this chemical is: ",
                                    "<font color=\"#6A5ACD\"><b>",
                                    max(summary_dat$max_result[which(summary_dat$year == max(summary_dat$year) &
                                                                         summary_dat$`Chemical Name` == i)]), 
                                    " ", unique(summary_dat$UOM), 
                                    " </b></font>. This value is <b>below</b> the federal standard for ",
                                    snake_chem, ". <br>")
                
            }
            #else if not detected 
            else if(max(summary_dat$max_result[which(summary_dat$year == max(summary_dat$year))]) < 0 & 
                    !is.infinite(max(summary_dat$max_result[which(summary_dat$year == max(summary_dat$year))])))
            {
                text[[i]] <- paste0("<br><b>", i, "</b><br>", "The most recent testing in ", 
                                    unique(str_to_title(summary_dat$Town)),
                                    " was conducted in ", max(summary_dat$year), ". ",
                                    "The federal standard for ", snake_chem, " is ",  
                                    unique(summary_dat$`Maximum Contaminant Level (MCL)`),
                                    ". The highest level reported in ", max(summary_dat$year), 
                                    " in your town for this chemical is: <font color=\"#6A5ACD\"><b>Not Detected </b></font>.
                          This value is <b>below</b> the federal standard. <br>"
                         )
                
            }
            
            else{
                text[[i]] <- (paste0("<br><b>", i, "</b><br>",   "The public water system in ", unique(str_to_title(input$town)), 
                                     "did not report testing results for ", snake_chem, ". <br>"))
            }
            
            
        }
        
        return(paste0(text))
        
    })


    # ##### Graphs ####

    output$hint <- renderText({

        req(input$town)
        req(input$year)
        req(input$chemicals)
        return(paste0("<br><span style='color: #CD5B45'>Hint: Hover over the graphs below to learn more! All results
                      are shown in nanograms per liter (ng/L). For more info, 
                      see the <a href = ",'#FAQ' ,">FAQ</a></span><br><br>"))
    })
    

    
    #download button not currently active, requires Rmd file that has not been set up yet
    observeEvent(input$download, {
        show_alert(title = "This feature is not currently available", text = "Stay tuned!")
    })
    
    
    
    #create interactive plot
    output$dw <- renderPlotly({
        req(input$town)
        req(input$year)
        req(input$chemicals)

        town_dat <- dat()

        town_dat_det <- dat() %>%
            filter(nd_flag == "Detect")

        town_dat_nd <- dat() %>%
            filter(nd_flag == "ND")

            pfas_plot <- ggplot(town_dat) +
                geom_jitter(aes(x = as.factor(year), y = result,
                                color = town_select,
                                alpha = town_select,
                                label = `PWS Name`,
                                label2 = date,
                                label3 = Result),
                            width = 0.25) +
                geom_hline(aes(yintercept =  MCL,
                               label4 = `Maximum Contaminant Level (MCL)`),
                           color = "#6E8B3D", linetype="dashed") +
                coord_flip() +
                scale_shape_manual(values=c(16, 1),
                                   breaks = c("Detect", "ND"),
                                   labels = c("Detect", "Not Detected"),
                                   guide = "none")+
                scale_alpha_manual(values = c(0.5, 1), guide="none") +
                scale_color_manual(values = c("light blue", "#6A5ACD"),
                                   breaks = c("fade","highlight"),
                                   labels = c("Other towns in Massachusetts",
                                              "My town")) +
                scale_y_continuous(limits = c(0, NA)) + 
                facet_wrap(~`Chemical Name`+UOM, ncol = 1, scales = "free", 
                           labeller = labeller(`Chemical Name` = label_value, UOM = label_value, .multi_line = FALSE)) +
                ggthemes::theme_pander() +
                xlab("")+
                ylab("Concentration") +
                theme(axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      panel.grid.major.y = element_blank(),
                      panel.grid.major.x = element_line(color = "snow2"),
                      strip.text.x = element_text(color = "#556B2F", face = "bold"),
                      text = element_text(family = "Arial"))
        

         #manually fix height of plot
            fig <- ggplotly(pfas_plot, height = length(unique(town_dat$`Chemical Name`))*300, tooltip = c("label", "label2", "label3", "label4"))
        


        #manually fix the legend
        for(i in seq_along(1:length(unique(town_dat$chemname)))){
            if(i > 1){
                fig$x$data[[1]]$name <- "Other Towns in MA"
                fig$x$data[[i]]$showlegend <- FALSE

                fig$x$data[[i+1]]$name <- paste(str_to_title(input$town))
                fig$x$data[[length(unique(town_dat$chemname))+i]]$showlegend <- FALSE

            }
            else{
                fig$x$data[[1]]$name <- "Other Towns in MA"
                fig$x$data[[2]]$name <- paste(str_to_title(input$town))


            }
        }

        fig$x$layout$legend$title$text <- ""



        sub_box <- ggplot(town_dat_nd) +
            geom_jitter(aes(x = as.factor(year), y = result,
                            color = town_select,
                            alpha = town_select,
                            label = `PWS Name`,
                            label2 = date,
                            label3 = Result),
                        width = 0.25) +
            coord_flip() +
            scale_shape_manual(values=c(16, 1),
                               breaks = c("Detect", "ND"),
                               labels = c("Detect", "Not Detected"),
                               guide = "none")+
            scale_alpha_manual(values = c(0.5, 1), guide="none") +
            scale_color_manual(values = c("light grey", "#6A5ACD"),
                               breaks = c("fade","highlight"),
                               labels = c("Other towns in Massachusetts",
                                          "My town")) +
            facet_wrap(~`Chemical Name`+UOM, ncol = 1,scales = "free") +
            ggthemes::theme_pander() +
            xlab("")+
            ylab("") +
            theme(legend.position = "none",
                  axis.text.x = element_blank(),
                  axis.ticks = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  strip.text.x = element_blank())

        #manually fix height of plot

        sub_box_ly <- ggplotly(sub_box, #height = length(unique(town_dat_nd$chemname))*300,
                               tooltip = c("label", "label2", "label3", "label4"))

        #sub_box_ly$x$data$showlegend <- FALSE
        
        for(i in seq_along(1:length((sub_box_ly$x$data)))){
            sub_box_ly$x$data[[i]]$showlegend <- FALSE
            #sub_box_ly$x$data[[length(unique(town_dat$chemname))+i]]$showlegend <- FALSE
        }
        
        
        subplot(list(sub_box_ly, fig), widths = c(.1, .9),
                nrows = 1)
        
    })
    
    
    
    ## add table of results 
    dat_formatted <- reactive(
        dat() %>% 
            filter(Town == input$town) %>% 
            #filter(!town_select %in% c("fade")) %>% 
            select(-c(result:town_select))
    )
    
    output$dw_table <- DT::renderDataTable(
        dat_formatted() , options = list(pageLength = 25,
                                         autoWidth = TRUE, 
                                         order = list(list(2, 'desc')))
    )
    # 
    # ##### Load HTML text on other pages ####
    # output$FAQ_text <- renderUI(
    #     return(includeHTML("html/FAQ.html"))
    # )
    # 
    # output$about <- renderUI(
    #     return(includeHTML("html/about.html"))
    # )
    
})
