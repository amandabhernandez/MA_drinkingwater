

library(tidyverse)
library(shiny)
library(shinyWidgets)
library(plotly)
library(lubridate)
library(fontawesome)


#load data and clean
ma_dw_clean <- read_csv("data/MA drinking water data.csv")  %>% 
  mutate(Town = factor(Town),
         year = fct_rev(factor(year(date))),
         chemname = factor(chemname),
         `Chemical Name` = factor(`Chemical Name`)) 


# headline creation function
summary_headline <- function(input_chems, input_town){
  text <- list()
  for(i in unique(input_chems)){
    
    # get town-specific data: most recent test and highest level in that year
    summary_dat <- ma_dw_clean %>%
      filter(Town %in% c(input_town)) %>%
      filter(`Chemical Name` == i) %>%
      mutate(year = as.numeric(as.character(year))) %>%
      group_by(Town, year, `Chemical Name`, MCL, `Maximum Contaminant Level (MCL)`, UOM) %>%
      summarize(max_result = max(result))
    
    snake_chem <- str_to_title(i)
    
    if(length(summary_dat$Town) == 0){
      text[[i]] <- (paste0("<br><b>", snake_chem, "</b><br>",   "The public water system in ", unique(str_to_title(input_town)), 
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
      text[[i]] <- (paste0("<br><b>", i, "</b><br>",   "The public water system in ", unique(str_to_title(input_town)), 
                           "did not report testing results for ", snake_chem, ". <br>"))
    }
    
    
  }
  
  return(paste0(text))
}

# note: use this to help id slow responses
# profvis::profvis({
#   runApp('~/Desktop/R/HSPH-SM/MA_drinkingwater')
# })