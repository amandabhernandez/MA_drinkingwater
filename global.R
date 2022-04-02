
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(lubridate)

#load data and clean
ma_dw_clean <- read_csv("data/MA drinking water data.csv")  %>% 
  mutate(Town = factor(Town),
         year = fct_rev(factor(year(date))),
         chemname = factor(chemname),
         `Chemical Name` = factor(`Chemical Name`))

# profvis::profvis({
#   runApp('MA_drinkingwater_carcinogens')
#   })