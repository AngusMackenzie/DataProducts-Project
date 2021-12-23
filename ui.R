##### Data Products Final Project

#### Set up ####
## Clear Environment Data
rm(list=ls())
## turns off scientific notation
options(scipen=999) 

library(shiny);library(shinythemes);library(shinyWidgets);library(tidyverse)

#### Colours
col_bg <- '#222d32';col_fg <- '#3C8DBC';col_ct <- '#024788'
col_ml <- '#E66100';col_fc <- '#5D3A9B';col_ln <- '#748696'


#### Data ####

dfdata <- mtcars %>% select(mpg:qsec)

dfkey <- data.frame(variable = names(dfdata),
                    description = c("Miles/(US) gallon", "Number of cylinders","Displacement (cu.in.)",
                                    "Gross horsepower", "Rear axle ratio","Weight (1000 lbs)","1/4 mile time"),
                    shorter = c("Miles/gallon", "Cylinders","Displacement", "Horsepower", "Axle ratio","Weight","1/4 mile"))

#### Information

vtitle <- paste0("<span style = 'color:white;font-size:16pt'><b>", "Motor Trend Regression Tool", "</b></span><br>")

vinfo <- paste0("<span style = 'color:white;font-size:16pt'>", 
                    "Select variables, then choose a new independent value between 50% of minimum and 200% of maximum. ", 
                    "An prediction from a linear regression model will then be shown</span>")

#### Value Start


  
#### UI ####

shinyUI(fluidPage(theme = shinytheme("flatly"),
                div(HTML(vtitle),
                    style="flex-grow: 3; display: inline-block; width: 100%; height: 40px; background-color:#222d32;
                    padding: 5px; text-align: center; margin: auto; letter-spacing: 0.98pt;
                    border: 2px solid; border-radius: 5px; border-color:white;"),
                sidebarPanel(
                  pickerInput(inputId = "axis_x", label = "Select Independent Variable (x-axis):",
                              choices = dfkey$description[dfkey$description != "1/4 mile time"], selected = "Displacement (cu.in.)"),
                  pickerInput(inputId = "axis_y", label = "Select Dependent Variable (y-axis):",
                              choices = dfkey$description[dfkey$description != "Displacement (cu.in.)"], selected = "1/4 mile time"),
                  div(style="display: flex; align-items: center; justify-content: space-around;",
                  knobInput(inputId = "find_value", label = "Select New Displacement:",
                            value = round(mean(dfdata$disp), 1), min = round(min(dfdata$disp)*0.5,1), 
                            max = round(max(dfdata$disp)*2,1), displayPrevious = TRUE, step = 0.1,
                            lineCap = "round", fgColor = col_fg, inputColor = col_ml
                  )),
                  div(HTML(vinfo),
                      style="flex-grow: 3; display: inline-block; width: 100%; height: 100%; background-color:#222d32;
                      padding: 5px; text-align: left; margin: auto; letter-spacing: 0.98pt;
                      border: 2px solid; border-radius: 2px; border-color:white;"),
                  width = 3),
                mainPanel(
                  plotOutput("car_scatter"),
                  width = 9)
                ))

