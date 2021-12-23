##### Data Products Final Project

#### Set up ####
## Clear Environment Data
rm(list=ls())
## turns off scientific notation
options(scipen=999) 

library(shiny);library(shinythemes);library(shinyWidgets);library(tidyverse);library(ggtext); library(ggrepel)

#### Colours
col_bg <- '#222d32';col_fg <- '#3C8DBC';col_ct <- '#024788'
col_ml <- '#E66100';col_fc <- '#5D3A9B';col_ln <- '#748696'


#### Data ####

dfdata <- mtcars %>% select(mpg:qsec)

dfkey <- data.frame(variable = names(dfdata),
                    description = c("Miles/(US) gallon", "Number of cylinders","Displacement (cu.in.)",
                                    "Gross horsepower", "Rear axle ratio","Weight (1000 lbs)","1/4 mile time"),
                    shorter = c("Miles/gallon", "Cylinders","Displacement", "Horsepower", "Axle ratio","Weight","1/4 mile"))

#### Server ####

shinyServer(function(session, input, output) {
  
  #### Data
  plot_data <- reactive({
    
    vcols <- c(dfkey$variable[dfkey$description == input$axis_x], dfkey$variable[dfkey$description == input$axis_y])
    
    dfpdata <- dfdata %>% select(all_of(vcols))
        names(dfpdata) <- c("x", "y")
        
    return(dfpdata)
    
  })
  
  #### Update Choice
  observeEvent(input$axis_x,{
    vselect <- ifelse(input$axis_x == input$axis_y, sample(dfkey$description[dfkey$description != input$axis_x],1), input$axis_y)
    
    updatePickerInput(session, inputId = "axis_y", 
                      choices = dfkey$description[dfkey$description != input$axis_x], selected = vselect)
  })
  observeEvent(input$axis_y,{
    vselect <- ifelse(input$axis_y == input$axis_x, sample(dfkey$description[dfkey$description != input$axis_y],1), input$axis_x)
    
    updatePickerInput(session, inputId = "axis_x", 
                      choices = dfkey$description[dfkey$description != input$axis_y], selected = vselect)
  })
  
  #### Update Value
  observe({
    updateKnobInput(session, inputId = "find_value", 
                    label = paste("Select New ", dfkey$shorter[dfkey$description == input$axis_x]), 
                    value = round(mean(plot_data()$x), 1),
                    options = list(
                      min = round(min(plot_data()$x)*0.5, 1),
                      max = round(max(plot_data()$x)*2, 1) ) )
  })
  
  #### Plot
  output$car_scatter <- renderPlot({
    
    my_lm <- lm(y ~ x, data = plot_data())
    
    dfpoint <- data.frame(x = input$find_value, y = predict(my_lm, newdata = data.frame(x = input$find_value))) %>% 
      mutate(lab_t = paste0(dfkey$shorter[dfkey$description == input$axis_y], "\n", round(y,2)))
    
    plot_data() %>%
      ggplot(aes(x = x, y = y)) + geom_point(size = 0.75, color = col_ct) + geom_point(data = dfpoint, size = 1.75, color = col_ml) +
      geom_label_repel(data = dfpoint, aes(label = lab_t), color = col_ml, size = 6) +
      labs(x = paste0("<span style = 'color:white;font-size:16pt'>", input$axis_x), 
           y = paste0("<span style = 'color:white;font-size:16pt'>", input$axis_y)) +
      theme_bw() + theme(
        axis.text = element_text(size = 12),
        axis.title.x = element_textbox_simple(
          lineheight = 1, width = 0.5, 
          hjust = 0.5, vjust = 0.5, halign = 0.5, valign = 0.5, 
          linetype = 1,  box.color = col_ln,  fill = col_bg,  r = unit(1, "pt"),
          padding = ggplot2::margin(5, 5, 2, 5), 
          margin = ggplot2::margin(2, 2, 2, 2) 
        ),
        axis.title.y = element_textbox_simple(
          lineheight = 1, width = 0.5, 
          hjust = 0.5, vjust = 0.5, halign = 0.5, valign = 0.5, 
          linetype = 1,  box.color = col_ln,  fill = col_bg,  r = unit(1, "pt"),
          orientation = "left-rotated",
          padding = ggplot2::margin(5, 5, 2, 5), 
          margin = ggplot2::margin(2, 2, 2, 2) 
        )
      )
    
    
  }, height = 650)
  
})

