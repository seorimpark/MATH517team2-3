library(shiny)
library(shinyWidgets)
library(tidyverse)

shinyUI(
  pageWithSidebar(
    headerPanel("Covid deaths in the US"),
    sidebarPanel(
      selectInput("Group", "Select the period", choices = c("By Total", "2020", "2021")),
      conditionalPanel(condition = "input.Group == '2020'",radioButtons("Month", "Please select the month", choices = c("Total","January","February","March","April",
                                                                                                                           "May","June","July","August","September",
                                                                                                                           "October","November","December"))),
      conditionalPanel(condition = "input.Group == '2021'",radioButtons("Month", "Please select the month", choices = c("Total","January","February","March","April",
                                                                                                                           "May","June","July","August","September"))),
      
      
      ###
      selectInput("AgeGroup", "Select the age group",
                  choices = c("All Ages","0-17 years","18-29 years" ,
                              "30-39 years","40-49 years","50-64 years", 
                              "65-74 years","75-84 years","85 years and over")),
      selectInput("PlaceD", "Select the Place of death",
                  choices = c("Total - All Places of Death","Healthcare setting,
                              inpatient", "Healthcare setting, outpatient or emergency room",
                              "Healthcare setting, dead on arrival","Decedent's home" ,
                              "Hospice facility", "Nursing home/long term care facility",
                              "Other","Place of death unknown")),
      
    ),
    mainPanel(
      plotOutput("myPlot")
    )
  )
)



#"All Ages","0-17 years","18-29 years" ,"30-39 years","40-49 years","50-64 years", "65-74 years","75-84 years","85 years and over"




#"Total - All Places of Death","Healthcare setting, inpatient", "Healthcare setting, outpatient or emergency room","Healthcare setting, dead on arrival","Decedent's home" ,"Hospice facility", "Nursing home/long term care facility","Other","Place of death unknown"



