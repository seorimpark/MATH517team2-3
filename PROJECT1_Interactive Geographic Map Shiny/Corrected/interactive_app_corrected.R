library(shiny)
library(usmap)
library(tidyverse)
library(scales) 
library(zoo)


################################################################################################################################################
#loading the data###############################################################################################################################################
#setwd("~/Desktop/Github/SCV/SCV_project/MATH517team2-3/PROJECT1_Interactive Geographic Map Shiny/")

df0<-read_csv("Data_us.csv")
df1<- df0[,-c(1,17)]
# transforming the columns start date and end date to date format:
df3<-df1
df3$`Start Date`<- as.Date(df3$`Start Date`, format="%m/%d/%Y")
df3
df3$`End Date`<- as.Date(df3$`End Date`, format="%m/%d/%Y")
df3
#transform the other columns ( "Group","HHS Region","State","Place of Death" "Age group") into factors:
df4<-df3
#col_names <- colnames(df4[,4:10])
#df4[col_names] <- lapply(df4[col_names] , factor)
df4
df<-df4
df<-as.data.frame(df)



df_Data<- df %>% filter (Group=='By Month')
df_Data$Month<-sprintf("%02d", as.numeric(df_Data$Month))
df_Data$Date <- as.yearmon(paste(df_Data$Year, df_Data$Month), "%Y %m")


################################################################################################################################################

################################################################################################################################################
#sorting the date depending on the category:###############################################################################################################################################
Select_Age_Group<-function(DataFrame, agegroup)
{ 
  age_groups<-unique(df$`Age group`)
  if(agegroup %in% age_groups)
  {
    df1= DataFrame %>% filter(`Age group` == agegroup )
    df1<-as.data.frame(df1)
    return(df1)
  }
  else{
    warning("Age group selected not in the list, the returned dataframe has not been filtered")
    return (DataFrame)
  }
}

Select_Group<-function(DataFrame, group)
{ 
  all_groups<-unique(df$`Group`)
  if(group %in% all_groups)
  {
    df1= DataFrame %>% filter(`Group` == group )
    df1<-as.data.frame(df1)
    return(df1)
  }
  else{
    warning("Group selected not in the list, the returned dataframe has not been filtered")
    return (DataFrame)
  }
}

Select_HHSRegion<-function(DataFrame, region)
{ 
  all_regions<-unique(df$`HHS Region`)
  if(region %in% all_regions)
  {
    df1= DataFrame %>% filter(`HHS Region` == region )
    df1<-as.data.frame(df1)
    return(df1)
  }
  else{
    warning("HHS Region selected not in the list, the returned dataframe has not been filtered")
    return (DataFrame)
  }
}


Select_State<-function(DataFrame, state)
{ 
  all_states<-unique(df$State)
  if(state %in% all_states)
  {
    df1= DataFrame %>% filter(`State` == state )
    df1<-as.data.frame(df1)
    return(df1)
  }
  else{
    warning("State selected not in the list, the returned dataframe has not been filtered")
    return (DataFrame)
  }
}


Select_PlaceDeath<-function(DataFrame, place_d)
{ 
  all_places<-unique(df$`Place of Death`)
  if(place_d %in% all_places)
  {
    df1= DataFrame %>% filter(`Place of Death` == place_d )
    df1<-as.data.frame(df1)
    
    return(df1)
  }
  else{
    warning("Place of death selected not in the list, the returned dataframe has not been filtered")
    return (DataFrame)
  }
}


Select_all<-function(DataFrame, agegroup,place_d,group, m, y)
{
  # I want to use %>% but not quite confortable, I ll use brute force first:
  df1=Select_Age_Group(DataFrame,agegroup)
  df2=Select_PlaceDeath(df1,place_d)
  df3=df2 %>% filter(`Group` == group )
  if(group=="By Total")
  {return(as.data.frame(df3))}
  else if(group=="By Year") 
  {
    df4=df3 %>% filter(`Year` == y )
    return(as.data.frame(df4))
  }
  else if(group=="By Month")
  {
    df4=df3 %>% filter(`Year` == y )
    df5=df4 %>% filter(`Month` == m )
    return(as.data.frame(df5))
  }
  
  
}

################################################################################################################################################


################################################################################################################################################
##standardise_pop<-function(data)##############################################################################################################################################
us_popul<-statepop 

standardise_pop<-function(data)
{
  data[,2]=data[,2]*1000000/us_popul$pop_2015
  data<-as.data.frame(data)
  return(data)
}


data_to_plot<-function(DataFrame, agegroup,place_d,group, m, y){
  
  df_to_plot<-Select_all(DataFrame, agegroup,place_d,group, m, y)
  Death_by_state<-df_to_plot %>% 
    group_by(State) %>% 
    summarise(`COVID-19 Deaths`= sum(`COVID-19 Deaths`,na.rm=TRUE))
  
  Death_by_state<-Death_by_state[-c(34,41,47),]
  Death_by_state<-standardise_pop(Death_by_state)
  
  Total_D<-Death_by_state$`COVID-19 Deaths`
  us_TotalD<-as.data.frame(us_popul)
  us_TotalD$pop_2015<-Total_D
  us_TotalD<-us_TotalD[,-c(1,2)]
  colnames(us_TotalD)[1]<-"state"
  us_TotalD<-as.data.frame(us_TotalD)
  return(us_TotalD)
}

pick_data<-function(DataFrame, agegroup,place_d, date){
  
  DataFrame0<-Select_Age_Group(DataFrame, agegroup)
  DataFrame1<-Select_PlaceDeath(DataFrame0, place_d)
  df_to_plot<- DataFrame1 %>% filter(Date == date) #Feb 2020
  
  Death_by_state<-df_to_plot %>% 
    group_by(State) %>% 
    summarise(`COVID-19 Deaths`= sum(`COVID-19 Deaths`,na.rm=TRUE))
  
  Death_by_state<-Death_by_state[-c(34,41,47),]
  Death_by_state<-standardise_pop(Death_by_state)
  
  Total_D<-Death_by_state$`COVID-19 Deaths`
  us_TotalD<-as.data.frame(us_popul)
  us_TotalD$pop_2015<-Total_D 
  us_TotalD<-us_TotalD[,-c(1,2)]
  colnames(us_TotalD)[1]<-"state"
  us_TotalD<-as.data.frame(us_TotalD)
  return(us_TotalD)
}
#pick_data(df_Data,"All Ages", "Total - All Places of Death" ,df_Data$Date[6] )
Month_to_number<-function(m)
{
  if(m=="January"){return(1)}
  else if(m=="February"){return(2)}
  else if(m=="March"){return(3)}
  else if(m=="April"){return(4)}
  else if(m=="May"){return(5)}
  else if(m=="June"){return(6)}
  else if(m=="July"){return(7)}
  else if(m=="August"){return(8)}
  else if(m=="September"){return(9)}
  else if(m=="October"){return(10)}
  else if(m=="November"){return(11)}
  else if(m=="December"){return(12)}
  else if(m== "Total"){return(0)}
}




# ui <- fluidPage(
#   
#   titlePanel("Covid deaths in the US"),
#     sidebarPanel(
#       selectInput("Group", "Select the period", choices = c("Yearly Total", "Monthly")),
#       conditionalPanel(condition = "input.Group == 'Month' ",sliderInput("Time", "Select the month and Month and Year", min = as.Date("2020-03-01"),max =as.Date("2021-09-01"),value=as.Date("2020-12-01"), animate = animationOptions(interval = 3000, loop = TRUE),timeFormat="%b %Y") ),
#       conditionalPanel(condition = "input.Group == 'Yearly Total'",radioButtons("Year", "Please select the Year", choices = c("2020","2021"))),
#       selectInput("AgeGroup", "Select the age group",
#                   choices = c("All Ages","0-17 years","18-29 years" ,
#                               "30-39 years","40-49 years","50-64 years", 
#                               "65-74 years","75-84 years","85 years and over")),
#       selectInput("PlaceD", "Select the Place of death",
#                   choices = c("Total - All Places of Death","Healthcare setting,
#                               inpatient", "Healthcare setting, outpatient or emergency room",
#                               "Healthcare setting, dead on arrival","Decedent's home" ,
#                               "Hospice facility", "Nursing home/long term care facility",
#                               "Other","Place of death unknown")),
#       
#     ),
#     mainPanel(
#       plotOutput("myPlot")
#     ))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Covid deaths in the US"),
  
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    selectInput("Group", "Select the period", choices = c("Yearly Total", "Monthly")),
    #conditionalPanel(condition = "input.Group == 'Monthly' ",sliderInput("Time", "Select the Month and Year", min = as.Date("2020-03-01"),max =as.Date("2021-09-01"),value=as.Date("2020-12-01"), animate = animationOptions(interval = 3000, loop = TRUE),timeFormat="%b %Y") ),
    conditionalPanel(condition = "input.Group == 'Yearly Total'",radioButtons("Year", "Please select the Year", choices = c("2020","2021"))),
    conditionalPanel(condition = "input.Group == 'Monthly' ",sliderTextInput("Time","Select the Month and Year", choices= unique(df_Data$Date), selected = df_Data$Date[5],animate = animationOptions(interval = 3000, loop = TRUE)) ),
    
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
    
 
    
    # Show a plot of the generated distribution
  mainPanel(
    plotOutput("myPlot")
  )
      
    )
  




server <- function(input, output) {
  
  output$myPlot <- renderPlot({
    distType_age <- input$AgeGroup
    distType_placeofdeath <- input$PlaceD
    distGroup<- input$Group  # so it would be Yearly Total or Monthly
    distTime<-input$Time
    distYear<-input$Year
    print(distTime)
    if (distGroup == "Yearly Total")
    {
      dt=data_to_plot(df,distType_age,distType_placeofdeath,"By Year",0,distYear)
    }else if (distGroup == "Monthly"){
      dt=pick_data(df_Data,  distType_age,distType_placeofdeath, distTime)
    }
    ###
    #DataFrame, agegroup,place_d,group, m=0, y=0
    #randomVec <- rnorm(size, mean = as.numeric(input$mean), sd = as.numeric(input$sd))
    plot_usmap(data=dt, values = "pop_2015", color = "red") + 
      scale_fill_continuous(name = "us corona deaths per million ", low="lightpink",high="Red",label = scales::comma) + 
      theme(legend.position = "right")
    
  }
  
  )
 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)



