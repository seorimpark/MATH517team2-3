library(shiny)
library(usmap)
library(tidyverse)
library(scales) 

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


################################################################################################################################################
################################################################################################################################################
#data_to_plot<-function(DataFrame, agegroup)################################################################################################################################################
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

################################################################################################################################################
shinyServer(
  function(input, output, session) {
      output$myPlot <- renderPlot({
      distType_age <- input$AgeGroup
      distType_placeofdeath <- input$PlaceD
      distGroup<- input$Group  # so it would be total, 2020, or 2021
      distMonth<-0
      distYear<-0
      if (distGroup == "2020")
      {
        distYear<-2020
        distGroup<-"By Year"
       # print(input$Month)
         if(input$Month != "Total")
         {
           distMonth<-Month_to_number(input$Month)
           distGroup<- "By Month"
         }
      }else if (distGroup == "2021"){
        distYear<-2021
        distGroup<-"By Year"
        if(input$Month != "Total")
        {
          distMonth<-Month_to_number(input$Month)
          #distMonth<-19
          distGroup<- "By Month"
        }
        }
      ###
      #function(DataFrame, agegroup,place_d,group, m=0, y=0)
      dt=data_to_plot(df,distType_age,distType_placeofdeath,distGroup,distMonth,distYear)
        #DataFrame, agegroup,place_d,group, m=0, y=0
        #randomVec <- rnorm(size, mean = as.numeric(input$mean), sd = as.numeric(input$sd))
        plot_usmap(data=dt, values = "pop_2015", color = "red") + 
          scale_fill_continuous(name = "us corona deaths per million ", low="lightpink",high="Red",label = scales::comma) + 
          theme(legend.position = "right")
      
    }
    
    )
  }
)


