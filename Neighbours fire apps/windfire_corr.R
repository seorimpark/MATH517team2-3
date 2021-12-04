
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(maps)
library(dplyr)
library(zeallot)
library(ppls)
# install.packages("https://cran.r-project.org/src/contrib/Archive/ppls/ppls_1.6-1.1.tar.gz", repos = NULL)

# Read data
load("data_train_DF.RData")
data = data_train_DF

# Remove NA values from CNT and BA
data <- data[!is.na(data$CNT),]
data <- data[!is.na(data$BA),]

# Keep variables we need with clim1 and clim2 wind components
data = data[, c("CNT", "BA", "lon", "lat", "area", "year", "month", "clim1", "clim2")]
data = data[data$CNT>0 & data$BA>0,]

# Function returning correlations for BA and CNT
windfire <- function(sub, param = "BA") {
  n = nrow(sub)
  windfire=0
  for (i in 1:n) {
    c(lon, lat, wind_east, wind_north) %<-% sub[i, c("lon", "lat", "clim1", "clim2")]
    fire_dir = NA
    
    if (nrow(sub[sub$lon==lon+0.5 & sub$lat==lat+0.5,]) != 0) {
      fire_dir = rbind(sub[sub$lon==lon+0.5 & sub$lat==lat+0.5, param]*c(sqrt(2)/2,sqrt(2)/2), fire_dir)
    }
    
    if (nrow(sub[sub$lon==lon-0.5 & sub$lat==lat-0.5,]) != 0) {
      fire_dir = rbind(sub[sub$lon==lon-0.5 & sub$lat==lat-0.5, param]*c(-sqrt(2)/2,-sqrt(2)/2), fire_dir)
    }
    
    if (nrow(sub[sub$lon==lon+0.5 & sub$lat==lat-0.5,]) != 0) {
      fire_dir = rbind(sub[sub$lon==lon+0.5 & sub$lat==lat-0.5, param]*c(sqrt(2)/2,-sqrt(2)/2), fire_dir)
    }
    
    if (nrow(sub[sub$lon==lon-0.5 & sub$lat==lat+0.5,]) != 0) {
      fire_dir = rbind(sub[sub$lon==lon-0.5 & sub$lat==lat+0.5, param]*c(-sqrt(2)/2,sqrt(2)/2), fire_dir)
    }
    
    if (nrow(sub[sub$lon==lon+0.5 & sub$lat==lat,]) != 0) {
      fire_dir = rbind(sub[sub$lon==lon+0.5 & sub$lat==lat, param]*c(1,0), fire_dir)
    }
    
    if (nrow(sub[sub$lon==lon-0.5 & sub$lat==lat,]) != 0) {
      fire_dir = rbind(sub[sub$lon==lon-0.5 & sub$lat==lat, param]*c(-1,0), fire_dir)
    }
    
    if (nrow(sub[sub$lon==lon & sub$lat==lat+0.5,]) != 0) {
      fire_dir = rbind(sub[sub$lon==lon & sub$lat==lat+0.5, param]*c(0,1), fire_dir)
    }
    
    if (nrow(sub[sub$lon==lon & sub$lat==lat-0.5,]) != 0) {
      fire_dir = rbind(sub[sub$lon==lon & sub$lat==lat-0.5, param]*c(0,-1), fire_dir)
    }
    
    if (!is.null(nrow(fire_dir))) {
      fire_dir = apply(fire_dir, 2, sum, na.rm = TRUE)
      
      fire_dir = normalize.vector( fire_dir )
      wind_dir = normalize.vector( c(wind_east, wind_north) )
      
      windfire[i] = sum(fire_dir * wind_dir)
    } else {
      windfire[i] = NA
    }
  }
  
  return(windfire)
}

# Computation time too long, so we load data from previous computation
# for (year in 1993:2015) {
#   for (month in 3:9) {
#     data[data$year==year & data$month==month, "windfire_BA"] = windfire(data[data$year==year & data$month==month,], param = "BA")
#   }
# }
# 
# for (year in 1993:2015) {
#   for (month in 3:9) {
#     data[data$year==year & data$month==month, "windfire_CNT"] = windfire(data[data$year==year & data$month==month,], param = "CNT")
#   }
# }

# Load (or save) data with correlations
# saveRDS(data, file = "windfire_corr.Rds")
windfire_corr = readRDS(file = "windfire_corr.Rds")

############# Application #############

ui <- fluidPage(
  
  # Application title
  titlePanel("Correlation between wind and aggregate fire direction"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(style="text-align: justify;",
               "This app aims to model the effect of wind on the spread of wildfires. 
               In the first tab, the correlation value defined in the report is represented 
               on a US map for each location (calculated according to the neighbours which 
               are at most 8). Wildfires can be represented either by the variable BA (aggregated burnt 
               area of wildfires in acres) or by CNT (number of wildfires). The slider 
               indicates the month. Note that there is a gap between each year, months 
               going from March to September. In the second tab, there is a histogram 
               representing the distribution of correlation values. The dashed red line is 
               the median."),
      radioButtons("param", "Compute wind correlation with:",
                   c("BA" = "BA",
                     "CNT" = "CNT")),
      sliderTextInput(
                  inputId = "time",
                  label = "Month:",
                  choices = c(outer(3:9, 1993:2015, paste, sep="/")),
                  selected = "9/2015",
                  animate = TRUE,
                  width = "100%")
    ),
    
    mainPanel(
      # Show both plots
      tabsetPanel(
        tabPanel("US Map", br(), plotOutput("usMap")),
        tabPanel("Histogram", br(), plotOutput("barPlot"))
      )
    )
  )
)


server <- function(input, output) {
  
  output$usMap <- renderPlot({
    
    # Extracting month and year
    time <- unlist( strsplit(input$time, "/") )
    c(month, year) %<-% as.numeric(time)
    
    # Subset
    selection <- windfire_corr[data$year == year & data$month == month,]
    selection <- na.omit(selection)
    
    # US map
    us <- map_data('state')
    
    # Plot correlations 
    theme_set(theme_gray(base_size=16))
    
    if (input$param == "BA") {
      ggplot() + 
        geom_map(aes(map_id=region), fill="white", color="black", size=0.15, map=us, data=us) +
        expand_limits(x = us$long, y = us$lat) +
        geom_point(data=selection, aes(x=lon, y=lat, size=area*2.7, colour=windfire_BA), shape=15) +
        scale_colour_gradient2(name="Correlation", low="red", mid="grey50", high="green", midpoint=0, limits=c(-1,1)) +
        scale_size_identity() + 
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
              plot.title = element_text(size=20, hjust=0.5, vjust=2)) + 
        ggtitle('Wind and fire aggregate direction correlation by location in the US') + 
        coord_fixed(1.3)
    } else {
      ggplot() + 
        geom_map(aes(map_id=region), fill="white", color="black", size=0.15, map=us, data=us) +
        expand_limits(x = us$long, y = us$lat) +
        geom_point(data=selection, aes(x=lon, y=lat, size=area*2.7, colour=windfire_CNT), shape=15) +
        scale_colour_gradient2(name="Correlation", low="red", mid="grey50", high="green", midpoint=0, limits=c(-1,1)) +
        scale_size_identity() + 
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
              axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
              plot.title = element_text(size=20, hjust=0.5, vjust=2)) + 
        ggtitle('Wind and fire aggregate direction correlation by location in the US') + 
        coord_fixed(1.3)
    }
    
  })
  
  output$barPlot <- renderPlot({
    
    # Extracting month and year
    time <- unlist( strsplit(input$time, "/") )
    c(month, year) %<-% as.numeric(time)
    
    # Subset
    selection <- windfire_corr[data$year == year & data$month == month,]
    selection <- na.omit(selection)
    
    # Plot histograms
    theme_set(theme_bw(base_size=16))
    
    if (input$param == "BA") { 
      ggplot(data = selection, aes(x = windfire_BA)) +
        geom_histogram(aes(y=..count../sum(..count..)),
                       bins=100,
                       color="black", 
                       fill="dodgerblue") + 
        scale_x_continuous(limits = c(-1, 1)) +
        scale_y_continuous(limits = c(0, 0.1), labels = scales::percent) +
        labs(title="Histogram representing the distribution of correlations between wind and fire direction with median", x="Correlation between wind and fire aggregate direction", y="Frequency") +
        geom_vline(xintercept = median(selection$windfire_BA), color = "red", linetype = "dashed", size=1.5)
    } else {
      ggplot(data = selection, aes(x = windfire_CNT)) +
        geom_histogram(aes(y=..count../sum(..count..)),
                       bins=100,
                       color="black", 
                       fill="dodgerblue") + 
        scale_x_continuous(limits = c(-1, 1)) +
        scale_y_continuous(limits = c(0, 0.1), labels = scales::percent) +
        labs(title="Histogram representing the distribution of correlations between wind and fire direction with median", x="Correlation between wind and fire aggregate direction", y="Frequency") +
        geom_vline(xintercept = median(selection$windfire_CNT), color = "red", linetype = "dashed", size=1.5)
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
