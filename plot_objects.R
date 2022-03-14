#US Population = 330,195,000
#Radford Population = 18,249
#Montgomery Population = 98,535
#Fairfax Population = 1,147,532
#Durham Population = 321,488
#Salem Population = 25,301


##                    Load packages into library
##...............................................................

library(plotly)         #used to create graphs
library(htmlwidgets)    #used for exporting the graph to an html file
library(janitor)        #adorn_totals function used to create grand total row for covidDf




##              Import records and filter by location
##...............................................................

#import csv
covidDf <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

#create grand total row and give it the name "US_Total" 
covidDf <- adorn_totals(covidDf, where = "row",,,,, 12:ncol(covidDf))
covidDf[nrow(covidDf), c("Admin2", "Combined_Key")] <- "US_Total"



#create a dataframe of locations
locations <- data.frame(
  name = c(
    "US_Total",
    "Radford",
    "Montgomery",
    "Fairfax",
    "Durham",
    "Salem"),
  
  location = c(
    "US_Total",
    "Radford, Virginia, US",
    "Montgomery, Virginia, US",
    "Fairfax, Virginia, US",
    "Durham, North Carolina, US",
    "Salem, Virginia, US"),
  
  population = c(
    330195000,
    18249,
    98535,
    1147532,
    321488,
    25301)
)


#filter records for each location in the location dataframe
covidDfSubset <- subset(covidDf, Combined_Key %in% locations$location)




##                     Perform calculations
##...............................................................

#create a new dataframe listing the dates as rows
totalCases <- data.frame("Date" = colnames(covidDfSubset[,12:ncol(covidDfSubset)]))

#clean dates
totalCases$Date <- as.Date(totalCases$Date, format = "X%m.%d.%y")

#pull in totals for each location as a new column
for (name in locations$name) {
  totalCases[ ,name] <- as.numeric(covidDfSubset[  #use as.numeric to just export the values without the column names
    which(covidDfSubset$Admin2 == name),  #use which to return the row number for the selected locaiton
    12:ncol(covidDfSubset)])  #return date columns
}




#calculate new cases
newCases <- totalCases[-1 ,]  #exclude the first row so number of rows match the next calculation
newCases[ ,2:ncol(totalCases)] <- sapply(totalCases[ ,2:ncol(totalCases)], diff, lag=1)  #calculate change amount for locations




#calculate moving average
movingAverage <- newCases  #copy dataframe to get date column
movingAverage[ ,2:ncol(movingAverage)] <- NA  #set values of locations to NA

#loop through each location by referring to its name in the list
for (name in locations$name) {

  #set up loop variables
  i = nrow(movingAverage)
  period = 7
  
  #for every 7 records in the newCases dataframe, take the mean and record it in movingAverage dataframe
  while(i-period > 0){
    movingAverage[i, name] = mean(newCases[i:(i-period+1), name])
    
    i = i-period
  }
}


#calculate cases per 100,000 people (cases / population * 100,000)
weightedMovingAverage <- movingAverage

for (name in locations$name) {
  
  weightedMovingAverage[ ,name] <- weightedMovingAverage[ ,name] / locations[which(locations$name == name), "population"] * 100000
}




##                   Plot allPlot using plotly
##...............................................................


#plot line graph of all locations
allPlot <- plot_ly(
  weightedMovingAverage,
  x = ~Date,
  y = ~US_Total,
  name = 'US Average',
  type = 'scatter',
  mode = 'lines',
  line = list(shape = "spline", width = 9, color = 'rgb(220,220,220)'), #shape=hvh for bargraph style, linear for regular, and spline for smoothed
  hovertemplate = '%{x}: <b>%{y:.1f}</b>', #only show y value in hover text to 1 decimal place
  connectgaps = TRUE) %>%
  
  add_trace(
    y = ~Radford,
    name = 'Radford',
    type = 'scatter', 
    mode = 'lines',
    line = list(shape = "spline", width = 4, color = 'rgb(194,1,27)'), #shape=hvh for bargraph style, linear for regular, and spline for smoothed
    connectgaps = TRUE) %>%
  
  add_trace(
    y = ~Montgomery,
    name = 'Montgomery',
    type = 'scatter', 
    mode = 'lines',
    line = list(shape = "spline", width = 4, color = 'rgb(232,119,34)'), #shape=hvh for bargraph style, linear for regular, and spline for smoothed
    connectgaps = TRUE) %>%
  
  add_trace(
    y = ~Fairfax,
    name = 'Fairfax',
    type = 'scatter', 
    mode = 'lines',
    line = list(shape = "spline", width = 4, color = 'rgb(30,98,56)'), #shape=hvh for bargraph style, linear for regular, and spline for smoothed
    connectgaps = TRUE) %>%
  
  add_trace(
    y = ~Durham,
    name = 'Durham',
    type = 'scatter', 
    mode = 'lines',
    line = list(shape = "spline", width = 4, color = 'rgb(75,156,211)'), #shape=hvh for bargraph style, linear for regular, and spline for smoothed
    connectgaps = TRUE) %>%
  
  add_trace(
    y = ~Salem,
    name = 'Salem',
    type = 'scatter', 
    mode = 'lines',
    line = list(shape = "spline", width = 4, color = 'rgb(200,150,250)'), #shape=hvh for bargraph style, linear for regular, and spline for smoothed
    connectgaps = TRUE,
    visible = 'legendonly') %>%
  
  layout(
    
    #add a title
    title = "New Daily Cases Per 100,000 People",
    
    xaxis = list(range = c(
      #zoom x axis to exclude blank values at beginning
      head(na.omit(newCases$Date[newCases$Fairfax != 0]),n=1),  #first record that does not equal 0
      tail(newCases$Date, n=1)),  #last record
      
      #hide x axis title
      title = FALSE,
      
      #hide vertical gridlines
      showgrid = FALSE),
    
    
    yaxis = list(
      #change y axis title
      title = 'New Cases',
      
      #zoom y axis to 4
      range = c(0,80))
  )




##             Plot individual locations using plotly
##...............................................................

#merge newCases and movingAverage dataframes for plotting
newAndMovingAverageDf <- merge(newCases, movingAverage, by = "Date", suffixes = c("New", "MovingAverage"))

#specify colors
lineColor <- 'rgb(150,100,200)'



#plot radford
radfordPlot <- plot_ly(
  data = newAndMovingAverageDf,
  x = ~Date,
  y = ~RadfordNew,
  name = 'New cases',
  hovertemplate = '%{x}: <b>%{y:f}</b>', #only show y value in hover text as whole numbers
  type = 'bar') %>%
  
  add_trace(
    y = ~RadfordMovingAverage,
    name = 'Moving average',
    type = 'scatter',
    mode = 'lines',
    line = list(shape = "spline", width = 3, color = lineColor), #shape=hvh for bargraph style, linear for regular, and spline for smoothed
    connectgaps = TRUE) %>%
  
  layout(
    
    #add a title
    title = "Radford",
    
    xaxis = list(range = c(
      #zoom x axis to exclude blank values at beginning
      head(na.omit(newCases$Date[newCases$Radford != 0]),n=1),  #first record that does not equal 0
      tail(newCases$Date, n=1)),  #last record
      
      #hide x axis title
      title = FALSE),
    
    #change y axis title and set lines to every 20
    yaxis = list(
      title = 'New Cases',
      dtick = 20),
    
    #hide the legend
    showlegend = FALSE
  )




#plot montgomery
montgomeryPlot <- plot_ly(
  data = newAndMovingAverageDf,
  x = ~Date,
  y = ~MontgomeryNew,
  name = 'New cases',
  hovertemplate = '%{x}: <b>%{y:f}</b>', #only show y value in hover text as whole numbers
  type = 'bar') %>%
  
  add_trace(
    y = ~MontgomeryMovingAverage,
    name = 'Moving average',
    type = 'scatter',
    mode = 'lines',
    line = list(shape = "spline", width = 3, color = lineColor), #shape=hvh for bargraph style, linear for regular, and spline for smoothed
    connectgaps = TRUE) %>%
  
  layout(
    
    #add a title
    title = "Montgomery",
    
    xaxis = list(range = c(
      #zoom x axis to exclude blank values at beginning
      head(na.omit(newCases$Date[newCases$Montgomery != 0]),n=1),  #first record that does not equal 0
      tail(newCases$Date, n=1)),  #last record
      
      #hide x axis title
      title = FALSE),
    
    #change y axis title and set lines to every 20
    yaxis = list(
      title = 'New Cases',
      dtick = 20),
    
    #hide the legend
    showlegend = FALSE
  )




#plot fairfax
fairfaxPlot <- plot_ly(
  data = newAndMovingAverageDf,
  x = ~Date,
  y = ~FairfaxNew,
  name = 'New cases',
  hovertemplate = '%{x}: <b>%{y:f}</b>', #only show y value in hover text as whole numbers
  type = 'bar') %>%
  
  add_trace(
    y = ~FairfaxMovingAverage,
    name = 'Moving average',
    type = 'scatter',
    mode = 'lines',
    line = list(shape = "spline", width = 3, color = lineColor), #shape=hvh for bargraph style, linear for regular, and spline for smoothed
    connectgaps = TRUE) %>%
  
  layout(
    
    #add a title
    title = "Fairfax",
    
    xaxis = list(range = c(
      #zoom x axis to exclude blank values at beginning
      head(na.omit(newCases$Date[newCases$Fairfax != 0]),n=1),  #first record that does not equal 0
      tail(newCases$Date, n=1)),  #last record
      
      #hide x axis title
      title = FALSE),
    
    #change y axis title and set lines to every 20
    yaxis = list(
      title = 'New Cases',
      dtick = 20),
    
    #hide the legend
    showlegend = FALSE
  )




#plot durham
durhamPlot <- plot_ly(
  data = newAndMovingAverageDf,
  x = ~Date,
  y = ~DurhamNew,
  name = 'New cases',
  hovertemplate = '%{x}: <b>%{y:f}</b>', #only show y value in hover text as whole numbers
  type = 'bar') %>%
  
  add_trace(
    y = ~DurhamMovingAverage,
    name = 'Moving average',
    type = 'scatter',
    mode = 'lines',
    line = list(shape = "spline", width = 3, color = lineColor), #shape=hvh for bargraph style, linear for regular, and spline for smoothed
    connectgaps = TRUE) %>%
  
  layout(
    
    #add a title
    title = "Durham",
    
    xaxis = list(range = c(
      #zoom x axis to exclude blank values at beginning
      head(na.omit(newCases$Date[newCases$Durham != 0]),n=1),  #first record that does not equal 0
      tail(newCases$Date, n=1)),  #last record
      
      #hide x axis title
      title = FALSE),
    
    #change y axis title and set lines to every 20
    yaxis = list(
      title = 'New Cases',
      dtick = 20),
    
    #hide the legend
    showlegend = FALSE
  )




#plot salem
salemPlot <- plot_ly(
  data = newAndMovingAverageDf,
  x = ~Date,
  y = ~SalemNew,
  name = 'New cases',
  hovertemplate = '%{x}: <b>%{y:f}</b>', #only show y value in hover text as whole numbers
  type = 'bar') %>%
  
  add_trace(
    y = ~SalemMovingAverage,
    name = 'Moving average',
    type = 'scatter',
    mode = 'lines',
    line = list(shape = "spline", width = 3, color = lineColor), #shape=hvh for bargraph style, linear for regular, and spline for smoothed
    connectgaps = TRUE) %>%
  
  layout(
    
    #add a title
    title = "Salem",
    
    xaxis = list(range = c(
      #zoom x axis to exclude blank values at beginning
      head(na.omit(newCases$Date[newCases$Salem != 0]),n=1),  #first record that does not equal 0
      tail(newCases$Date, n=1)),  
      
      #hide x axis title
      title = FALSE),
    
    #change y axis title and set lines to every 20
    yaxis = list(
      title = 'New Cases',
      dtick = 20),
    
    #hide the legend
    showlegend = FALSE
  )




##                      View/export plot
##...............................................................

# print(allPlot)
# 
# #export plot as html file
# htmlwidgets::saveWidget(
#   widget = partial_bundle(allPlot),  #partial_bundle only includes necessary plotly dependencies to reduce file size
#   file = "plot.html")
