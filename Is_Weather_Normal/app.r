library(shiny)
library(shinyjs)
library(rnoaa)

# Creating a function to convert Celsius to Fahrenheit.
to_fahrenheit <- function(temp) {
  fahrenheit <- temp * (9/5) + 32
  return(fahrenheit)
}

# Storing the API Key
Sys.setenv(rnoaa_api_key="*********") # Obfuscated for demo purposes.
Sys.getenv("rnoaa_api_key")

# Create a dataframe of measures that a user can choose to compare.
df_measures<-data.frame("datatype" = c("PRCP","SNOW","SNWD","TAVG","TMAX","TMIN")
                        , "datatype_description" = c("Precipitation (tenths of mm)","Snowfall (mm)","Snow depth (mm)","Average temperature (degrees F)","Maximum temperature (degrees F)","Minimum temperature (degrees F)"))

# Pull a dataframe of the first 1000 'CITY' locations.
df_locations<-ncdc_locs(locationcategoryid='CITY', sortfield='name', limit=1000, token = Sys.getenv("rnoaa_api_key"))$data
# Because the 'CITY' locations gets cut off, pull the next set starting at 1001.
df_locations<-rbind(df_locations,ncdc_locs(locationcategoryid='CITY', sortfield='name', limit=1000, offset=1001, token = Sys.getenv("rnoaa_api_key"))$data)

# Filtering to only US locations using regex.
df_us_locations<-df_locations[grepl("*US",df_locations$name),]

# Parsing the State out from each location Name.
df_us_locations$state<-substr(df_us_locations$name,nchar(df_us_locations$name)-4,nchar(df_us_locations$name)-3)
df_us_locations[which(df_us_locations$state=='.,'),]$state<-'DC' # DC needs to be renamed as it gets parsed as ".,"

# Parsing the City out from each location Name.
df_us_locations$city<-sapply(strsplit(df_us_locations$name,","),`[`,1)

ui <- shinyUI(
  
  fluidPage(
    
    useShinyjs(),
  
    # Application title
    titlePanel("Is today's weather normal or abnormal?"),
    
    # Defining the sidebar elements.
    sidebarLayout(
      
      sidebarPanel(
        
        # Show the user a sorted list of states
        selectInput("user_state", 
                    "Choose a US State:", 
                    choice=unique(df_us_locations[order(df_us_locations$state),]$state) 
                    ),
        
        # Show the user a sorted list of cities.
        selectInput("user_city", 
                    "Choose a City:", 
                    choice=unique(df_us_locations[order(df_us_locations$city),]$city) 
        ),
        
        # Show the user a button to pull city data.
        actionButton(inputId = "pull_city_data", label = "Pull data for selected city"), 
        
        # Show the user options of measures to select and compare.
        selectInput("user_measure",
                    "Choose a measure to compare:",
                    choice=df_measures$datatype_description)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        # Initially hide the "Please wait: Pulling data" message until the user cliks the select button.
        hidden(
           div(id="messageText"
               ,"Please wait: Pulling data"
                )
           ),
        
        #dataTableOutput("mytable1"),
        
        plotOutput("distPlot"),
        
        plotOutput("linePlot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Initialize zScore as NULL so it will update.
  zScore <- NULL
  
  # Dynamically change the City list based on which State the user selects
  stateVar = reactive({
    unique(df_us_locations[df_us_locations$state == input$user_state,]$city)
  })
  observe({
    updateSelectInput(session, "user_city",
                      choices = stateVar()
    )})
  
  # Function to update the message text above the plots.
  update_messageText <- function(z, t, mwd) {
    
    #print(paste("update_messageText is called with values",as.character(z),"and",as.character(mwd),sep=" "))
    
    # If z is numeric, evaluate the weather.
    if (is.numeric(z) & !is.na(z)) {
      
      # If the z score is less than 1, then display the weather was normal.
      if (abs(z) < 1){
        html(id="messageText",paste('The',input$user_measure,'on',mwd,'of<b>',t,'</b>was <font color="green"><b>normal</b></font>!',sep=" "))
      } else if (abs(z) < 2){ # If the z score was less then 2, then display the weather was somewhat unusual.
        html(id='messageText',paste('The',input$user_measure,'on',mwd,'of<b>',t,'</b>was <font color="orange"><b>somewhat unusual</b></font>!',sep=' '))
      } else { # Otherwise, show the data was abnormal (above 2 z scores)
        html(id='messageText',paste('The',input$user_measure,'on',mwd,'of<b>',t,'</b>was <font color="red">abnormal</b></font>!',sep=' '))
      }
    }
  }
  
  # Update the current global "value" variable based on the user's selected measure.
  update_todaysValue <- function() {
    
    if (input$user_measure == "Precipitation (tenths of mm)"){
      todaysValue <<- max(df_station_unpivot[which(as.Date(df_station_unpivot$date) == max_weather_date),]$value.PRCP)
    } else if (input$user_measure == "Snowfall (mm)") {
      todaysValue <<- max(df_station_unpivot[which(as.Date(df_station_unpivot$date) == max_weather_date),]$value.SNOW)
    } else if (input$user_measure == "Snow depth (mm)") {
      todaysValue <<- max(df_station_unpivot[which(as.Date(df_station_unpivot$date) == max_weather_date),]$value.SNWD)
    } else if (input$user_measure == "Average temperature (degrees F)") {
      todaysValue <<- max(df_station_unpivot[which(as.Date(df_station_unpivot$date) == max_weather_date),]$value.TAVG)
    } else if (input$user_measure == "Maximum temperature (degrees F)") {
      todaysValue <<- max(df_station_unpivot[which(as.Date(df_station_unpivot$date) == max_weather_date),]$value.TMAX)
    } else if (input$user_measure == "Minimum temperature (degrees F)") {
      todaysValue <<- max(df_station_unpivot[which(as.Date(df_station_unpivot$date) == max_weather_date),]$value.TMIN)
    }
    
  }
  
  # Update the current global z score variable based on the user's selected measure.
  update_zScore <- function() {
    
    if (input$user_measure == "Precipitation (tenths of mm)"){
      zScore <<- max(df_station_unpivot[which(as.Date(df_station_unpivot$date) == max_weather_date),]$z.PRCP)
    } else if (input$user_measure == "Snowfall (mm)") {
      zScore <<- max(df_station_unpivot[which(as.Date(df_station_unpivot$date) == max_weather_date),]$z.SNOW)
    } else if (input$user_measure == "Snow depth (mm)") {
      zScore <<- max(df_station_unpivot[which(as.Date(df_station_unpivot$date) == max_weather_date),]$z.SNWD)
    } else if (input$user_measure == "Average temperature (degrees F)") {
      zScore <<- max(df_station_unpivot[which(as.Date(df_station_unpivot$date) == max_weather_date),]$z.TAVG)
    } else if (input$user_measure == "Maximum temperature (degrees F)") {
      zScore <<- max(df_station_unpivot[which(as.Date(df_station_unpivot$date) == max_weather_date),]$z.TMAX)
    } else if (input$user_measure == "Minimum temperature (degrees F)") {
      zScore <<- max(df_station_unpivot[which(as.Date(df_station_unpivot$date) == max_weather_date),]$z.TMIN)
    }
    
    #print(paste("The z score is",as.character(zScore),sep=" "))
  }
  
  # If the zScore has been calculated, update the z score, today's value, message text when the measure is changed.
  observeEvent(input$user_measure, {
    
    if (!is.null(zScore)) {
      update_zScore()
      update_todaysValue()
      update_messageText(zScore, todaysValue, max_weather_date)
      }
      
  
  })
  
  # Pull the weather data when the user clicks the Action Button.
  observeEvent(input$pull_city_data, {
    
    # Update the message text above the plots to show the data is pulling.
    html(id="messageText","Please wait: Pulling data (This can take 30-60 seconds)")
    show(id="messageText")

    # Pull the location ID for the user's selected State and City.
    location_id<-df_us_locations[which(df_us_locations$state == input$user_state & df_us_locations$city == input$user_city),]$id

    # Pull all the stations for the given location ID of the City/State the user selected.
    df_stations = ncdc_stations(datasetid='GHCND', locationid=location_id, limit=1000, token = Sys.getenv("rnoaa_api_key"))$data

    # Pull a vector of stations regex matching the name of the given city.
    station_list<-df_stations[grepl(paste(toupper(input$user_city),"*",sep=""),df_stations$name),]$id

    # Find the minimum and maximum available dates so we can iterate through and pull the full range
    min_weather_date<<-min(df_stations[grepl(paste(toupper(input$user_city),"*",sep=""),df_stations$name),]$mindate)
    max_weather_date<<-max(df_stations[grepl(paste(toupper(input$user_city),"*",sep=""),df_stations$name),]$maxdate)

    # Pull the initial data for the given stations at the maximum date available.
    df_station_data<-ncdc(datasetid = 'GHCND', stationid=station_list, limit=1000, token = Sys.getenv("rnoaa_api_key"), startdate = max_weather_date, enddate = max_weather_date)$data

    # Calculating the number of years in between the min and max dates.
    years_num<-as.numeric(substr(max_weather_date,1,4))-as.numeric(substr(min_weather_date,1,4))

    # Parse the current year, month, and day from the max_weather_date.
    cur_year<-substr(max_weather_date,1,4)
    cur_month<-substr(max_weather_date,6,7)
    cur_day<-substr(max_weather_date,9,10)

    # Iterate backwards through the same calendar date one year at a time (at a maximum of 25) and append the data to a dataframe.
    for (i in 0:min(c(years_num,25))) {

      i_year<-as.character(as.numeric(cur_year) - i)
      i_date<-paste(i_year,cur_month,cur_day,sep='-')

      print(paste('Pulling data for ',i_date,':', sep=''))

      df_station_data<-rbind(df_station_data, ncdc(datasetid = 'GHCND', stationid=station_list, limit=1000, token = Sys.getenv("rnoaa_api_key"), startdate = i_date, enddate = i_date)$data)

      # Because NOAA allows only 5 API calls per second, wait 0.21 seconds to not exceed the threshold.
      Sys.sleep(0.21)
    }


    # Aggregate all of the retrieved station data and take the average measure by date.
    # Some stations are incomplete, so we will "average by committee".
    df_station_agg<-setNames(aggregate(x=df_station_data[,c(1:2,4)], by=list(df_station_data[,c(1:2,4)]$date,df_station_data[,c(1:2,4)]$datatype), FUN=mean,na.rm=TRUE),c("date","datatype","dnu1","dnu2","value"))

    # Dropping unnecessary columns.
    df_station_agg<-df_station_agg[,c(1:2,5)]

    # Reshape the data in order to move measures to columns and have one row per date.
    df_station_unpivot<<-reshape(df_station_agg, direction="wide", idvar="date", timevar="datatype")

    # Converting "TAVG", "TMAX", "TMIN", and "TOBS" to degrees Fahrenheit.
    df_station_unpivot$value.TAVG<<-to_fahrenheit(df_station_unpivot$value.TAVG/10)
    df_station_unpivot$value.TMAX<<-to_fahrenheit(df_station_unpivot$value.TMAX/10)
    df_station_unpivot$value.TMIN<<-to_fahrenheit(df_station_unpivot$value.TMIN/10)
    df_station_unpivot$value.TOBS<<-to_fahrenheit(df_station_unpivot$value.TOBS/10)


    # Calculate z-scores
    df_station_unpivot$z.PRCP<<-scale(df_station_unpivot$value.PRCP)
    df_station_unpivot$z.SNOW<<-scale(df_station_unpivot$value.SNOW)
    df_station_unpivot$z.SNWD<<-scale(df_station_unpivot$value.SNWD)
    df_station_unpivot$z.TAVG<<-scale(df_station_unpivot$value.TAVG)
    df_station_unpivot$z.TMAX<<-scale(df_station_unpivot$value.TMAX)
    df_station_unpivot$z.TMIN<<-scale(df_station_unpivot$value.TMIN)
    df_station_unpivot$z.TOBS<<-scale(df_station_unpivot$value.TOBS)

    #output$mytable1 <- renderDataTable(df_station_unpivot)
    
    # Refresh the histogram with the retrieved data for the user's chosen measure.
    output$distPlot <- renderPlot({
      
      if (input$user_measure == "Precipitation (tenths of mm)"){
        dist <- df_station_unpivot$value.PRCP
        hist(dist
             ,main = "Histogram of Precipitation"
             ,xlab=input$user_measure
             )
      } else if (input$user_measure == "Snowfall (mm)") {
        dist <- df_station_unpivot$value.SNOW
        hist(dist
             ,main = "Histogram of Snowfall"
             ,xlab=input$user_measure
             )
      } else if (input$user_measure == "Snow depth (mm)") {
          dist <- df_station_unpivot$value.SNWD
          hist(dist
               ,main = "Histogram of Snow depth"
               ,xlab=input$user_measure)
      } else if (input$user_measure == "Average temperature (degrees F)") {
        dist <- df_station_unpivot$value.TAVG
        hist(dist
             ,main = "Histogram of Average temperature"
             ,xlab=input$user_measure
             )
      } else if (input$user_measure == "Maximum temperature (degrees F)") {
        dist <- df_station_unpivot$value.TMAX
        hist(dist
             ,main = "Histogram of Maximum temperature"
             ,xlab=input$user_measure)
      } else if (input$user_measure == "Minimum temperature (degrees F)") {
        dist <- df_station_unpivot$value.TMIN
        hist(dist
             ,main = "Histogram of Minimum temperature"
             ,xlab=input$user_measure)
      }
      
    })
    
    # Refresh the line plot with the retrieved data for the user's chosen measure.
    output$linePlot <- renderPlot({
      
      if (input$user_measure == "Precipitation (tenths of mm)"){
        plot(df_station_unpivot$value.PRCP~as.Date(df_station_unpivot$date)
             ,type="l"
             ,main = "Precipitation Over Time"
             ,xlab="Date"
             ,ylab=input$user_measure
             )
      } else if (input$user_measure == "Snowfall (mm)") {
        plot(df_station_unpivot$value.SNOW~as.Date(df_station_unpivot$date)
             ,type="l"
             ,main = "Snowfall Over Time"
             ,xlab="Date"
             ,ylab=input$user_measure
            )
      } else if (input$user_measure == "Snow depth (mm)") {
        plot(df_station_unpivot$value.SNWD~as.Date(df_station_unpivot$date)
             ,type="l"
             ,main = "Snow Depth Over Time"
             ,xlab="Date"
             ,ylab=input$user_measure
        )
      } else if (input$user_measure == "Average temperature (degrees F)") {
        plot(df_station_unpivot$value.TAVG~as.Date(df_station_unpivot$date)
             ,type="l"
             ,main = "Average Temperature Over Time"
             ,xlab="Date"
             ,ylab=input$user_measure
        )
      } else if (input$user_measure == "Maximum temperature (degrees F)") {
        plot(df_station_unpivot$value.TMAX~as.Date(df_station_unpivot$date)
             ,type="l"
             ,main = "Maximum Temperature Over Time"
             ,xlab="Date"
             ,ylab=input$user_measure
        )
      } else if (input$user_measure == "Minimum temperature (degrees F)") {
        plot(df_station_unpivot$value.TMIN~as.Date(df_station_unpivot$date)
             ,type="l"
             ,main = "Minimum Temperature Over Time"
             ,xlab="Date"
             ,ylab=input$user_measure
        )
  
      }
      
    })
    
    # Update the global today's "value" and Z score.
    update_todaysValue()
    update_zScore()

    # Update the message text with the new zScore, today's value, and maximum available weather date.
    update_messageText(zScore, as.character(todaysValue), max_weather_date)

  })
  
}

shinyApp(ui = ui, server = server)
