# Remove all variables
rm(list=ls())

library(dplyr)

# Read CSV File
UberData <- read.csv("Uber Request Data.csv")

#======= Data Cleaning and Preparation ========

# Check duplicate values

  sum(duplicated(UberData$Request.id)) #No Duplicates

# Checking and validating missing values

  # All rows should have Request Time Stamp, Pickup point and Request ID
  sum(is.na(UberData$Request.timestamp)) #No missing data
  sum(is.na(UberData$Request.id)) #No missing data
  sum(is.na(UberData$Pickup.point)) #No missing data
  
  # Check Similar values
  levels(UberData$Status) #All distinct values, unambiguous, No need to lower case
  levels(UberData$Pickup.point) #All distinct values, unambiguous, No need to lower case

  # Request.id and Pickup.point should not have any missing Values
  sum(is.na(UberData$Request.id)) #No missing values
  sum(is.na(UberData$Pickup.point)) #No missing values

  # Completed Trips should have Both Pickup and Drop Time stamps and Driver ID
  CompletedTrips <- UberData[which(UberData$Status == "Trip Completed"),]
  sum(is.na(CompletedTrips$Drop.timestamp)) #All Timestamps present
  sum(is.na(CompletedTrips$Driver.id)) #All Timestamps present
  
  # Cancelled Trips should have Pickup Time stamps and Driver ID
  CancelledTrips <- UberData[which(UberData$Status == "Cancelled"),]
  sum(is.na(CancelledTrips$Request.timestamp)) #All Timestamps present
  sum(is.na(CancelledTrips$Driver.id)) #All Timestamps present


# COLUMN Request.id : OK
# COLUMN Pickup.point : OK. 2 unambiguous levels. No issues. Not required to lowercase.
# COLUMN Driver.id : OK
# COLUMN Status : OK. 3 Unambiguous levels. No issues. Not required to lowercase.
# COLUMN Request.timestamp and Drop.timestamp
    # Should be a date vector
  

    # NOT USING LUBRIDATE LIBRARY, TRYING TO DO WITH R INBUILT FUNCTION
  
    
    #------ .. CORRECTING REQUEST TIME -------
    
    # Identifying rows with Date format %d/%m/%Y %H:%M using regex
    slashDates_Request <- grep(pattern = '^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{1,2}$',x=UberData$Request.timestamp)
    
    # Identifying rows with Date format %d-%m-%Y %H:%M:%S using regex
    hyphenDates_Request <- grep(pattern = '^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$',x=UberData$Request.timestamp)
    
    # Check for missed format by taking count of identified patterns in previous step and compare with dataset
    length(slashDates_Request) + length(hyphenDates_Request) == length(UberData$Request.timestamp) #TRUE , Hence No other date formats
  
    # New POSIXct column named POSIXRequestTime
    UberData$POSIXRequestTime <- as.POSIXct(NA)
    
    # Update POSIXct values to new column, hence standardising Date Time using respective identified format
    UberData$POSIXRequestTime[slashDates_Request] <- as.POSIXct(UberData$Request.timestamp[slashDates_Request],format = '%d/%m/%Y %H:%M')
    UberData$POSIXRequestTime[hyphenDates_Request] <- as.POSIXct(UberData$Request.timestamp[hyphenDates_Request],format = '%d-%m-%Y %H:%M:%S')
    
    # Check for any missing values
    which(is.na(UberData$POSIXRequestTime)) #No missing Values
    
    
    
    #------ .. CORRECTING DROP TIME ------
    
    # Identifying rows with Date format %d/%m/%Y %H:%M using regex
    slashDates_Drop <- grep(pattern = '^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{1,2}$',x=UberData$Drop.timestamp)
    
    # Identifying rows with Date format %d-%m-%Y %H:%M:%S using regex
    hyphenDates_Drop <- grep(pattern = '^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$',x=UberData$Drop.timestamp)
    
    # Check for missed format by taking count of identified patterns in previous step as well as NA in source column and compare with dataset
    length(slashDates_Drop) + length(hyphenDates_Drop) + sum(is.na(UberData$Drop.timestamp)) == length(UberData$Drop.timestamp) #TRUE , Hence No other date formats
    
    # New POSIXct column names POSIXRequestTime
    UberData$POSIXDropTime <- as.POSIXct(NA)
    
    # Update POSIXct values to new column, hence standardising Date Time
    UberData$POSIXDropTime[slashDates_Drop] <- as.POSIXct(UberData$Drop.timestamp[slashDates_Drop],format = '%d/%m/%Y %H:%M')
    UberData$POSIXDropTime[hyphenDates_Drop] <- as.POSIXct(UberData$Drop.timestamp[hyphenDates_Drop],format = '%d-%m-%Y %H:%M:%S')
    
    # Removing unwanted Columns Request.timestamp and Drop.timestamp as new columns created with POSIXct
    UberData <- UberData[,c(1:4,7:8)]
    
    # Deriving Columns
    UberData$HourOfTheDay <- NA # New Column to identify Which Clock Hour of the Day the request was made
    UberData$HourOfTheDay <- as.factor(format(UberData$POSIXRequestTime,"%H"))
    
    UberData$PartOfDay <- NA # New Column to identify Which Part of the Day the request was made
    UberData$PartOfDay <- sapply(as.numeric(as.vector(UberData$HourOfTheDay)),function(x){
      if(x %in% c(4,5,6)){
        return('Early Morning')
      }else if(x %in% c(7,8,9)){
        return('Morning')
      }else if(x %in% c(10,11)){
        return('Late Morning')
      }else if(x %in% c(12,13,14)){
        return('Afternoon')
      }else if(x %in% c(15,16,17)){
        return('Early Evening')
      }else if(x %in% c(18,19,20)){
        return('Late Evening')
      }else if(x %in% c(21,22,23)){
        return('Night')
      }else if(x %in% c(0,1,2,3)){
        return('Late Night')
      }else{
        return(NA)
      }
    })
    
    # Ordering the factor levels
    UberData$PartOfDay = factor(UberData$PartOfDay,levels = c('Early Morning','Morning','Late Morning','Afternoon','Early Evening','Late Evening','Night','Late Night'))
    
    
    
#======= Data Analysis ========
 
    library(ggplot2)
    
    
  #------ .. Univariate Analysis ------
    # Request.iD : It is unique identifier for each request
    # Pickup.point : Categorical variable (Airport, City)
      # Frequency of requests from Pickup point POV
      summary(UberData$Pickup.point)
    
      # Plot for the above
      # Bar chart is optimal to observe the frequency based on pickup location
      ggplot(UberData,aes(x=Pickup.point)) + 
        geom_bar() +
        labs(x = "Pickup Point", 
             y="Number of Requests", 
             title = "Overall Demand from Airport and City") +
        geom_text(aes(y=(..count..),
                  label=(..count..)),
                  stat='count',
                  vjust = -0.3,
                  size = 5)
      # Explanation : bar chart is used, labs has been used to change graph labels, geom_text for adding text to graph with 
      # vjust for adjusting text position, ..count.. identity to specify that the frequency itself is used and size for text size
        
      # VERDICT : There are equivalent numbers of requests both to and from airport
    
    # Driver.id : Unique driver id. Can serve as Categorical variable
    # Status : Categorical Variable ("Cancelled","No Cars Available","Trip Completed")
      # Frequency of Responses/Status to Requests
      summary(UberData$Status)
      
      # Pot for above
      # Bar chart is optimal to observe the frequency
      ggplot(UberData,aes(x=Status, 
             fill = Status)) + 
        geom_bar() + 
        scale_fill_manual(values = c('#b81313','#b81313','#6dc14b')) +
        geom_text(aes(y = (..count..), 
                  label = scales::percent((..count..)/sum(..count..))),
                  stat = 'count', 
                  vjust = -0.4,
                  size = 5) +
        labs(x = 'Status',
             y='Number of Requests', 
             title='Overall Response to Demand')
      # Explanation : bar chart is used, labs has been used to change graph labels, geom_text for adding text to graph with 
      # vjust for adjusting text position, ..count.. identity to specify that the frequency itself is used and size for text size
      # scale_fill_manual used to cange colors of bars with hex code
      
      # VERDICT : There is a significant difference in Demand and Supply. Only 42% customer requests are successfully served
      
    # POSIXRequestTime : Ordered Categorical
      summary(as.factor(format(UberData$POSIXRequestTime,'%a'))) 
      #Number of requests based on Week Day is almost equivalent
      
    # POSIXDropTime : Ordered Categorical
      summary(as.factor(format(UberData[which(UberData$Status == 'Trip Completed'),c("POSIXDropTime")],'%a'))) 
      #Number of drops are also equivalent. The difference in data is because of midnight drops
      
    # HourOfTheDay : Ordered Categorical
      summary(UberData$HourOfTheDay)
      
      # Plot for above
      # Bar chart is optimal to observe the frequency with color
      ggplot(UberData,aes(x=HourOfTheDay, fill = ..count..)) + 
        geom_bar() + 
        scale_fill_gradient(low = '#b81313', high = 'green', space = 'lab') +
        geom_text(aes(y=(..count..),
                      label=(..count..)),
                  stat='count',
                  angle=90,
                  hjust = -0.2)+
        labs(x = "Hour of The Day",
             y="Number of Requests", 
             fill = 'No. of Requests',
             title='Overall Demand Throughout The Day')+
        coord_cartesian(ylim = c(0,600))
      
      # Explanation : bar chart is used, labs has been used to change graph labels, geom_text for adding text to graph with 
      # vjust for adjusting text position, ..count.. identity to specify that the frequency itself is used and size for text size
      # scale_fill_manual used to cange colors of bars with hex code
      
      # Verdict : There is a high demand 5am to 9am and also 5pm to 9pm
      
      # Plot for demand based on time as well as day
      # Multiple Bar charts to observe and compare the frequency
      ggplot(UberData,aes(x=HourOfTheDay, fill = ..count..)) + 
        geom_bar() + 
        scale_fill_gradient(low = '#b81313', high = 'green', space = 'lab') +
        geom_text(aes(y=(..count..),
                  label=(..count..)),
                  stat='count',
                  vjust = -0.2,
                  size = 3)+
        labs(x = "Hour of The Day",
             y="Number of Requests", 
             fill = 'No. of Requests',
             title='Daily Demand Throughout The Day')+
        coord_cartesian(ylim = c(0,120))+
        facet_grid(format(POSIXRequestTime,"%d-%b")~.)
      
      # Explanation : used facet to above graph to check consistency of demand
      
      # Verdict : There is a similar demand everyday at similar timings
      
      
    # PartOfDay : Ordered Categorical
      summary(UberData$PartOfDay)
      
      # Plot to find the frequency of requests based on location/Pickup point
      # Bar chart is optimal to observe the frequency and comparision
      ggplot(UberData,aes(x=PartOfDay, fill = ..count..)) + 
        geom_bar() + 
        geom_text(aes(y=(..count..),
                  label=(..count..)),
                  stat='count',
                  vjust = -0.4,
                  size=4.7) +
        scale_fill_gradient(low = '#b81313', high = 'green', space = 'lab')+
        labs(x = "Part of The Day",
             y="Number of Requests", 
             fill = 'No. of Requests',
             title='Overall Demand Throughout The Day (Simplified)') +
        facet_grid(Pickup.point~.) + theme(strip.text.y = element_text(size = 15))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
        coord_cartesian(ylim = c(0,1300))
     
      # Explanation : bar chart is used, labs has been used to change graph labels, geom_text for adding text to graph with 
      # vjust for adjusting text position, ..count.. identity to specify that the frequency itself is used and size for text size
      # scale_fill_manual used to cange colors of bars with hex code 
      # used facet to check demand based on Pickup Location
      
      # Verdict : There is a high demand for cabs from City to Airport in the Early Morning and Morning hours
      #           There is a high demand for cabs from Airport to City in the Late Evening
      #           However the vice versa is not true
      #           This shows that there are more outbound flights in the Mornings and inbound flights in the evening
      #           Rest of the times there is quite less demand to and from Airport
      
      
  
      
      
  #------ .. Segmented Univariate Analysis ------
      
      
      
    # Analysis of Status based on Request Location
      
      UberData[,c('Pickup.point','Status')] %>%
        group_by(Pickup.point,Status) %>%
        summarise(Status.Count = length(Status))
          
      
      ggplot(UberData,aes(x = Status, fill = Status)) +
        geom_bar() +
        scale_fill_manual(values = c('#b81313','#400808','#6dc14b')) +
        labs(x = "Status", 
             y = "Number of Requests", 
             title = 'Overall Supply to Demand', 
             subtitle = 'Based on Status of Request and Location')+
        geom_text(data = UberData[UberData$Pickup.point == 'Airport',],
                  aes(y = (..count..), 
                  label = scales::percent(((..count..)/sum(..count..)))),
                  stat = 'count',
                  hjust = -0.03, 
                  size = 4)+
        geom_text(data = UberData[UberData$Pickup.point == 'City',],
                  aes(y = (..count..),
                      label = scales::percent(((..count..)/sum(..count..)))),
                  stat = 'count',
                  hjust = -0.03, 
                  size = 4)+
        coord_flip()+
        facet_grid(Pickup.point ~ .)+
        theme(legend.position="none",
              strip.text.y = element_text(size = 15))
      
      # Explanation : bar chart is used, labs has been used to change graph labels, geom_text for adding text to graph with 
      # vjust for adjusting text position, ..count.. identity to specify that the frequency itself is used and size for text size
      # Also geom text uses seperate subset of dataset for the sake of calculation based on request location
      # scale_fill_manual used to cange colors of bars with hex code
      
      # Verdict :  It is clearly visible that Customers requesting cabs from Airport get way more 'No Cars Available' compared to customers from City
      # Also Customers from city to Airport get a lot of rejects from drivers
      # Drivers are avoiding trip to airport and also this results in non availability of drivers from airport
        
    # Analyzing Status of requests from both locations at differents timings
      
      UberData %>%
        group_by(Pickup.point,HourOfTheDay,Status) %>%
        summarise(Status.Count = length(Status))
      
      # Plot to analyze demand and supply based on pickup point and Time of request
      # Multiple juxtaposed Bar chart is optimal to observe the frequency and copmarision
      UberData %>%
        group_by(Pickup.point,HourOfTheDay,Status) %>%
        summarise(Status.Count = length(Status)) %>%
      ggplot(aes(x=HourOfTheDay)) +
        geom_bar(aes(y=Status.Count,color = 'Total Demand'),
                 stat = 'identity', 
                 fill = 'grey') +
        geom_bar(aes(y=Status.Count,fill = Status), 
                 position = position_dodge(width=0.9),
                 stat = 'identity')+
        scale_fill_manual(values = c('#b81313','#400808','#6dc14b')) +
        scale_color_manual('',values = c('grey')) +
        facet_grid(Pickup.point ~ .)+
        theme(strip.text.y = element_text(size = 15))+
        labs(x='Hour Of The Day', 
             y='Count of Demand and Supply', 
             title='Demand and Supply Gap',
             subtitle='Based On Time of The Day')
      
      # Explanation : bar chart is used, labs has been used to change graph labels,
      # Text not used to keep it clean and simple and to find obvious inferences
      # Facet grid is used to seperate the graphs based on pickup point
      # scale_fill_manual used to cange colors of bars with hex code
      # Here two bar graphs are extrapolated to show the demand (in grey) and supply (in colors)
      
      # Analyzing Status of requests from both locations at different parts of the day for simplification  
      
      UberData %>%
        group_by(Pickup.point,PartOfDay,Status) %>%
        summarise(Status.Count = length(Status))
      
      # Plot to analyze demand and supply based on pickup point and TimeSlot of request
      # Multiple juxtaposed Bar chart is optimal to observe the frequency and copmarision
      UberData %>%
        group_by(Pickup.point,PartOfDay,Status) %>%
        summarise(Status.Count = length(Status)) %>%
        ggplot(aes(x=PartOfDay,fill = Status)) +
        geom_bar(aes(y=Status.Count, color = "Total Demand"),
                 stat = 'identity', 
                 fill ='grey') +
        geom_bar(aes(y=Status.Count), 
                 position = position_dodge(width=0.9),
                 stat = 'identity')+
        scale_fill_manual(values = c('#b81313','#400808','#6dc14b')) +
        scale_color_manual('',values = c('grey')) +
        facet_grid(Pickup.point ~ .)+
        theme(strip.text.y = element_text(size = 15))+
        labs(x='Hour Of The Day', 
             y='Count of Demand and Supply', 
             title='Demand and Supply Gap', 
             subtitle='Based On Time Slot') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      # Explanation : bar chart is used, labs has been used to change graph labels,
      # Text not used to keep it clean and simple and to find obvious inferences
      # Facet grid is used to seperate the graphs based on pickup point
      # scale_fill_manual used to cange colors of bars with hex code
      # theme is used to adjust the size of facet grid text
      # labs used to rename labels
      # Here two bar graphs are extrapolated t show the demand (in grey) and supply (in colors)
      
