#load data into R
WDI_Data <- read.csv("~/R/WDI_Data.csv")

#load latlon for countries
latlon <- read.csv("~/R/latlon.csv", sep = ";") 

#create subset of data
df <- WDI_Data[, 5:59]          

#create matrix containing indicator name[1] 
#and indicator number[2]
vector.indicator <- 0:1344      
indicator <- as.character(WDI_Data[1:1345, 3])
indicator <- cbind(indicator, vector.indicator)

#create matrix containing country name[1] 
#and country number[2]
vector.country <- seq(1, 333560, by = 1345)
country <- as.character(WDI_Data[vector.country, 1])
country <- cbind(country, vector.country)

#function for data subsetting and printing a plot 
#a - country
#b - indicator
choice <- function(a, b) {

  #stopifnot(((a < 248) && (a > 0)) || ((b < 1345) && (b > 0)))
  
  #subset desired data from "df" data frame
  data <- df[as.numeric(country[a, 2]) 
     + as.numeric(indicator[b, 2]), ]
  
  #loop to check if there are only NA values in the subset
  for (i in 1:55) {
    
    #if there's any data print line plot
    if (is.na(data[, i]) == F) { 
      
      #print("Plot available")
      plot(1960:2014, data, type = 'l', 
           ylab = indicator[b, 1], 
           xlab = "Year",
           main = paste(indicator[b, 1], 
                        "for", country[a,1])
           )
      
      #dummy variable
      #if e = 0 there are only NAs in the data subset
      e = 1
      break
      
    } else {
      e = 0
    }       
  }
  
  #if no values print appropriate message
  if (e == 0) {
      print("No data available. NAs generated. Plot won't be printed")

  } else {
    
    #print summary data
    print("See summary data:")
    summary(as.numeric(data))  
  }
}

#function for linear model
#c - country
#d - first indicator (endogenous variable)
#e - second indicator (exogenous variable)
reg <- function(c, d, e) {
  
  #endogenous variable
  endogenous <- as.numeric(df[as.numeric(country[c, 2]) 
                              + as.numeric(indicator[d, 2]), ])
  
  #exogenous variable
  exogenous <- as.numeric(df[as.numeric(country[c, 2]) 
                             + as.numeric(indicator[e, 2]), ])
  
  #loop to check if there are only NA values in the endogenous subset
  for (i in 1:55) {
    
    #initialize dummy variable
    q = 0
    
    #check endogenous subset
    if (is.na(endogenous[i]) == F) { 
        q = 0      
        break
        
      } else {
        q = 1      
      }
  }
  
  #loop to check if there are only NA values in the exogenous subset 
  for (i in 1:55) {
    
    #initialize dummy variable
    p = 0
    
    #check exogenous subset
    if (is.na(exogenous[i]) == F) { 
      p = 0      
      break
      
    } else {
      p = 1      
    }
  } 
  
  #create model if no NAs
  if (q == 0 & p == 0) {
    
    #create model
    model <- lm(endogenous ~ exogenous)
    names(model$coefficients)[2] <- "Explanatory variable"
    
    #show summary
    return(summary(model))
  
  #otherwise print appropriate message
  } else if (q == 1 & p == 0) {    
    print("Choose other indicator for endogenous variable")
  } else if (q == 1 & p == 1) {    
    print("Choose other indicator for endogenous variable") 
    print("Choose other indicator for exogenous variable")
  } else if (q == 0 & p == 1) {
    print("Choose other indicator for exogenous variable") 
  }
}

timeseries <- function(a, b) {
  
  #stopifnot(((a < 248) && (a > 0)) || ((b < 1345) && (b > 0)))
  
  #subset desired data from "df" data frame
  data <- df[as.numeric(country[a, 2]) 
             + as.numeric(indicator[b, 2]), ]

}

############ widget slider demo ####################

#subset data
data <- df[as.numeric(country[3, 2]) 
           + as.numeric(indicator[66, 2]), ]

#years vector
years <- as.Date(seq(ISOdate(1960,1,1), ISOdate(2014,1,1), "years"),
                 "%Y-%m-%d", tz="")

#change column names to match xts requirements
colnames(data) <- years

#transpose data to mach xts requirements
data <- t(data)

#check datatype
#str(data)

#create xts object
data_xts <- as.xts(data)

#library(dygraphs)

#widget
slider_test <- dygraph(data_xts, main = "xts_test") %>% 
  dyRangeSelector(dateWindow = c("1960-01-01", "2014-01-01"))


######### map widget demo #############
require(ggmap)

#use latlon csv
