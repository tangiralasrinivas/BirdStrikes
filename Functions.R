
#---------------------------- Libraries ----------------------------- 
#   #Installing pacman
#   if (!require("pacman")) install.packages("pacman"); library(pacman)
#   #enable pacman to call the required libraries
# pacman::p_load(
#   base,
#   jsonlite,
#   lubridate,
#   plotly,
#   purrr,
#   shiny,
#   shinyjs,
#   sp,
#   tidyverse,
#   urltools,
#   utils,
#   xml2,
#   xts,
#   zoo,
#   tidyverse,
#   shinydashboard,
#   shinythemes,
#   shinyWidgets,
#   car,
#   caret,
#   Rtsne,
#   rebird,
#   leaflet.extras,
#   leaflet,
#   shinyBS,
#   owmr,
#   flexdashboard
# )
require(xgboost)
require(base)
require(jsonlite)
require(lubridate)
require(plotly)
require(purrr)
require(shiny)
require(shinyjs)
require(sp)
require(tidyverse)
require(urltools)
require(utils)
require(xml2)
require(xts)
require(zoo)
require(tidyverse)
require(shinydashboard)
require(shinythemes)
require(shinyWidgets)
require(car)
require(caret)
require(Rtsne)
require(rebird)
require(leaflet.extras)
require(leaflet)
require(shinyBS)
require(owmr)
require(flexdashboard)

  
#---------------------------- Predefined Values ----------------------------- 
  ###### Predefined Values 
  #Getting the Airfield Dataset
  data <- readRDS("AIRFIELDS_MASTER_V2.RDS")

  #List of Airfields to select in the User Interface
  airfields<-c("KDEN - Denver Int" = "KDEN",
               "KDFW - Dallas Ft Worth Int" = "KDFW",
               "KORD - Chicago OHare Int" =  "KORD",
               "KSMF - Sacramento Int" = "KSMF")
  
  #Airfields for Long & Lat
  air <- data.frame(
    airfields = c("KDEN",
                  "KDFW",
                  "KORD",
                  "KSMF"),
    latitude = c(39.86169815,
                 32.896801,
                 41.9786,
                 38.69540024),
    longitude = c(-104.6729965,
                  -97.038002,
                  -87.9048,
                  -121.5910034),
    strikes = c(2814,
                2170, 1557, 1891),
    modelfilename = c(
     # "data/model_xgb_KDEN.RDS",
      "model_kden_final.RDS",
      "model_kden_final.RDS",
      "model_kden_final.RDS",
      "model_kden_final.RDS"
    ),
    avgfilename = c(
      "Avgs_KDEN.RDS",
      "Avgs_KDFW.RDS",
      "Avgs_KORD.RDS",
      "Avgs_KSMF.RDS"
    )
  )
  
  #Ebird API Key
  EBIRD_KEY <- 'ldto4uofjnf7'
  
  #Weather APi
  Sys.setenv(OWM_API_KEY = "99f521810d0fef37f59930f36dbb2256")
  

  
#---------------------------- Functions ----------------------------- 
  # Purpose: Function generates the input to the model based on the
  # Averages of the strikes dataset based in Day Of Year. After generating 
  # the input, executes the predict on the model and returns the  
  # strike risk level and probability
  #
  # Input: Airfield - KDEN/KDFW/KORD/KSMF
  #        StrikeDate - Date to display the Strike Risk level
  #
  # Output: Dataframe containing data for model Input
  getDataAndRunPredict <- function(airfield, strikedate) {
    
    # Get Averages file name from the custom dataframe
    fileNames <-
      air %>% select(avgfilename, modelfilename) %>% filter(airfields == airfield)

    
    # Read the Averages from file system
    data <- readRDS(paste0(fileNames$avgfilename))

    # Convert the input to date format and convert to Day of Week
    data <-
      data %>% filter(DAYOFYEAR == lubridate::yday(as.Date(strikedate)))
    
    # Load model from RDS
    model <- readRDS((paste0(fileNames$modelfilename)))
    
    # Call predict with type = raw to get the risk levels
    predicted.raw <- predict(model, data, type = "raw")
    
    # Call Predict with type = prob to get probabilities
    predicted.prob <- predict(model, data, type = "prob")
    
    predicted.prob
    # Create results dataframe for use in UI
    strike.results <-
      data.frame("STRIKERISKLEVEL" = predicted.raw,
                 "STRIKEPROBABILITY" = predicted.prob)
    
    # Return dataframe
    strike.results
  }