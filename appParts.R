pilotApp <- function() {
  tagList(
    setBackgroundColor(
      color = c("#F7FBFF", "#87CEFA"),
      gradient = "radial",
      direction = "bottom"
    ),
    div(
      class = "container",
      h1("Bird Strike Risk Predictor", class = "title fit-h1"),
      # p(HTML('<img src="images/WTF.png" width="50%" alt="3" class="center">')),
      p(HTML("The predictive model developed from analyzing Denver International Airport&#39s bird strikes; 
             this tool enables pilots and other interested parties the information needed to prepare for any preflight planning operations. 
             To utilize the tool, Select the Airfield and Date of interest. <br> <p style  = 'font-size: 17px; text-align:center'> 
             <i> *Historical Strike Count is based based on data between January 1st, 2000 to December 31st, 2019 for strikes at or below 2000 feet. 
             Any strikes not meeting this critera were disregarded.</i></p>"),style="font-size:21px;text-align:center"),
      
      fluidRow(tags$div(class = "container",
                        column(
                          6,
                          div(
                            class = "selectAirfield",
                            tags$style(
                              type = 'text/css',
                              ".selectize-input { font-size: 18px; line-height: 20px;} .selectize-dropdown { font-size: 18px; line-height: 20px; }"
                            ),
                            selectInput("airfield", tags$h4(tags$b(
                              "Select Airfield:"
                            )),
                            airfields),
                            style = "width: 60%; height: 200%",
                            class = "search"
                          )
                        ),
                        column(
                          6,
                          # tags$style('.input-sm {font-size: 14px; } label {font-weight: 500; margin-bottom: 15px; }'),
                          div(
                            class = "selectSearchAirfield",
                            airDatepickerInput(
                              "date",
                              tags$h4(tags$b("Select Date: ")),
                              value = as.character(Sys.Date()),
                              minDate = as.character(Sys.Date()),
                              maxDate = Sys.Date() + 150,
                              width = "12px"
                            ),
                            style = "width: 60%; height: 60%",
                            class = "search"
                          )
                        ))),
      
      fluidRow(tags$br()),
      
      fluidRow(
        tags$div(column(4,
                        tags$div(tags$h4(tags$b(
                          "Bird Strike Risk Level:"
                        ))),
                        wellPanel(
                          tags$style(".well {background-color:Azure;}"),
                          tags$h4(
                            p(uiOutput("vboxrisk"), style = "font-size: 100%; color:blue;")
                          ))
        ),
        style = "width: 91%; margin-left: 121px;"),
        
        tags$div(column(4,
                        tags$div(tags$h4(tags$b(
                          "Historical Strike Count: "
                        )),
                        wellPanel(
                          tags$style(".well {background-color:Azure;}"),
                          tags$h4(
                            p(uiOutput("vboxstrikes"), style = "font-size: 150%; color:blue;")
                          ))
                        )),
                 style = "width: 91%; margin-left: 692px;"),
        
      ),
      
      fluidRow(tags$div(column(
        6,
        p(HTML("<p>Map Information:</p><ul>
  <li>The blue circle around the marker indicates  a 10 mile radius around the Airfield selected. </li>
  <li>The heat map marked on the map indicate the reported birds in the  <b> LAST 24 HOURS </b> within a 30 mile radius of the Airfield selected.</li>
  <li>The icons on cities local to the Airfield shows the <b> CURRENT </b> weather conditions such as precipitation, temperature, sky conditions (zooming out of the map provides overall precipitation trends) </li>
</ul>"),style="font-size:16px;")
      ),
      style = "width: 200%;")),
      
      fluidRow(tags$br()),
      
      fluidRow(tags$br()),
      
      fluidRow(column(
        12,
        align = "center",
        div(
          width = 10,
          title = "Map of Airfield:",
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          height = "100%",
          leafletOutput(
            outputId = "map",
            width = "100%",
            height = 600
          )
        )# closes the div of the map 
      )),
      fluidRow(tags$br(),tags$p("This application and model was constructed using data provided by eBird from the Cornell Lab of Ornithology. 
                               The availability of this predictive bird strike model is intended to be used for flight planning in order to reduce 
                               the likelihood of a bird strike. Please refer to the credits and disclaimers for data source information and references. 
                               Our team and affiliation is not liable for losses incurred as a result of a bird strike."))
      #ends the fluid row on line 93
    )
  )
}


engineFailureApp <- function() {
  output.data <- readRDS("engine_pred_data.RDS")
  numengs <- levels(output.data$numengs)
  season <- levels(output.data$season)
  
  tagList(
    div(
      class = "container",
      h1("Engine Failure Analysis", class = "title fit-h1"),
      fluidRow(tags$br()),
      
      p(
        HTML(
          "Based on our modeling and statistical analysis, we analyze engine failure in various scenarios and provide recommendations to the Aircrew. 
          To utilize the App, select the Number of Engines and the current Season."),
        style = "font-size:21px;"),
      
      fluidRow(tags$br()),
      
      fluidRow(tags$div(class = "container",
                        column(
                          6,
                          div(
                            class = "selectNumengs",
                            tags$style(
                              type = 'text/css',
                              ".selectize-input { font-size: 18px; line-height: 20px;} .selectize-dropdown { font-size: 18px; line-height: 20px; }"
                            ),
                            selectInput("numengs",
                                        tags$div(tags$h4(
                                          tags$b("Select Number of Engines:")
                                        )),
                                        choices = numengs),
                            style = "width: 60%; height: 200%; margin-left: 121px;",
                            class = "Engsearch"
                          )
                        ),
                        column(
                          6,
                          div(
                            class = "selectSeasons",
                            selectInput("season",
                                        tags$div(tags$h4(
                                          tags$b("Select Season:")
                                        )),
                                        choices = season),
                            style = "width: 60%; height: 60%;",
                            class = "Engsearch"
                          )
                        ))),
      
      fluidRow(tags$br()),
      
      fluidRow(tags$br()),
      
      fluidRow(
        tags$div(
          class = "container",
          tags$div(tags$h4(
            tags$b("Probability of Engine Failure by Phase of Flight and Altitude")
          )),
          plotlyOutput("phPlot"),
          style = "width: 100%; height: 400%;"
        )
      ),
      
      fluidRow(tags$br()),
      
      p(
        HTML(
          "Our prediction output shows the probability of engine failure for each phase of flight based on altitude levels. 
          The comparison between phase of flight scenarios is consistent with our exploratory data analysis, such that we determined there is a greater number of engine 
          failures upon the climb phase of flight than on approach. However, with prediction, we observe that the probability of engine failure converges at 
          higher altitude levels regardless of the phase of flight. Likely this could be due to the lower number of birds flying at the higher altitude levels. 
          Overall, we conclude that the probability of engine failure is most impacted by phase of flight at the lower altitude levels. Therefore, we recommend that:"),
        style = "font-size:21px;"),
      p(
        HTML(   
          "<ul>
          <li style='font-size:20px'>Pilots should clear lower altitudes as efficiently as possible during the climb phase. This can be achieved by 
          adjusting speed and flap settings to provide higher rates of climb</li><br>
          <li style='font-size:20px'> Flight crews should delay takeoff and landing when there are reported cases of nearby flocks of birds </li> <br>
          <li style='font-size:20px'> Aviation experts should provide pilot training programs for more emergency landing procedures.</li></ul><br>"),
        style = "font-size:21px;"),
      
      fluidRow(tags$br()),
      
      fluidRow(
        tags$div(
          class = "container",
          tags$div(tags$h4(
            tags$b("Probability of Engine Failure by Sky Conditions and Altitude")
          )),
          plotlyOutput("skyPlot"),
          style = "width: 100%; height: 400%;"
        )
      ),
      
      fluidRow(tags$br()),
      
      p(
        HTML(
          "Our prediction output shows the probability of engine failure for the various sky conditions based on altitude levels. 
          Initially during exploratory analysis, no cloud conditions were associated with the largest number of engine failures, 
          though margins were slight between all conditions, and we determined results to be inconclusive due to the large number of unknown sky conditions. 
          Post modeling, our prediction output shows that no cloud conditions yield 2x less probability of engine failure than some cloud and overcast conditions, 
          and this is consistent over all altitude levels. However, as expected, the probability of engine failure decreases for all bird sizes as altitude increases. 
          We continue to check our hypothesis that there is an overall lower number of birds flying at higher altitude levels. To mitigate engine failure at the lower altitude levels 
          in poor sky conditions (overcast or some cloud), we recommend that:"),
        style = "font-size:21px;"),
      
      p(
        HTML(   
          "<ul>
          <li style='font-size:20px'> Pilots should remain alert at lower altitudes and in reduced visibility conditions</li><br>
          <li style='font-size:20px'> Flight crews / Pilot training programs should dispel any misconceptions that birds only fly in clear skies </li> <br>
          <li style='font-size:20px'> Pilot training programs should consider adding emergency landing / taking off simulations in reduced visibility situations.</li></ul><br>"),
        style = "font-size:21px;")
    )
  )
}