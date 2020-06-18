source("Functions.R")

output.data <- readRDS("engine_pred_data.RDS")

output.data$predicted <- round(output.data$predicted, 4)

function(input, output, session) {
  
  # Renders the map on the pilot app page
  
  output$map <- renderLeaflet({
    #get the coordinates of the input of the user
    coord <- air %>%
      filter(airfields == input$airfield)
    lat <- coord[1, 2]
    long <- coord[1, 3]
    
    #get the weather data on the map
    owm_data <-
      find_city(lat = coord[1, 2],
                lon = coord[1, 3] ,
                units = "imperial") %>%
      owmr_as_tibble()
    
    #get the bird data
    birds <- rebird::ebirdgeo(
      species = NULL,
      lat = coord[1, 2],
      lng = coord[1, 3],
      back = 1,
      dist = as.numeric(units::set_units(30, "mi")),
      key = EBIRD_KEY
    )
    #group the bird data
    birds <- birds %>% group_by(lat, lng) %>%
      summarise(howMany = sum(howMany))
    
    #map the data
    leaflet(birds) %>% addProviderTiles(providers$OpenStreetMap) %>%
      setView(zoom = 10.5,
              lat = coord[1, 2],
              lng = coord[1, 3]) %>%
      addProviderTiles(
        providers$OpenWeatherMap.Precipitation,
        options = providerTileOptions(apiKey = "99f521810d0fef37f59930f36dbb2256")
      ) %>%
      owmr::add_weather(owm_data,
                        template = "<b>{{name}}</b>, {{temp}}<c2><b0>F",
                        icon = owm_data$weather_icon) %>%
      addMarkers(
        lng = air$longitude,
        lat = air$latitude,
        popup = names(air)
      ) %>%
      addCircles(
        ~ as.numeric(air$longitude),
        ~ as.numeric(air$latitude),
        weight = 1,
        radius = ~ 10000
      ) %>%
      leaflet.extras::addHeatmap(
        lat = ~ birds$lat,
        lng = ~ birds$lng,
        blur = 20,
        max = 0.05,
        radius = 15
      )
  })
  
  
  # Renders the text required for the historical strikes
  
  output$vboxstrikes <- renderUI({
    t <- data %>%
      filter(`AIRPORT ID` == input$airfield) %>%
      group_by(`AIRPORT ID`) %>%
      summarise(STRIKES = sum(STRIKECOUNT)) %>%
      arrange(-STRIKES)
    
    tags$h2(tags$b(
      prettyNum(t[1, 2], big.mark = ","),
      style = "font-size: 100%; color:steelblue;",
      HTML(
        "<p style = 'font-size: 10px; opacity:0;'><b>Click for Engine Failure Risk</b></p>"
      )
    ))
  })
  
  # Renders the text required for Bird strikes
  output$vboxrisk <- renderUI({
    risk <- getDataAndRunPredict(input$airfield, input$date)
    
    if (risk[1, 1] == "L") {
      tags$h2(tags$b(
        "LOW RISK",
        style = "font-size: 100%; color:green;",
        HTML(
          "<p style = 'font-size: 10px; opacity:0;'><b>Click for Engine Failure Risk</b></p>"
        )
      ))
    }
    else if (risk[1, 1] == "M") {
      tags$h2(tags$b(
        "MEDIUM RISK",
        style = "font-size: 100%; color:Chocolate",
        HTML(
          "<p style = 'font-size: 10px; opacity:0;'><b>Click for Engine Failure Risk</b></p>"
        )
      ))
    }
    else
      tags$h2(tags$b(
        tags$a(onclick = "$('li:eq(2) a').tab('show');", "HIGH RISK", style = "font-size: 100%; color:FireBrick"),
        style = "font-size: 100%; color:FireBrick",
        HTML(
          "<p style = 'font-size: 10px;' > <b>Click for Engine Failure Risk</b></p>"
        )
      ))
  })
    
    ###################################################################################################################
    
    # Engine Failure App Server Components
    
    ###################################################################################################################
  

  output$phPlot <- renderPlotly({
    analyze.phs <-
      output.data %>% dplyr::select(height, phase_of_flt, sky, eng_fail, predicted) %>%
      dplyr::filter((output.data$numengs == input$numengs) &
                      (output.data$height > 0 &
                         output.data$height < 10000) &
                      (output.data$season == input$season)
                    &
                      output.data$phase_of_flt %in% c(
                        "approach",
                        "arrival",
                        "climb",
                        "departure",
                        "descent",
                        "landingroll",
                        "takeoffrun"
                      )
      )
    
    ggplot(analyze.phs, aes(height, predicted, color = phase_of_flt)) +
      stat_smooth(
        method = "glm",
        family = binomial,
        formula = y ~ x,
        alpha = 0,
        size = 1,
      ) +
      theme_bw() +
      theme(
        text = element_text(family = "Calibri", face = "bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(
          size = 10,
          face = "bold",
          angle = 0
        ),
        axis.text.y = element_text(
          size = 10,
          face = "bold",
          angle = 0
        ),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.background = element_rect(fill = "transparent"),
        # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA),
        # bg of the plot
        legend.background = element_rect(fill = "transparent"),
        # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      ) +
      scale_y_continuous(limits = c(0, 0.015)) +
      labs(
        x = "Altitude",
        y = "Probability of Engine Failure",
        color = "Phase of Flight"
      )
    ggplotly()
  })
  
  output$skyPlot <- renderPlotly({
    analyze.phs <-
      output.data %>% dplyr::select(height, phase_of_flt, sky, eng_fail, predicted) %>%
      dplyr::filter((output.data$numengs == input$numengs) &
                      (output.data$height > 0 &
                         output.data$height < 10000) &
                      (output.data$season == input$season) &
                      (output.data$sky %in% c("nocloud", "overcast", "somecloud"))
      )
    
    ggplot(analyze.phs, aes(height, predicted, color = sky)) +
      stat_smooth(
        method = "glm",
        family = binomial,
        formula = y ~ x,
        alpha = 0,
        size = 1,
      ) +
      theme_bw() +
      theme(
        text = element_text(family = "Calibri", face = "bold"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(
          size = 10,
          face = "bold",
          angle = 0
        ),
        axis.text.y = element_text(
          size = 10,
          face = "bold",
          angle = 0
        ),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        panel.background = element_rect(fill = "transparent"),
        # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA),
        # bg of the plot
        legend.background = element_rect(fill = "transparent"),
        # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      ) +
      scale_y_continuous(limits = c(0, 0.011)) +
      labs(
        x = "Altitude",
        y = "Probability of Engine Failure",
        color = "Sky Conditions"
      )
    ggplotly()
  })
}



