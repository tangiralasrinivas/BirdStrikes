#  #Requiring all of the predefined libraries and functions
source("appParts.R") 
source("Functions.R")
 

# #---------------------------- OTHER UI -----------------------------
shinyUI(
  navbarPage(
    title = tags$b("Bird Strike"),
    theme = "style/style.css",
    footer = includeHTML("footer.html"),
    fluid = TRUE,
    collapsible = TRUE,
    # tab panel 1 - Home
    tabPanel(
      tags$b("Home"),
      includeHTML("home.html"),
      tags$script(src = "plugins/scripts.js"),
      tags$head(tags$link(rel = "stylesheet",
                          type = "text/css"))
    ),
    # tab panel 2 - Neighborhood Browser
    tabPanel(tags$b("Strike Risk"),
             pilotApp()),
    
    # tab panel 3 - Location Comparison
    tabPanel(tags$b("Engine Failure Risk"),
             engineFailureApp()),
    
    # tab panel 4 - About
    tabPanel(
      tags$b("About the Team"),
      includeHTML("about.html"),
      shinyjs::useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet",
                  type = "text/css",
                  href = "plugins/carousel.css"),
        tags$script(src = "plugins/holder.js")
      ),
      tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
      )
    )
    
  )
)
