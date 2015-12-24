shinyUI(fluidPage(
        
    # Application title
    titlePanel("Baseball Hall of Fame - Batters"),
    
    sidebarLayout(
        
        # Sidebar with icon and usage notes
        sidebarPanel(
            img(src='mlb_hof_logo.png', align = "left", style="width:128px;height:128px;"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h3("Usage"),
            p("Using the slider values as minimum amounts this app shows the user the number of players in MLB history 
              that have passed those milestones in their careers, the number of players in that group that made it to 
              the Hall of Fame as well as the percentage chance that players with similar stats in the future will also make it into the Hall."),
            br(),
            br(),
            h3("Notes"),
            p("This app is intended to be a simplistic prototype of a more robust application that handles a wider range of inputs and 
              an enhanced prediction algorithm that uses data well beyond the scope of the current version.")
        ),
        
        # Slider inputs and table output of parameters and dervied prediction values
        mainPanel(
            sliderInput("hits",
                        "Number of hits:",
                        min = 0,
                        max = 4256,
                        value = 2000),
            sliderInput("hrs",
                        "Number of home runs:",
                        min = 0,
                        max = 762,
                        value = 300),
            sliderInput("stls",
                        "Number of steals:",
                        min = 0,
                        max = 1406,
                        value = 200),
            tableOutput("tableValues")
        )
    )
))