library(leaflet)

# Choices for drop-downs
vars <- c(
  "Dependency Ratio" = "dependency_ratio",
  "Total Populaiton" = "total_pop"
  )


navbarPage("Global Urban Demographic Dataset (GUDD)", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Data Explorer"),
                                      
                                      selectInput("color", "Color", vars),
                                      selectInput("size", "Size", vars, selected = "total_pop"),
                                      conditionalPanel("input.color == 'dependency_ratio' || input.size == 'total_pop'",
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                                       numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      ),
                                      
                                      plotOutput("histDRatio", height = 200),
                                      plotOutput("scatterplot", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
                        )
                    )
           ),
           
           tabPanel("Data explorer",
                    fluidRow(
                      column(3,
                             selectInput("country_name", "Country", c("All Countries"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                      ),
                      column(3,
                             conditionalPanel("city_name",
                                              selectInput("city_name", "Cities", c("All cities"=""), multiple=TRUE)
                             )
                      ),                      )
                    ),
                    hr(),
                    DT::dataTableOutput("zonetable"),
           
           conditionalPanel("false")
)

