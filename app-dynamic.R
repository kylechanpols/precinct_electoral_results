#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)

#source("try.R") #load & manipulate data

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Dashboard Proof of Concept"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("select_election", h3("Please choose an election:"),
                        choices = list("2020 Presidential" = "2020 Presidential",
                                       "2016 Presidential" = "2016 Presidential",
                                       selected = "2020 Presidential")),
            uiOutput("select_candidates"),
            shiny::actionButton("update", "Update")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput(outputId = "finalmap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        # create leaflet
        pal <- colorFactor(c("#FF0000","#4C00FF",
                             "#00E5FF", "#00FF4D", "#FFFF00",
                             "#0013FF"), domain = map$winner, na.color = "#808080")
        
        
        getDataset <- reactive({
            tmp <- map
            if(!is.null(input$select_election))
                map_out <- tmp[tmp$election == input$select_election,]
            return(map_out)

        })
        
        getCandidates <- reactive({
            map_out <- getDataset()
            req(map_out$winner)
            return(na.omit(unique(map_out$winner)))
            })
        
        filterDataset <- reactive(
            {
                req(input$candidates)
                map_out <- getDataset()
                m <- map_out[map_out$winner %in% input$candidates,]
            }
            )
        
        output$select_candidates <- renderUI({
                checkboxGroupInput("candidates",
                                   "Please choose the candidates you want to display:",
                                   choices = getCandidates(),
                                   selected=NULL,
                                   inline=FALSE)
            })
        
        #render now
        
        drawmap <- eventReactive(input$update,{
            
            l <- NULL
            
            map_out <- filterDataset()
            

            
            labels <- sprintf("County: %s <br/> Precinct ID: %s <br/> Winner: %s <br/>
                          Winner Party: %s <br/> Votes Received pc.:%g <br/> Votes Received: %g",
                              map_out$COUNTY, map_out$SRPREC, map_out$winner, map_out$winner_party,
                              round((map_out$elect_max_prop)*100,3), map_out$elect_max) %>%
                lapply(htmltools::HTML) 
            
            l <- leaflet(map_out) %>%
                addTiles() %>%
                addPolygons(
                    fillColor = ~ pal(map_out$winner),
                    color = "white",
                    dashArray = "1",
                    fillOpacity = 0.7,
                    label = labels
                ) %>%
                leaflet::addLegend(
                    pal = pal, values = ~winner,
                    opacity = 0.7, title = NULL
                ) %>%
                setView(lat=37.82873220922176, lng=-122.28539235976625, zoom=10)
            
            }
        )
        
        output$finalmap <- renderLeaflet({
            drawmap()
            }
        )
}

# Run the application 
shinyApp(ui = ui, server = server)
