library(shiny)
library(ggmap)
ui <- fluidPage(
                    textInput('test',label = 'test', placeholder = 'City, postcode'),
                    textOutput('test')
  )
server <- function(input, output) {
  
  output$test<-reactive({
    location = ifelse(input$test=="", 'Diamond Creek, 3089', input$test)                 
    geocode(location, output = "all")$results[[1]]$formatted_address
                        
                        })
  }
shinyApp(ui = ui, server = server)