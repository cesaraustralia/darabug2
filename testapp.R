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
  
  # output$tempresponse<- renderPlot({
  # ggplot()+geom_line(aes(1:10,1:10))
  # })
  }
shinyApp(ui = ui, server = server)