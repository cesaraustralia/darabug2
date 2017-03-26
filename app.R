library(shiny)
library(raster)
library(sp)
library(reshape2)
library(ggplot2)
library(plyr)
library(ggmap)
library(leaflet)
library(shinythemes)
library(maptools)  ## For wrld_simpl
library(rgdal)

# global
source('./getBug.R')
source('./develop.fun.R')


################## INTIALISE DATA #######################
myLabelFormat = function(...,dates=FALSE){ 
  if(dates){ 
    function(type = "numeric", cuts){ 
      format(as.Date(cuts, origin="1970-01-01"), '%b-%d')
    } 
  }else{
    labelFormat(...)
  }
}

Tmin<-brick('data/mu_Tmin_for_DOY_ag10.tif')
Tmax<-brick('data/mu_Tmax_for_DOY_ag10.tif')

curYear <- format(Sys.time(), '%Y')

bug.files<- list.files('bugs/')
bugs<-sapply(X = bug.files, FUN = strsplit, '[.]')
bugs<-unlist(bugs)[(1:length(bugs))*2-1]
bugList<-list()

#build list of all bugs with data 
 # test 
for (bug in bugs){
  insect<-getBug(bug)
  bugList[insect$name]<-bug
}


r<- Tmax[[1]]
projection(r)<-'+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
## Example SpatialPolygonsDataFrame
data(wrld_simpl)
SPDF <- subset(wrld_simpl, NAME=="Australia")
## crop and mask
r1 <- mask(r, SPDF)

#UI 
ui <- 
  shinyUI(navbarPage("DARABUG2",selected = 'Local', theme = shinytheme("superhero"),
                     #################### LOCAL UI ####################
                     tabPanel("Local",
                          fluidRow(
                            column(width = 4,offset =0, "",
                              selectInput("species", label = h4("1. Species observed:"), 
                                          choices = bugList, 
                                          selected = bugList[[1]],
                                          width = '100%'),
                              dateInput('startDate', label = h4("2. Date observed:"), 
                                        value = paste0(curYear,'-6-1'), 
                                        min = paste0(curYear,'-1-1'), 
                                        max = paste0(curYear,'-12-31'),
                                        format = "dd-MM", startview = "month", weekstart = 0,
                                        language = "en", width = '100%'),
                              uiOutput("startStage"),
                              textInput("location",label = h4("4. Input city and postcode, or select location on map "), placeholder = "Location, Postcode"),
                              textOutput('location'),
                              HTML('<br/>'),
                              fluidRow(
                                column(10, offset = 1,
                                       leafletOutput("map1")
                                )
                              )
                              
                            ),
                            column(8, 
                                   fluidRow(
                                     column(4,
                                            HTML('<br/>'),
                                            h4('5. Run simulation'),
                                            actionButton("update", "Run")
                                           
                                     ),
                                     column(4, 
                                             HTML('<br/>'),
                                             h4('6. Run another simulation or reset plot'),
                                             actionButton("reset", "Reset"),
                                             HTML('<br/><br/><br/>')
                                     ),
                                     column(4, 
                                            HTML('<br/>'),
                                            h4('7. Download data as table'),
                                            downloadButton('downloadData.csv', 'Download'),
                                            HTML('<br/><br/><br/>')
                                     )
                                   ),
                                   fluidRow(
                                     div(
                                       style = "position:relative",
                                       plotOutput("phenology", 
                                                  hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                                       uiOutput("hover_info")
                                     )
                                   )
                                   )
                          )    
                     ),
                     ################# REGIONAL UI ########################
           tabPanel("Regional", id = 'regional',includeCSS("styles.css"),
                    div(class="outer",includeCSS("styles.css"),
                        tags$style(type = "text/css", ".outer {color:black; position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                        leafletOutput("map2", width = "100%", height = "100%"),
                        absolutePanel(top = 100, right = 10, draggable=TRUE,
                                      h4(tags$b('Regional Prediction')),
                                      selectInput("species2", label = h5("1. Species observed:"), 
                                                  choices = bugList, 
                                                  selected = bugList[[1]],
                                                  width = '100%'),
                                      uiOutput("startStage2"),
                                      dateInput('startDate2', label = h5("3. Date observed:"), 
                                                value = paste0(curYear,'-6-1'), 
                                                min = paste0(curYear,'-1-1'), 
                                                max = paste0(curYear,'-12-31'),
                                                format = "dd-MM", startview = "month", weekstart = 0,
                                                language = "en", width = '100%'),
                                      # actionButton('reload', 'Reload'),
                                      h5('5. Run simulation'),
                                      actionButton("update2", "Run", width = '100%'),
                                      uiOutput("endStage2"),
                                      sliderInput('dateRange2',label = h5('7. Restrict legend range:'),
                                                     value = as.Date(c(paste0(curYear,'-1-1'), paste0(curYear,'-12-31'))),
                                                     min = as.Date(paste0(curYear,'-1-1')),max = as.Date(paste0(curYear,'-12-31'))),
                                      HTML('<br/>')
                                      
                        ))
           ),
           ################ ABOUT UI #######################
           tabPanel("About",
                    fluidRow(column(5,'',
                                    h1('DARABUG2'),
                                    h2('Description'),
                                    h4('Effective management of insect pests in crops requires an understanding of the rate at which insects develop. For example, it might be important to know how long a damaging stage of a pest may persist in the crop, or when eggs might hatch. The DARABUG2 program provides a convenient and readily available means of predicting development times using different insect models. Gridded climatic data of daily temperatures is used in these models to generate estimates of the dates of occurrence for each stage throughout the whole life-cycle of an insect.'),
                                    h2('Climate data'),
                                    h4('The Australian gridded climatic data used to estimate developmental times is derived from 15-year averages of the max and min temperatures at each day of the year. A daily temperature profile is calculated using a simple trigonometric function, with an amplitude spanning the max and min daily temperatures over a period of 24 hours.'),
                                    h2('Insect data'),
                                    h4('The rate of growth and development of insects and other invertebrates is strongly influenced by temperature. The temperature dependence of development varies between species, thus each species has a unique temperature response. Indeed, even within a species the temperature dependence may vary between different stages. This is accounted for in the model by assigning unique developmental functions to each stage of each insect. This functional response is derived from empirical data.'),
                                    h4('The temperature - growth rate relationship for each of the pest species modelled in this platform can be viewed opposite. The species-specific variables and rate functions for each were derived from published records, and can be varied in consultation with Dr James Maino. Similarly, new insect models for different pests can be added to this platform at any time, when based on published empirical data.'),
                                    h4('For the maintenance and updating of this tool, please contact Dr James Maino (info@cesaraustralia.com).
                                       ')
                                    ),
                             column(5,'',
                                    selectInput("species3", label = h4("Select species:"), 
                                                choices = bugList, 
                                                selected = bugList[[1]]),
                                    plotOutput('tempresponse'),
                                    HTML('<br/>'),
                                    textOutput('source')
                                    )
                      
                    )
           )
  )
)
# SERVER 
server <- function(input, output, session){
  ######################### LOCAL PLOT ###################################
  output$startStage <- renderUI({
    insect<-getBug(input$species)
    stageList<-lapply(1:length(names(insect$dev.funs)), FUN = function(x) x)
    names(stageList)<-names(insect$dev.funs)
    selectInput("startStage", label = h4("3. Life stage observed:"), 
                choices = stageList, 
                selected = 2, width = '100%')
  })
  ## Make your initial map
  output$map1 <- renderLeaflet({
    leaflet() %>%
      setView(lng = 135.51, lat = -25.98, zoom = 4) %>%
      setMaxBounds(lng1 = 111.975 ,
                   lat1 = -9.975,
                   lng2 = 156.975,
                   lat2 = -44.975)%>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>',
        options = providerTileOptions( minZoom = 4)
      )
  })
  
  ## Observe mouse clicks and add circles
  observeEvent({
    input$map1_click
    # input$map1_zoom
    }, {
    ## Get the click info like had been doing
    click <- input$map1_click
    clat <- click$lat
    clng <- click$lng
    # address <- revgeocode(c(clng,clat))
    
    ## Add the circle to the map proxy
    ## so you dont need to re-render the whole thing
    ## I also give the circles a group, "circles", so you can
    ## then do something like hide all the circles with hideGroup('circles')
    
    leafletProxy('map1') %>% # use the proxy to save computation
      clearShapes() %>%
      addCircles(lng=clng, lat=clat, # group='circles',
                 weight=1, radius=20000/input$map1_zoom, color='black', fillColor='orange',
                 # popup=address, 
                 fillOpacity=0.5, opacity=1)
  })
  output$location <- reactive({ 
    fulllocation = ifelse(input$location=="", 'Diamond Creek, 3089', paste0(input$location, ', Australia'))             
    location<-try(geocode(fulllocation, output = "all")$results[[1]]$formatted_address)
    if(class(location)=='try-error'){
      location = 'Diamond Creek, 3089'
      stop('cannot find location')
      }
    sprintf(location)
  })
  observe({
    click <- input$map1_click
    if(!is.null(click)){
      fullLocation <- revgeocode( c(click$lng,click$lat), output = 'more')
      location<-paste0(fullLocation$locality,', ', fullLocation$administrative_area_level_1)
      # This will change the value of input$location, based on location
      if(nchar(location)<5) location =""
      updateTextInput(session, "location", value = location)
    }
  })

  values <- reactiveValues()
  values$df <- NULL
  values$count<-1
  values$raster<-r1
  # values$raster[]<-NA
  values$regionalSim<-NULL
  # values$setpoints <- data.frame(start = NULL, species = NULL)
  observe({
    if(input$reset>0){
     isolate({ 
       values$df <- NULL
       values$count <-1
     })
    }
  })
  newEntry <- observe({
    if(input$update>0) {
      location<-paste0(isolate(input$location),', Australia')
      if(nchar(location) <16)location = 'Diamond Creek, VIC 3089' # if unrealistic location length
      fullLocation <- geocode(location, output = 'more')
      
      withProgress(message = "LOADING. PLEASE WAIT...", value = 0, { # create progress bar
        isolate({
          startDay<-as.numeric(format(input$startDate,'%j'))
          TMAX <- extract(Tmax, matrix(c(fullLocation$lon, fullLocation$lat), ncol = 2))
          TMIN <- extract(Tmin, matrix(c(fullLocation$lon, fullLocation$lat), ncol = 2))
          startStage<-as.numeric(input$startStage)
          insect<-getBug(input$species)
          location<-paste0(fullLocation$locality,', ', fullLocation$administrative_area_level_1)
          # browser()
          data<-develop(TMAX,TMIN, startDay, startStage, insect)
          
        })
        # browser()
        updateTextInput(session, "location", value = location)
      })
      df<-as.data.frame(data[1,,])
      df$stage<-names(insect$dev.funs)
      df$life<-insect$life
      df$location<-location
      df$species<-paste0(stringr::str_pad( isolate(values$count), 2, pad='0'), '. ',insect$name, '\n',location)
      mdf <- melt(df, measure.vars = c("Time_start", "Time_end"))
      mdf$value <- as.Date(paste0(curYear,'-1-1')) + mdf$value
      isolate({
        values$df <-rbind(values$df ,mdf)
        values$setpoints <-rbind(values$setpoints,
                                 data.frame(start=as.Date(paste0(curYear,'-1-1'))+startDay-1,
                                            species=df$species[1]
                                 )
        )
        values$count<-values$count+1
      })
    }
  })
  output$phenology<- renderPlot({
    if(input$update>0){
      if(isolate(length(values$df))){
        if(TRUE){
          data<-values$df
          data$life<-factor(data$life)
          if('adult'%in%levels(data$life))
            data$life<-factor(data$life, levels =  c(levels(factor(data$life))[-1],levels(factor(data$life))[1])  ,ordered = TRUE)
         
          
          # setPoints = list()
          # for(i in 1:nrow(values$setpoints)){
          #   x<-isolate(as.Date(values$setpoints[i,1]))
          #   y<-isolate(values$setpoints[i,2])
          #   setPoints[[i]]<-geom_point(aes(x=x,y=y),
          #                                shape = 124, size = 10,colour = 'red')
          # }
  
          p<-ggplot(data)+
            geom_line(aes(value, species, colour = life),size = 6) +
            geom_point(aes(value, species), colour = 'black', size=6)+
            ylab(NULL) +
            xlab(NULL) +
            # setPoints+
            geom_vline(xintercept = isolate(as.numeric(input$startDate)))+
            geom_text(aes(x=isolate(input$startDate), label="date observed", y=data$species[1]), colour=rgb(0.5,0.5,0.5), vjust = 2.2,hjust = .33)+
            scale_x_date(limits = c(min(data$value), max(data$value)),
                         date_breaks = "1 weeks",date_minor_breaks = '1 week',
                         date_labels = "%d %b" )+
            theme_bw()+
            theme(text = element_text(size=20,family='Nirmala UI', color = 'white'),
                  axis.text.x = element_text(color = 'white',angle=45, vjust=1, hjust=1),
                  axis.text.y = element_text(color = 'white'),
                  legend.title=element_blank(),
                  legend.key = element_blank(),
                  panel.grid.minor = element_blank(), 
                  panel.grid.major = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  legend.background = element_blank(),
                  axis.line.x = element_line(color="white"),
                  plot.background = element_blank()
            )
          # # browser()
          # p<-ggplot(data ,aes(value, Stage_duration, colour = life))+
          #     geom_point(colour = 'black')
          return(p)
        }
      }
    }else{return(NULL)}

  },bg="transparent")
  output$hover_info <- renderUI({
    # browser()
    input$plot_hover
    data<-isolate(values$df)
    if(length(data)>0){
      
      hover <- input$plot_hover
      point <- nearPoints(data, coordinfo = hover, threshold = 10, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; color:black; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Stage: </b>", point$stage, "<br/>",
                      "<b> ",point$variable,": </b>", format(point$value,'%d-%b'), "<br/>",
                      "<b> Duration (d): </b>", round(point$Stage_duration,1), "<br/>"
                      # "<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px
                      )))
      )
    }else{return(NULL)}
  })
  output$downloadData.csv <- downloadHandler(
    filename = function() { paste(input$dataset, '.csv', sep='') },
    content = function(file) {
      df<-values$df
      dfw<-dcast(df,formula = species + location + life + stage + Stage_duration ~variable)
      dfw$Time_end<-as.Date(dfw$Time_end,origin = '1970-1-1')
      dfw$Time_start<-as.Date(dfw$Time_start,origin = '1970-1-1')
      write.csv(dfw, file)
    }
  )
  ################################### REGIONAL PLOT #################################

  output$startStage2 <- renderUI({
    insect<-getBug(input$species2)
    stageList<-lapply(1:length(names(insect$dev.funs)), FUN = function(x) x)
    names(stageList)<-names(insect$dev.funs)
    selectInput("startStage2", label = h5("2. Life stage observed:"), 
                choices = stageList, 
                selected = 1, width = '100%')
  })
  output$endStage2 <- renderUI({
    insect<-getBug(input$species2)
    stageList<-lapply(1:length(names(insect$dev.funs)), FUN = function(x) x)
    names(stageList)<-names(insect$dev.funs)
    selectInput("endStage2", label = h5("6. Life stage to predict:"), 
                choices = stageList, 
                selected = length(stageList), width = '100%')
  })
    # A reactive expression that returns a cropped raster based on screen bounds
    # in bounds right now
    rasterBounds <- reactive({
      input$stage2
        if (is.null(input$map2_bounds))
          return(isolate(values$raster))
        bounds <- input$map2_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        # browser()
        tryCatch(crop(values$raster, extent(c(lngRng, latRng))),
                 error = function(x)values$raster)
    })
    pal <- reactive( {
      # browser()
              # if 
              # tryCatch(colorNumeric(c("#EE0000","#B2DFEE","#007f00"), values(rasterBounds()),
              #           na.color = "transparent"), 
              #          error = colorNumeric(c("#EE0000","#B2DFEE","#007f00"), 1,
              #                               na.color = "transparent"))
      colorNumeric(c("#EE0000","#B2DFEE","#007f00"), values(rasterBounds()),
                   na.color = "transparent")
                })
    output$map2 <- renderLeaflet({
      leaflet() %>%
        setView(lng = 135.51, lat = -25.98, zoom = 4) %>%
        addTiles(
          # urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          # attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          # options = providerTileOptions( minZoom = 4)
        )
      # %>% addRasterImage(rasterBounds(), opacity=0.5, layerId = 'rasimg')
    })
    observe({
      if(input$update2>0&!all(is.na(values(rasterBounds())))){
        # browser()
        leafletProxy('map2')%>% 
          removeTiles(layerId="rasimg") %>% 
          addRasterImage(rasterBounds(), opacity=0.5, layerId = 'rasimg',colors = pal()) %>%
          clearControls() %>% 
        addLegend(pal = pal(), values = values(rasterBounds()),
                  title = paste0(names(getBug(input$species2)$dev.funs)[as.integer(input$endStage2)],
                                 ' monitoring \n date'), position = 'bottomleft',
                  labFormat = myLabelFormat(dates=TRUE))
      }else{
        # if no data clear raster image
        leafletProxy('map2')%>%
          removeImage(layerId="rasimg")%>%
          clearControls()
        }
    })
    TMAX <- Tmax+0.000001 # save to memory with +0.000001
    TMIN <- Tmin+0.000001
    observe({
      if(input$update2 > 0) {
        withProgress(message = "LOADING. PLEASE WAIT...", { # create progress bar
          isolate({
            startDay<-as.numeric(format(input$startDate2,'%j'))
            startStage<-rep(as.numeric(input$startStage2),length(TMAX[[1]]))
            endStage<-as.numeric(input$endStage2)
            insect<-getBug(input$species2)
            # load('data.Rdata') # bypass computation during debugging
            data<-develop(TMAX,TMIN, startDay, startStage, insect)
            values$regionalSim<-data
            # browser()
            values$raster[]<-data[,endStage,'Time_end']
            values$raster <- mask(values$raster, SPDF)
          })
        })
      }
    })
    observe({
      input$endStage2
      input$dateRange2
      # browser()
      if(!is.null(values$regionalSim)){
        # browser()
        isolate({
        endStage<-as.numeric(input$endStage2)
        values$raster[]<-values$regionalSim[,endStage,'Time_end']
        # browser()
        minDate<-as.numeric(format(input$dateRange2[1],'%j'))
        maxDate<-as.numeric(format(input$dateRange2[2],'%j'))
        values$raster[values$raster[]<minDate]<-NA
        values$raster[values$raster[]>maxDate]<-NA
        values$raster <- mask(values$raster, SPDF)
        })
      }
    })
    
    ########################## INSECT PLOT PAGE ###########################
    # include about here
    output$tempresponse<- renderPlot(bg="transparent",{
      temps<- seq(0,50,length = 1000)
      insect<-getBug(input$species3)
      df <- data.frame(temp = temps)
      stages<-names(insect$dev.funs)
      for (stage in stages){
        df[,stage]<-insect$dev.funs[[stage]](temps)  
      }
      dl<-reshape2::melt(df, id = 'temp', variable.name = 'stage', 
                         value.name = 'dev')
      p<-ggplot() + geom_line(data = dl, aes(x = temp, y = dev, linetype = stage, color = stage)) +
        xlab('Temperature (C)')+
        ylab('Development rate (1/d)')+ 
        theme(text = element_text(size=20, colour = 'white'), #family='Nirmala UI',
              axis.text = element_text(size=20, colour = 'white'),
              legend.title=element_blank(),
              legend.key = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              legend.background = element_blank(),
              axis.line.x = element_line(color="white"),
              axis.line.y = element_line(color="white"),
              plot.background = element_blank(),
              axis.ticks = element_line(color = "white")
        )
      
      
      return(p)
    })
    output$source<-reactive({
      insect<-getBug(input$species3)
      return(paste('Source:', insect$source))
      })  
    
  } # server(...


  
  # output$vals <- renderPrint({
  #   hover <- input$plot_hover 
  #   y <- nearPoints(values$df, input$plot_hover)[,c('stage','variable','value','Stage_duration')]
  #   # y <- nearPoints(data(), input$plot_hover)["wt"]
  #   y<-subset(y, variable == 'Time_start')
  #   req(nrow(y) != 0)
  #   # y is a data frame and you can freely edit content of the tooltip 
  #   # with "paste" function 
  #   
  #   print(sprintf('%s stage begins on %s \nlasting %1.1f days','egg', format(as.Date('2016-1-1'),'%d-%b'), 2.33))
  # })


shinyApp(ui = ui, server = server)
