


library(shinythemes)
library(ggplot2)
library(shiny)
library(stringr)
library(leaflet)
library(plotly)
library(DT)
ozona_hr <- readRDS("data/hourly_outside_zona2Tue_Jul_19.rds")
ozona <- readRDS("data/instant_outside_zona2Tue_Jul_19.rds")
izona_hr <- readRDS("data/hourly_inside_zona2Tue_Jul_19.rds")
izona <- readRDS("data/instant_inside_zona2Tue_Jul_19.rds")
oestrella_hr <- readRDS("data/hourly_outside_estrella2Tue_Jul_19.rds")
oestrella <- readRDS("data/instant_outside_estrella2Tue_Jul_19.rds")
iestrella_hr <- readRDS("data/hourly_inside_estrella2Tue_Jul_19.rds")
iestrella <- readRDS("data/instant_inside_estrella2Tue_Jul_19.rds")

# User interface ----
ui <-
  navbarPage(strong("IQ Air Devices in Torreon, Mexico"),
             collapsible = F, inverse = TRUE, theme = shinytheme("united"),
             ## Home page ----
             tabPanel("About",
                      fluidRow(
                        column(
                          p(("Developed February 2022"), br(),br(),
                          strong("Shiny App Creator:")," Flannery Black-Ingersoll, MPH", br(),
                            strong("Principal Investigator:"), "M. Patricia Fabian, ScD, BUSPH", style ="text-align:center;color:black;font-size:22px"),
                          br(),
                          p("This Shiny Application was built to support
                         the data collected from ", a(strong("IQ Air Visual Pro Devices"), href="https://www.iqair.com/us/air-quality-monitors/airvisual-series"), "installed in Torreon, Mexico. As of 2/15/2022, we
                         have four devices installed in Torreon, Mexico. The interactive plots and tables provided show
                            hourly and 15-minute data findings. These devices 
                            record meteorological and air quality data, including:", br(),br(),
                            strong("(a) Particulate matter (1, 2.5, and 10 (ug/m^3))"), br(),
                            strong("(b) Temperature (Celsius and Fahrenheit provided)"),br(),
                            strong("(c) Humidity (%)"), br(),
                            strong("(d) CO2 (ppm)"),br(),
                            style="text-align:justify;color:black;background-color:light blue;padding:15px;border-radius:10px;font-size:18px"),
                          p("Please note that this is",strong("raw data"),"and has not been QC'd, so there may be outliers. We aim to update these plots weekly.",style="text-align:center;color:black;background-color:light blue;padding:15px;border-radius:10px;font-size:20px"),
                          br(),
                          br(),
                          p("Explore the tabs at the top to get started!", 
                            style="text-align:center;color:black;background-color:papayawhip;padding:15px;border-radius:10px;font-size:18px"),
                          
                          width=12),
                        br(),
                        br(),
                        br(),
                        br(),
                        p(strong("Last Updated: February 16, 2022"),style="text-align:center;font-size:16px")
                        ),
                      
                      hr()
                      
                      ),
             ## Zona page ----
             tabPanel("Industrial Zone",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Time Series Plots", br(),
                                   
                                     mainPanel(width = 12,
                                               selectInput("var1", 
                                                           label = "Hourly Outdoor",
                                                           choices = c("PM10 (ug/m^3)",
                                                                       "PM2.5 (ug/m^3)",
                                                                       "PM1 (ug/m^3)",
                                                                       "Temp (Fahrenheit)",
                                                                       "Temp (Celsius)",
                                                                       "CO2 (ppm)",
                                                                       "Humidity (%)"), 
                                                           selected = "PM2.5 (ug/m^3)"),
                                               plotlyOutput("plot1a"),
                                               selectInput("var2", 
                                                           label = "Hourly Indoor",
                                                           choices = c("PM10 (ug/m^3)",
                                                                       "PM2.5 (ug/m^3)",
                                                                       "PM1 (ug/m^3)",
                                                                       "Temp (Fahrenheit)",
                                                                       "Temp (Celsius)",
                                                                       "CO2 (ppm)",
                                                                       "Humidity (%)"), 
                                                           selected = "PM2.5 (ug/m^3)"),
                                               plotlyOutput("plot1b"),
                                               selectInput("var3", 
                                                           label = "15-min Outdoor",
                                                           choices = c("PM10 (ug/m^3)",
                                                                       "PM2.5 (ug/m^3)",
                                                                       "PM1 (ug/m^3)",
                                                                       "Temp (Fahrenheit)",
                                                                       "Temp (Celsius)",
                                                                       "CO2 (ppm)",
                                                                       "Humidity (%)"), 
                                                           selected = "PM2.5 (ug/m^3)"),
                                               plotlyOutput("plot1c"),
                                               selectInput("var4", 
                                                           label = "15-min Indoor",
                                                           choices = c("PM10 (ug/m^3)",
                                                                       "PM2.5 (ug/m^3)",
                                                                       "PM1 (ug/m^3)",
                                                                       "Temp (Fahrenheit)",
                                                                       "Temp (Celsius)",
                                                                       "CO2 (ppm)",
                                                                       "Humidity (%)"), 
                                                           selected = "PM2.5 (ug/m^3)"),
                                               plotlyOutput("plot1d"))
                                   ),
                          tabPanel("Map", br(),
                                   
                                   helpText(h4("Zona Industrial, Torreon, Mexico")),
                                   br(),
                                   leafletOutput("map1", width="1800", height="800"),
                                   br()),
                          tabPanel("Hourly Data Table: Outdoors", br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       br(strong("Units:")),
                                       br("PM2.5 (ug/m^3)"), 
                                       br("PM10 (ug/m^3)"), 
                                       br("CO2 (ppm)"), 
                                       br("PM1 (ug/m^3)"), 
                                       br("Humidity (%)")),
                                     mainPanel(
                                       DT::dataTableOutput("table1"),
                                       br()))),
                          tabPanel("Hourly Data Table: Indoors",br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       br(strong("Units:")),
                                       br("PM2.5 (ug/m^3)"), 
                                       br("PM10 (ug/m^3)"), 
                                       br("CO2 (ppm)"), 
                                       br("PM1 (ug/m^3)"), 
                                       br("Humidity (%)")),
                                     mainPanel(
                                       DT::dataTableOutput("table2"),
                                       br()))),
                        ))),
             
             ## Estrella page ----
             tabPanel("Estrella",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Time Series Plots", br(),
                                   
                                     mainPanel(width = 12,
                                               selectInput("var1a", 
                                                           label = "Hourly Outdoor",
                                                           choices = c("PM10 (ug/m^3)",
                                                                       "PM2.5 (ug/m^3)",
                                                                       "PM1 (ug/m^3)",
                                                                       "Temp (Fahrenheit)",
                                                                       "Temp (Celsius)",
                                                                       "CO2 (ppm)",
                                                                       "Humidity (%)"), 
                                                           selected = "PM2.5 (ug/m^3)"),
                                               plotlyOutput("plot2a"), 
                                               selectInput("var2a", 
                                                           label = "Hourly Indoor",
                                                           choices = c("PM10 (ug/m^3)",
                                                                       "PM2.5 (ug/m^3)",
                                                                       "PM1 (ug/m^3)",
                                                                       "Temp (Fahrenheit)",
                                                                       "Temp (Celsius)",
                                                                       "CO2 (ppm)",
                                                                       "Humidity (%)"), 
                                                           selected = "PM2.5 (ug/m^3)"),
                                               plotlyOutput("plot2b"),
                                               selectInput("var3a", 
                                                           label = "15-min Outdoor",
                                                           choices = c("PM10 (ug/m^3)",
                                                                       "PM2.5 (ug/m^3)",
                                                                       "PM1 (ug/m^3)",
                                                                       "Temp (Fahrenheit)",
                                                                       "Temp (Celsius)",
                                                                       "CO2 (ppm)",
                                                                       "Humidity (%)"), 
                                                           selected = "PM2.5 (ug/m^3)"),
                                               plotlyOutput("plot2c"), 
                                               selectInput("var4a", 
                                                           label = "15-min Indoor",
                                                           choices = c("PM10 (ug/m^3)",
                                                                       "PM2.5 (ug/m^3)",
                                                                       "PM1 (ug/m^3)",
                                                                       "Temp (Fahrenheit)",
                                                                       "Temp (Celsius)",
                                                                       "CO2 (ppm)",
                                                                       "Humidity (%)"), 
                                                           selected = "PM2.5 (ug/m^3)"),
                                               plotlyOutput("plot2d"))
                                   ),
                          tabPanel("Map", br(),
                                   helpText(h4("Estrella, Torreon, Mexico")),
                                   br(),
                                   leafletOutput("map2", width="1800", height="800"),
                                   br()),
                          tabPanel("Hourly Data Table: Outdoors", br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       br(strong("Units:")),
                                       br("PM2.5 (ug/m^3)"), 
                                       br("PM10 (ug/m^3)"), 
                                       br("CO2 (ppm)"), 
                                       br("PM1 (ug/m^3)"), 
                                       br("Humidity (%)")),
                                     mainPanel(
                                       DT::dataTableOutput("table3"),
                                       br()))),
                          tabPanel("Hourly Data Table: Indoors", br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       br(strong("Units:")),
                                       br("PM2.5 (ug/m^3)"), 
                                       br("PM10 (ug/m^3)"), 
                                       br("CO2 (ppm)"), 
                                       br("PM1 (ug/m^3)"), 
                                       br("Humidity (%)")),
                                     mainPanel(
                                       DT::dataTableOutput("table4"),
                                       br()))),
                        )))
  )



# Define server logic for random distribution app ----
server <- function(input, output) {
  ## Zona ----
  # Create the map
  output$map1 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -103.4614819148291, lat = 25.561707130807406, zoom = 15 )
  }) 
   
  # Tables 
  output$table1 <- renderDataTable({
    ozona_hr
  })
  output$table2 <- renderDataTable({
    izona_hr
  })
  output$table3 <- renderDataTable({
    oestrella_hr
  })
  output$table4 <- renderDataTable({
    iestrella_hr
  })
  output$plot1a <- renderPlotly({
    yvar <- switch(input$var1,
                   "PM10 (ug/m^3)" = ozona_hr$PM10,
                   "PM2.5 (ug/m^3)" = ozona_hr$PM2.5,
                   "PM1 (ug/m^3)" = ozona_hr$PM1,
                   "CO2 (ppm)" = ozona_hr$CO2,
                   "Humidity (%)" = ozona_hr$Humidity,
                   "Temp (Fahrenheit)" = ozona_hr$Temp.Fahrenheit,
                   "Temp (Celsius)" = ozona_hr$Temp.Celsius
    )
    plot1<- ggplotly(ggplot(ozona_hr, aes(x=datetime, y=yvar))+ 
                       geom_point(color = "red")+
                       ggtitle(paste0("Hourly Outdoor ", input$var1))+
                       ylab(paste0(input$var1))+
                       xlab("Datetime")+
                       theme_minimal()+ 
                       theme(plot.title = element_text(face = "bold", size = 18))+
                       theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  output$plot1b <- renderPlotly({
    yvar <- switch(input$var2,
                   "PM10 (ug/m^3)" = izona_hr$PM10,
                   "PM2.5 (ug/m^3)" = izona_hr$PM2.5,
                   "PM1 (ug/m^3)" = izona_hr$PM1,
                   "CO2 (ppm)" = izona_hr$CO2,
                   "Humidity (%)" = izona_hr$Humidity,
                   "Temp (Fahrenheit)" = izona_hr$Temp.Fahrenheit,
                   "Temp (Celsius)" = izona_hr$Temp.Celsius
    )
    plot2<- ggplotly(ggplot(izona_hr, aes(x=datetime, y=yvar))+ 
                       geom_point(color = "red")+
                       ggtitle(paste0("Hourly Indoor ", input$var2))+
                       ylab(paste0(input$var2))+
                       xlab("Datetime")+
                       theme_minimal()+ 
                       theme(plot.title = element_text(face = "bold", size = 18))+
                       theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  output$plot1c <- renderPlotly({
    yvar <- switch(input$var3,
                   "PM10 (ug/m^3)" = ozona$p1,
                   "PM2.5 (ug/m^3)" = ozona$p2,
                   "PM1 (ug/m^3)" = ozona$p01,
                   "CO2 (ppm)" = ozona$co,
                   "Humidity (%)" = ozona$hm,
                   "Temp (Fahrenheit)" = ozona$tp,
                   "Temp (Celsius)" = ozona$tp_C
    )
    plot3<- ggplotly(ggplot(ozona, aes(x=datetime, y=yvar))+
                       geom_point(color = "red")+
                       ggtitle(paste0("Instant Outdoor ", input$var3))+
                       ylab(paste0(input$var3))+
                       xlab("Datetime")+
                       theme_minimal()+ 
                       theme(plot.title = element_text(face = "bold", size = 18))+
                       theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  output$plot1d <- renderPlotly({
    yvar <- switch(input$var4,
                   "PM10 (ug/m^3)" = izona$p1,
                   "PM2.5 (ug/m^3)" = izona$p2,
                   "PM1 (ug/m^3)" = izona$p01,
                   "CO2 (ppm)" = izona$co,
                   "Humidity (%)" = izona$hm,
                   "Temp (Fahrenheit)" = izona$tp,
                   "Temp (Celsius)" = izona$tp_C
    )
    plot4<- ggplotly(ggplot(izona, aes(x=datetime, y=yvar))+ 
                       geom_point(color = "red")+
                       ggtitle(paste0("Instant Indoor ", input$var4))+
                       ylab(paste0(input$var4))+
                       xlab("Datetime")+
                       theme_minimal()+ 
                       theme(plot.title = element_text(face = "bold", size = 18))+
                       theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  ### Estrella ----
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -103.42271353500566, lat = 25.562758975144888, zoom = 15 )
  })
  
  
  # Tables 
  output$table1 <- renderDataTable({
    oestrella_hr
  })
  output$table2 <- renderDataTable({
    iestrella_hr
  })
  output$table3 <- renderDataTable({
    oestrella_hr
  })
  output$table4 <- renderDataTable({
    iestrella_hr
  })
  output$plot2a <- renderPlotly({
    yvar <- switch(input$var1a,
                   "PM10 (ug/m^3)" = oestrella_hr$PM10,
                   "PM2.5 (ug/m^3)" = oestrella_hr$PM2.5,
                   "PM1 (ug/m^3)" = oestrella_hr$PM1,
                   "CO2 (ppm)" = oestrella_hr$CO2,
                   "Humidity (%)" = oestrella_hr$Humidity,
                   "Temp (Fahrenheit)" = oestrella_hr$Temp.Fahrenheit,
                   "Temp (Celsius)" = oestrella_hr$Temp.Celsius
    )
    plot5<- ggplotly(ggplot(oestrella_hr, aes(x=datetime, y=yvar))+ 
                       geom_point(color = "red")+
                       ggtitle(paste0("Hourly Outdoor ", input$var1a))+
                       ylab(paste0(input$var1a))+
                       xlab("Datetime")+
                       theme_minimal()+ 
                       theme(plot.title = element_text(face = "bold", size = 18))+
                       theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  output$plot2b <- renderPlotly({
    yvar <- switch(input$var2a,
                   "PM10 (ug/m^3)" = iestrella_hr$PM10,
                   "PM2.5 (ug/m^3)" = iestrella_hr$PM2.5,
                   "PM1 (ug/m^3)" = iestrella_hr$PM1,
                   "CO2 (ppm)" = iestrella_hr$CO2,
                   "Humidity (%)" = iestrella_hr$Humidity,
                   "Temp (Fahrenheit)" = iestrella_hr$Temp.Fahrenheit,
                   "Temp (Celsius)" = iestrella_hr$Temp.Celsius
    )
    plot6<- ggplotly(ggplot(iestrella_hr, aes(x=datetime, y=yvar))+ 
                       geom_point(color = "red")+
                       ggtitle(paste0("Hourly Indoor ", input$var2a))+
                       ylab(paste0(input$var2a))+
                       xlab("Datetime")+
                       theme_minimal()+ 
                       theme(plot.title = element_text(face = "bold", size = 18))+
                       theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  output$plot2c <- renderPlotly({
    yvar <- switch(input$var3a,
                   "PM10 (ug/m^3)" = oestrella$p1,
                   "PM2.5 (ug/m^3)" = oestrella$p2,
                   "PM1 (ug/m^3)" = oestrella$p01,
                   "CO2 (ppm)" = oestrella$co,
                   "Humidity (%)" = oestrella$hm,
                   "Temp (Fahrenheit)" = oestrella$tp,
                   "Temp (Celsius)" = oestrella$tp_C
    )
    plot1<- ggplotly(ggplot(oestrella, aes(x=datetime, y=yvar))+
                       geom_point(color = "red")+
                       ggtitle(paste0("Instant Outdoor ", input$var3a))+
                       ylab(paste0(input$var3a))+
                       xlab("Datetime")+
                       theme_minimal()+ 
                       theme(plot.title = element_text(face = "bold", size = 18))+
                       theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  output$plot2d <- renderPlotly({
    yvar <- switch(input$var4a,
                   "PM10 (ug/m^3)" = iestrella$p1,
                   "PM2.5 (ug/m^3)" = iestrella$p2,
                   "PM1 (ug/m^3)" = iestrella$p01,
                   "CO2 (ppm)" = iestrella$co,
                   "Humidity (%)" = iestrella$hm,
                   "Temp (Fahrenheit" = iestrella$tp,
                   "Temp (Celsius)" = iestrella$tp_C
    )
    plot1<- ggplotly(ggplot(iestrella, aes(x=datetime, y=yvar))+ 
                       geom_point(color = "red")+
                       ggtitle(paste0("Instant Indoor ", input$var4a))+
                       ylab(paste0(input$var4a))+
                       xlab("Datetime")+
                       theme_minimal()+ 
                       theme(plot.title = element_text(face = "bold", size = 18))+
                       theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold")))
  })
  
}


# Run app ----
shinyApp(ui, server)

