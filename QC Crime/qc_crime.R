#encoding UTF-8

library(shiny)
library(dplyr)
library(plotly)
library(kableExtra)
library(shinyWidgets)
library(stringr)
library(shinydashboard)
library(sf)
library(viridis)

data <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                        '/extracted_data.csv')) %>%
  mutate(date_f = as.Date(date, format = '%Y-%m-%d')) %>%
  filter(date_f < as.Date('2018-08-23'))%>%
  mutate(modus = replace(modus, modus == 'Swpu ( Stolen While Parked)', 'Swpu (Stolen While Parked)'),
         modus = replace(modus, modus == 'Bukas -Kotse', 'Bukas Kotse'),
         modus = str_to_title(trimws(modus)),
         month = lubridate::month(date_f, label = T, abbr = F),
         day = lubridate::wday(date_f, label = T, abbr = F),
         day = factor(day,
                      levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday',
                                 'Friday', 'Saturday', 'Sunday'),
                      ordered = T),
         crime = toupper(crime),
         brgy = stringr::str_replace(trimws(gsub("\\s*\\([^\\)]+\\)","",
                                                 gsub(",.*$", "", location))), 'Ã±', 'ñ'),
         brgy = ifelse(brgy == 'Santo Domingo', 'Matalahib',
                       ifelse(brgy == 'New Era', 'Constitution Hills',
                              ifelse(brgy == 'Ns Amoranto', 'N.S. Amoranto',
                                     ifelse(brgy ==  'Old Balara', 'Matandang Balara',
                                            ifelse(brgy == 'North Fairview', 'Fairview',
                                                   ifelse(brgy == 'San Martin De Porres', 'San Martin de Porres',
                                                          ifelse(brgy == 'Sto Nino', 'Santo Niño',
                                                                 ifelse(brgy == 'St Peter', 'Saint Peter',
                                                                        ifelse(brgy == 'Greater Lagro', 'Fairview', brgy))))))))))




#count of modus


data %>% select(modus, date) %>% distinct() %>%
  group_by(modus) %>% summarise(days = n()) %>%
  left_join(modus, by = 'modus') %>%
  kbl() %>% kable_minimal()


qc1 <- st_read(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                              '/gadm36_PHL_shp/gadm36_PHL_2.shp'))%>%
  filter(NAME_2 == 'Quezon City')

qc2 <- st_read(paste0(dirname(rstudioapi::getSourceEditorContext()$path),
                      '/gadm36_PHL_shp/gadm36_PHL_3.shp'))%>%
  filter(NAME_2 == 'Quezon City')



# UI ----------------------------------------------------------------------
ui <- fluidPage(
  shinytheme = 'flatly',
  titlePanel('Quezon City Crimes'),
  useShinydashboard(), #to use valuebox without using shinydashboard
  tags$style(HTML('
                  .tabbable > .nav > li > a                  {font-weight: bold; background-color: ;  color:black}
                  .tabbable > .nav > li[class=active]    > a {background-color: ; color:#e84c3d}
                  ')),
  br(),
  fluidRow(
    column(5,
           wellPanel(div(id='month_text',
                         pickerInput(inputId = 'month', label = 'Month(s): ',
                                     multiple = T, choices = c('July', 'August'),
                                     selected = c('July', 'August'), width = '70%')
           )),
           tags$style(type='text/css', '#month_text {color : #e84c3d;}')
    ),
    column(6, offset = 1,
           wellPanel(
             p("Data shown is not reliable as it's extracted when the
          website is no longer working properly.")
           )
    )
  ),
  tabsetPanel(
    type = 'tabs',
    tabPanel(
      'Daily Crime', style ='color : #e84c3d',
      br(),
      fluidRow(
        column(12, plotlyOutput('crime_daily'))
      ),
      fluidRow(
        column(3),
        column(6,
               selectInput('crime', 'Crime Type',
                           multiple = F, choices = sort(unique(data$crime)), width = '100%')
        )
      )
    ),
    tabPanel('Crime Type & Police Station',
             fluidRow(
               column(5, tableOutput('crime_type')),
               column(7, plotOutput('station_crime'))
             )
    ),
    tabPanel('Map',
             fluidRow(
               br(),
               column(2,
                      varSelectInput('legend', 'Legend',
                                     multiple = F, data[, c('crime', 'day', 'station')],
                                     width = '100%')
               ),
               column(5, plotOutput('map1')),
               column(5, plotOutput('map2'))
             )     
    ),
    tabPanel('Summary',
             br(),
             fluidRow(
               valueBoxOutput('total_crime'),
               valueBoxOutput('avgdaily_crime')
             ),
             fluidRow(
               column(12, tableOutput('avgcrimes_per_day'))
             ),
             fluidRow(
               column(12, offset = 3, DT::dataTableOutput('modus', width = '50%'))
             )
    )
  )
)



# Server ------------------------------------------------------------------
server <- function(input, output){
  
  #data by month
  data_month <- data
  
  data_month <- eventReactive(input$month, {
    data %>%
      filter(month %in% input$month)
  })
  
  
  #daily crime
  output$crime_daily <- renderPlotly({
    crime_selected <- data %>%
      filter(crime == input$crime) %>%
      group_by(date_f) %>%
      summarise(crime_selected = n())
    
    
    data_month() %>% group_by(date_f) %>%
      summarise(crime_total = n()) %>%
      left_join(crime_selected, by = 'date_f') %>%
      plot_ly(x = ~date_f, y = ~crime_selected, name = input$crime, type = 'bar',
              marker = list(color = '#2c3b4f')) %>%
      add_trace(y = ~crime_total, name = 'Total Crimes', type = 'scatter', mode = 'lines+markers',
                marker = list(color = '#e84c3d'), line = list(color = '#e84c3d')) %>%
      layout(title = list(text = 'Daily Count of Crimes',
                          font = list(color = '#e84c3d')),             
             xaxis = list(title = ''),
             yaxis = list(title = 'Total Crimes'),
             legend = list(orientation = 'h'))
  })
  
  #count per crime type
  output$crime_type <- function(){
    data_month() %>% group_by(crime) %>%
      summarise(count = n()) %>%
      rename_all(toupper) %>%
      kbl(booktabs = T) %>%
      kable_styling(bootstrap_options = 'striped', font_size = 11,
                    full_width = F) %>%
      row_spec(0, color = '#e84c3d')
  }
  
  #count of crimes per station
  output$station_crime <- renderPlot({
    data_month() %>% group_by(station) %>% tally() %>%
      mutate(station = factor(station, level = sort(station, decreasing = T))) %>%
      ggplot(aes(station, n)) +
      geom_bar(stat = 'identity', fill = '#2c3b4f') +
      coord_flip() +
      theme_classic()+
      labs(title = 'Count of Crimes per Station') +
      theme(axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5, color = '#e84c3d', size = 14)
      )
    
  })
  
  output$map1 <- renderPlot({
    
    qc1 %>%
      ggplot() + 
      geom_sf(size = 1, color = "black") + 
      geom_point(data = data_month(), aes(x=lng, y =lat, color = !!input$legend)) +
      coord_sf() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = 'white'),
            axis.text = element_blank(), axis.ticks = element_blank(),
            axis.title = element_blank())
  })
  
  output$map2 <- renderPlot({
    brgy <- data_month() %>%
      group_by(brgy) %>%
      summarise(crime = n())
    
    qc2 %>%
      left_join(brgy, by = c('NAME_3' = 'brgy')) %>%
      ggplot() + 
      geom_sf(aes(fill = crime, color = crime), size = 1) + 
      coord_sf() +
      scale_fill_viridis(option = 'mako') +
      scale_color_viridis(option = 'mako') +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = 'white'),
            axis.text = element_blank(), axis.ticks = element_blank(),
            axis.title = element_blank())
  })
  
  
  # Summary -----------------------------------------------------------------
  
  #total crimes
  output$total_crime <- renderValueBox({
    valueBox(
      nrow(data_month()), "Total Crimes", icon = icon('plus', lib = 'glyphicon'),
      color = 'red'
    )
  })
  
  #average daily crimes
  output$avgdaily_crime <- renderValueBox({
    data <- data_month() %>% group_by(date_f) %>%
      summarise(crime_daily = n())
    
    
    valueBox(
      round(mean(data$crime_daily), 0), "Average Daily Crimes", icon = icon('stats', lib = 'glyphicon'),
      color = 'orange'
    )
  })
  
  #average crimes per day
  output$avgcrimes_per_day <- function(){
    data_month() %>%
      group_by(date_f, day) %>%
      tally() %>%
      group_by(day) %>%
      summarise(avg_crime = round(mean(n))) %>%
      tidyr::spread(day, avg_crime) %>%
      kbl(booktabs = T) %>%
      kable_styling(bootstrap_options = 'striped', font_size = 14,
                    position = 'center') %>%
      row_spec(0, color = '#e84c3d') %>%
      add_header_above(c('Average Crimes' = 7), color = '#e84c3d', font_size = 18)
  }
  
  # list of modus
  output$modus <- DT::renderDataTable({
    
    data_month() %>%
      group_by(modus) %>% summarise(crime = n()) %>%
      DT::datatable(options = list(pageLength = 5))
  })
  
  
}

shinyApp(ui, server)
