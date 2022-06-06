
# package version ---------------------------------------------------------

# [1] shinydashboardPlus_2.0.3 shinyWidgets_0.6.4       scales_1.1.1             plotly_4.10.0            ggdark_0.2.1            
# [6] ggplot2_3.3.5            stringr_1.4.0            dplyr_1.0.8              bslib_0.3.1              shiny_1.7.1 



# libraries ---------------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggdark)
library(plotly)
library(scales)
library(shinyWidgets)
library(stringr)
library(shinydashboardPlus)


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

netflix <- read.csv('netflix_titles.csv') %>%
  mutate(platform = 'Netflix')

amazon <- read.csv('amazon_prime_titles.csv') %>%
  mutate(platform = 'Amazon Prime')

disney <- read.csv('disney_plus_titles.csv') %>%
  mutate(platform = 'Disney Plus')

hulu <- read.csv('hulu_titles.csv') %>%
  mutate(platform = 'Hulu')


data <- netflix %>% bind_rows(amazon) %>% bind_rows(disney) %>% bind_rows(hulu) %>%
  mutate_all(~ str_trim(.))


# customize the theme
db_theme <- bs_theme(
  base_font = font_google("Oxygen"),
  code_font = font_google("Courier Prime"),
  version = 4, bootswatch = 'darkly'
)


color_mapping = c(Netflix = '#B81D24', `Disney Plus` = '#1738B7', `Amazon Prime` = '#00A8E1', Hulu = '#5C9730')

# bs_theme_preview(db_theme) #check the theme



# ui ----------------------------------------------------------------------

ui <- fluidPage(
  theme = db_theme,
  useShinydashboard(),
  # add this customization as using useShinyDashboard turns active tab and hovering to white
  tags$style(HTML("
        .navbar { background-color:#375A7F;}
        .navbar-default .navbar-nav > li > a,
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover,
        .navbar-default .navbar-nav > li > a:hover {color:black;background-color:#375A7F;text-decoration}
                  ")),
  navbarPage(
    title = 'Online Streaming of Movies and Shows',
    tabPanel('Home',
             fluidRow(
               column(8, plotOutput('total_count')),
               column(4, plotOutput('distribution_type'))
             ),
             br(), br(),
             fluidRow(
               column(12, plotOutput('movie_per_year'))
             )
    ),
    tabPanel('Search',
             sidebarLayout(
               sidebarPanel(width=3,
                            checkboxGroupButtons(
                              inputId = 'platform',
                              label = strong('Choose Platform(s)'),
                              choices = sort(unique(data$platform)),
                              status = 'primary'
                            ),
                            # inline widgets
                            div(style='display: inline-block;vertical-align:top; width: 150px;',
                                materialSwitch(
                                  inputId = 'movie',
                                  label = 'Movies',
                                  value = TRUE,
                                  status = 'success'
                                )
                            ),
                            div(style='display: inline-block;vertical-align:top; width: 150px;',
                                materialSwitch(
                                  inputId = 'series',
                                  label = 'TV Series',
                                  value = TRUE,
                                  status = 'warning'
                                ),
                            ),
                            radioButtons(
                              inputId = 'search_by',
                              label = strong('Search by'),
                              choices = c('Title',
                                          'Director', 'Actor/Actress'),
                              selected = 'Title'),
                            selectizeInput(
                              inputId = 'search_box',
                              label = '',
                              choices = NULL,
                              selected = NULL
                            )
               ),
               
               
               mainPanel(
                 p(strong('NOTE:'), 'The data used in this dashboard did not undergo preprocessing.',
                   style='text-align:justify;color:black;background-color:#D48477;padding:15px;border-radius:10px'),
                 uiOutput('result'),
               )
             )),
    tabPanel('Browse'),
    tabPanel('Reviews'),
    tabPanel('Request')
  )
) 



# server ------------------------------------------------------------------

server <- function(input, output, session) {
  ## Home Tab
  output$total_count <- renderPlot({
    data %>% 
      select(platform, type, title) %>% distinct() %>%
      group_by(platform, type) %>%
      tally() %>%
      ggplot(aes(x = platform, y = n)) +
      geom_bar(aes(fill = platform), stat = 'identity') +
      facet_wrap(~type, ncol = 2) +
      scale_fill_manual(values = color_mapping) +
      dark_theme_bw() %+replace%
      theme(axis.title = element_blank(), legend.position = 'None',
            axis.text = element_text(size =14), strip.text = element_text(size=14)) +
      labs(title = 'Movies and TV Shows')
  })
  
  output$distribution_type <- renderPlot({
    data %>%
      select(platform, type, title) %>% distinct() %>%
      group_by(platform, type) %>%
      tally() %>%
      group_by(platform) %>%
      mutate(proportion = n/sum(n)*100,
             ypos = cumsum(proportion)-0.5*proportion) %>%
      ggplot(aes(x = '', y = proportion, fill = type)) +
      geom_bar(stat="identity", color="white") +
      geom_text(aes(label = paste0(round(proportion, 1), '%')), position = position_stack(vjust = .5)) +
      coord_polar("y") +
      facet_wrap(.~platform) +
      dark_theme_bw() %+replace%
      theme(legend.title = element_blank(), axis.title = element_blank(),
            axis.ticks = element_blank(), axis.text = element_blank(),
            plot.background = element_rect(fill = 'black'),
            strip.text = element_text(size=14), legend.text = element_text(size = 12))
  }, bg = '#222222')
  
  
  output$movie_per_year <- renderPlot({

    data %>% select(type, title, release_year, platform) %>% distinct() %>%
      group_by(type, release_year, platform) %>% tally() %>%
      mutate(release_year = as.numeric(release_year)) %>%
      ggplot(aes(x = release_year, y = n, group = platform)) +
      geom_point(aes(color = platform)) +
      geom_line(aes(color = platform)) +
      facet_wrap(type ~., nrow = 2, scales = 'free_y') +
      scale_color_manual(values = color_mapping) +
      dark_theme_bw() %+replace%
      theme(legend.title = element_blank(), axis.title.y = element_blank(), legend.position = 'top',
            plot.background = element_rect(fill = 'black'), strip.text = element_text(size=14), axis.title.x = element_text(size=14),
            axis.text = element_text(size=14), legend.text = element_text(size = 14)) +
      xlab('Year')



  }, height = 600)
  
  
  ## Search Tab
  observeEvent(input$platform, {
    
    choices <- data %>% filter(platform %in% input$platform)
    
    if (input$movie == F){
      choices <- choices %>% filter(type != 'Movie')
    }
    
    if (input$series == F){
      choices <- choices %>% filter(type != 'TV Show')
    }    
    
    
    if (input$search_by == 'Title'){
      updateSelectizeInput(session, 'search_box', choices = sort(unique(choices$title)), selected = '', server = TRUE,
                           options = list(maxOptions = nrow(data)))  
    } else if (input$search_by == 'Director'){
      updateSelectizeInput(session, 'search_box', choices = sort(unique(choices$director)), selected = '', server = TRUE,
                           options = list(maxOptions = nrow(data)))  
    } else {
      updateSelectizeInput(session, 'search_box', choices = sort(unique(choices$cast)), selected = '', server = TRUE,
                           options = list(maxOptions = nrow(data)))  
    }
    
  })
  
  
  observeEvent(input$movie, {
    
    choices <- data %>% filter(platform %in% input$platform)
    
    if (input$movie == F){
      choices <- choices %>% filter(type != 'Movie')
    }
    
    if (input$series == F){
      choices <- choices %>% filter(type != 'TV Show')
    }    
    
    
    if (input$search_by == 'Title'){
      updateSelectizeInput(session, 'search_box', choices = sort(unique(choices$title)), selected = '', server = TRUE)  
    } else if (input$search_by == 'Director'){
      updateSelectizeInput(session, 'search_box', choices = sort(unique(choices$director)), selected = '', server = TRUE)  
    } else {
      updateSelectizeInput(session, 'search_box', choices = sort(unique(choices$cast)), selected = '', server = TRUE)  
    }
    
  })
  
  observeEvent(input$series, {
    
    choices <- data %>% filter(platform %in% input$platform)
    
    if (input$movie == F){
      choices <- choices %>% filter(type != 'Movie')
    }
    
    if (input$series == F){
      choices <- choices %>% filter(type != 'TV Show')
    }    
    
    
    if (input$search_by == 'Title'){
      updateSelectizeInput(session, 'search_box', choices = sort(unique(choices$title)), selected = '', server = TRUE)  
    } else if (input$search_by == 'Director'){
      updateSelectizeInput(session, 'search_box', choices = sort(unique(choices$director)), selected = '', server = TRUE)  
    } else {
      updateSelectizeInput(session, 'search_box', choices = sort(unique(choices$cast)), selected = '', server = TRUE)  
    }
    
  })
  
  
  observeEvent(input$search_by, {
    
    choices <- data %>% filter(platform %in% input$platform)
    
    if (input$movie == F){
      choices <- choices %>% filter(type != 'Movie')
    }
    
    if (input$series == F){
      choices <- choices %>% filter(type != 'TV Show')
    }    
    
    
    if (input$search_by == 'Title'){
      updateSelectizeInput(session, 'search_box', choices = sort(unique(choices$title)), selected = '', server = TRUE)  
    } else if (input$search_by == 'Director'){
      updateSelectizeInput(session, 'search_box', choices = sort(unique(choices$director)), selected = '', server = TRUE)  
    } else {
      updateSelectizeInput(session, 'search_box', choices = sort(unique(choices$cast)), selected = '', server = TRUE)  
    }
    
  })
  
  
  
  
  search_result <- eventReactive(input$search_box, {
    
    filtered <- data
    
    if (input$movie == F){
      filtered <- data %>% filter(type != 'Movie')
    }
    
    if (input$series == F){
      filtered <- data %>% filter(type != 'TV Show')
    }
    
    
    if (input$search_by == 'Title'){
      filtered <- filtered %>% filter(platform %in% input$platform, title == input$search_box)
    } else if (input$search_by == 'Director'){
      filtered <- filtered %>% filter(platform %in% input$platform, director == input$search_box)
    } else {
      filtered <- filtered %>% filter(platform %in% input$platform, cast == input$search_box)
    }
    
    
    filtered %>% select(title, director, cast, description, platform)
    
  })
  
  
  output$result <- renderUI({
    req(input$platform, input$search_box)
    
    result <- unique(as.vector(search_result()$title))
    result_count <- length(result)
    
    
    lapply(1:result_count, function(i){
      
      show <- search_result() %>% filter(title == result[i])
      
      userBox(
        title = userDescription(
          title = show$title[1],
          subtitle = unique(show$director)[1],
          type = 2,
          image = 'sample_pic.jpg'
        ),
        width = 12,
        status = "black",
        navPills(
          id = "pillItem",
          navPillsItem(
            left = 'Cast',
            color = "green",
            right =  unique(show$cast)[1]
          ),
          navPillsItem(
            left = "Description",
            color = "green",
            right = unique(show$description)[1]
          ),
          navPillsItem(
            left = "Platform",
            color = "red",
            right = str_c(as.vector(show$platform), collapse = ', ')
          )
        )
      )
    })
  })
  
}



shinyApp(ui, server)
