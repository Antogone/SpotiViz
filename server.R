library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(tidyverse)
library(sodium)
library(plotly)
library(ggthemes)
library(gghighlight)
library(RColorBrewer)
library(scales)
library(ggTimeSeries)

shinyServer(function(input, output) {

#SIDEBAR MENU 
  output$sidebarpanel <- renderUI({
    sidebarMenu(
      menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Second Page", tabName = "second", icon = icon("th")),
      menuItem("Third Page", tabName = "third", icon = icon("th")),
      menuItem("Fourth Page", tabName = "fourth", icon = icon("th"))
    )
  }
  )
  
  
#BODY des pages
  output$body <- renderUI({
    tabItems(
      
      
      # First tab
      tabItem(tabName ="dashboard", class = "active",
              fluidPage(
                fluidRow(
                selectInput("select", label = h3("Discographie"), 
                            choices = albums_aes %>% group_by(artist_name) %>% select(artist_name) %>% distinct, 
                            selected = 1),
                  column(12,  uiOutput("album")),
                ),
                br(),
                fluidRow(
                  column(12,plotlyOutput("dance"))
                )
              )
      ),
      
      
      
      
      # Second tab
      tabItem(tabName = "second",
              fluidPage(
                fluidRow(
                  selectInput("artiste", label = ("Artiste"), 
                              choices = albums_aes %>% group_by(artist_name) %>% select(artist_name) %>% distinct, 
                              selected = 1)),
                  
                fluidRow(
                  uiOutput("second_sel")
                  )
                )
              )
      )
  })
    
    
    

  
  #Tableau de page 2 
  output$results2 <-  DT::renderDataTable({
    datatable(albums_aes, options = list(autoWidth = TRUE,
                                         searching = FALSE))
  })
  


  # Images et Titres Albums
  output$album <- renderUI({
    sub_alb <<- reactive(albums_aes %>% filter(artist_name == input$select))
    
    lapply(1:as.integer(nrow(sub_alb())), function(i) {
        column(2,
               tags$div(class="alb", style="width:100%;",
                tags$span(sub_alb()[i,] %>% select(album_name) %>% as.character(),style="width:10px"),
                tags$a(href=sub_alb()[i,] %>% select(album_url) %>% as.character(),target="_blank", 
                       tags$img(src=sub_alb()[i,] %>% select(img) %>% as.character(),height=150,width=150))
               )
             )
      
    })
  })
  
  


  
  output$second_sel<- renderUI({
    fluidPage(
      fluidRow(
    selectInput("alb", label = "Album", 
                choices = albums_aes %>% filter(artist_name == input$artiste) %>% select(album_name) %>% distinct, 
                selected = 1)
      ),
    fluidRow(
      column(12,plotlyOutput("dancelight"))
    )
    )
  })
  
  
  
  output$track <- renderDataTable(discographie %>% filter(album_name ==input$alb,artist_name ==input$artiste) %>% arrange(track_number) %>% select(track_name,external_urls.spotify) %>% distinct())
  
  
  output$stata <- renderUI({
      fluidRow(
        column(4, span("1")),
               column(4, span("2"))
      )
      fluidRow(
        column(4, span("3")),
        column(4,span("4"))
      )
    
  })
  
  
  output$tracklist <- renderUI({
    selectInput("tr", label = "track", 
               choices = albums_aes %>% filter(artist_name == input$artiste & album_name == input$alb) %>% select(track_name) %>% distinct, selected = 1)
  })
  
  output$dance <- renderPlotly({
    discographie %>% 
      filter(artist_name == input$select) %>% 
      arrange(track_number) %>% 
      ggplot(aes(x=danceability, fill=album_name,text = paste(album_name),color=album_name))+
      geom_density(alpha=0.5)+
      labs(x="Danceability", y="Density",color="Albums",fill="Albums") +
      theme_minimal()+
      ggtitle("Distribution of Danceability Data") ->A
      ggplotly(A,tooltip=c("text","danceability"))
})
  
  
  
  output$dancelight <- renderPlotly({
    discographie %>% 
      filter(artist_name == input$artiste) %>% 
      arrange(track_number) %>% 
      ggplot(aes(x=danceability, fill=album_name,color=album_name))+
      geom_density(alpha=0.5)+
      labs(x="Danceability", y="Density",color="Albums",fill="Albums") +
      theme_minimal()+
      scale_fill_brewer(palette="Set2")+
      scale_color_brewer(palette="Set2")+
      ggtitle("Distribution of Danceability Data") +
      gghighlight(album_name == input$alb) ->A
    ggplotly(A,tooltip = c("danceability"))
  })
  
  
  
  
})
