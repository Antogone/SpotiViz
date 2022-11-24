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
library(ggradar)

data_alb <- discographie %>% group_by(artist_name,artist_id,album_name,album_id) %>% 
  select(danceability,energy,speechiness,acousticness,valence,instrumentalness) %>% 
  mutate_all(~median(.)) %>% distinct()

data_ttl <- discographie %>%  
  select(artist_name,track_name,danceability,energy,speechiness,acousticness,valence,instrumentalness) %>% distinct


shinyServer(function(input, output) {

#SIDEBAR MENU 
  output$sidebarpanel <- renderUI({
    sidebarMenu(
      menuItem("Accueil", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Album", tabName = "second", icon = icon("th")),
      menuItem("Chansons", tabName = "third", icon = icon("th")),
      menuItem("Relative", tabName = "fourth", icon = icon("th"))
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
                ),
                fluidRow(column(12,plotOutput("modeKey"))),
                fluidRow(column(12,plotOutput("DE")))
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
              ),
      
      #Third tab
      tabItem(tabName = "third",
              fluidPage(
                fluidRow(
                  selectInput("artist", label = ("Artiste"), 
                              choices = albums_aes %>% group_by(artist_name) %>% select(artist_name) %>% distinct, 
                              selected = 1)),
                
                fluidRow(
                  uiOutput("third_sel")
                )
              )
      ),
      tabItem(tabName ="fourth",
              fluidPage(
                fluidRow(
                  selectInput("selecta", label = h3("Artist Relatif"), 
                              choices = albums_aes %>% group_by(artist_name) %>% select(artist_name) %>% distinct, 
                              selected = 1),
                  column(12,  uiOutput("Rela")),
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
               tags$div(class="rela", style="width:100%; height:100%;",
                tags$span(sub_alb()[i,] %>% select(album_name) %>% as.character(),style="width:10px"),
                tags$a(href=sub_alb()[i,] %>% select(album_url) %>% as.character(),target="_blank", 
                       tags$img(src=sub_alb()[i,] %>% select(img) %>% as.character(),height=150,width=150))
               )
             )
      
    })
  })
  
  
  output$Rela <- renderUI({
    sub_ttl <<- reactive(related %>% inner_join(liste_artiste %>% select(name,id),by=c("relative"="id")) %>%
                           filter(name.y == input$selecta))
    
    lapply(1:as.integer(nrow(sub_ttl())), function(i) {
      column(2,
             tags$div(class="rela", style="width:100%;",
                      tags$span(sub_ttl()[i,] %>% select(name.x) %>% as.character(),style="width:10px"),
                      tags$a(href=sub_ttl()[i,] %>% select(external_urls.spotify) %>% as.character(),target="_blank",
                             tags$img(src=sub_ttl()[i,] %>% select(img) %>% as.character(),height=150,width=150))
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
    ),
    fluidRow(
      column(12,plotlyOutput("valencelight"))
    ),
    fluidRow(
      column(12,plotOutput("DELight"))
    ),
    fluidRow(
      column(12,plotOutput("modeKeyLight"))
    ),
    fluidRow(
      column(12,plotOutput("chartalb"))
    )
    )
  })
  
  output$third_sel<- renderUI({
    fluidPage(
      fluidRow(
        selectInput("ttl", label = "Titre", 
                    choices = discographie %>% filter(artist_name == input$artist) %>% select(track_name) %>% distinct, 
                    selected = 1)
      ),
      fluidRow(
        column(12,plotOutput("modeKeyLightTrack"))
      ),
      fluidRow(
        column(12,plotOutput("chartalbTrack"))
      )
    )
  })
  
  
  output$track <- renderDataTable(discographie %>% filter(album_name ==input$alb,artist_name ==input$artiste) %>% arrange(track_number) %>% select(track_name,external_urls.spotify) %>% distinct())
  
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
      ggtitle("Distribution de la Dansabilité") ->A
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
      ggtitle("Distribution de la Dansabilité") +
      gghighlight(album_name == input$alb) ->A
    ggplotly(A,tooltip = c("danceability"))
  })
  
  output$DE <- renderPlot({
    discographie %>% 
      filter(artist_name==input$select) %>% 
      ggplot(aes(x=danceability,y=energy,color=album_name)) +
      geom_point() -> A
    A
  })
  
  output$modeKey <- renderPlot({
    discographie %>% 
      filter(artist_name==input$select) %>% 
      group_by(key_mode) %>% 
      ggplot(aes(x=key_mode, fill=album_name))+
      geom_bar(width=0.5)+
      labs(x="Key", y="Number of Songs") +
      theme_minimal()+
      ggtitle("Nombre d'accords par Album") +
      theme(legend.position ="bottom")->A
    
    A
  })
  
  
  output$valencelight <- renderPlotly({
    discographie %>% 
      filter(artist_name == input$artiste,album_name == input$alb) %>% 
      arrange(track_number,album_name) %>% 
      ggplot(aes(x=track_name,y=valence,fill=track_name)) +
      geom_col()+
      labs(x="", y="Valence",color="Track",fill="Track") +
      theme_minimal()+
      theme(axis.text.x = element_blank())+
      ggtitle("Valence des Chansons") ->A
    
    ggplotly(A,tooltip = c("valence","track_name"))
  })
  
  
  output$modeKeyLight <- renderPlot({
    discographie %>% 
      filter(artist_name==input$artiste) %>% 
      group_by(key_mode) %>% 
      ggplot(aes(x=key_mode, fill=album_name))+
      geom_bar(width=0.5)+
      labs(x="Key", y="Number of Songs") +
      theme_minimal()+
      ggtitle("Part des accords de l'album sur l'ensemble des accords de l'artiste")+
      gghighlight(album_name == input$alb) -> A
    A
  })
  
  
  output$chartalb <- renderPlot({
      data_alb %>% 
      filter(artist_name== input$artiste,album_name==input$alb) %>% 
      ungroup %>% 
      rename("group" = "album_name") %>% 
      select(-1,-2,-4) %>% 
      mutate_at(vars(-group), ~as.numeric(.)) %>%
      ggradar(
        grid.min = 0,
        grid.max=1,
        group.line.width = 1, 
        group.point.size = 1,
        gridline.min.linetype = 1,
        gridline.mid.linetype = 2,
        gridline.max.linetype = 1,
        group.colours = c("#892CB5"),
        gridline.mid.colour = "grey",
        plot.legend=F) -> A
    A
  })
  
  
  output$chartalbTrack <- renderPlot({
    data_ttl %>% 
      filter(artist_name== input$artist,track_name==input$ttl) %>% 
      ungroup %>% 
      rename("group" = "track_name") %>% 
      select(-1,) %>% 
      mutate_at(vars(-group), ~as.numeric(.)) %>%
      ggradar(
        grid.min = 0,
        grid.max=1,
        group.line.width = 1, 
        group.point.size = 1,
        gridline.min.linetype = 1,
        gridline.mid.linetype = 2,
        gridline.max.linetype = 1,
        group.colours = c("#892CB5"),
        gridline.mid.colour = "grey",
        plot.legend=F) -> A
    A
  })
  
  output$modeKeyLightTrack <- renderPlot({
    discographie %>% 
      filter(artist_name==input$artist) %>% 
      group_by(key_mode) %>% 
      ggplot(aes(x=key_mode, fill=track_name))+
      geom_bar(width=0.5)+
      labs(x="Key", y="Number of Songs") +
      theme_minimal()+
      ggtitle("Part des accords de l'album sur l'ensemble des accords de l'artiste")+
      gghighlight(track_name == input$ttl) -> A
    A
  })
  
  output$DELight <- renderPlot({
    discographie %>% 
      filter(artist_name==input$artiste,album_name == input$alb) %>% 
      ggplot(aes(x=danceability,y=energy,color=track_name)) +
      geom_point(size=1.5) -> A
    A
  })
  
  
  
  
  
})
