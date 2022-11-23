library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(tidyverse)
library(sodium)

header <- dashboardHeader( title = "SpotiViz")
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))

ui<-dashboardPage(header, sidebar, body,
                  tags$head(
                    tags$style(HTML(".main-sidebar {background-color:  #191414 !important;}
                  .logo {
                              background-color: #1DB954 !important;
                              }
                              .navbar {
                              background-color: #1DB954 !important;
                              }
                    #logoutbtn > li > a{
                  color: #FFFFFF !important;}
                  
                 #sidebarpanel > ul > li:hover>a {
color: #1DB954 !important;
background: #191414 !important;
border-left-color: #1DB954 !important;
                 }

#sidebarpanel > ul > li:hover>a > span{
color: #1DB954 !important;                 }

                 #sidebarpanel > ul > li.active>a {
color: #1DB954 !important;
background: #191414 !important;
border-left-color: #1DB954 !important;
                 }

#sidebarpanel > ul > li.active>a > span{
color: #1DB954 !important;                 }

#sidebarpanel > ul > li > a > span{
color : #FFFFFF !important;
}
                  "))
  )
)
