## beprstats (Belgium Pigeon Racing Statistics) Shiny/R app ui.R                                           
##                                                                      
## Author(s) :
## -----------
## Grégoire Vincke http://www.gregoirevincke.be            
##                                                                      
## Licences : 
## ---------
## CC-BY for the web page http://www.yapluka.be/sapps/beprstats/
## See http://creativecommons.org/licenses/by/2.0/be/ for more informations       
##
## GPLv2 for source code on https://github.com/gvincke/beprstats 
## See LICENCE.txt or http://www.gnu.org/licenses/old-licenses/gpl-2.0.html for more informations

Sys.setlocale("LC_ALL", "fr_FR.UTF-8")#to be sure that accents in text will be allowed in plots

library(shiny)

# Define UI for zplot
shinyUI(navbarPage("Belgium Pigeon Racing Statistics",id="main",#http://shiny.rstudio.com/reference/shiny/latest/navbarPage.html
  header=tags$div(id="lang-div",selectInput("language",uiOutput('uiSBlanguage'),list("English" = "en","Français" = "fr", "Nederlands" ="nl"),selected = "fr",selectize=FALSE)),#Must be defined in UI.R instead of server.R to avoid error of type "trying to select more than one value because input$language is only set after server.R load                
  tabPanel(uiOutput('uiPanelTitle'),value="main",
  fluidPage(
  tags$head(
    tags$head(includeScript("www/js/google-analytics.js")),
    tags$script(type="text/javascript",src="js/scripts.js"),
    tags$style(type='text/css', "select#races,select#editions { width: 150px; display : inline; }"),
    tags$style(type="text/css", "h1,h2,h3,h4,h5,h6 {color:#317EAC;}"),
    tags$style(type="text/css", "label, div.shiny-input-container div { display: inline; }"),
    tags$style(type="text/css", "hr {margin-bottom:8px; margin-top:8px;}"),
    tags$style(type='text/css', "select#language { width: 150px; display : inline; }"),
    tags$style(type="text/css", "#lang-div {z-index:1000;position:absolute;top:5px;right:130px;}")
  ),
  fluidRow(
    column(3,
      wellPanel(
        selectInput(inputId="races",label=uiOutput("uiSBRaces"),choices="",selectize=FALSE,multiple=FALSE),#Label is translated, so have to be set in server.R, but the list must be set in UI.R to be setted before of server.R computation to be filled by Towns values : soit un select est défini ici avec choices ="" et un observe dans server.R le rempli par après; soit le select est directement défini dans server.R mais du coup ne peut être exploité par un script js
        selectInput(inputId="editions",label=uiOutput("uiSBRacesEditions"),choices="",selectize=FALSE,multiple=FALSE),#Label is translated, so have to be set in server.R, but the list must be set in UI.R to be setted before of server.R computation to be filled by Towns values : soit un select est défini ici avec choices ="" et un observe dans server.R le rempli par après; soit le select est directement défini dans server.R mais du coup ne peut être exploité par un script js
        hr(),
        uiOutput("uiSBRacesLocLink")        
      ),
      fluidRow(
        column(12,
          uiOutput('uiSBsummary')
        )
      ),
      fluidRow(
        column(12,
          uiOutput('uiSBlicence')    
        )
      )
    ),
    column(9,
      uiOutput("uiMain")
    )
  )
)
),
tabPanel(uiOutput('uiCreditsTitle'),value="credits",
         fluidPage(
           tags$head(
             tags$style(type='text/css', ".col-sm-8 { width: 100%; }")
           ),
           fluidRow(
             column(6,uiOutput('uiCredits1')),
             column(6,uiOutput('uiCredits2'))
          )
        )
      )
    )
)