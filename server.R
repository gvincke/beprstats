## beprstats (Belgium Pigeon Racing Statistics) Shiny/R app server.R                                           
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

# Sys.setlocale("LC_ALL", "fr_FR.UTF-8")#to be sure that accents in text will be allowed in plots

library(shiny)
library(png)

# Create a reactive object here that we can share between all the sessions.
SRV <- reactiveValues(count=0)#Session reactive values
cc <- readPNG("www/img/cc_by_320x60.png")

shinyServer(function(input, output, session) {
  # https://gist.github.com/trestletech/9926129
  # Increment the number of sessions when one is opened.
  # We use isolate() here to:
  #  a.) Provide a reactive context
  #  b.) Ensure that this expression doesn't take a reactive dependency on
  #      SRV$count -- if it did, every time SRV$count changed, this expression
  #      would run, leading to an infinite loop.
  isolate(SRV$count <- SRV$count + 1)
  
  # When a session ends, decrement the counter.
  session$onSessionEnded(function(){
    # We use isolate() here for the same reasons as above.
    isolate(SRV$count <- SRV$count - 1)
  })
  
  # Lang selection
  lang <- read.delim("data/lang.csv", header = TRUE, sep = "\t", as.is = TRUE,row.names=1) 
  tr <- function(text){ # translates text into current language
    return(sapply(text,function(s) lang[s,input$language], USE.NAMES=FALSE))
  }
  
  #races results
  results <- read.csv("data/results.csv", sep=",", dec=".")
  results$speedkm<-(results$speed/1000)*60
  
  observe({#http://stackoverflow.com/questions/28119964/dynamic-input-selector-based-on-uploaded-data
    v<-sort(as.vector(unique(results$racename))) 
    v<-c(" "="empty",v)
    updateSelectInput(#http://www.inside-r.org/packages/cran/shiny/docs/updateSelectInput
      session,
      "races",
      choices=v)
  })
  
  
  
  observe({#http://stackoverflow.com/questions/28119964/dynamic-input-selector-based-on-uploaded-data
    data<-results
    data<-subset(data,racename %in% c(input$races))
    v<-sort(as.vector(unique(data$racedate))) 
    v<-c(" "="empty",v)
    updateSelectInput(#http://www.inside-r.org/packages/cran/shiny/docs/updateSelectInput
      session,
      "editions",
      choices=v)
  })

  getInputValues<-reactive({
    return(input)#collect all inputs
  })


  getComputedValues<-reactive({
    v<-getInputValues() # get all values of input list
    cv<-list()#created empty computed values list
    cv$data<-results
    cv$data<-subset(cv$data,racename %in% c(v$races))
    cv$data<-subset(cv$data,racedate %in% c(v$editions))
    cv$data$distkm<-cv$data$dist/1000
    return(cv)
  })
  
  output$plot <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    m<-matrix(c(1,2,3,4,5,6),2,3,byrow=TRUE)
    layout(m,width=c(1,3,2,1,3,2))
    if(v$races!="empty" & v$editions!="empty"){
      
      #http://stackoverflow.com/questions/29185996/plot-empty-groups-in-boxplot
      par(bty="n")
      catmax<-max(cv$data$cat)
      agemax<-max(cv$data$age)
      
      cv$data$cattoplot <- factor(cv$data$cat,levels = 0:catmax)#create categories and force adding of empty groups
      boxplot(speed~cattoplot,data=cv$data, col=rainbow(length(unique(cv$data$age))),range=1.5,varwidth=TRUE,ylab="Vitesse (m/min)",xlab="Catégorie",main="Distribution des vitesses par catégorie",names=c(tr("Youngsters"), tr("Yearlings"),tr("Olds")))# ,ylim=c(800,1800) , range=1.5 by default : gestion des valeurs extrèmes. varwidth=Largeur proportionnelle à la racine carrée du nombre d’observation par groupe
      
      cv$data$agetoplot <- factor(cv$data$age,levels = 0:agemax)#create categories and force adding of empty groups
      boxplot(speed~agetoplot,data=cv$data, col=rainbow(length(unique(cv$data$age))),range=1.5,varwidth=TRUE,ylab="Vitesse (m/min)",xlab="Age (années)",main="Distribution des vitesses par classe d'age")# ,ylim=c(800,1800) , range=1.5 by default : gestion des valeurs extrèmes. varwidth=Largeur proportionnelle à la racine carrée du nombre d’observation par groupe

      plot(cv$data$distkm,cv$data$speed,ylim=c(min(cv$data$speed),max(cv$data$speed)),ylab="Vitesse (m/min)",xlab="Distance (km)",main="Distribution des vitesses par unité de distance")
      
      n <- table(factor(cv$data$cat,levels = 0:catmax))#http://r.789695.n4.nabble.com/creating-empty-cells-with-table-td798211.html
      bp<-barplot(n, xlab="Catégorie",ylab="Nombre de pigeons",main="Nombre de pigeons par catégorie",col=rainbow(length(unique(cv$data$age))),ylim=c(0,max(n*1.2)),names=c(tr("Youngsters"), tr("Yearlings"),tr("Olds")))
      text(x = bp, y = n, label = n, pos = 3, cex = 0.8, col = "red")## Add text at top of bars
      
      agemax<-max(cv$data$age)
      n <- table(factor(cv$data$age,levels = 0:agemax))#http://r.789695.n4.nabble.com/creating-empty-cells-with-table-td798211.html
      bp<-barplot(n, xlab="Age (années)",ylab="Nombre de pigeons",main="Nombre de pigeons par classe d'age",col=rainbow(length(unique(cv$data$age))),ylim=c(0,max(n*1.2)))
      text(x = bp, y = n, label = n, pos = 3, cex = 0.8, col = "red")## Add text at top of bars

      
      hist(cv$data$speed,breaks=20,xlab="Vitesse (m/min)",ylab="Nombre de pigeons",main="Nombre de pigeons par classe de vitesse")
    }
  })
  
  output$Datas = renderDataTable({
    v<-getInputValues()
    cv<-getComputedValues()
    cv$data
  })


#UI
output$uiPanelTitle <- renderUI({
  tr("Title")
})


output$uiSBlanguage<- renderUI({
  strong(HTML(paste(tr("Language"),":",sep=" ")))
})

output$uiSBRaces <- renderUI({
  strong(HTML(paste(tr("Races"),"*",":",sep=" ")))
})
output$uiSBRacesLocLink <- renderUI({
  HTML(paste("*",":",tr("RacesLocLink"),sep=" "))
})
output$uiSBRacesEditions <- renderUI({
  strong(HTML(paste(tr("Editions"),":",sep=" ")))
})

output$uiSBsummary <- renderUI({
  mainPanel(
    # Faire des requêtes sur tous les concours de même id et donner stats générales dès que concours est sélectionné
    # Second bloc : stats générales de cette édition
    HTML(),
  )
})

output$uiSBlicence <- renderUI({
  HTML(paste("<a rel='license' href='http://creativecommons.org/licenses/by/2.0/be/'><img alt='Licence Creative Commons' style='border-width:0' src='img/cc_by_80x15.png' /></a>&nbsp;",tr("LicenceSeeCredits"),sep=""))
})

output$uiMain <- renderUI({
  tabsetPanel(id="Tabset",selected=1,
              tabPanel(
                tr("Statistics"),
                plotOutput("plot",height=700),#,height = "auto"
                value=1
              ),
              tabPanel(
                tr("Datas"),
                dataTableOutput('Datas'),
                value=2
              ),
              tabPanel(
                tr("Help"),
                h4(paste(tr("TechnicalInformations"),":",sep=" ")),
                p(HTML(tr("SessionCounter")," :",SRV$count)),
                value=3
              )
  )
})


output$uiCreditsTitle <- renderUI({
  tr("Credits")
})

output$uiCredits1 <- renderUI({
  mainPanel(
    p(HTML(paste("<strong>",tr("Author"),":</strong> Grégoire Vincke - <a href='http://www.gregoirevincke.be' target='_blank'>http://www.gregoirevincke.be</a> - ",tr("December")," 2015",sep=""))),
    p(HTML(paste("<strong>",tr("Translations")," :</strong> ",tr("TranslationsHowTo"),sep=""))),
    p(HTML(paste("<strong>Licences :</strong> <ul><li><strong>Animation :</strong> <a rel='license' href='http://creativecommons.org/licenses/by/2.0/be/'><img alt='Licence Creative Commons' style='border-width:0' src='img/cc_by_80x15.png' /></a>",tr("CreditsLicence"),"</li><li><strong>Code :</strong> ",tr("SourceCodeLocation"),"</li></ul>",sep=""))),
    p(HTML(paste("<strong>",tr("Softwares")," :</strong> ",tr("SoftwaresIUsed")," :"))),
    HTML("<ul>"),
    HTML('<li><strong>R</strong> : R Core Team (2013). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL <a href="http://www.R-project.org/" target=_blank">http://www.R-project.org/</a>.</li>'),
    HTML('<li><strong>RStudio</strong> : RStudio Team (2012). RStudio: Integrated Development for R. RStudio, Inc., Boston, MA URL <a href="http://www.rstudio.com/" target=_blank">http://www.rstudio.com/</a>.</li>'),
    HTML('<li><strong>shiny</strong> : RStudio and Inc. (2014). shiny: Web Application Framework for R. R package version 0.10.2.1. <a href="http://CRAN.R-project.org/package=shiny" target=_blank">http://CRAN.R-project.org/package=shiny</a> and <a href="http://shiny.rstudio.com" target=_blank">http://shiny.rstudio.com</a>.</li>'),
    HTML("</ul>"),
    p(HTML(paste("<strong>",tr("OtherApps")," :</strong> ","<ul><li><a href='http://yapluka.be/sapps/beprmap/' target='_blank'>Belgium Pigeon Racing Map</a></li></ul>",sep="")))
  )
})

output$uiCredits2 <- renderUI({
  mainPanel(
    p(HTML(paste("<strong>",tr("Sources")," :</strong> ",tr("SourcesiUsed")," :",sep=""))),
    HTML("<ul>"),
    HTML("<li>&nbsp;</li>"),
    HTML("</ul>")
  )
})


})
