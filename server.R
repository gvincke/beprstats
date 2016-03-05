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
library(plotrix)#legend.color
library(png)

# options(shiny.trace=TRUE)
# options(shiny.error = browser)

# Create a reactive object here that we can share between all the sessions.
SRV <- reactiveValues(count=0)#Session reactive values
cc <- readPNG("www/img/cc_by_320x60.png")

# TODO
# Ajouter legendes et titres et explications dans neutralisation et poteau
# Réfléchir a montrer les changement de classement (combien de pigeons changent de position sans neutralisations)
# Créer classement général : avec ou sans poteau !!!!! (classement vitesse puis par date+heure de constatation !!)
# Compter combien de victoires générale par age et catégorie et le mettre dans le summary
# Summary adapter le template grace aux colonnes et mettre des boites de texte à droite des plots
# Calculer un classement ou tout le monde est considéré comme au poteau, et comparer avec autres modes de classements en calculant le % de pigeons qui changent de place
# Distance : proposer de changer le cadre de référence et afficher l'es heures de vol en y distance en x et ligne de vitesse en oblique : pour ça ajouter les heures de vol avec et sans neutralisation ?
# Faire un scrpit de vérification des données de distance ne fut-ce que entre fichier int et doublage femelles, et entre différentes éditions du même concours
# Expliquer en quoi c'est difficile de post-traiter : faudrais : coordonnées lieux du lâcher, coordonnées des amateurs, datetime de la constatation et pas que time, heures de neutralisation, et TOUS les résultats, pas que ceux classés, sexe de tous les pigeons, age de tous les pigeons, pays de tous les pigeons
# Pour chaque facteur de variation dans le plot distance associer un plot de comptage des nombres par critères avec une répatition 3/4 1/4 conditionnelles (Afficher les nombres par catégorie) : ou ajouter les nombres dans les légendes !!! en face de chaque niveau de chaque facteur ?
# Femelles : sexe = 0 pour sexe inconnu, et >0 = rank dans le doublage femelle ? (oui mais s'il y a plusieurs doublages ?)
# Titres des graphiques en varaible puis paste pour le complément (plotDistance par exemple)
# résultats nationaux Belgique :  toutes les femelles sont doublées car c'est gratuit : compéraison des sexes est alors possible !

#Done :
# Hypothese Philippens : poteau = pour réduire impact de la distance en dessous de 800m/min donc identifier dans le plot de distance l'effet du poteau, avec en rouge ceux qui perdent des places et en vert ceux qui en gagnent. Normalement sous le poteau je devrais trouver du vert, et au delà du rouge ... Confirmé par Sébastien casaerts et par le CFW : 800m/min est la vitesse minimale de vol du pigeon. Donc en dessous de cette vitesse il s'est OBLIGATOIREMENT arrété, et le but du poteau est de limiter cet impact d'un arrêt, qui est d'autant plus grand que la distance est longue
# Créer liste des concours avec leur caractéristiques (neutralisations, nombre de pigeons par catégories, etc)
# CSV : import fichier par fichier APRES selection de la course et de l'édition
# Ajouter les nombres bruts aux barplots : pour ça il faut avoir créé liste des concours avec leur caractéristiques (neutralisations, nombre de pigeons par catégories, etc)
# Calculer les vitesses à partir des valeurs de constatation selon le poteau, pour comparer et montrer absurdité des poteaux
# Ajouter la catégorie dans les facteurs de variation : et une select pour n'affichier qu'une seule catégorie quand nécéssaire
# http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
# Gain et Pertes : faire for i dans les catégories existantes et afficher autant de plot que de catégorie
# Supprimer le plotNeutral, et le fisionner avec le plot Poteau pour en faire un plot Classements ou on peut sélectionner ce qu'on met en x (catpos ou rank) et ce qu'on met en y (rank ou rankWN)
# Barcelone 2010 et 2009 : vérifier que les deux premiers ne sont pas avec ds vitesses calculées trop grandes du a un jours de ocntatation erronée, car sortent du poteau !! En fait vérifier chaque cocours que le classement soit bien respecté, car dans les barcelone il y a toujours 3 - 4 pigeons issus du poteau qui ont des vitesses énormes dues à un jour de constatation erroné !

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
  
  getSpeedScaleSelection<-reactive({
    speedselect <- read.delim("data/lang-speedscaleselection.csv", header = TRUE, sep = "\t", as.is = TRUE) 
    row.names(speedselect)<-speedselect$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
    l<-list()
    for(i in 1:nrow(speedselect)){
      l[[speedselect[[input$language]][i]]]<-speedselect$key[i]
    }
    return(l)
  })
  
  yesno <- read.delim("data/lang-yesno.csv", header = TRUE, sep = "\t", as.is = TRUE) 
  row.names(yesno)<-yesno$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
  getYesNoSelection<-reactive({
    l<-list()
    for(i in 1:nrow(yesno)){
      l[[yesno[[input$language]][i]]]<-yesno$key[i]
    }
    return(l)
  })
  
  categories <- read.delim("data/lang-categories.csv", header = TRUE, sep = "\t", as.is = TRUE) 
  row.names(categories)<-categories$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
  getCategorySelection<-reactive({
    l<-list()
    for(i in 1:nrow(categories)){
      l[[categories[[input$language]][i]]]<-categories$key[i]
    }
    return(l)
  })
  
  roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {#http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
  }
  
  #races results
  races <- read.csv("data/races.csv", sep=",", dec=".")
  # Create a solution of read import and merge several csv files = more efficient : http://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
  # Question : mieux vaut il crer une grosse DB d'abord, et réduire les données ensuite, ou d'abord récolter les infos, faire les choix, et créer la db nécéssaire à la volée ?
  
  observe({#http://stackoverflow.com/questions/28119964/dynamic-input-selector-based-on-uploaded-data
    v<-sort(as.vector(unique(races$name))) 
    v<-c(" "="empty",v)
    updateSelectInput(#http://www.inside-r.org/packages/cran/shiny/docs/updateSelectInput
      session,
      "races",
      choices=v)
  })
  
  
  observe({#http://stackoverflow.com/questions/28119964/dynamic-input-selector-based-on-uploaded-data
    race<-subset(races,name %in% c(input$races))
    v<-sort(as.vector(unique(race$date))) 
    v<-c(" "="empty",v)
    updateSelectInput(#http://www.inside-r.org/packages/cran/shiny/docs/updateSelectInput
      session,
      "editions",
      choices=v)
  })
  
  
  DistFactors <- read.delim("data/lang-distancefactors.csv", header = TRUE, sep = "\t", as.is = TRUE) 
  row.names(DistFactors)<-DistFactors$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
  observe({#http://stackoverflow.com/questions/28119964/dynamic-input-selector-based-on-uploaded-data
    l<-list()
    l<-c(" "="unselected",l)
    for(i in 1:nrow(DistFactors)){
      l[[DistFactors[[input$language]][i]]]<-DistFactors$key[i]
    }
    updateSelectInput(#http://www.inside-r.org/packages/cran/shiny/docs/updateSelectInput
      session,
      "distfactors",
      choices=l)
  })
  
  RankingMethods <- read.delim("data/lang-rankingmethods.csv", header = TRUE, sep = "\t", as.is = TRUE) 
  row.names(RankingMethods)<-RankingMethods$key #to have key in both row.names and $key. If we whant only as row.names add row.names=1 to read.delim
  observe({#http://stackoverflow.com/questions/28119964/dynamic-input-selector-based-on-uploaded-data
    l<-list()
#     l<-c(" "="unselected",l)
    for(i in 1:nrow(RankingMethods)){
      l[[RankingMethods[[input$language]][i]]]<-RankingMethods$key[i]
    }
    updateSelectInput(#http://www.inside-r.org/packages/cran/shiny/docs/updateSelectInput
      session,
      "rankingmethods",
      choices=l)
  })
  

  getInputValues<-reactive({
    return(input)#collect all inputs
  })

  getResults<-reactive({
    withProgress(message = tr('ImportData'), value = 0, {
      race<-subset(races,name %in% input$races)
      files <- list.files(path = "./data/rds",pattern = paste("*-",race$id,"-[[:digit:]]{1}.rds",sep=""))
      result <- do.call(rbind, lapply(files, function(x) {
        incProgress(1/length(files), detail = paste(tr('NbFilesToImport'),":",length(files)))
        readRDS(paste(".","data","rds",x,sep="/"))}
        ))
      return(result)
    })
  })

  getComputedValues<-reactive({
    v<-getInputValues() # get all values of input list
    cv<-list()#created empty computed values list

    # Import all editions of this race to allow summary to be active
    if(v$races!="empty" ){#& v$editions== "empty"
      results<-getResults()
      results$speedkmh<-(results$speed/1000)*60
      
      cv$data<-results
      cv$dataS<-results#pour le summary
      if(v$races!="empty"){
        cv$data<-subset(cv$data,racename %in% c(v$races))
        cv$dataS<-subset(cv$dataS,racename %in% c(v$races))
      }
      if(v$editions!="empty"){
        cv$data<-subset(cv$data,racedate %in% c(v$editions))
      }
      if(v$racecat!="-1"){
        cv$data<-subset(cv$data,cat %in% c(v$racecat))
      }
      if(v$speedneutral=='n'){
        cv$data$speedtoplot<-cv$data$speedWN
        cv$dataS$speedtoplot<-cv$dataS$speedWN
      } else {
        cv$data$speedtoplot<-cv$data$speed
        cv$dataS$speedtoplot<-cv$dataS$speed
      }
      if(v$speedloosegain=='n'){
        cv$data<-subset(cv$data,speedtoplot >= 800)
        cv$dataS<-subset(cv$dataS,speedtoplot >= 800)
      }
      
      #Calculer un rang total ok, mais faut surtout rang calculé DANS la catégorie !
      cv$data <- do.call("rbind", as.list(by(cv$data, cv$data["cat"], transform, catrank=rank(-speed,na.last='NA',ties.method='min'))))
      cv$data <- do.call("rbind", as.list(by(cv$data, cv$data["cat"], transform, catrankWN=rank(-speedWN,na.last='NA',ties.method='min'))))
      rownames(cv$data) <- 1:nrow(cv$data)#Reconstruire les indices changés par le(s) do.call précédent
      cv$data$catrankDiff<-cv$data$catrank-cv$data$catrankWN
      cv$data$catposcatrankDiff<-cv$data$catpos-cv$data$catrank
      cv$data$catposcatrankWNDiff<-cv$data$catpos-cv$data$catrankWN
  
      cv$data$rank<-rank(-cv$data$speed,na.last='NA',ties.method='min')
      cv$data$rankWN<-rank(-cv$data$speedWN,na.last='NA',ties.method='min')
      cv$data$rankDiff<-cv$data$rank-cv$data$rankWN
      cv$data$catposrankDiff<-cv$data$catpos-cv$data$rank
      cv$data$catposrankWNDiff<-cv$data$catpos-cv$data$rankWN
      cv$data$racedate <- factor(cv$data$racedate)#http://stackoverflow.com/questions/1195826/drop-factor-levels-in-a-subsetted-data-frame
      cv$dataS$racedate <- factor(cv$dataS$racedate)#http://stackoverflow.com/questions/1195826/drop-factor-levels-in-a-subsetted-data-frame
      cv$data$distkm<-cv$data$dist/1000
      cv$datatoshow<-subset(cv$data,select=c(ring,age,owner,location,cat,catpos,ownerpos,dist,time,inneutral,speed,speedWN,rank,rankWN,catrankDiff))#,racename,racedate,,catposcatrankWNDiff
      names(cv$datatoshow)[1]<-paste("Ring")
      names(cv$datatoshow)[2]<-paste("Age")
      names(cv$datatoshow)[3]<-paste("Owner")
      names(cv$datatoshow)[4]<-paste("Location")
      names(cv$datatoshow)[5]<-paste("C")#Cat
      names(cv$datatoshow)[6]<-paste("CR")#Cat Rank
      names(cv$datatoshow)[7]<-paste("OR")#Owner Rank
      names(cv$datatoshow)[8]<-paste("Dist")
      names(cv$datatoshow)[9]<-paste("Time")
      names(cv$datatoshow)[10]<-paste("IN")#In Neutral
      names(cv$datatoshow)[11]<-paste("S")#Speed
      names(cv$datatoshow)[12]<-paste("SWN")#Speed Without Neutral
      names(cv$datatoshow)[13]<-paste("R")#Rank
      names(cv$datatoshow)[14]<-paste("RWN")#Rank Without Neutral
      names(cv$datatoshow)[15]<-paste("CRD")#Cat Rank Diff
    }
    return(cv)
  })
  
  output$plotSummary <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    if(v$races=="empty"){
      m<-matrix(c(1,2),2,1,byrow=TRUE)#matrix(c(1,1,2),3,1,byrow=TRUE)
      layout(m,width=c(1,1))
      par(bty="n")
      plot(0,0,type='l',ylab="Vitesse (m/min)",xlab="Dates",main="Distribution des vitesses par édition",ylim=c(800,1200),xaxt="n")
      text(0,1000,"Sélectionnez un concours dans la liste déroulante à gauche", col='red',cex=1.5)
      plot(0,0,type='l',ylab="Nombre de pigeons",xlab="Dates",main="Nombre total de pigeons",ylim=c(0,1000),xaxt="n")
    }
    
    if(v$races!="empty"){
      withProgress(message = tr('ImportData'), value = 0, {
        m<-matrix(c(1,2),2,1,byrow=TRUE)#matrix(c(1,1,2),3,1,byrow=TRUE)
        layout(m,width=c(1,1))
        #Nombre total d'engagés par édition, par catégorie
        #Podium du nombre de victoires totales par catégories, et par age
        par(bty="n")
        if(v$speedscale=='man'){
          boxplot(speedtoplot~racedate,data=cv$dataS, col='green',range=1.5,varwidth=TRUE,ylim=c(v$speed[1],v$speed[2]),ylab="Vitesse (m/min)",xlab="Dates",main="Distribution des vitesses par édition",xaxt="n")# col=rainbow(length(unique(cv$dataS$racedate))),ylim=c(800,1800) , range=1.5 by default : gestion des valeurs extrèmes. varwidth=Largeur proportionnelle à la racine carrée du nombre d’observation par groupe
        } else {
          boxplot(speedtoplot~racedate,data=cv$dataS, col='green',range=1.5,varwidth=TRUE,ylab="Vitesse (m/min)",xlab="Dates",main="Distribution des vitesses par édition",xaxt="n")# col=rainbow(length(unique(cv$dataS$racedate))),ylim=c(800,1800) , range=1.5 by default : gestion des valeurs extrèmes. varwidth=Largeur proportionnelle à la racine carrée du nombre d’observation par groupe
        }
        incProgress(1/2, detail = "Plotting boxplot")
        axis(1, at=1:length(unique(cv$dataS$racedate)), labels=unique(cv$dataS$racedate))#las = 2,, cex.axis = 0.8
        
        # Solution : obtenir les valeurs, recréer une DB avec sructure et list puis as.matrix comme dans http://www.theanalysisfactor.com/r-11-bar-charts/ exemple datass<-structure(list(c(20,25),c(18,20),c(25,30)), .Names=c("a","b","c"), class = "data.frame", row.names=c(NA,-2L)) puis barplot(as.matrix(datass)) 
        n<-list()
        races <- read.csv("data/races.csv", sep=",", dec=".")
        dates<-as.vector(unique(factor(races[races$name==v$races, "date"])))
        for(d in dates){
          #catnb
          N0<-races[races$date == d & races$name==v$races, "cn0"]
          N1<-races[races$date == d & races$name==v$races, "cn1"]
          N2<-races[races$date == d & races$name==v$races, "cn2"]
          #Calculer le nombre de clasés car ce n'est pas toujours mentionné ni standard.
          n0<-length(cv$dataS[cv$dataS$racedate == d & cv$dataS$racename==v$races & cv$dataS$cat =="0","speed"])
          n1<-length(cv$dataS[cv$dataS$racedate == d & cv$dataS$racename==v$races & cv$dataS$cat =="1","speed"])
          n2<-length(cv$dataS[cv$dataS$racedate == d & cv$dataS$racename==v$races & cv$dataS$cat =="2","speed"])
          n[[d]]<-c((N0+N1+N2)-(n0+n1+n2),n0+n1+n2)
        }
        m<-as.matrix(structure(n, class = "data.frame", .Names=dates, row.names=c(NA,-2L)))
        bp<-barplot(m, xlab="Dates",ylab="Nombre de pigeons",main="Nombre total de pigeons",col=c('gray','green'),ylim=c(0,roundUpNice(max((m[1,]+m[2,])*1.1))),names=dates)#col=rainbow(length(unique(dates)))
        #points(x = bp, y = (m[1,]+m[2,])+(m[2,]*0.5),col='red')
        text(x = bp, y = m[1,]+m[2,], label = m[1,]+m[2,], pos = 3, cex = 0.8, col = "red")## Add text at top of bars
        text(x = bp, y = m[1,]+(m[2,]/2), label = m[2,], cex = 0.8, col = "black")## Add text at top of bars
        text(x = bp, y = m[1,]/2, label = m[1,], cex = 0.8, col = "black")## Add text at top of bars
        legend('topright',legend = c('Classés','Non classés'),col=c('green','gray'),pch=15)#,horiz=TRUE
        incProgress(1/2, detail = "Plotting barplot")
      })
    }
  })
  
  output$plotCatAndAge <- renderPlot({
    v<-getInputValues()
    cv<-getComputedValues()
    if(v$editions=='empty'){
      m<-matrix(c(1,2,3,4,5,6),2,3,byrow=TRUE)
      layout(m,width=c(1,1.5,4,1,1.5,4))
      par(bty="n")
      
      m <- matrix( c(0,1),nrow=1,ncol=2)
      dimnames(m) = list(c(),c("f1", "f2"))
      boxplot(f1~f2,m,col="white",xaxt="n",xlab="",ylab="Vitesse (m/min)",ylim=c(800,1600),main="Distribution des vitesses")
      
      m <- matrix( c(0,0,0,1,2,3),nrow=3,ncol=2)
      dimnames(m) = list(c("1","2","3"),c("f1", "f2"))
      boxplot(f1~f2,m,col="white",ylab="Vitesse (m/min)",xlab="Catégorie",ylim=c(800,1600),main="Distribution des vitesses par catégorie",names=c(tr("Youngsters"), tr("Yearlings"),tr("Olds")))
      
      m <- matrix( c(0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8),nrow=8,ncol=2)
      dimnames(m) = list(c(),c("f1", "f2"))
      boxplot(f1~f2,m,col="white",ylab="Vitesse (m/min)",xlab="Age (années)",ylim=c(800,1600),main="Distribution des vitesses par classe d'age")
      text(4.5,1200,"Sélectionnez un concours ET une de ses éditions dans les listes déroulantes à gauche",col='red',cex=2)
      
      barplot(c(0),xaxt="n",xlab="",ylab="Nombre de pigeons",main="Nombre total de pigeons",ylim=c(0,1000))
      barplot(c(0,0,0),xlab="Catégorie",ylab="Nombre de pigeons",main="Nombre de pigeons par catégorie",ylim=c(0,1000),names=c(tr("Youngsters"), tr("Yearlings"),tr("Olds")))
      barplot(c(0,0,0,0,0,0,0,0),xlab="Age (années)",ylab="Nombre de pigeons",main="Nombre de pigeons par classe d'age",names=c(1:8),ylim=c(0,1000))
      
    }

    if(v$races!="empty" & v$editions!="empty"){
      withProgress(message = tr('ImportData'), value = 0, {
        m<-matrix(c(1,2,3,4,5,6),2,3,byrow=TRUE)
        layout(m,width=c(1,1.5,4,1,1.5,4))
        
        #http://stackoverflow.com/questions/29185996/plot-empty-groups-in-boxplot
        par(bty="n")
        
        if(v$speedscale=='man'){
          boxplot(speedtoplot~racedate,data=cv$data, col='green',range=1.5,varwidth=TRUE,ylim=c(v$speed[1],v$speed[2]),ylab="Vitesse (m/min)",xlab="",main="Distribution des vitesses")# ,ylim=c(800,1800) , range=1.5 by default : gestion des valeurs extrèmes. varwidth=Largeur proportionnelle à la racine carrée du nombre d’observation par groupe
        } else {
          boxplot(speedtoplot~racedate,data=cv$data, col='green',range=1.5,varwidth=TRUE,ylab="Vitesse (m/min)",xlab="",main="Distribution des vitesses")# ,ylim=c(800,1800) , range=1.5 by default : gestion des valeurs extrèmes. varwidth=Largeur proportionnelle à la racine carrée du nombre d’observation par groupe
        }
        incProgress(1/6, detail = "Plotting boxplot 1")
        
        catmax<-max(cv$data$cat, na.rm=TRUE)
        agemax<-max(cv$data$age, na.rm=TRUE)
        
        cv$data$cattoplot <- factor(cv$data$cat,levels = 0:catmax)#create categories and force adding of empty groups
        if(v$speedscale=='man'){
          boxplot(speedtoplot~cattoplot,data=cv$data, col=rainbow(length(unique(cv$data$age))),range=1.5,varwidth=TRUE,ylim=c(v$speed[1],v$speed[2]),ylab="Vitesse (m/min)",xlab="Catégorie",main="Distribution des vitesses par catégorie",names=c(tr("Youngsters"), tr("Yearlings"),tr("Olds")))# ,ylim=c(800,1800) , range=1.5 by default : gestion des valeurs extrèmes. varwidth=Largeur proportionnelle à la racine carrée du nombre d’observation par groupe
        } else {
          boxplot(speedtoplot~cattoplot,data=cv$data, col=rainbow(length(unique(cv$data$age))),range=1.5,varwidth=TRUE,ylab="Vitesse (m/min)",xlab="Catégorie",main="Distribution des vitesses par catégorie",names=c(tr("Youngsters"), tr("Yearlings"),tr("Olds")))# ,ylim=c(800,1800) , range=1.5 by default : gestion des valeurs extrèmes. varwidth=Largeur proportionnelle à la racine carrée du nombre d’observation par groupe
        }
        incProgress(1/6, detail = "Plotting boxplot 2")
        
        cv$data$agetoplot <- factor(cv$data$age,levels = 0:agemax)#create categories and force adding of empty groups
        if(v$speedscale=='man'){
          boxplot(speedtoplot~agetoplot,data=cv$data, col=rainbow(length(unique(cv$data$age))),range=1.5,varwidth=TRUE,ylim=c(v$speed[1],v$speed[2]),ylab="Vitesse (m/min)",xlab="Age (années)",main="Distribution des vitesses par classe d'age")# ,ylim=c(800,1800) , range=1.5 by default : gestion des valeurs extrèmes. varwidth=Largeur proportionnelle à la racine carrée du nombre d’observation par groupe
        } else {
          boxplot(speedtoplot~agetoplot,data=cv$data, col=rainbow(length(unique(cv$data$age))),range=1.5,varwidth=TRUE,ylab="Vitesse (m/min)",xlab="Age (années)",main="Distribution des vitesses par classe d'age")# ,ylim=c(800,1800) , range=1.5 by default : gestion des valeurs extrèmes. varwidth=Largeur proportionnelle à la racine carrée du nombre d’observation par groupe
        }
        incProgress(1/6, detail = "Plotting boxplot 3")
        
        #catnb
        N0<-races[races$date == v$editions & races$name==v$races, "cn0"]
        N1<-races[races$date == v$editions & races$name==v$races, "cn1"]
        N2<-races[races$date == v$editions & races$name==v$races, "cn2"]
        #Calculer le nombre de clasés car ce n'est pas toujours mentionné ni standard.
        n0<-length(cv$dataS[cv$dataS$racedate == v$editions & cv$dataS$racename==v$races & cv$dataS$cat =="0","speed"])
        n1<-length(cv$dataS[cv$dataS$racedate == v$editions & cv$dataS$racename==v$races & cv$dataS$cat =="1","speed"])
        n2<-length(cv$dataS[cv$dataS$racedate == v$editions & cv$dataS$racename==v$races & cv$dataS$cat =="2","speed"])
        
        n<-c((N0+N1+N2)-(n0+n1+n2),n0+n1+n2)
        m<-as.matrix(structure(list(n), class = "data.frame", .Names=v$editions, row.names=c(NA,-2L)))
        bp<-barplot(m, xlab=" ",xaxt="n",ylab="Nombre de pigeons",main="Nombre total de pigeons",col=c('gray','green'),ylim=c(0,roundUpNice(max((m[1,]+m[2,])*1.1))))
        text(x = bp, y = m[1,]+m[2,], label = m[1,]+m[2,], pos = 3, cex = 1, col = "red")## Add text at top of bars
        if(m[2,]>0){text(x = bp, y = m[1,]+(m[2,]/2), label = m[2,], cex = 1, col = "black")}## Add text at top of bars
        if(m[1,]>0){text(x = bp, y = m[1,]/2, label = m[1,], cex = 1, col = "black")}## Add text at top of bars
        incProgress(1/6, detail = "Plotting barplot 1")
        
        #Pour utiliser les couleurs de rainbow : Créer un box plot avec uns seule des deux cat, et superposer à ce boxplot le second en mettant entre les deux un par(new=TRUE)
        N<-c(N0,N1,N2)#total
        nc<-c(n0,n1,n2)#classés
        ncl<-nc
        ncl[ncl==0]<-" "
        nnc<-c(N0-n0,N1-n1,N2-n2)#non classés
        nncl<-nnc
        nncl[nncl==0]<-" "
        bp<-barplot(N, xlab="Catégorie",ylab="Nombre de pigeons",main="Nombre de pigeons par catégorie",col=rainbow(length(unique(cv$data$age))),ylim=c(0,roundUpNice(max((m[1,]+m[2,])*1.1))),names=c(tr("Youngsters"), tr("Yearlings"),tr("Olds")))
        text(x = bp, y = N, label = N, pos = 3, cex = 1, col = "red")## Add text at top of bars
        text(x = bp, y = nnc+nc/2, label = ncl, cex = 1, col = "black")
        par(new=TRUE)     
        bp<-barplot(nnc, xlab="Catégorie",ylab="Nombre de pigeons",main="Nombre de pigeons par catégorie",col=c('gray'),ylim=c(0,roundUpNice(max((m[1,]+m[2,])*1.1))),names=c(tr("Youngsters"), tr("Yearlings"),tr("Olds")))
        text(x = bp, y = nnc/2, label = nncl, cex = 1, col = "black")
        incProgress(1/6, detail = "Plotting barplot 2")
        
        agemax<-max(cv$data$age, na.rm=TRUE)
        n <- table(factor(cv$data$age,levels = 0:agemax))#http://r.789695.n4.nabble.com/creating-empty-cells-with-table-td798211.html
        bp<-barplot(n, xlab="Age (années)",ylab="Nombre de pigeons",main="Nombre de pigeons par classe d'age",col=rainbow(length(unique(cv$data$age))),ylim=c(0,roundUpNice(nrow(cv$data))))
        text(x = bp, y = n, label = n, pos = 3, cex = 1, col = "red")## Add text at top of bars
        incProgress(1/6, detail = "Plotting barplot 3")
      })
    }
  })

output$plotDistance <- renderPlot({
  v<-getInputValues()
  cv<-getComputedValues()

  #catmax<-max(cv$data$cat, na.rm=TRUE)
  par(bty="n")#http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html mais je commente oma = c(1, 1, 4, 1),mar=c(1,1,1,1), sinon on ne vois plus les labels
  if(v$editions=="empty"){
    plot(0,0,type='l',ylab="Vitesse (m/min)",xlab="Distance (km)",main="Distribution des vitesses en fonction de la distance parcourrue",pch=20,col='gray50',xlim=c(0,1000),ylim=c(800,1200))
    text(500,1000,"Sélectionnez un concours ET une de ses éditions dans les listes déroulantes à gauche", col='red',cex=1.5)
  }
  if(v$races!="empty" & v$editions!="empty"){
    withProgress(message = tr('ImportData'), value = 0, {
    if(v$speedscale=='man'){
      plot(cv$data$distkm,cv$data$speedtoplot,ylim=c(v$speed[1],v$speed[2]),ylab="Vitesse (m/min)",xlab="Distance (km)",main="",pch=20,col='gray50')#,ylim=c(min(cv$data$speed),max(cv$data$speed))
    } else {
      plot(cv$data$distkm,cv$data$speedtoplot,ylab="Vitesse (m/min)",xlab="Distance (km)",main="",pch=20,col='gray50')#,ylim=c(min(cv$data$speed),max(cv$data$speed))
    }
    incProgress(1/2, detail = "Plotting distance")
    if(v$flh==TRUE){#Tracer les lignes d'heures de vol
      dist.min<-min(cv$data$dist)
      dist.max<-max(cv$data$dist)
      dist<-c(dist.min,dist.max)
      minutes<-seq(60,7200,by=60)#24*5*60 = nb de minutes pour 5j
      for(i in 1:length(minutes)){
        #obtenir les coordonées des deux points : calculer la vitesse ici pour couvrir la dist dans le temps impartis
        speed.min<-dist.min/minutes[i]
        speed.max<-dist.max/minutes[i]
        speed<-c(speed.min,speed.max)
        #calculer le modèle de régression linéaire
        #     modl<-lm(speed~dist)
        # abline(modl)
        distkm<-dist/1000
        lines(distkm,speed,lty=2,col='gray')
        text(c(distkm[1]),c(speed.min),paste(as.character(minutes[i]/60),"h",sep=""),col='gray90')
      }
    }
    
    if(v$lm==TRUE & v$distfactors=='unselected'){
      reg <- lm(cv$data$speedtoplot~cv$data$distkm)#http://www.ats.ucla.edu/stat/r/faq/scatter.htm
      abline(coef(reg),col="gray50",lwd=2)#http://www.ats.ucla.edu/stat/r/faq/scatter.htm
      abline(coef(reg),col="black",lwd=1,lty=2)
      text(max(cv$data$distkm),(max(cv$data$distkm)*coef(reg)[2]+coef(reg)[1])*1.02,bquote(.(tr('Slope')) == .(round(coef(reg)[2],3)) ~~ R^2 == .(round(summary(reg)$r.squared,2))),pos=2,col='gray50',cex=1.5) 
    }

    if(v$distfactors=='unselected'){
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1, 1, 2, 1), new = TRUE)#http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", main="Distribution des vitesses en fonction de la distance parcourrue")#plot invisible qui se met en surcouche du précédent #http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
    }
    
    if(v$distfactors=='cat'){
      cats<-c(0,1,2)
      col<-c('red','orange','yellow')
      for(i in 1:3){
        sub.data<-subset(cv$data,cat %in% c(cats[i]))
        points(sub.data$distkm,sub.data$speedtoplot,pch=20,col=col[i])
      }
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1, 1, 2, 1), new = TRUE)#http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", main="Distribution des vitesses en fonction de la distance parcourrue")#plot invisible qui se met en surcouche du précédent #http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
      legend('top',legend = c(tr("Youngsters"), tr("Yearlings"),tr("Olds")),col=col,pch=20,title = tr('Category'),xpd=TRUE,horiz=TRUE)#,inset=c(-0.01,0)
    }
    
    if(v$distfactors=='age'){
      agemax<-max(cv$data$age, na.rm=TRUE)
      ages<-c(0:agemax)
      cv$data$agetoplot <- factor(cv$data$age,levels = ages)
      col<-rainbow(length(ages))
      for(i in 1:length(ages)){
        sub.data<-subset(cv$data,age %in% c(ages[i]))
        points(sub.data$distkm,sub.data$speedtoplot,pch=20,col=col[i])
      }
      
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1, 1, 2, 1), new = TRUE)#http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", main="Distribution des vitesses en fonction de la distance parcourrue")#plot invisible qui se met en surcouche du précédent #http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
      legend('top',legend = ages,col=col,pch=20,title = tr('PigeonsAge'),xpd=TRUE,horiz=TRUE)#,inset=c(-0.01,0)
    }
    
    if(v$distfactors=='date'){#Baser sur jconstat et non les dates !! C'est plus parlant et c'est plus facile de créer une liste de couleur fixe      j<-c(0:4)
      j<-c(0:4)
      cv$data$jtoplot <- factor(cv$data$jconstat,levels = j)
      col<-rainbow(length(j))
      for(i in 1:length(j)){
        sub.data<-subset(cv$data,jconstat %in% c(j[i]))
        points(sub.data$distkm,sub.data$speedtoplot,pch=20,col=col[i])
          if(v$lm==TRUE & nrow(sub.data)>1){
            reg <- lm(sub.data$speedtoplot~sub.data$distkm)#http://www.ats.ucla.edu/stat/r/faq/scatter.htm
            abline(coef(reg),col=col[i],lwd=2)#http://www.ats.ucla.edu/stat/r/faq/scatter.htm
            abline(coef(reg),col='black',lwd=1,lty=2)#http://www.ats.ucla.edu/stat/r/faq/scatter.htm
            text(max(sub.data$distkm),(max(sub.data$distkm)*coef(reg)[2]+coef(reg)[1])*1.02,bquote(.(tr('Slope')) == .(round(coef(reg)[2],3)) ~~ R^2 == .(round(summary(reg)$r.squared,2))),pos=2,col='black',cex=1.5)# ,srt=(round(coef(reg)[2],2)*(180*pi))+180 : ne marche que dans une graphique carré, sinon la paralaxe pose problème : de plus il faut des unités ou 1x = 1y car les degrés de rotation sont absolut, et non dépendnat du système de coordonnées || ceci remet à l'horizontal ??? ,srt=atan2(max(sub.data$distkm),(max(sub.data$distkm)*coef(reg)[2]+coef(reg)[1])*1.02)*(pi/180)  || voir https://stat.ethz.ch/pipermail/r-help/2006-February/087559.html
          }
      }
      
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1, 1, 2, 1), new = TRUE)#http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", main="Distribution des vitesses en fonction de la date de constatation")#plot invisible qui se met en surcouche du précédent #http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
      legend('top',legend = j+1,col=col,pch=20,title = tr('ClockingDay'),xpd=TRUE,horiz=TRUE)#,inset=c(-0.01,0)
      
    }
    
      if(v$distfactors=='duration'){
        if(v$speedneutral=="y"){
          dmax<-ceiling(max(cv$data$flightduration, na.rm=TRUE)/60/60)#arrondir à l'heure supérieure
          d<-c(1:dmax)
          col<-rainbow(dmax)#pas length(d) car on exclu le 0
          for(i in 1:dmax){#pas length(d) car on exclu le 0
            sub.data<-subset(cv$data,flightduration <= d[i]*60*60 & flightduration > d[i-1]*60*60)
            points(sub.data$distkm,sub.data$speedtoplot,pch=20,col=col[i])
          }
        } else {
          dmax<-ceiling(max(cv$data$flightdurationWN, na.rm=TRUE)/60/60)#arrondir à l'heure supérieure
          d<-c(1:dmax)
          col<-rainbow(dmax)#pas length(d) car on exclu le 0
          for(i in 1:dmax){#pas length(d) car on exclu le 0
            sub.data<-subset(cv$data,flightdurationWN <= d[i]*60*60 & flightdurationWN > d[i-1]*60*60)
            points(sub.data$distkm,sub.data$speedtoplot,pch=20,col=col[i])
          }
        }

        par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1, 1, 2, 1), new = TRUE)#http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
        plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", main="Distribution des vitesses en fonction de durée du vol, en heures")#plot invisible qui se met en surcouche du précédent #http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
        color.legend.labels<-seq(1,dmax,by=4)
        color.legend.labels<-paste(color.legend.labels,"h",sep='')
        color.legend(1,1,1.03,0,color.legend.labels,col,gradient="y")
      }
    
      if(v$distfactors=='neutral'){
        col<-c('gray90','red')
        for(i in c(0,1)){
          sub.data<-subset(cv$data,inneutral == as.factor(i))#inneutral = 0 ou 1 selon que l'heure de constatation est dans la neutralisation ou pas
          points(sub.data$distkm,sub.data$speedtoplot,pch=20,col=col[i+1])
        }
        par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1, 1, 2, 1), new = TRUE)#http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
        plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", main="Distribution des vitesses en fonction de la date de constatation")#plot invisible qui se met en surcouche du précédent #http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
        legend('top',legend = c('Non','Oui'),col=col,pch=20,title = 'Constatation durant une neutralisation',xpd=TRUE,horiz=TRUE)#,inset=c(-0.01,0)
        
      }
      
      if(v$distfactors=='gainorloose'){
        sub.data<-subset(cv$data,catposcatrankDiff < 0)
        if(nrow(sub.data)>0){
          points(sub.data$distkm,sub.data$speedtoplot,pch=20,col=paste('red',ceiling(sub.data$catposcatrankDiff/(min(sub.data$catposcatrankDiff)/4)),sep=""))#Il y a 4 niveaux de couleur red plus cest foncé plus la différence de classement est grande
        }
        sub.data<-subset(cv$data,catposcatrankDiff > 0)
        if(nrow(sub.data)>0){
          points(sub.data$distkm,sub.data$speedtoplot,pch=20,col=paste('green',ceiling(sub.data$catposcatrankDiff/(max(sub.data$catposcatrankDiff)/4)),sep=""))#Il y a 4 niveaux de couleur green plus cest foncé plus la différence de classement est grande
        }
        sub.data<-subset(cv$data,catposcatrankDiff == 0)
        if(nrow(sub.data)>0){
          points(sub.data$distkm,sub.data$speedtoplot,pch=20,col='yellow')
        }
        par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1, 1, 2, 1), new = TRUE)#http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
        plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab='',ylab='',main="Distribution des vitesses en fonction de l'impact des gains et pertes en dessous de 800m/min")#plot invisible qui se met en surcouche du précédent #http://dr-k-lo.blogspot.be/2014/03/the-simplest-way-to-plot-legend-outside.html
        legend('right',legend = c('Positif','Inchangé','Négatif'),col=c('Green','yellow','red'),pch=20,title = 'Impact sur le classement',xpd=TRUE)#,inset=c(-0.01,0),xpd=TRUE,horiz=TRUE 
      }
    incProgress(1/2, detail = "Plotting factor")
    })
  }


})

output$plotRankings <- renderPlot({
  v<-getInputValues()
  cv<-getComputedValues()
  m<-matrix(c(1,2,3,4),2,2,byrow=TRUE)
  layout(m,width=c(1,1,1,1))
  withProgress(message = tr('ImportData'), value = 0, {
    #Plot avec titre et légende
    par(bty="n")
    plot(0,0,xlim=c(-1,1),ylim=c(-1,1),xaxt='n', yaxt='n',xlab='',ylab='',type='l',main='Comparaison des classements')
    if(v$rankingmethods=='co'){
      text(0,1,tr('RankingMethodCO'))
      xlab<-tr('RankingMethodCOLabel')
    } else {
      text(0,1,tr('RankingMethodCCAN'))
      xlab<-tr('RankingMethodCCANLabel')
    }
    if(v$speedneutral=='y'){
      text(0,0.9,tr('RankingMethodCCAN'))
      ylab<-tr('RankingMethodCCANLabel')
    } else {
      text(0,0.9,tr('RankingMethodCCSN'))
      ylab<-tr('RankingMethodCCSNLabel')
    }
    if(v$editions=='empty'){
      text(0,0.5,"Sélectionnez un concours ET une de ses éditions dans les listes déroulantes à gauche", col='red',cex=1.5)
    }
    
    if(v$distfactors=='cat'){
      cats<-c(0,1,2)
      col<-c('red','orange','yellow')
      legend('bottomright',legend = c(tr("Youngsters"), tr("Yearlings"),tr("Olds")),col=col,pch=20,title = tr('Category'))#,inset=c(-0.01,0)
    }
    if(v$distfactors=='age'){
      agemax<-max(cv$data$age, na.rm=TRUE)
      ages<-c(0:agemax)
      col<-rainbow(length(ages))
      legend('bottomright',legend = ages,col=col,pch=20,title = tr('PigeonsAge'),xpd=TRUE)#,inset=c(-0.01,0),,horiz=TRUE
    }
    if(v$distfactors=='date'){
      days<-c(0:4)
      col<-rainbow(length(days))
      legend('bottomright',legend = days+1,col=col,pch=20,title = tr('ClockingDay'))#,inset=c(-0.01,0),horiz=TRUE,,xpd=TRUE
    }
    if(v$distfactors=='duration'){
      if(v$speedneutral=="y"){
        dmax<-ceiling(max(cv$data$flightduration, na.rm=TRUE)/60/60)#arrondir à l'heure supérieure
        d<-c(1:dmax)
        col<-rainbow(dmax)#pas length(d) car on exclu le 0
      } else {
        dmax<-ceiling(max(cv$data$flightdurationWN, na.rm=TRUE)/60/60)#arrondir à l'heure supérieure
        d<-c(1:dmax)
        col<-rainbow(dmax)#pas length(d) car on exclu le 0
      }
      color.legend.labels<-seq(1,dmax,by=4)
      color.legend.labels<-paste(color.legend.labels,"h",sep='')
      color.legend(0.97,0.75,1,-1,color.legend.labels,col,gradient="y")
      text(0.8,-0.015,"Durée du vol (en heures)",srt=90)#,srt=90,pos=2
    }
    if(v$distfactors=='neutral'){
      col<-c('gray90','red')
      legend('bottomright',legend = c('Non','Oui'),col=col,pch=20,title = 'Constatation durant une neutralisation')#,inset=c(-0.01,0) 
    }
    if(v$distfactors=='gainorloose'){
      legend('bottomright',legend = c('Positif','Inchangé','Négatif'),col=c('Green','yellow','red'),pch=20,title = 'Impact sur le classement')#,inset=c(-0.01,0),xpd=TRUE,horiz=TRUE
    }
    incProgress(1/4, detail = "Plotting plot 1")
    
    for(i in 0:2){# ne marche pas à cause d ela supperposition des plots pour mettre la légende dans la marge : recréer chaque plot indépendant et mettre légende dans un 4ème tout petit plot et jouer avec les layouts plutôt qu'avec les supperpositions
      par(bty="n",pty="s")#pty="s" force le plot à être carré ,oma = c(1, 1, 4, 1),mar=c(4,2,1,1) #,oma = c(1,1,3,4),mar=c(1,1,4,1)
      if(v$races!="empty" & v$editions!="empty"){
        sub.data<-subset(cv$data,cat == i)
        if(nrow(sub.data)>0){#reverse y axis : https://stat.ethz.ch/pipermail/r-help/2005-December/084726.html
          if(v$rankingmethods=='co'){x<-sub.data$catpos}else{x<-sub.data$catrank}
          if(v$speedneutral=='y'){
            plot(x,sub.data$catrank,ylim=rev(range(sub.data$catrank)),xlab='',ylab=ylab,pch=20,col='gray50', axes=FALSE)      #TODO : choix entre un scatterplot coloré ou des lignes verticales !
          } else {
            plot(x,sub.data$catrankWN,ylim=rev(range(sub.data$catrankWN)),xlab='',ylab=ylab,pch=20,col='gray50', axes=FALSE)      #TODO : choix entre un scatterplot coloré ou des lignes verticales !
          }
          if(v$distfactors=='cat'){
            for(j in 1:3){
              sub.sub.data<-subset(sub.data,cat %in% c(cats[j]))
              if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
              if(v$speedneutral=='y'){
                points(x,sub.sub.data$catrank,pch=20,col=col[j])
              } else {
                points(x,sub.sub.data$catrankWN,pch=20,col=col[j])
              }
            }
          }
          if(v$distfactors=='age'){
            for(j in 1:length(ages)){
              sub.sub.data<-subset(sub.data,age %in% c(ages[j]))
              if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
              if(v$speedneutral=='y'){
                points(x,sub.sub.data$catrank,pch=20,col=col[j])
              } else {
                points(x,sub.sub.data$catrankWN,pch=20,col=col[j])
              }
            }
          }
          
          if(v$distfactors=='date'){
            for(j in 1:length(days)){
              sub.sub.data<-subset(sub.data,jconstat %in% c(days[j]))
              if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
              if(v$speedneutral=='y'){
                points(x,sub.sub.data$catrank,pch=20,col=col[j])
              } else {
                points(x,sub.sub.data$catrankWN,pch=20,col=col[j])
              }
            }
          }
          
          if(v$distfactors=='duration'){
            if(v$speedneutral=="y"){
              for(j in 1:dmax){#pas length(d) car on exclu le 0
                sub.sub.data<-subset(sub.data,flightduration <= d[j]*60*60 & flightduration > d[j-1]*60*60)
                if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
                points(x,sub.sub.data$catrank,pch=20,col=col[j])
              }
            } else {
              for(j in 1:dmax){#pas length(d) car on exclu le 0
                sub.sub.data<-subset(sub.data,flightdurationWN <= d[j]*60*60 & flightdurationWN > d[j-1]*60*60)
                if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
                points(x,sub.sub.data$catrankWN,pch=20,col=col[j])
              }
            }
          }
          if(v$distfactors=='neutral'){
            if(v$speedneutral=="y"){
              for(j in c(0,1)){
                sub.sub.data<-subset(sub.data,inneutral == as.factor(j))#inneutral = 0 ou 1 selon que l'heure de constatation est dans la neutralisation ou pas
                if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
                points(x,sub.sub.data$catrank,pch=20,col=col[j+1])
              }
            } else {
              for(j in c(0,1)){
                sub.sub.data<-subset(sub.data,inneutral == as.factor(j))#inneutral = 0 ou 1 selon que l'heure de constatation est dans la neutralisation ou pas
                if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
                points(x,sub.sub.data$catrankWN,pch=20,col=col[j+1])
              }
            }
          }
    
          if(v$distfactors=='gainorloose'){
            if(v$speedneutral=='y'){
              sub.sub.data<-subset(sub.data,catposcatrankDiff < 0)
              if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
              points(x,sub.sub.data$catrank,pch=20,col='red')
              
              sub.sub.data<-subset(sub.data,catposcatrankDiff > 0)
              if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
              points(x,sub.sub.data$catrank,pch=20,col='green')
              
              sub.sub.data<-subset(sub.data,catposcatrankDiff == 0)
              if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
              points(x,sub.sub.data$catrank,pch=20,col='yellow')
            } else {
              sub.sub.data<-subset(sub.data,catposcatrankWNDiff < 0)
              if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
              points(x,sub.sub.data$catrankWN,pch=20,col='red')
              
              sub.sub.data<-subset(sub.data,catposcatrankWNDiff > 0)
              if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
              points(x,sub.sub.data$catrankWN,pch=20,col='green')
              
              sub.sub.data<-subset(sub.data,catposcatrankWNDiff == 0)
              if(v$rankingmethods=='co'){x<-sub.sub.data$catpos}else{x<-sub.sub.data$catrank}
              points(x,sub.sub.data$catrankWN,pch=20,col='yellow')
            }
          }
          lines(c(min(sub.data$catrank),max(sub.data$catrank)),c(min(sub.data$catrankWN),max(sub.data$catrankWN)),lty=3,col='black') # Dois être répété dans chaque nouveau plot précédent le "vide" qui place titre et légende sinon ne s'affiche pas correctement     
        } else { # Empty plot if there is no pigeons in this category
          par(bty="n",pty="s")
          plot(0,0,ylim=rev(range(cv$data$catrankWN)),xlim=range(cv$data$catrank),xlab='',ylab=ylab,main='',pch=20,col='white',axes=FALSE)
        }
      } else {
        par(bty="n",pty="s")
        plot(0,0,ylim=c(0,1000),xlim=c(0,1000),xlab='',ylab=ylab,main='',pch=20,col='white',axes=FALSE)
      }
  
      axis(3)# Draw the x axis
      axis(2)# Draw the y axis
      mtext(xlab, side=3, line=3)#http://stackoverflow.com/questions/12302366/moving-axes-labels-in-r
      if(i == 0){title=tr('Youngsters')}
      if(i == 1){title=tr('Yearlings')}
      if(i == 2){title=tr('Olds')}
      # Position of plot title in the origin corner : http://r.789695.n4.nabble.com/Positioning-text-in-top-left-corner-of-plot-td831723.html
      par(xpd=T)
      text(-0.15*(par("usr")[2]-par("usr")[1]),par("usr")[4]+0.14*(par("usr")[4]-par("usr")[3]),title,font=2,cex=1.5)
      par(xpd=F)
      incProgress(1/4, detail = paste("Plotting plot",i))
    }
  })
})

  output$Datas = renderDataTable({
    v<-getInputValues()
    cv<-getComputedValues()
    cv$datatoshow
  })


#UI
output$uiPanelTitle <- renderUI({
  tr("Title")
})


output$uiSBlanguage<- renderUI({
  strong(HTML(paste(tr("Language"),":",sep=" ")))
})

output$uiSBRacesTitle <- renderUI({
  h4(paste(tr("Selectarace"),":",sep=" "))
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

output$uiSBGeneralSettingsTitle <- renderUI({
  h4(paste(tr("GeneralSettings"),":",sep=" "))
})

output$uiSBAdditionalSettingsTitle <- renderUI({
  h4(paste(tr("AdditionalSettings"),":",sep=" "))
})

output$uiSBPigeonsSpeed <- renderUI({
  fluidRow(column(12,
      h5(HTML(paste("Sélection des pigeons",":",sep=" "))),
      selectInput("racecat", label=HTML(paste(tr("Category")," :",sep="")), choices=getCategorySelection(),selected="auto",selectize=FALSE),
      h5(HTML(paste(tr("PigeonsSpeed"),":",sep=" "))),
      selectInput("speedscale", label=HTML(paste(tr("Scale")," :",sep="")), choices=getSpeedScaleSelection(),selected="auto",selectize=FALSE),
      conditionalPanel(
        condition = "input.speedscale == 'man'",
        sliderInput("speed", label = "", min = 200, max = 2200, step=50, value = c(600, 1200))
      ),
      selectInput("speedloosegain", label=HTML(paste(tr("800mmIncluded")," :",sep="")), choices=getYesNoSelection(),selected="auto",selectize=FALSE),
      selectInput("speedneutral", label=HTML(paste(tr("CalculatedWithNeutral")," :",sep="")), choices=getYesNoSelection(),selected="auto",selectize=FALSE)          
  ))
})

output$uiSBsummary <- renderUI({
  mainPanel(
    # Faire des requêtes sur tous les concours de même id et donner stats générales dès que concours est sélectionné
    # Second bloc : stats générales de cette édition
    #HTML()
  )
})

output$uiSBlicence <- renderUI({
  HTML(paste("<a rel='license' href='http://creativecommons.org/licenses/by/2.0/be/'><img alt='Licence Creative Commons' style='border-width:0' src='img/cc_by_80x15.png' /></a>&nbsp;",tr("LicenceSeeCredits"),sep=""))
})

output$uiSBDistFact <- renderUI({
  strong(HTML(paste(tr("Highlightfactors"),":",sep=" ")))
})

output$uiSBFlightLinesHours <- renderUI({
  HTML(tr("ShowHourLines"))
})

output$uiSBLmLines <- renderUI({
  HTML(tr("ShowLm"))
})

output$uiSBRankingMethods <- renderUI({
  strong(HTML(paste(tr("RankingMethod"),":",sep=" ")))
})

output$uiMain <- renderUI({
  tabsetPanel(id="Tabset",selected=1,
              tabPanel(
                tr("Summary"),
                plotOutput("plotSummary",height=700),#,height = "auto" ,height=700
                value=1
              ),
              #conditionalPanel(condition = "input.editions!='empty'",
              tabPanel(
                tr("CatAndAge"),
                plotOutput("plotCatAndAge",height=700),#,height = "auto"
                value=2
              ),
              tabPanel(
                tr("Distance"),
                plotOutput("plotDistance",height=700),
                value=3
              #)
              ),
              tabPanel(
                tr("Rankings"),
                plotOutput("plotRankings",height=700),
                value=6
              ),
            
              tabPanel(
                tr("Datas"),
                dataTableOutput('Datas'),
                value=4
              ),
              tabPanel(
                tr("Help"),
                h4(paste(tr("TechnicalInformations"),":",sep=" ")),
                p(HTML(tr("SessionCounter")," :",SRV$count)),
                value=5
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
