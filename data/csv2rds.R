setwd("/media/datas/perso/pigeons/ShinyApps/beprstats/data/")

files <- list.files(path = "results",pattern = '*.csv')
for(file in files){
  if(file!="empty.csv"){
    data<-read.csv(paste("./results",file,sep="/"), sep=",", dec=".")
  #   rdafilename<-paste(".","rda",paste(substr(basename(file), 1, nchar(basename(file)) - 4),"rda",sep="."),sep="/")
    rdsfilename<-paste(".","rds",paste(substr(basename(file), 1, nchar(basename(file)) - 4),"rds",sep="."),sep="/")
  #   save(data,file=rdafilename)
    saveRDS(data,file=rdsfilename)
  }
}