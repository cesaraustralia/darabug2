# start with stage, day of year, dev functions, and 365 x Tmax and Tmin
library(raster)
library(reshape2)
library(ggplot2)
library(plyr)
library(geosphere)

# to do: make plot of temp response?
setwd('C:/Users/james/Dropbox (cesar)/Sustainable Agriculture/Customer Files/Current (active) Projects/1207 Queensland DAF/1207CR2_National Pest Information Service/DARABUGv2/darabug2')
source('./develop.fun.R')
source('./getBug.R')

# required data
Tmin<-brick('E:/AWAP_daily_downloaded/mu_Tmin_for_DOY_ag10.tif')
Tmax<-brick('E:/AWAP_daily_downloaded/mu_Tmax_for_DOY_ag10.tif')
# Tmin<-aggregate(Tmin, fact = 2)
# Tmax<-aggregate(Tmax, fact = 2)
# Tmin<-Tmin+0.000001
# Tmax<-Tmax+0.000001
# Tmin<-extract(Tmin,200)
# Tmax<-extract(Tmax,200)
# TMAX <- extract(Tmax, matrix(c(long, lat), ncol = 2))
# TMIN <- extract(Tmin, matrix(c(long, lat), ncol = 2))
TMAX<- Tmax
TMIN<- Tmin
# compare with orginal darabug data
df<-read.table('C://Users/james/Dropbox (cesar)/Sustainable Agriculture/Customer Files/Current (active) Projects/1207 Queensland DAF/1207CR2_National Pest Information Service/DARABUGv1/darabug_all files for NPIS_Vic/swanhill.txt', skip = 1)
TMIN<-df$V4
TMAX<-df$V5


singleSite <- is.null(names(TMAX))



# args
startDay = 245
startStage =  rep(1,1)
long = 146.922046
lat =  -36.091804
clearOutput <- TRUE
bugs<-'budworm'
insect<-getBug(bugs)
targetStage <- length(names(insect$dev.funs))

myTime<-Sys.time()
data<-develop(TMAX,TMIN, startDay, startStage, getBug(bugs[1]))
Sys.time()-myTime

if(singleSite){
 df<- subset(data, variable =='Time_start')  
}

if(!singleSite){
  eventTime<-data[,targetStage,'Time_start']
  r<-TMAX[[1]]
  r[]<-eventTime
  plot(r)
}

if(singleSite){
  if(clearOutput){mdfAll<-NULL}
  for(bug in bugs){
    insect<-getBug(bug)
    data<-develop(TMAX,TMIN, startDay, startStage, insect)
    
    df<-as.data.frame(data[1,,])
    df$stage<-names(insect$dev.funs)
    df$life<-insect$life
    df$species<-factor(insect$name)
    df$order<-NA
    mdf <- melt(df, measure.vars = c("Time_start", "Time_end"))
    mdf$value <- as.Date('2015-1-1') + mdf$value
    mdfAll<-rbind(mdfAll,mdf)
    mdfAll$order[is.na(mdfAll$order)]<-length(unique(mdfAll$species))
  }
  myLevels<-unique(mdfAll[,c('order','species')])
  mdfAll$species <-factor(mdfAll$species,levels=myLevels$species[myLevels$order],ordered=TRUE)
  
  data=mdfAll

  p<-ggplot(data)+
    geom_line(aes(value, species, colour = life),size = 6) +
    geom_point(aes(value, species), colour = 'black', size = 6)+
    ylab(NULL) +
    xlab(NULL) +
    # geom_point(aes(x=as.Date(startDate),y=species[1]),
    #            shape = 124, size = 10,colour = 'red')+
    scale_x_date(limits = c(min(data$value), max(data$value)),
                 date_breaks = "2 weeks",date_minor_breaks = '2 weeks',
                 date_labels = "%d %b" )+
    theme_bw()+theme(text = element_text(size=20,family='Nirmala UI'),
                     axis.text.x = element_text(angle=45, vjust=1, hjust=1),
                     legend.title=element_blank(),
                     legend.key = element_blank())
  print(p)


}
