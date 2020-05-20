fallarmyworm<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Fall armyworm'
  sciname<-'Spodoptera frugiperda'
  source <- 'Du Plessis, H., M.-L. Schlemmer, and J. Van den Berg. 2020. The Effect of Temperature on the Development of Spodoptera frugiperda (Lepidoptera: Noctuidae). 
  Upper threshold from Valez-Torrez 2012'
  # development as a function of temperature, C
  egg =function(temp){
    DDREQ  = 35.73               
    TUPPER = 39.8
    TLOWER = 13.01
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }
  L1=function(temp){
    DDREQ  = 47.14               
    TUPPER = 39.8
    TLOWER =  8.49
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }
  L2=function(temp){
    DDREQ  = 31.98              
    TUPPER = 39.8
    TLOWER = 10.60
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }
  L3=function(temp){
    DDREQ  = 21.58               
    TUPPER = 39.8
    TLOWER =  13.47
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }
  L4=function(temp){
    DDREQ  = 24.78               
    TUPPER = 39.8
    TLOWER = 13.11
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }
  L5=function(temp){
    DDREQ  = 36.03               
    TUPPER = 39.8
    TLOWER = 11.21
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }
  L6=function(temp){
    DDREQ  = 32.57               
    TUPPER = 39.8
    TLOWER = 14.85
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }
  pupa=function(temp){
    DDREQ  = 150.29               
    TUPPER = 39.8
    TLOWER = 13.06
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }

  dev.funs<-list(egg=egg,L1=L1,L2=L2,L3=L3,L4=L4,L5=L5,L6=L6,
                 pupa=pupa)
  
  life<-c('egg','immature','immature','immature','immature','immature','immature','pupa') # possibly write script to search for adult and egg/pupa and assume all else immature.
  return(list(name=name,dev.funs=dev.funs,life=life, sciname=sciname, 
              source = source))
}