bogong_moth<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Bogong moth'
  sciname<-'Agrotis infusa'
  source <- 'R. R. Rawat (1958) - The biology and morphology of Agrotis infusa (Boisd), PhD Thesis University of Adelaide.'
  # development as a function of temperature, C
  egg =function(temp){
    DDREQ  = 60.7               
    TUPPER = 32.0
    TLOWER =  9.4
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }
  larva=function(temp){
    DDREQ  = 357.1               
    TUPPER = 32.0
    TLOWER =  9.2
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }
  prepupa=function(temp){
    DDREQ  = 36.9               
    TUPPER = 30
    TLOWER =  10.4
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }
  pupa=function(temp){
    DDREQ  = 230.7               
    TUPPER = 32
    TLOWER =  9.7
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }
  adult=function(temp){
    DDREQ  = 203.4               
    TUPPER = 30
    TLOWER =  3.7
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    return(dev.rate.out/DDREQ)
  }
  
  
  dev.funs<-list(egg=egg,larva=larva,prepupa=prepupa,pupa=pupa,adult=adult)
  
  life<-c('egg','immature','immature','pupa','adult') # possibly write script to search for adult and egg/pupa and assume all else immature.
  return(list(name=name,dev.funs=dev.funs,life=life, sciname=sciname, 
              source = source))
}