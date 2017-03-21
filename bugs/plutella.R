plutella<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Diamondback moth'
  sciname<-'Plutella xylostella'
  source <- 'Plutella data source'
  # development as a function of temperature, C
  egg =function(temp){
    DDREQ  = 72.5
    TUPPER = 30.0
    TLOWER =  6.2
    TMIN   =  7.2
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L1=function(temp){
    DDREQ  = 45.9                             
    TUPPER = 30.0
    TLOWER =  5.9
    TMIN   =  6.9
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L2=function(temp){
    DDREQ  = 18.6                          
    TUPPER = 30.0
    TLOWER = 13.0
    TMIN   = 14.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L3=function(temp){
    DDREQ  = 21.7            
    TUPPER = 30.0
    TLOWER = 11.5
    TMIN   = 12.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L4=function(temp){
    DDREQ  = 63.7                                 
    TUPPER = 30.0
    TLOWER =  5.4
    TMIN   =  6.4
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  pupa=function(temp){
    DDREQ  = 75.2                                  
    TUPPER = 30.0
    TLOWER =  8.5
    TMIN   =  9.5
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
 
  dev.funs<-list(egg=egg,L1=L1,L2=L2,L3=L3,L4=L4,
                 pupa=pupa)
  
  life<-c('egg','immature','immature','immature','immature','pupa') # possibly write script to search for adult and egg/pupa and assume all else immature.
  
  return(list(name=name,dev.funs=dev.funs,life=life, source = source))
}