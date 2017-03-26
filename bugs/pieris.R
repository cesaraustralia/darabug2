pieris<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Cabbage white butterfly'
  sciname<-'Pieris rapae'
  source <- 'Jones, R.E. & P.M. Ives, 1979. The adaptiveness of searching and host selection behaviour in Pieris rapae (L.). Australian Journal of Ecology 4, 75-86.'
  
  # development as a function of temperature, C
  egg =function(temp){
    DDREQ  = 50.5            
    TUPPER = 30.0
    TLOWER = 10.2
    TMIN   = 11.2
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L1=function(temp){
    DDREQ  = 40.4                            
    TUPPER = 30.0
    TLOWER = 10.2
    TMIN   = 11.2
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L2=function(temp){
    DDREQ  = 23.8                            
    TUPPER = 30.0
    TLOWER = 11.3
    TMIN   = 12.3
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L3=function(temp){
    DDREQ  = 23.2                               
    TUPPER = 30.0
    TLOWER = 10.5
    TMIN   = 11.5
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L4=function(temp){
    DDREQ  = 28.7                               
    TUPPER = 30.0
    TLOWER = 10.6
    TMIN   = 11.6
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L5=function(temp){
    DDREQ  = 54.7                                 
    TUPPER = 30.0
    TLOWER =  8.4
    TMIN   =  9.4
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
 pupa=function(temp){
    DDREQ  = 135.2                                   
    TUPPER = 30.0
    TLOWER =  6.7
    TMIN   =  7.7
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  adult=function(temp){
    DDREQ  = 70.0                               
    TUPPER = 30.0
    TLOWER = 10.0
    TMIN   = 10.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  
  dev.funs<-list(egg=egg,L1=L1,L2=L2,L3=L3,L4=L4,L5=L5,
                 pupa=pupa,adult=adult)
  
  life<-c('egg','immature','immature','immature','immature','immature','pupa','adult') # possibly write script to search for adult and egg/pupa and assume all else immature.
  
  return(list(name=name,dev.funs=dev.funs,life=life, sciname=sciname, source = source))
}