whitefly<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Silverleaf whitefly'
  sciname<-'Bemisia tabaci'
  source <- HTML('Qiu, B. et al., 2003. Effect of temperature on the development and reproduction of Bemisia tabaci B biotype (Homoptera: Aleyrodidae) Entomologia sinica 10:1 43-29 <br> 
  Enkegaard, A. 1993. The poinsettia strain of the cotton whitefly, Bemisia tabaci (Homoptera: Aleyrodidae), biological and demographic parameters on poinsettia (Euphorbia pulcherrima) in relation to temperature. Bulletin of Entomological Research 83, 535-546.')
  # development as a function of temperature, C
  egg =function(temp){
    DDREQ  = 89.96               
    TUPPER = 38
    TLOWER =  11.0
    TMIN   =  12.45
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L1=function(temp){
    DDREQ  = 27.83               
    TUPPER = 38.0
    TLOWER = 11.0
    TMIN   = 12.48
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L2=function(temp){
    DDREQ  = 48.01               
    TUPPER = 38.0
    TLOWER = 9
    TMIN   = 10.53
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L3=function(temp){
    DDREQ  = 33.59                  
    TUPPER = 38.0
    TLOWER = 11.0
    TMIN   = 12.42
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L4=function(temp){
    DDREQ  = 64.18                   
    TUPPER = 38.0
    TLOWER = 10.0
    TMIN   = 11.08
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  adult=function(temp){
    DDREQ  = 369.7                  
    TUPPER = 38.0
    TLOWER = 8.0
    TMIN   = 14.0
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  
  dev.funs<-list(egg=egg,L1=L1,L2=L2,L3=L3,L4=L4,adult=adult)
  life<-c('egg','immature','immature','immature','immature','adult') # possibly write script to search for adult and egg/pupa and assume all else immature.
  return(list(name=name,dev.funs=dev.funs,life=life, sciname=sciname, 
              source = source))
}
