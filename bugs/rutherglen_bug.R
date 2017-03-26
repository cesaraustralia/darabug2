rutherglen_bug<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Rutherglen bug'
  sciname<-'Nysius vinitor'
  source <- 'McDonald, G & A.M. Smith, 1988. Phenological development and seasonal distribution of the Rutherglen bug, Nysius vinitor Bergroth (Hemiptera; Lygaeidae), on various hosts in Victoria, south-eastern Australia.  Bulletin of Entomological Research 78, 673-682.'
  
  # development as a function of temperature, C
  egg =function(temp){
    DDREQ  = 68.1    
    TUPPER = 35.0    
    TLOWER =  14.5    
    TMIN   =  15.0    
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L1=function(temp){
    DDREQ  = 39.0                                 
    TUPPER = 35.0    
    TLOWER =  16.1    
    TMIN   =  16.6    
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L2=function(temp){
    DDREQ  = 39.0                              
    TUPPER = 35.0    
    TLOWER = 16.1    
    TMIN   = 16.6    
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L3=function(temp){
    DDREQ  = 39.0                
    TUPPER = 35.0    
    TLOWER = 16.1    
    TMIN   = 16.6    
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L4=function(temp){
    DDREQ  = 39.0                                     
    TUPPER = 35.0    
    TLOWER = 16.1    
    TMIN   = 16.6    
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L5=function(temp){
    DDREQ  = 39.0                
    TUPPER = 35.0    
    TLOWER = 16.1    
    TMIN   = 16.6  
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  
  adult=function(temp){
    DDREQ  =  40.0                                      
    TUPPER =  35.0    
    TLOWER =  17.2    
    TMIN   =  17.7    
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
 
  dev.funs<-list(egg=egg,L1=L1,L2=L2,L3=L3,L4=L4,
                 L5=L5, adult = adult)
  
  life<-c('egg','immature','immature','immature','immature','immature', 'adult') # possibly write script to search for adult and egg/pupa and assume all else immature.
  
  return(list(name=name,dev.funs=dev.funs,life=life, source = source))
}