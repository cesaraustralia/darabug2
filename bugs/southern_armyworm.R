southern_armyworm<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Southern armyworm'
  sciname<-'Persectania ewingii'
  source <- 'Helm, K.F., 1975. Migration of armyworm Persectania ewingii moths in spring and the origin of outbreaks. Journal of Australian Entomological Society 14, 229-26.'
  
  # development as a function of temperature, C
  egg =function(temp){
    DDREQ  = 136.0                   
    TUPPER = 26.2     
    TLOWER =  6.4     
    TMIN   =  8.0    
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L1=function(temp){
    DDREQ  = 110.0                   
    TUPPER = 27.2         
    TLOWER = 4.0               
    TMIN   = 6.0    
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L2=function(temp){
    DDREQ  = 110.0                   
    TUPPER = 27.2         
    TLOWER = 4.0               
    TMIN   = 6.0  
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L3=function(temp){
    DDREQ  = 110.0                   
    TUPPER = 27.2         
    TLOWER = 4.0               
    TMIN   = 6.0  
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L4=function(temp){
    DDREQ  = 110.0                   
    TUPPER = 27.2         
    TLOWER = 4.0               
    TMIN   = 6.0  
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L5=function(temp){
    DDREQ  = 110.0                   
    TUPPER = 27.2         
    TLOWER = 4.0               
    TMIN   = 6.0  
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  L6=function(temp){
    DDREQ  = 110.0                   
    TUPPER = 27.2         
    TLOWER = 4.0               
    TMIN   = 6.0  
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  pupa=function(temp){
    DDREQ  =264.0                       
    TUPPER = 30.6     
    TLOWER =  9.3     
    TMIN   =  9.3    
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  adult=function(temp){
    DDREQ  = 50.0                       
    TUPPER = 31.0     
    TLOWER =  6.4     
    TMIN   =  6.4    
    dev.rate.out<-temp-TLOWER
    dev.rate.out[temp>TUPPER|temp<=TLOWER]<-0
    dev.rate.out[temp>TLOWER&temp<TMIN]<-TMIN - TLOWER
    return(dev.rate.out/DDREQ)
  }
  
  
  dev.funs<-list(egg=egg,L1=L1,L2=L2,L3=L3,L4=L4,L5=L5,L6=L6,
                 pupa=pupa,adult=adult)
  
  life<-c('egg','immature','immature','immature','immature','immature','immature','pupa','adult') # possibly write script to search for adult and egg/pupa and assume all else immature.
  return(list(name=name,dev.funs=dev.funs,life=life, sciname=sciname, 
              source = source))
}