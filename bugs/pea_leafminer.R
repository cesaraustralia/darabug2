pea_leafminer<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Pea leafminer'
  sciname<-'Liriomyza huidobrensis'
  source <- 'Mujica et al 2016 https://cipotato.org/riskatlasforafrica/7-3-9serpentine-leafminer-fly-liriomyza-huidobrensis-mujica-et-al/'
  
  # development rate, 1/d, as a function of temperature, C
  egg =function(temp){
    # Model 13: SharpeDeMichelle 13
    p  =  51.183 
    Tl =  306.164 
    Ha = -79899.471 
    Hl = -101873.013
    x=temp+273 # convert to Kelvin
   dev.rate.out = 
     (p*(x/298.16)*exp((Ha/1.987)*((1/298.16)-(1/x))))/
     (1 + exp((Hl/1.987)*((1/Tl)-(1/x))))
    return(dev.rate.out)
  }
  
  larvae=function(temp){
    # Model 23: Lactin 1
    Tl =  39.514 
    p  =  0.01 
    dt =  2.891 
    L  = -1.069
    x  = temp
    dev.rate.out = exp(p*x) - exp(-(p*Tl-(x-Tl))/dt) + L 
    dev.rate.out[dev.rate.out<0] = 0
    return(dev.rate.out)
  }

 pupa=function(temp){
   # Model 46: Janish 1
   Dmin = 6.841 
   Topt = 29.037 
   K    = 0.127
   x = temp
   dev.rate.out = 2/(Dmin*(exp(K*(x-Topt))+exp(-K*(x-Topt)))) 
   return(dev.rate.out)
 }
  
  dev.funs<-list(egg=egg,larvae=larvae, pupa=pupa)
  life<-c('egg','immature','pupa') # possibly write script to search for adult and egg/pupa and assume all else immature.
  
  return(list(name=name,dev.funs=dev.funs,life=life, sciname=sciname, source = source))
}