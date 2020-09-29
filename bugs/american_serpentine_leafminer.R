american_serpentine_leafminer<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'American serpentine leafminer'
  sciname<-'Liriomyza trifolii'
  source <- 'Mujica et al 2016 https://cipotato.org/riskatlasforafrica/liriomyza-trifolii-2/'
  
  # development rate, 1/d, as a function of temperature, C
  egg =function(temp){
    # Model 26: Tb Model
    sy  = 0.168 
    b   = 0.214 
    Tb  = 8.047 
    DTb = 4.331
    x=temp
   dev.rate.out = sy*exp((b*(x-Tb)-exp(b*(x-Tb)/DTb)))
    return(dev.rate.out)
  }
  
  larvae=function(temp){
    # Model 13: SharpeDeMichelle 13
    p  =  28.839 
    Tl =  308.063 
    Ha = -62054.236 
    Hl = -79759.509
    x=temp+273 # convert to Kelvin
    dev.rate.out = 
      (p*(x/298.16)*exp((Ha/1.987)*((1/298.16)-(1/x))))/
      (1 + exp((Hl/1.987)*((1/Tl)-(1/x))))
  }

 pupa=function(temp){
   # Model 26: Tb Model
   sy  = 0.057 
   b   = 0.211 
   Tb  = 10.511 
   DTb = 3.635
   x=temp
   dev.rate.out = sy*exp((b*(x-Tb)-exp(b*(x-Tb)/DTb)))
   return(dev.rate.out)
 }
  
  dev.funs<-list(egg=egg,larvae=larvae, pupa=pupa)
  life<-c('egg','immature','pupa') # possibly write script to search for adult and egg/pupa and assume all else immature.
  
  return(list(name=name,dev.funs=dev.funs,life=life, sciname=sciname, source = source))
}