leafminer<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Vegetable leafminer'
  sciname<-'Liriomyza sativae'
  source <- 'https://cipotato.org/riskatlasforafrica/7-3-10-liriomyza-sativae-mujica-et-al/'
  
  # development rate, 1/d, as a function of temperature, C
  egg =function(temp){
    p  = 0.411
    To = 295.9
    Tl = 179.804
    Th = 313.431
    Ha = 16312.606
    Hl =-1180.651
    Hh = 526984.333
    x=temp+273 # convert to Kelvin
   dev.rate.out = p*(x/(To))*exp((Ha/1.987)*((1/To)-(1/x)))/(1+exp((Hl/1.987)*((1/Tl)-(1/x)))+exp((Hh/1.987)*((1/Th)-(1/x))))
    return(dev.rate.out)
  }
  
  L1=function(temp){
    Dmin = 2.743
    Topt = 33.519
    K    = 0.118
    x = temp
    dev.rate.out = 2/(Dmin*(exp(K*(x-Topt))+exp(-K*(x-Topt)))) 
    return(dev.rate.out)
  }

 pupa=function(temp){
   p  = 0.049
   To = 288.52
   Tl = 284.349
   Th = 308.51
   Ha = 16661.255
   Hl =-192502.313
   Hh = 25785.133
   x=temp+273 # convert to Kelvin
   dev.rate.out = p*(x/(To))*exp((Ha/1.987)*((1/To)-(1/x)))/(1+exp((Hl/1.987)*((1/Tl)-(1/x)))+exp((Hh/1.987)*((1/Th)-(1/x))))
   return(dev.rate.out)
 }
  
  dev.funs<-list(egg=egg,L1=L1, pupa=pupa)
  life<-c('egg','immature','pupa') # possibly write script to search for adult and egg/pupa and assume all else immature.
  
  return(list(name=name,dev.funs=dev.funs,life=life, sciname=sciname, source = source))
}