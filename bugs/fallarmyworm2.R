fallarmyworm2<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Fall armyworm 2'
  sciname<-'Spodoptera frugiperda'
  source <- 'Ali, A., R. G. Luttrell, and J. C. Schneider. 1990. Effects of Temperature and Larval Diet on Development of the Fall Armyworm (Lepidoptera: Noctuidae). Annals of the Entomological Society of America 83:725–733.'
  # development as a function of temperature, C
  schoolfield = function(tempK, RHO25, HA, HL=-1e9, HH=-1e9, TL=1, TH=1){ 
    R = 1.987 # cal/degree/mol (Boltzmanns constant) 
    RHO25 * tempK/298.15 * exp((HA/R)*(1/298.15 - 1/tempK)) /
      (1 + exp((HL/R)*(1/TL - 1/tempK)) + 
         exp((HH/R)*(1/TH - 1/tempK)))
  }
  egg =function(temp){
    tempK = temp + 273.15
    dev.rate = 0.0011*tempK*exp(21.7 - 6480.9/tempK)/
      (1 + exp(-132.45 + 38356/tempK) + exp(86.96 - 27119.3/tempK))
    return(dev.rate)
  }
  L1=function(temp){
    tempK = temp + 273.15
    dev.rate = 
      schoolfield(tempK=tempK, RHO25=0.3542, HA=11429)
    return(dev.rate)
  }
  L2=function(temp){
    tempK = temp + 273.15
    dev.rate = 
      schoolfield(tempK=tempK, RHO25=0.6774, HA=21377, TH=306.7, HH=44561)
    return(dev.rate)  
  }
  L3=function(temp){
    tempK = temp + 273.15
    dev.rate = 
      schoolfield(tempK=tempK, RHO25=0.5463, HA=15457, TH=310.9, HH=71604)
    return(dev.rate)
  }
  L4=function(temp){
    tempK = temp + 273.15
    dev.rate = 
      schoolfield(tempK=tempK, RHO25=0.8922, HA=28132, TH=300.4, HH=30324)
    return(dev.rate)
  }
  L5=function(temp){
    tempK = temp + 273.15
    dev.rate = 
      schoolfield(tempK=tempK, RHO25=0.4130, HA=10631)
    return(dev.rate)
  }
  L6=function(temp){
    tempK = temp + 273.15
    dev.rate = 
      schoolfield(tempK=tempK, RHO25=0.2038, HA=15601, TH=309.1, HH=83892)
    return(dev.rate)
  }
  pupa=function(temp){
    tempK = temp + 273.15
    dev.rate = 0.00051*tempK*exp(5.35 - 1593.69/tempK)/
      (1 + exp(-70.79 + 20894.94/tempK))
    return(dev.rate)
  }
  
  dev.funs<-list(egg=egg,L1=L1,L2=L2,L3=L3,L4=L4,L5=L5,L6=L6,
                 pupa=pupa)
  
  life<-c('egg','immature','immature','immature','immature','immature','immature','pupa') # possibly write script to search for adult and egg/pupa and assume all else immature.
  return(list(name=name,dev.funs=dev.funs,life=life, sciname=sciname, 
              source = source))
}