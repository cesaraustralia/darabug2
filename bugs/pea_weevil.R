pea_weevil<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Pea weevil'
  sciname<-'Pieris rapae'
  source <- 'Jones, R.E. & P.M. Ives, 1979. The adaptiveness of searching and host selection behaviour in Pieris rapae (L.). Australian Journal of Ecology 4, 75-86.'
  
  # development as a function of temperature, C
  egg =function(temp){
    A = .820253E+00 
    B = .261192E+02
    C = .423281E+00
    D = .425179E+02
    E = .345013E+01
    dev.rate.out <- A*((temp^2/(temp^2 + D^2))-exp(-((B-temp)/C))) 	# for Temp >  E
    dev.rate.out[temp<=E]<-0
    dev.rate.out[dev.rate.out<0]<-0
    return(dev.rate.out)
  }
  L1=function(temp){
    A = .140181E+01  
    B = .370427E+02 
    C = .209829E+01 
    D = .497327E+02 
    E = .723781E+01
    dev.rate.out <- A*((temp^2/(temp^2 + D^2))-exp(-((B-temp)/C))) 	# for Temp >  E
    dev.rate.out[temp<=E]<-0
    dev.rate.out[dev.rate.out<0]<-0
    return(dev.rate.out)
  }
  L2=function(temp){
    A = .383515E+00 
    B = .422945E+01
    C =  .170635E+00
    D = .000000E+00
    E = .000000E+00
    dev.rate.out<-temp
    dev.rate.out[temp<=D]<-A/(1 + exp(B + C*temp[temp<=D]))
    dev.rate.out[temp>D]<-A/(1 + exp(B + C*(2*D-temp[temp>D]))) 
    return(dev.rate.out)
  }
  L3=function(temp){
    A = .502088E+00  
    B = .319914E+02 
    C = .504497E+01 
    D = .247524E+02 
    E = .120495E+02
    dev.rate.out <- A*((temp^2/(temp^2 + D^2))-exp(-((B-temp)/C))) 	# for Temp >  E
    dev.rate.out[temp<=E]<-0
    dev.rate.out[dev.rate.out<0]<-0
    return(dev.rate.out)
  }
  L4=function(temp){
    A = .354262E+00
    B = .496535E+01
    C = .147134E+00
    D = 0
    E = 0
    dev.rate.out<-temp
    dev.rate.out[temp<=D]<-A/(1 + exp(B + C*temp[temp<=D]))
    dev.rate.out[temp>D]<-A/(1 + exp(B + C*(2*D-temp[temp>D]))) 
    return(dev.rate.out)
  }
  L5=function(temp){
    A = .820253E+00 
    B = .261192E+02
    C = .423281E+00
    D = .425179E+02
    E = .345013E+01
    dev.rate.out <- A*((temp^2/(temp^2 + D^2))-exp(-((B-temp)/C))) 	# for Temp >  E
    dev.rate.out[temp<=E]<-0
    dev.rate.out[dev.rate.out<0]<-0
    return(dev.rate.out)
  }
 pupa=function(temp){
   A = .526893E+00 
   B = .363370E+02 
   C = .173200E+01 
   D = .439175E+02 
   E = .532353E+01
   dev.rate.out <- A*((temp^2/(temp^2 + D^2))-exp(-((B-temp)/C))) 	# for Temp >  E
   dev.rate.out[temp<=E]<-0
   dev.rate.out[dev.rate.out<0]<-0
   return(dev.rate.out)
  }
  adult=function(temp){
    A = .3722559-01  
    B = .5182166-01 
    C = .2400409+02 
    D = .155651E+01 
    E = .160000E+02
    dev.rate.out <-  A*(exp(B*temp) - exp(B*C-(C-temp)/D))     	#	for Temp <  C
    dev.rate.out[temp>C]<-0
  }
  
  dev.funs<-list(egg=egg,L1=L1,L2=L2,L3=L3,L4=L4,L5=L5,
                 pupa=pupa,adult=adult)
  
  life<-c('egg','immature','immature','immature','immature','immature','pupa','adult') # possibly write script to search for adult and egg/pupa and assume all else immature.
  
  return(list(name=name,dev.funs=dev.funs,life=life, sciname=sciname, source = source))
}