common_armyworm_nonlinear<-function(){
  # returns a structure containing species name, developmental function for each stage, and life-history category for each stage (egg, immature, pupa, adult)
  name <- 'Common armyworm (non-linear)'
  sciname<-'Mythimna convecta'
  source <- ' Mcdonald G (1990) Simulation-Models for the Phenological Development of Mythimna-Convecta (Walker) (Lepidoptera, Noctuidae) . Australian Journal of Zoology 38, 649-663. https://doi.org/10.1071/ZO9900649'
  
  fun1 = function(temp, A, B, C, D, E){
    X = temp - E
    r = A*(X^2/(X^2 + D^2) - exp(-(B-X)/C))
    r[temp<E] = 0
    r[r<0] = 0
    return(r)
  }
  fun2 = function(temp, A, B, C, D, E){
    X = temp - E
    r = A*(exp(B*X) - exp(B*C-(C-X)/D) )
    r[temp>C] = 0
    return(r)
  }
  fun3 = function(temp, A, B, C, D){
    r = A/(1 + exp(B+C*temp))
    r[temp>D] = A/(1 + exp(B + C*(2*D-temp[temp>D])) )
    return(r)
  }
  
  # development as a function of temperature, C
  egg =function(temp){
    fun1(temp, 1.64334,	38.9221,	1.3655,	67.9816,	-0.281956)
  }
  L1=function(temp){
    fun1(temp, 2.66464,	35.9973,	0.31312,	63.7412,	-0.578628)
  }
  L2=function(temp){
    fun1(temp, 2.85488,	34.4016,	0.72273,	55.3808,	1.86146)
  }
  L3=function(temp){
    # error in parameters (units in percent) so scale A by 100
    fun2(temp, 5.21059/100,	0.12376,	35.3535,	5.344,	5.5)
  }
  L4=function(temp){
    fun1(temp, 6.88455,	40.6814,	1.17638,	108.642,	1.80656)
  }
  L5=function(temp){
    fun3(temp, 0.484623,	3.59709,	-0.107351,	33)
  }
  L6=function(temp){
    fun1(temp, 7.21212,	43.6124,	1.99229,	175.095,	-0.850374)
  }
  pupa=function(temp){
    fun1(temp, 0.338976,	39.5858,	1.44951,	47.87,	-0.124911)
  }
  
  dev.funs<-list(egg=egg,L1=L1,L2=L2,L3=L3,L4=L4,L5=L5,L6=L6,
                 pupa=pupa)
  
  life<-c('egg','immature','immature','immature','immature','immature','immature','pupa') # possibly write script to search for adult and egg/pupa and assume all else immature.
  return(list(name=name,dev.funs=dev.funs,life=life, sciname=sciname, 
              source = source))
}