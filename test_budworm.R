library(tidyverse) 
source("bugs/budworm.R")

d = read_csv("data/Daglish1991.csv")
bud = budworm()

# L1 duration
dur = 1./unlist(lapply(d$Temperature.C, bud$dev.funs$L1))
# add duration of L2 to pupa
for (i in 3:9){
  dur = dur +  1./unlist(lapply(d$Temperature.C, bud$dev.funs[[i]]))
}

d$pred = dur

ggplot(d, aes(Temperature.C, Duration.d)) + 
  geom_point() +
  geom_line(aes(y = pred)) + 
  ylim(0, 60) + 
  theme_bw()
