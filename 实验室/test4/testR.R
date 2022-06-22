library('lubridate')
library('plyr')
library('dplyr')


mt <- sample(1:5, 20, ncol = 2) %>% matric(nc = 4) %>% data.frame
print(mt)



