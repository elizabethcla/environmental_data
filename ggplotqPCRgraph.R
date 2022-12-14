install.packages("here")
require(here)
require("ggplot2")
library("ggplot2")
dat_eDNA = read.csv(here("data", "QuantificationCqResults.csv"))
require("dplyr")
eDNA_summ <- dat_eDNA %>% group_by(column) %>% summarize(mean = mean(Cq),
            sd = sd(Cq))
p= dat_eDNA %>% ggplot(aes(y = Starting.Quantity, x = Cq, group = column, color= Individual)) + geom_point() +scale_x_reverse() 
p
p+ coord_cartesian(ylim = c(.0003, 26))
p+ coord_cartesian(xlim = c(40,23),(ylim = c(.0003, 20)))
p+ coord_cartesian(ylim = c(.0003, 26))
p+ coord_cartesian(xlim = c(40,23),(ylim = c(.0003, 3)))
p+ coord_cartesian(xlim = c(40,30),(ylim = c(.0003, .03)))

p+ coord_cartesian(ylim = c(.0003, .05))
p+ coord_cartesian(ylim = c(.0003, .01))



q= p + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1))  

p+stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), 
                                                                                 geom="errorbar", color="red", width=0.2) +
  stat_summary(fun.x=sd, geom="errorbar", color= "red")


q+ coord_cartesian(ylim = c(.0003, 26))
q+ coord_cartesian(ylim = c(.0003, .01))









p= dat_eDNA %>% ggplot(aes(y = Starting.Quantity, x = Cq, group = column, color= Individual)) + geom_point() +scale_x_reverse(3,.0003)

ggplot(dat_eDNA, aes(y = Starting.Quantity, x = Cq, colour=supp, group=supp)) + geom_errorbar(aes(colour="black", width=.1) +geom_line() +geom_point())
  
  stat_summary(fun.y=mean, geom="point", color= "blue", width= .001)