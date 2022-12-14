install.packages("here")
require("here")

dat_bird = read.csv(here( "data", "bird.sta.csv"))
dat_habitat = read.csv(here( "data", "hab.sta.csv"))
pairs(dat_habitat[c("long", "elev", "slope", "lat")])
