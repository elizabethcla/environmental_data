install.packages("here")
require("here")

catrate = read.csv(here( "data", "catrate.csv"))
rope = read.csv(here( "data", "rope.csv"))
delomys = read.csv(here( "data", "delomys.csv"))
here("/Users/lizclark/environmental_data/")
head(rope)
head(delomys)
head(catrate)
plot(success ~ pond, data = catrate, main = "Liz Clarks catrate success vs pond plot")
