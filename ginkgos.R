require(here)
ginkgo_df = read.csv(here("data", "ginkgo_data_2022.csv"))
(nrow(ginkgo_df))/10
trees=(data.frame(ginkgo$site_id, ginkgo_df$seeds_present)) 
nrow(subset(unique(trees, select=site_id)))

sum(ginkgo_df$seeds_present=="TRUE")/10
boxplot(
  petiole_length ~ seeds_present,
  data = ginkgo_df)

require(ggplot2)
dat = read.csv(here("data", "ginkgo_data_2022.csv"))
names(dat)
