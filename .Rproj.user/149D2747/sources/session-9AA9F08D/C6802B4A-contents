require(here)
rope = read.csv(here("data", "rope.csv"))
rm(list = ls())
head(rope)

rope = rope = read.csv(here("data", "rope.csv"))
rope$rope.type=factor(rope$rope.type)
poop=levels(rope$rope.type)
                        
                        n_obs = 121
                        n_groups = 6
                        
                  
ss_tot = sum((rope$p.cut-mean(rope$p.cut))^2)
df_tot = n_obs-1
 
blaze=
                    
agg_resids=aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) x-mean(x))
str(agg_resids)

agg_sum_sq_resids=aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) sum((x-mean(x))^2))
str(agg_sum_sq_resids)


ss_within=sum(agg_sum_sq_resids$x)
ss_within

ss_among = ss_tot - ss_within
ss_among
df_among= n_groups-1

df_tot=  n_obs - 1                     

df_within= n_obs-n_groups
                        
ms_among  =  ss_among / (df_among)
ms_within = ss_within / (n_obs - n_groups)

f_ratio=ms_among/ms_within

f_pval= 1-pf(q=f_ratio, ms_within, df_within)

fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
 
summary(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)

anova_fit_1$"Sum Sq"
0.36714-0.10164
rope_blaze = droplevels(
  subset(
    rope,
    rope.type %in% c("BLAZE"))
)

rope2 = droplevels(
  subset(
    rope,
    rope.type %in% c("PI", "VEL", "XTC"))
)

shapiro.test(rope2$rope.type[rope2$rope.type==""])
shapiro.test(veg2$pine[veg2$treatment=="clipped"])
boxplot(
  p.cut ~ rope.type,
  data = rope2,
  las = 2,
  xlab = "",
  ylab = "Proportion Rope Cut",
  main = "Subset of Rope Data")
mtext("Rope Type", side = 1, line = 3)

fit_rope_2 = lm(p.cut ~ rope.type, data=rope2)

rope2_hsd = TukeyHSD(aov(fit_rope_2))
class(rope2_hsd)
lapply(agg_resids$x,
       function(x) shapiro.test(x)$p)
fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)
shapiro.test(residuals(fit_rope_1))
shapiro.test(residfit)
round(rope2_hsd$rope.type, digits = 4)

bartlett.test(p.cut~rope.type, data=rope)

require(palmerpenguins)
pen_fem = subset(penguins, sex == "female")
boxplot(formula= body_mass_g ~species,
        data= pen_fem,
        main="female penguin body mass")
bartlett.test(formula= body_mass_g ~species,
              data= pen_fem)
fempenfit= lm(formula= body_mass_g ~species,
                                      data= pen_fem)
shapiro.test(residuals(fempenfit))
fem_hsd = TukeyHSD(aov(fempenfit))
round(fem_hsd$species, digits = 4)
