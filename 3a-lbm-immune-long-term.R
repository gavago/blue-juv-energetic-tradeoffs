library(tidyverse)
library(lmerTest)
library(gtools)

source("functions/vif.mer function.R") # function is "vif.mer"

# data summarized by subj year month
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)
apply(full_data_month, 2, function(x) sum(is.na(x)))



# - A2b neo/immunity constrained by body condition --- neo ~ cr_resid + age + sex ----
# iffy q-q plot
# results - pos corr btw neo and cr: estimate = .273630 +- 1.96*0 .032827, p score = <2e-16

neo_lbm_glm_month <- glmer(avg_neo_sg ~ 
                            sex +
                            age +
                           log2(avg_cr_resid) + 
                           (1|subj), 
                         family = Gamma("log"), 
                         data = full_data_month)
qqnorm(residuals(neo_lbm_glm_month))
qqline(residuals(neo_lbm_glm_month))
summary(neo_lbm_glm_month)
vif.mer(neo_lbm_glm_month) # all < 1.000002

full_data_month %>% 
  filter(avg_neo_sg < 2000, avg_cr_resid < .75) %>% 
  ggplot(aes(y = avg_neo_sg, x = avg_cr_resid, color = sex)) +
  geom_smooth(method = "lm") + 
  geom_point() +
  theme_minimal() + 
  labs(y = "avg. Neopterin ng/ml",
       x =  "avg. Estimated Lean Body Mass") # title = "Body Condition Constraint on Neopterin"


# graveyard ----

hist(full_data_month$age)
hist(scale(full_data_month$age))
hist(log2(full_data_month$age))

