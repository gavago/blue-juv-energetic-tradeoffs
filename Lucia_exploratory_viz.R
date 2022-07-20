install.packages("tidyverse")
library(tidyverse)
install.packages("mgcv")
library(mgcv)

full_data <- read.csv("data/full_dataset_juv_immune_energetics.csv", header = T)


view(full_data)

full_data_no_hi_CP <- full_data %>% filter(stdsg_CP < 50000)

full_data_no_hi_CP_reorder <- factor(full_data$fai_cat, levels=c("low", "med", "hi")) 
view(full_data_no_hi_CP_reorder)

boxplot(stdsg_CP~fai_cat,
        data = full_data_no_hi_CP, 
        main="Fruit availability and Energy Balance",
        xlab="Fruit Availability",
        ylab="C-peptide",
        col="light green",
        border="dark green")

boxplot(neo_sg~fai_cat,
        data = full_data_no_hi_neo, 
        main="Fruit availability and Neopterin",
        xlab="Fruit Availability",
        ylab="Neopterin",
        col="light blue",
        border="dark blue")

boxplot(Cr~fai_cat,
        data = full_data,
        main="Fruit Availability and Lean Tissue",
        xlab="Fruit Availability",
        ylab="Creatinine",
        col="orange",
        border="black")

full_data %>% ggplot(aes(x = fai_cat)) + geom_bar()

full_data %>% filter(fai_cat == "low")  #346
full_data %>% filter(fai_cat == "med") #117
full_data %>% filter(fai_cat == "hi") #167
#half are in low -- med and hi similar in size

# w higher neo values taken out
full_data %>%
  ggplot(aes(x = neo_sg, y = stdsg_CP, color = sex)) + geom_smooth() + geom_point() + xlim(0, 3000) + ylim(0, 50000) 

# w linear regression model 
full_data %>%
  ggplot(aes(x = neo_sg, y = stdsg_CP, color = sex)) + geom_smooth(method = lm) + geom_point() + xlim(0, 3000) + ylim(0, 50000) 
#just to get better sense of general pattern w bulk of data

# all values included w linear regression 
full_data %>% ggplot (aes(x = neo_sg, y = stdsg_CP, color = sex)) + geom_smooth(method = lm) + geom_point()

lm_cp_neo <- lm(stdsg_CP ~ neo_sg, data = full_data)

summary(lm_cp_neo)

full_data_no_hi_neo <- full_data %>% filter(neo_sg < 3000)
  
lm_cp_neo_no_high_neo <- lm(stdsg_CP ~ neo_sg, data = full_data_no_high_neo)

summary(lm_cp_neo_no_high_neo)

# w 
#significant positive correlation. more energy balance, more energy able to allocate to immune function

neo_CP_cor <- cor.test(formula = ~ neo_sg + stdsg_CP,
                       data = full_data)
view(neo_CP_cor)
# statistic = 3.934012, p-value = 9.407339e-05, estimate = 0.1642774, 
# ci low = 0.08254503, ci hi = 0.24381473


full_data %>% 
  ggplot(aes(x = Cr, y = stdsg_CP, color = sex)) + geom_smooth() + xlim(0, 2) + ylim(0,40000)
#higher energy balance positively correlated w amt of lean tissue

Cr_CP_cor <- cor.test(formula = ~ stdsg_CP + Cr,
                       data = full_data)
view(Cr_CP_cor)
#significant positive correlation

cor.test 
full_data %>% 
  ggplot(aes(x = age, y =  Cr, color = sex)) + geom_smooth()
#just to check


neo_Cr_cor <- cor.test(formula = ~ neo_sg + Cr,
                              data = full_data)
view(neo_Cr_cor)

boxplot(neo_sg~n_agg_r,
        data = full_data_no_hi_neo, 
        main="Neopterin and Aggression",
        ylab="Neopterin",
        xlab= "Aggression Received",
        col="light green",
        border="dark green")


boxplot(neo_sg~n_agg_g,
        data = full_data_no_hi_neo, 
        main="Neopterin and Aggression",
        ylab="Neopterin",
        xlab= "Aggression Given",
        col="light green",
        border="dark green")

boxplot(Cr~n_agg_g,
        data = full_data_no_hi_neo, 
        main="Neopterin and Aggression",
        ylab="Neopterin",
        xlab= "Aggression Received",
        col="light green",
        border="dark green")

full_data %>% ggplot(aes(x = n_agg_r)) + geom_histogram()
full_data %>% ggplot(aes(x = n_agg_g)) + geom_histogram()

d <- data.frame(group = as.factor(c(1,2,3,4,5,6,7,8)), x=c(1,2,3,4,5,6,7,8))

full_data %>% ggplot(aes(x = n_agg_r, y = neo_sg)) + geom_boxplot()

ggplot(full_data, aes(x = n_agg_r, y = neo_sg)) + geom_boxplot()

full_data %>% 
  ggplot(aes(x = n_agg_r)) + geom_bar()

                     