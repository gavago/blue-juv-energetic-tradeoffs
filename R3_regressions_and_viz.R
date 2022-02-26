full_data <- read.csv("full dataset juv immune energetics.csv", header = T)

full_data$time <- unclass(as.POSIXct(full_data$time))


# Exploration H1 - neo~cp & H2 cr-sg~cp, cr-sg~neo -------

# H1 - neo cp - regression and viz ------
#viz
full_data %>%
  filter(neo_sg < 3000) %>%
  ggplot(., aes(y = neo_sg, x = stdsg_CP, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") #highest cp values are low neo

# non linear relationship...
for_gam <- fulld_data %>% 
  select(neo_sg, stdsg_CP, age, sex, subj) %>%
  mutate(subj = as.factor(subj)) %>%
  na.omit()
neo_cp_gam <- gam(neo_sg ~ sex + s(stdsg_CP, k = 5) + s(age, k = 5) + s(subj, bs = "re"), family = gaussian("log"), data = for_gam)
gam.check(neo_cp_gam)
plot.gam(neo_cp_gam, pages = 1)
summary(neo_cp_gam)


# expected linear relationship
neo_cp_lm <- glmer(neo_sg ~ sex + scale(stdsg_CP) + scale(age) + (1|subj), family = Gamma("log"), data = fulld_data)
qqnorm(residuals(neo_cp_lm))
summary(neo_cp_lm)


# H2 - cr_sg neo - regression and viz  ------

# now neo and cr_Sg
#viz
full_data %>%
  filter(neo_sg < 3000) %>%
  ggplot(., aes(y = cr_resid, x = neo_sg, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm")

crsg_neo_lm <- lmer(cr_resid ~ sex + scale(neo_sg) + scale(age) + (1|subj), data = full_data)
qqnorm(residuals(crsg_neo_lm))
summary(crsg_neo_lm)



# time of day effects ------
str(full_data)

lmer(neo_sg ~ time + (1|subj), data = full_data) %>% summary()
lmer(stdsg_CP ~ time + (1|subj), data = full_data) %>% summary()
plot(full_data$time, full_data$neo_sg) # no time of day effects...
plot(full_data$time, full_data$stdsg_CP) 


