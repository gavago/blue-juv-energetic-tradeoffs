library(tidyverse)

# data includes fgcs and overall is summarized by subj year month
load("data/full_data_month_udata_fgc_behav.RData", verbose = T)


# regressions to be run here will test relationships in the concept map

# H1a & b - cp neo - viz and regression -----
# - H1a is neo energetically constrained? --- neo ~ cp + age + sex ------

# - H1b does neo eat into available energy? --- cp ~ neo + age + sex ----
# ---- if so (if neo coef < 0) is this because increase neo corresponds with lower feeding?
# ---- f ~ neo + age + sex 

# - additional exploration cp neo -----
# LF says high neo outliers have lower cp, worth checking somehow
# NATG suggestion: bin neopterin using quantcut into 6 even bins,
# visualize with boxplot with y = cp and x = neo_bin,
# test with anova to see if cp differs by neo bin


# H2ab - neo cr_resid - viz and regression ----
# - H2a neo/immunity prioritized over growth --- cr_resid ~ neo + age + sex  ----
# - H2b neo/immunity constrained by body condition --- neo ~ cr_resid + age + sex ---


# H3 investment in affiliative behavior vs immunity