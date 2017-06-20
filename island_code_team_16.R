# codes for the Island project
# Please note that this script requires the uploaded .csv file to work properly

# ---------------- script_setting --------------
# library declarations
library("stringr")
library("gdata")
library("reshape2")
library("plyr")
library("data.table")
library("ggplot2")
library("lubridate")
library("xtable")
library("car")
library("pwr")

# ggplot2 theme
theme_set(theme_bw())

# ---------------- treatment_order -------------
# set seed
set.seed(7777777)
treat_order <- rep(c(123, 231, 312), 14)
# randomised order
ran_order <- sample(treat_order, size = 42, replace = F)
# order for each one
chris <- ran_order[1:14]
zoe <- ran_order[15:28]
harry <- ran_order[29:42]
# id for mine
harry_id <- str_c("3", rep(c(0, 1), c(7, 7)), 3, 0, 1:7, harry)

# -------- power_analysis --------------
# power analysis for treatment
power_analysis <- pwr.t.test(d = 0.45, sig.level = 0.05, power = 0.8, 
                             type = 'paired', alternative = 'two.sided')

# -------- data_import -----------------
# Import data
island <- read.csv("island_team_16.csv", header = T, stringsAsFactors = F, 
                   colClasses = c("ID" = "factor", "AGE_GROUP" = "factor", 
                                  "TREATMENT" = "factor", "MALE" = "factor", 
                                  "DOB" = "Date"))
island$AGE_GROUP <- factor(island$AGE_GROUP, 
                           levels = c("YOUNG", "MEDIUM", "OLD"), ordered = T)
island$MALE <- factor(island$MALE)
island$TREATMENT <- factor(island$TREATMENT, 
                           levels = c("CONTROL", "CALM", "LOUD"), ordered = T)
island <- as.data.table(island)

# ------------ graphical_analysis ------------
# summary of response
response_summary <- with(island, summary(TIME))
# distribution of the response
response_hist <- ggplot(island, aes(x = TIME, y = ..density..)) + 
  geom_bar(binwidth = 5, colour = "black", fill = NA) + 
  labs(y = "DENSITY", title = "HISTOGRAM OF THE RESPONSE") + 
  geom_density(size = 1, adjust = 0.7)
response_hist

# main effects
# over gender
gender_box <- ggplot(island, aes(x = MALE, y = TIME)) + geom_boxplot() + 
  scale_x_discrete(limit = c("TRUE", "FALSE"), label = c("MALE", "FEMALE")) + 
  labs(x = "GENDER", title = "RESPONSE OVER GENDER")
gender_box

# over age
age_box <- ggplot(island, aes(x = AGE_GROUP, y = TIME)) + geom_boxplot() + 
  labs(x = "AGE GROUP", title = "RESPONSE OVER AGE GROUP")
age_box

# over treatment
treatment_box <- ggplot(island, aes(x = TREATMENT, y = TIME)) + geom_boxplot() + 
  labs(x = "TREATMENT", title = "RESPONSE OVER TREATMENT")
treatment_box

# interaction with gender
int_male_data <- island[, .(GROUP_MEAN = mean(TIME)), by = .(TREATMENT, MALE)]
int_male_plot <- ggplot(int_male_data, aes(x = TREATMENT, y = GROUP_MEAN)) + 
  geom_line(aes(colour = MALE, linetype = MALE, group = MALE)) + 
  labs(y = "TIME", colour = "GENDER", linetype = "GENDER", 
       title = "INTERACTIONS WITH GENDER") + 
  scale_linetype_discrete(limits = c("TRUE", "FALSE"), 
                          label = c("MALE", "FEMALE")) + 
  scale_colour_discrete(limits = c("TRUE", "FALSE"), 
                        label = c("MALE", "FEMALE"))
int_male_plot

# interaction with age
int_age_data <- island[, .(GROUP_MEAN = mean(TIME)), by = .(TREATMENT, AGE_GROUP)]
int_age_plot <- ggplot(int_age_data, aes(x = TREATMENT, y = GROUP_MEAN)) + 
  geom_line(aes(colour = AGE_GROUP, linetype = AGE_GROUP, group = AGE_GROUP)) + 
  labs(y = "TIME", colour = "AGE GROUP", linetype = "AGE GROUP", 
       title = "INTERACTIONS WITH AGE GROUP")
int_age_plot

# ------------- anova -----------
# run anova
island_aov <- aov(TIME ~ MALE * TREATMENT + AGE_GROUP * TREATMENT + 
                    MALE : AGE_GROUP + Error(ID / TREATMENT), island)
# take a look
summary(island_aov)
# see the effects
effects_table <- model.tables(island_aov)
effects_table

# sphericity test
# create a format it wide
island_wide <- dcast(island, ... ~ TREATMENT, value.var = "TIME")
dvm <- with(island_wide, cbind(CONTROL, CALM, LOUD))
idata <- with(island, data.frame(TREATMENT = factor(levels(TREATMENT))))
mlm <- lm(dvm ~ 1)
anova_car <- Anova(mlm, idata = idata, idesign = ~ TREATMENT, type = 3)
summary(anova_car)

# confidence intervals for main effects
# generate ci for treatment
setkey(island, TREATMENT)
con_cal <- t.test(island["CONTROL", TIME], island["CALM", TIME], paired = T, 
                  var.equal = T, conf.level = 0.99)
con_loud <- t.test(island["CONTROL", TIME], island["LOUD", TIME], paired = T, 
                   var.equal = T, conf.level = 0.99)
cal_loud <- t.test(island["CALM", TIME], island["LOUD", TIME], paired = T, 
                   var.equal = T, conf.level = 0.99)

# create aggregate data for each subject
id_aggregate <- island[, .(AGE_GROUP = AGE_GROUP[1], MALE = MALE[1], 
                           ID_MEAN = mean(TIME)), by = ID]

# ci for gender
setkey(id_aggregate, MALE)
male_female <- t.test(id_aggregate[MALE == T, ID_MEAN], 
                      id_aggregate[MALE == F, ID_MEAN], 
                      var.equal = F, paired = F, conf.level = 0.99)

# ci for age
setkey(id_aggregate, AGE_GROUP)
young_medium <- t.test(id_aggregate["YOUNG", ID_MEAN], 
                       id_aggregate["MEDIUM", ID_MEAN], 
                       paired = F, var.equal = F, conf.level = 0.99)
young_old <- t.test(id_aggregate["YOUNG", ID_MEAN], 
                    id_aggregate["OLD", ID_MEAN], 
                    paired = F, var.equal = F, conf.level = 0.99)
medium_old <- t.test(id_aggregate["MEDIUM", ID_MEAN], 
                     id_aggregate["OLD", ID_MEAN], 
                     paired = F, var.equal = F, conf.level = 0.99)