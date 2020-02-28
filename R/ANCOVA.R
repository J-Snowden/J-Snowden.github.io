library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(datarium)
library(haven)
library(foreign)
library(ggplot2)
library(emmeans)

setwd("c:/Users/jsnowden/Desktop/School")

urbanhs <- read_sav("UrbanHS.sav")

urbanhs$NGA <- as.factor(urbanhs$NGA)

urbanhs <- urbanhs %>%
  select(ID, NGA, Grade9GPA, MeanReadMath, RatioCreditsAttemptedEarned, MinorityWhite, FRLYesNo) %>%
  rename(MeanRM = MeanReadMath, RatioCredits = RatioCreditsAttemptedEarned)

#Create scatter plot for covariate MeanRM and GPA
ggscatter(
  urbanhs, x = "MeanRM", y = "Grade9GPA",
  color = "NGA", add = "reg.line"
  )+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = NGA)
  )+
  xlab("Mean Reading and Math Score")+
  ylab("9th Grade GPA")+
  ggtitle("Relationship Between GPA and TCAP Scores")
 


  

#Create scatter plot for covariate RatioCredits and GPA
ggscatter(
  urbanhs, x = "RatioCredits", y = "Grade9GPA",
  color = "NGA", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = NGA)
  )+
  xlab("Ratio of Credits Earned to Credits Attempted")+
  ylab("9th Grade GPA")+
  ggtitle("Relationship Between GPA and Credit Ratio")

#Run the ANCOVA controlling for MeanRM and RatioCredits
model <- lm(Grade9GPA ~ MeanRM + RatioCredits + NGA, data = urbanhs)


res.aov <- urbanhs %>% anova_test(Grade9GPA ~ RatioCredits + MeanRM + NGA)
get_anova_table(res.aov)

#Get the Estimated Marginal Means
pwc <- urbanhs %>%
  emmeans_test (
    Grade9GPA ~ NGA, covariate = c("RatioCredits", "MeanRM"),
    p.adjust.method = "bonferroni"
  )
pwc

#Print the EMM's
get_emmeans(pwc)

# Visualization: line plots with p-values
pwc <- pwc %>% add_xy_position(x = "NGA", fun = "mean_se")
ggline(get_emmeans(pwc), x = "NGA", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )+
  #xlab("9th Grade Academy")+
  ylab("Estimated Marginal Means")+
  scale_x_discrete("9th Grade Academy", labels = c("Did not Attend", "Attended"))+
  ggtitle("Estimate Marginal Means for 9th Grade Academy")
 
