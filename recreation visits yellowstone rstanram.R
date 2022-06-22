library(ggplot2)
library(rstanarm)
library(broom.mixed)

ysnp <-  read.csv("~/Downloads/ysnp.csv", header=1)
ysnp <- filter(ysnp, Recreation.Visits > 60000)
ysnp$TotalSnowfall.In.[ysnp$TotalSnowfall.In.>0]<-1
names(ysnp)[names(ysnp) == 'TotalSnowfall.In.'] <- 'SnoworNot'
ggplot(ysnp, aes(x= Consumer.Sentiment.Index, y= Recreation.Visits, color=SnoworNot)) + geom_point()
yellowstone_sim <- stan_glm(
  Recreation.Visits ~ Consumer.Sentiment.Index + SnoworNot, 
  data = ysnp, family = neg_binomial_2,
  prior_intercept = normal(488768, 250671.5, autoscale = TRUE),
  prior = normal(0, 5, autoscale = TRUE), 
  prior_aux = exponential(1, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735)
prior_summary(yellowstone_sim)
pp_check(yellowstone_sim) + 
  xlim(0, 1100000) + 
  xlab("Recreation.Visits")
tidy(yellowstone_sim, conf.int = TRUE, conf.level = 0.80)
