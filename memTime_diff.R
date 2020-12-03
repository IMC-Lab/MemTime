# calculate memTime telescope effects as a function of distance in time
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(devtools)
library(ggsci)
library(patchwork)
library(lme4)
library(lmerTest)
library(emmeans)

# load in data
data <- read.csv('data/temporal_distance.csv')
data <- data %>% 
  mutate_at(vars(ansTime, trueTime, responseTime), function(x){sapply(x, paste0, '-01')}) %>%
  mutate(ansTime=as.Date(ansTime),
         trueTime=as.Date(trueTime),
         responseTime=as.Date(responseTime),
         condition=factor(condition, levels=c('Memory','Possible Past', 'Possible Future'))) %>% 
  filter(is.na(ansTime) == F) 

# compute temporal error & temporal distance
data$error <- as.numeric(round(difftime(data$ansTime,data$trueTime, units='days')/(365.25/12), 0))
data$distance <- as.numeric(abs(round(difftime(data$responseTime,data$trueTime, units='days')/(365.25/12), 0)))

# plot - exploratory
ggplot(data, aes(x=distance, y=error, color=condition)) +
  facet_wrap(~ condition) + 
  geom_hline(yintercept = 0) + 
  geom_point(size=2.5) + 
  geom_line(size=0.5, alpha=0.7) + 
  theme_classic() +
  labs(y='Temporal error', x='Temporal distance')
ggsave('figures/memtime_scatter.png')


# analyze
model <- lmer(error ~ condition*distance + (1|subID) + (1|distance), data = data,
              control = lmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8, maxeval=5000)))
summary(model)

# plot - model results
model_trends <- emtrends(model, ~ condition, 'distance') %>% as.data.frame()
model_df <- emmeans(model, ~ condition*distance, at=list(distance=seq(0,130,5))) %>% as.data.frame()

ggplot(model_df, aes(x=distance, y=emmean, fill=condition)) +
  facet_wrap(~ condition) + 
  geom_hline(yintercept = 0, size=0.2, linetype='dashed') + 
  geom_point(data=data, aes(x=distance, y=error, color=condition), alpha=0.4) + 
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), alpha=0.5) + 
  geom_line(aes(color=condition), size=1) + 
  theme_classic() +
  theme(text=element_text(size=16, family = 'Times New Roman'),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text = element_text(size=16)) + 
  labs(y='Temporal error (months)', x='Temporal distance (months)', color='Condition', fill='Condition')
ggsave('figures/summary_plot.png', height=5, width=10, dpi='retina')
