
library(pacman)
pacman::p_load(here, tidyverse , lme4)
here::i_am("Code/Linus-model-code.R")

# load data
data <- read.csv(here("Data", "subset_dt.csv"))


# prepare data
names(data)
data <- data %>%filter(dom == "Gain" , # only use gain data
         sampling_total_switch >= 1 # omit trials without switches
         ) %>% select(  
                       participant , 
                       choice , 
                       sampling_total , 
                       sampling_switch_ratio , 
                       exval , 
                       seen_exval , 
                       seen_winner) %>% mutate (
                         # paper = as.factor(paper) , 
                         participant = as.factor(participant) , 
                         choice = ifelse(choice == "A", 0, 1) , # recode to obtain numeric data
                         exval = ifelse(exval == "A", 0, 1) ,
                         seen_exval = ifelse(seen_exval == "A", 0, 1) ,
                         seen_winner = ifelse(seen_winner == "A", 0, 1) ,
                         seen_exval_choice = ifelse(choice == seen_exval, 1, 0) ,
                         seen_winner_choice = ifelse(choice == seen_winner, 1, 0) ,
                         I = as.factor(ifelse(seen_exval == seen_winner, 1, 0))) # indicator variable, indicating whether option with better average outcome is also option with better frequent outcome

nrow(data) 


# When I check gain_dt from main_all_dt, I realized that I eliminated the case
# where seen_exval_winner_same is "special". It shows the cases AT LEAST one of the 
# seen_exval or seen_winner is NA. So it eliminates the cases where we have a clear winner 
# with unclear exval or vice versa


# visualizations of core variables

## number of sampled outcomes
data %>% 
  ggplot(aes(x = sampling_total)) + 
  geom_histogram(color = "black", alpha = .1, bins = 100) + 
  labs(x = "Number of Sampled Outcomes" , 
       y = "Frequency") + 
  theme_minimal()

## switching frequency
data %>% 
  ggplot(aes(x = sampling_switch_ratio)) + 
  geom_histogram(color = "black", alpha = .1) + 
  labs(x = "Switching Frequency" , 
       y = "Frequency") + 
  theme_minimal()

## switching frequency and number of sampled outcomes

means <- data %>% 
  mutate(sampling_switch_ratio = round(sampling_switch_ratio, 1)) %>% # round switching frequency to one decimal point
  group_by(sampling_switch_ratio) %>% 
  summarize(sampling_total_mean = mean(sampling_total)) # for each decimal point, compute mean number of sampled outcomes

data %>%
  ggplot(aes(x = sampling_switch_ratio, y = sampling_total)) + 
  geom_jitter(color = "gray", alpha = .2) + 
  geom_point(data = means, aes(y = sampling_total_mean), size = 2) + 
  labs(x = "Switching Frequency" , 
       y = "Number of Sampled Outcomes") + 
  theme_minimal()

## switching frequency and maximization 

props <- data %>% 
  mutate(sampling_switch_ratio = round(sampling_switch_ratio, 2)) %>% # round switching frequency to 2 decimal points
  group_by(sampling_switch_ratio, I, seen_exval_choice) %>% 
  summarize(N = n()) %>% # compute the of number trials for each combination of the above variables
  mutate(prop = round(N / sum(N), 2)) %>% # compute relative number of trials (proportion)
  filter(seen_exval_choice == 1) # only keep proportions for trials where observed expected value was maximized

props %>% 
  filter(!is.na(I)) %>%  # drop trials where observed expected value and observed winner are the same 
  ggplot(aes(x = sampling_switch_ratio, y = prop, color = I)) +  
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_minimal()

# statistical modeling 

data <- data %>% filter(!is.na(I)) 
nrow(data) # 5364

# pooled (no hierarchical structure)
m1 <- glm(seen_exval_choice ~ sampling_switch_ratio , data = data,  family = binomial)
summary(m1)

m2 <- glm(seen_exval_choice ~ sampling_switch_ratio + I , data = data, family = binomial)
summary(m2)
AIC(m1, m2)
BIC(m1, m2)

m3 <- glm(seen_exval_choice ~ sampling_switch_ratio *  I , data = data, family = binomial)
summary(m3)
AIC(m2, m3)
BIC(m2, m3)

# multilevel model
m4 <- glmer(seen_exval_choice ~ sampling_switch_ratio * I + (1|participant), # we should add a random effect for the papers
            data = data,  family = binomial)
summary(m4)
AIC(m3, m4)
BIC(m3, m4)


# Bayesian implementation

# library(brms)
m4B <- brm(seen_exval_choice ~ 1 + sampling_switch_ratio * I  + (1|participant) ,
           family = bernoulli() , 
           data = data , 
           warmup = 1000, iter = 2000 , 
           cores = 8 , 
           chains = 8 , 
           seed = 6151) 
summary(m4B)
plot(m4B)

## predictive accuracy
pp_check(m4B, type = "bars") # posterior predictive check
loo(m4B) # leave one out cross validation