# You should write code here to analyze your model results, e.g. computing WTP,
# market simulations, sensitivity analyses, etc.

# Load all libraries
library(tidyverse)
library(knitr)
library(patchwork)
library(kableExtra)
library(here)
library(logitr)
library(fastDummies)
library(maddTools)


# Base model
load(here("models","model.RData"))
# View summary of results
summary(model)
coefs <- coef(model)
coefs

# Read in market scenarios
scenarios <- read_csv(here('data', 'scenarios.csv'))
view(scenarios)

sim_multi <- predict(
    model,
    newdata = scenarios,
    altID = 'altID',
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE)

View(sim_multi)

# Save simulations
save(
    sim_multi,
    file = here("data", "simulation.RData")
)

allsims <- sim_multi %>%
    ggplot(aes(
        x = as.factor(altID), y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width = 0.3) +
    facet_wrap(~obsID) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = 'Take the vaccine (1=yes,2=no)?', y = 'Probability') +
    theme_bw()

allsims

ggsave(
    filename = here('data', 'sim_multi.png'), 
    width = 8, height = 7
)

sim1 <- sim_multi %>%
    filter(obsID == 1) %>%
    ggplot(aes(
        x = as.factor(altID), y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width = 0.3) +
    facet_wrap(~obsID) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = 'Alternative', y = 'Market Share') +
    theme_bw()

# SENSITIVITY ANALYSIS

baseline <- data.frame(
    altID = c(1, 2),
    obsID = c(1, 1),
    value   = c(0, 0),
    penalty = c(0, 0),
    accessibility_1   = c(0, 0),
    accessibility_3  = c(0, 0),
    accessibility_10 = c(0, 0),
    incentive_grocery_store = c(0, 0),
    incentive_internet = c(0, 0),
    incentive_sport_tickets = c(0, 0),
    outsideGood = c(0, 1)
)

# make probability for option 1 as high as possible
# instead of market share do probability of taking the shot
value <- seq(0, 1000) # Define sensitivity price levels
n <- length(value) # Number of simulations (21)
scenarios_cash_value <- rep_df(baseline, n) # Repeat the baseline data frame n times
scenarios_cash_value$obsID <- rep(seq(n), each = 2) # Reset obsIDs
scenarios_cash_value$value[which(scenarios_value$altID == 1)] <- value
scenarios_cash_value
sens_cash_value <- predict(
    model,
    newdata = scenarios_cash_value, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(value, starts_with("predicted_")) 
share_cash_plot <- sens_cash_value %>% 
    ggplot(aes( x = value, y = predicted_prob, 
                ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = sens_cash_value %>% filter(value <= 100, value >= 500), 
        linetype = "solid") +
    expand_limits(x = c(0, 1000), y = c(0, 1)) +
    labs(x = 'Value of Cash Incentive', y = 'Probability of taking a COVID-19 vaccine') +
    theme_bw()
share_cash_plot


#Grocery Store plots
scenarios_grocery_value <- rep_df(baseline, n) # Repeat the baseline data frame n times
scenarios_grocery_value$obsID <- rep(seq(n), each = 2) # Reset obsIDs
scenarios_grocery_value$value[which(scenarios_grocery_value$altID == 1)] <- value
scenarios_grocery_value$incentive_grocery_store[which(scenarios_grocery_value$altID == 1)] <- 1

scenarios_grocery_value
sens_grocery_value <- predict(
    model,
    newdata = scenarios_grocery_value, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(value, starts_with("predicted_")) 
share_grocery_plot <- sens_grocery_value %>% 
    ggplot(aes( x = value, y = predicted_prob, 
                ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = sens_grocery_value %>% filter(value <= 100, value >= 500), 
        linetype = "solid") +
    expand_limits(x = c(0, 1000), y = c(0, 1)) +
    labs(x = 'Value of Grocery Store Incentive', y = 'Probability of taking a COVID-19 vaccine') +
    theme_bw()
share_grocery_plot


#Internet plots
scenarios_internet_value <- rep_df(baseline, n) # Repeat the baseline data frame n times
scenarios_internet_value$obsID <- rep(seq(n), each = 2) # Reset obsIDs
scenarios_internet_value$value[which(scenarios_internet_value$altID == 1)] <- value
scenarios_internet_value$incentive_internet[which(scenarios_internet_value$altID == 1)] <- 1

scenarios_internet_value
sens_internet_value <- predict(
    model,
    newdata = scenarios_internet_value, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(value, starts_with("predicted_")) 
share_internet_plot <- sens_internet_value %>% 
    ggplot(aes( x = value, y = predicted_prob, 
                ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = sens_internet_value %>% filter(value <= 100, value >= 500), 
        linetype = "solid") +
    expand_limits(x = c(0, 1000), y = c(0, 1)) +
    labs(x = 'Value of Internet Rebate Incentive', y = 'Probability of taking a COVID-19 vaccine') +
    theme_bw()
share_internet_plot

#Sporting event plots
scenarios_sports_value <- rep_df(baseline, n) # Repeat the baseline data frame n times
scenarios_sports_value$obsID <- rep(seq(n), each = 2) # Reset obsIDs
scenarios_sports_value$value[which(scenarios_sports_value$altID == 1)] <- value
scenarios_sports_value$incentive_sport_tickets[which(scenarios_sports_value$altID == 1)] <- 1

scenarios_sports_value
sens_sports_value <- predict(
    model,
    newdata = scenarios_internet_value, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(value, starts_with("predicted_")) 

share_sports_plot <- sens_sports_value %>% 
    ggplot(aes( x = value, y = predicted_prob, 
                ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = sens_sports_value %>% filter(value <= 100, value >= 500), 
        linetype = "solid") +
    expand_limits(x = c(0, 1000), y = c(0, 1)) +
    labs(x = 'Value of Sporting Event Tickets Incentive', y = 'Probability of taking a COVID-19 vaccine') +
    theme_bw()
share_sports_plot

#Penaltt plots
penalty <- seq(0, 1000) # Define sensitivity price levels
n <- length(value) # Number of simulations (21)
scenarios_penalty <- rep_df(baseline, n) # Repeat the baseline data frame n times
scenarios_penalty$obsID <- rep(seq(n), each = 2) # Reset obsIDs
scenarios_penalty$penalty[which(scenarios_penalty$altID == 1)] <- penalty
sens_penalty <- predict(
    model,
    newdata = scenarios_penalty, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(penalty, starts_with("predicted_"))

share_penalty_plot <- sens_penalty %>% 
    ggplot(aes( x = value, y = predicted_prob, 
                ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = sens_value %>% filter(value <= 100, value >= 500), 
        linetype = "solid") +
    expand_limits(x = c(0, 1000), y = c(0, 1)) +
    labs(x = 'Value of Penalty', y = 'Probability of taking a COVID-19 vaccine') +
    theme_bw()
share_penalty_plot

#Accessibility tornado plots
accessibility_cases <- tribble(
    ~obsID, ~altID, ~attribute,    ~case,  ~amount,
    2,      1,     'accessibility_1',       'high',  1,
    3,      1,     'accessibility_1',       'low',   0,
    4,      1,     'accessibility_3',     'high',  1,
    5,      1,     'accessibility_3',     'low',   0,
    6,      1,     'accessibility_10', 'low', 0,
    7,      1,     'accessibility_10', 'high', 1
)
# Define scenarios
n <- 7 # baseline + high & low for each attribute
scenarios_accessibility_atts <- rep_df(baseline, n) 
scenarios_accessibility_atts$obsID <- rep(seq(n), each = 2) # Reset obsIDs
# Replace scenarios with case values 
scenarios_accessibility_atts
scenarios_accessibility_atts <- scenarios_accessibility_atts %>% 
    left_join(cases, by = c("altID", "obsID")) %>% 
    mutate(
        attribute = ifelse(is.na(attribute), "other", attribute),
        case = ifelse(is.na(case), "base", case),
        accessibility_1 = ifelse(attribute == 'accessibility_1', amount, accessibility_1),
        accessibility_3 = ifelse(attribute == 'accessibility_3', amount, accessibility_3),
        accessibility_10 = ifelse(attribute == 'accessibility_10',amount, accessibility_10)
    )
scenarios_accessibility_atts
# For each case, simulate the market share predictions
sens_accessibility_atts <- predict(
    model,
    newdata = scenarios_accessibility_atts, 
    obsID = 'obsID', 
    ci = 0.95, 
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only attributes and predictions
    select(attribute, case, amount, predicted_prob)
sens_accessibility_atts
# Make a tornado diagram to show market sensitivity to multiple
labels <- data.frame( 
    attribute = c('accessibility_1', 'accessibility_3','accessibility_10'), 
    label = c(1, 3, 10)
)
tornado_accessibility_data <- sens_accessibility_atts %>% 
    filter(case != 'base') %>% 
    # Rename variables for plotting labels
    left_join(labels, by = 'attribute')
tornado_accessibility_data
tornado_accessibility_base <- ggtornado(
    data = tornado_data,
    baseline = sens_accessibility_atts$predicted_prob[1], 
    var = 'label',
    level = 'case',
    value = 'amount', 
    result = 'predicted_prob'
) 
# Change the fill colors, adjust labels
tornado_accessibility_plot <- tornado_base +
    scale_fill_manual(values = c("#67a9cf", "#ef8a62")) + 
    labs(x = 'Probability of taking a COVID-19 vaccine', y = 'Proximity of vaccination centers (miles)')

tornado_accessibility_plot
# Save plot
ggsave(
    filename = here('data', 'tornado_accessibility_plot.png'), 
    plot = tornado_plot,
    width = 5, height = 3
)

#Tornado plots
cases <- tribble(
    ~obsID, ~altID, ~attribute,    ~case,  ~amount,
    2,      1,     'value',       'high',  1000,
    3,      1,     'value',       'low',   0,
    4,      1,     'penalty',     'high',  1000,
    5,      1,     'penalty',     'low',   0,
    6,      1,     'incentive_grocery_store', 'low', 0,
    7,      1,     'incentive_grocery_store', 'high', 1,
    8,      1,     'incentive_internet', 'low', 0,
    9,      1,     'incentive_internet', 'high', 1,
    10,      1,     'incentive_sport_tickets', 'low', 0,
    11,      1,     'incentive_sport_tickets', 'high', 1,
)
# Define scenarios
n <- 11 # baseline + high & low for each attribute
scenarios_atts <- rep_df(baseline, n) 
scenarios_atts$obsID <- rep(seq(n), each = 2) # Reset obsIDs
# Replace scenarios with case values 
scenarios_atts <- scenarios_atts %>% 
    left_join(cases, by = c("altID", "obsID")) %>% 
    mutate(
        attribute = ifelse(is.na(attribute), "other", attribute),
        case = ifelse(is.na(case), "base", case),
        value = ifelse(attribute == 'value', amount, value),
        penalty = ifelse(attribute == 'penalty', amount, penalty),
        incentive_grocery_store = ifelse(attribute == 'incentive_grocery_store', amount, incentive_grocery_store),
        incentive_internet = ifelse(attribute == 'incentive_internet', amount, incentive_internet),
        incentive_sport_tickets = ifelse(attribute == 'incentive_sport_tickets', amount, incentive_sport_tickets)
    )
scenarios_atts
# For each case, simulate the market share predictions
sens_atts <- predict(
    model,
    newdata = scenarios_atts, 
    obsID = 'obsID', 
    ci = 0.95, 
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only attributes and predictions
    select(attribute, case, amount, predicted_prob)
sens_atts
# Make a tornado diagram to show market sensitivity to multiple
labels <- data.frame( 
    attribute = c('value', 'penalty',"incentive_grocery_store","incentive_internet","incentive_sport_tickets"), 
    label = c('Value', 'Penalty',"Grocery Store Gift Cards","Internet Rebates","Sporting Event Tickets")
)
tornado_data <- sens_atts %>% 
    filter(case != 'base') %>% 
    # Rename variables for plotting labels
    left_join(labels, by = 'attribute')
tornado_data
tornado_base <- ggtornado(
    data = tornado_data,
    baseline = sens_atts$predicted_prob[1], 
    var = 'label',
    level = 'case',
    value = 'amount', 
    result = 'predicted_prob'
) 
# Change the fill colors, adjust labels
tornado_plot <- tornado_base +
    scale_fill_manual(values = c("#67a9cf", "#ef8a62")) + 
    labs(x = 'Probability of taking a COVID-19 vaccine', y = 'Attribute')

tornado_plot
# Save plot
ggsave(
    filename = here('data', 'tornado_plot.png'), 
    plot = tornado_plot,
    width = 5, height = 3
)

##### SUBGROUP ANALYSIS
load(here("models","model_ethnicity.RData"))
summary(model_ethnicity)
coefs_ethnicity <- coef(model_ethnicity)

covariance_ethnicity <- vcov(model_ethnicity)
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs_ethnicity, covariance_ethnicity))
coef_draws
coef_draws_asian <- coef_draws %>%
    mutate(
        value = value + value_asian,
        penalty = penalty + penalty_asian,
        accessibility_1 = accessibility_1+accessibility_1_asian,
        accessibility_3 = accessibility_3+accessibility_3_asian,
        accessibility_10 = accessibility_10+accessibility_10_asian,
        incentive_grocery_store = incentive_grocery_store + incentive_grocery_store_asian,
        incentive_internet = incentive_internet + incentive_internet_asian,
        incentive_sport_tickets = incentive_sport_tickets + incentive_sport_tickets_asian,
        outsideGood = outsideGood + outsideGood_asian
    ) %>% select(value,penalty,accessibility_1,accessibility_3,accessibility_10,incentive_grocery_store,incentive_internet,incentive_sport_tickets,outsideGood)
scenarios
sim_asian <- maddTools::logitProbs(
    coefs = coef_draws_asian,
    newdata = scenarios, 
    obsID = 'obsID', 
    ci = 0.95
)

View(sim_multi_ethnicity)

# Save simulations
save(
    sim_multi_ethnicity,
    file = here("data", "simulation_ethnicity.RData")
)

allsims_ethnicity <- sim_multi_ethnicity %>%
    ggplot(aes(
        x = as.factor(altID), y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_col(fill = "grey", width = 0.6) +
    geom_errorbar(width = 0.3) +
    facet_wrap(~obsID) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(x = 'Take the vaccine (1=yes,2=no)?', y = 'Probability') +
    theme_bw()

allsims_ethnicity
