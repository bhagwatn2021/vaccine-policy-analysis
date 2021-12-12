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
model <- logitr(
    data   = data,
    outcome = "choice",
    obsID  = "obsID",
    pars   = c(
        "value", 
        "penalty",
        "accessibility_1",
        "accessibility_3",
        "accessibility_10",
        "incentive_cash",
        "incentive_grocery_store",
        "incentive_internet",
        "incentive_sport_tickets"
    )
)

# View summary of results
summary(model)
coefs <- coef(model)
coefs

# Read in market scenarios
scenarios <- read_csv(here('data', 'scenarios.csv'))
head(scenarios)

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
    sim_single,
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
    labs(x = 'Alternative', y = 'Market Share') +
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
    altID       = c(1, 2, 3, 4), 
    obsID       = c(1, 1, 1, 1),
    value       = c(100, 100, 100, 100),
    penalty = c(0, 0, 0, 0),
    accessibility_1   = c(0, 0, 0, 0),
    accessibility_3  = c(0, 0, 0, 0),
    accessibility_10 = c(0, 0, 0, 0),
    incentive_cash = c(1, 0, 0, 0),
    incentive_grocery_store = c(0, 1, 0, 0),
    incentive_internet = c(0, 0, 1, 0),
    incentive_sport_tickets = c(0, 0, 0, 1)
)

value <- seq(0, 1000) # Define sensitivity price levels
n <- length(value) # Number of simulations (21)
scenarios_cash <- rep_df(baseline, n) # Repeat the baseline data frame n times
scenarios_cash$obsID <- rep(seq(n), each = 4) # Reset obsIDs
scenarios_cash$value[which(scenarios_cash$altID == 1)] <- value

sens_cash <- predict(
    model,
    newdata = scenarios_cash, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(value, starts_with("predicted_")) 

share_cash_plot <- 
    sens_cash %>% 
    ggplot(aes(
        x = value, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = sens_cash %>% filter(value <= 100, value >= 500), 
        linetype = "solid") +
    expand_limits(x = c(0, 1000), y = c(0, 1)) +
    labs(x = 'Value of Cash Incentive', y = 'Market Share') +
    theme_bw()

share_cash_plot

value <- seq(0, 1000) # Define sensitivity price levels
n <- length(value) # Number of simulations (21)
scenarios_value <- rep_df(baseline, n) # Repeat the baseline data frame n times
scenarios_value$obsID <- rep(seq(n), each = 4) # Reset obsIDs
scenarios_value$value[which(scenarios_value$altID == 2)] <- value

sens_value <- predict(
    model,
    newdata = scenarios_value, 
    obsID = 'obsID', 
    ci = 0.95,
    returnData = TRUE) %>%
    # Keep only EV alternative
    filter(altID == 1) %>% 
    # Keep only prices and predictions
    select(value, starts_with("predicted_")) 

share_cash_plot <- 
    sens_value %>% 
    ggplot(aes(
        x = value, y = predicted_prob, 
        ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
    geom_ribbon(alpha = 0.2) +
    # Use a dashed line for the full range of prices
    geom_line(linetype = "dashed") +
    # Overlay solid line for range of prices included in survey
    geom_line(
        data = sens_value %>% filter(value <= 100, value >= 500), 
        linetype = "solid") +
    expand_limits(x = c(0, 1000), y = c(0, 1)) +
    labs(x = 'Value of Cash Incentive', y = 'Market Share') +
    theme_bw()

share_cash_plot

cases <- tribble(
    ~obsID, ~altID, ~attribute,    ~case,  ~value,
    2,      2,     'value',       'high',  1000,
    3,      2,     'value',       'low',   0,
    4,      2,     'penalty',     'high',  0,
    5,      2,     'penalty',     'low',   1000
)

# Define scenarios
n <- 5 # baseline + high & low for each attribute
scenarios_atts <- rep_df(baseline, n) 
scenarios_atts$obsID <- rep(seq(n), each = 4) # Reset obsIDs

# Replace scenarios with case values 
scenarios_atts <- scenarios_atts %>% 
    left_join(cases, by = c("altID", "obsID")) %>% 
    mutate(
        attribute = ifelse(is.na(attribute), "other", attribute),
        case = ifelse(is.na(case), "base", case),
        value = ifelse(attribute == 'value', value, value),
        penalty = ifelse(attribute == 'penalty', value, penalty)
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
    filter(altID == 2) %>% 
    # Keep only attributes and predictions
    select(attribute, case, value, predicted_prob)

sens_atts

# Make a tornado diagram to show market sensitivity to multiple

labels <- data.frame( 
    attribute = c('value', 'penalty'), 
    label = c('Value', 'Penalty')
)

tornado_data <- sens_atts %>% 
    filter(case != 'base') %>% 
    # Rename variables for plotting labels
    left_join(labels, by = 'attribute')

tornado_base <- ggtornado(
    data = tornado_data,
    baseline = sens_atts$predicted_prob[1], 
    var = 'label',
    level = 'case',
    value = 'value', 
    result = 'predicted_prob'
) 

# Change the fill colors, adjust labels
tornado_plot <- tornado_base +
    scale_fill_manual(values = c("#67a9cf", "#ef8a62")) + 
    labs(x = 'Market Share', y = 'Attribute')

# Save plot
ggsave(
    filename = here('data', 'tornado_plot.png'), 
    plot = tornado_plot,
    width = 5, height = 3
)
