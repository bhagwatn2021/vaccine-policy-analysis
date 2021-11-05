# You should write code here to estimate preliminary models using your
# pilot data

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(janitor)
library(here)
options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv(here("data", "choiceData.csv"))
view(data)

# Clean up names of created variables
data <- clean_names(data)

# Estimate the model
model <- logitr(
    data   = data,
    outcome = "choice",
    obsID  = "obs_id",
    pars   = c(
        "value", 
        "penalty",
        "accessibility",
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

# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values

df_value <- data.frame(value = unique(data$value)) %>% 
    mutate(
        diff    =value - min(value),
        utility = diff*coefs['value'])

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_value$utility))
ymax <- ceiling(max(df_value$utility))

# Plot the utility for each attribute
plot_value <- df_value %>% 
    ggplot() +
    geom_line(aes(x = value, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility') +
    theme_bw()

plot_value

df_accessibility <- data.frame(accessibility = unique(data$accessibility)) %>% 
    mutate(
        diff    = accessibility - min(accessibility),
        utility = diff*coefs['accessibility'])

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_accessibility$utility))
ymax <- ceiling(max(df_accessibility$utility))

plot_accessibility <- df_accessibility %>% 
    ggplot() +
    geom_line(aes(x = accessibility, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility') +
    theme_bw()

plot_accessibility

df_penalty <- data.frame(penalty = unique(data$penalty)) %>% 
    mutate(
        diff    = penalty - min(penalty),
        utility = diff*coefs['penalty'])

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_penalty$utility))
ymax <- ceiling(max(df_penalty$utility))

plot_penalty<- df_penalty %>% 
    ggplot() +
    geom_line(aes(x = penalty, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility') +
    theme_bw()

plot_penalty

df_cash <- data.frame(incentive_cash = unique(data$incentive_cash)) %>% 
    mutate(
        utility = incentive_cash*coefs['incentive_cash']
    )

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_cash$utility))
ymax <- ceiling(max(df_cash$utility))

plot_cash<- df_cash %>% 
    ggplot() +
    geom_point(aes(x = incentive_cash, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Cash incentive?', y = 'Utility') +
    theme_bw()

plot_cash

df_grocery_store <- data.frame(incentive_grocery_store = unique(data$incentive_grocery_store)) %>% 
    mutate(
        utility = incentive_grocery_store*coefs['incentive_grocery_store']
    )

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_grocery_store$utility))
ymax <- ceiling(max(df_grocery_store$utility))

plot_grocery_store <- df_grocery_store %>% 
    ggplot() +
    geom_point(aes(x = incentive_grocery_store, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Grocery store incentive?', y = 'Utility') +
    theme_bw()

plot_grocery_store

df_internet <- data.frame(incentive_internet = unique(data$incentive_internet)) %>% 
    mutate(
        utility = incentive_internet*coefs['incentive_internet']
    )

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_internet$utility))
ymax <- ceiling(max(df_internet$utility))

plot_internet <- df_internet %>% 
    ggplot() +
    geom_point(aes(x = incentive_internet, y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Internet bill incentive?', y = 'Utility') +
    theme_bw()

plot_internet


df_sport_tickets <- data.frame(incentive_sport_tickets  = unique(data$incentive_sport_tickets)) %>% 
    mutate(
        utility = incentive_sport_tickets *coefs['incentive_sport_tickets']
    )

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_sport_tickets$utility))
ymax <- ceiling(max(df_sport_tickets$utility))

plot_sport_tickets  <- df_sport_tickets  %>% 
    ggplot() +
    geom_point(aes(x = incentive_sport_tickets , y = utility)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Sporting event tickets incentive?', y = 'Utility') +
    theme_bw()

plot_sport_tickets 

