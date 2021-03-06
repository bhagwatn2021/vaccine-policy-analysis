# You should write code here to estimate preliminary models using your
# pilot data

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(janitor)
library(here)
library(cowplot)
options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set and make sure door-door vaccinations aren't in outsideGood:
data <- read_csv(here("data", "choiceData.csv"))
data <- dummy_cols(data, c('accessibility'))
data <- data %>% mutate(
    accessibility_0 = ifelse(accessibility_0 == 1 & outsideGood == 0, 1,0)
)

# Validate change for door to door vaccinations with dummy columns
view(data %>% filter(accessibility_0 == 1 & outsideGood == 0))
view(data %>% filter(outsideGood == 1))
view(data %>% filter(accessibility_0 == 0 & outsideGood == 0))
# Clean up names of created variables
data <- clean_names(data)
view(data)
# Estimate the model
model <- logitr(
    data   = data,
    outcome = "choice",
    obsID  = "obs_id",
    pars   = c(
        "value", 
        "penalty",
        "accessibility_1",
        "accessibility_3",
        "accessibility_10",
        "incentive_grocery_store",
        "incentive_internet",
        "incentive_sport_tickets",
        "outside_good"
    )
)

# View summary of results
summary(model)
coefs <- coef(model)
coefs

ses <- se(model)
ses
model
summary(model)


# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values

df_value <- data.frame(value = unique(data$value)) %>% 
    mutate(
        diff    = value - min(value),
        utility = diff*coefs['value'],
        upper =  diff*(coefs['value']+2*ses['value']),
        lower =  diff*(coefs['value']-2*ses['value'])
    )
    

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_value$lower))
ymax <- ceiling(max(df_value$upper))

# Plot the utility for each attribute
plot_value <- df_value %>% 
    ggplot(aes(x = value, y = utility,ymin=lower,ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility') +
    theme_bw()

plot_value

df_accessibility <- data.frame(accessibility = unique(data$accessibility)) %>% 
    mutate(
        diff    = accessibility - min(accessibility),
        utility = diff*coefs['accessibility'],
        upper =  diff*(coefs['accessibility']+2*ses['accessibility']),
        lower =  diff*(coefs['accessibility']-2*ses['accessibility'])
)

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_accessibility$lower))
ymax <- ceiling(max(df_accessibility$upper))

plot_accessibility <- df_accessibility %>% 
    ggplot(aes(x = accessibility, y = utility,ymin=lower,ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility') +
    theme_bw()

plot_accessibility

df_penalty <- data.frame(penalty = unique(data$penalty)) %>% 
    mutate(
        diff    = penalty - min(penalty),
        utility = diff*coefs['penalty'],
        upper =  diff*(coefs['penalty']+2*ses['penalty']),
        lower =  diff*(coefs['penalty']-2*ses['penalty'])
    )
# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_penalty$lower))
ymax <- ceiling(max(df_penalty$upper))

plot_penalty<- df_penalty %>% 
    ggplot(aes(x = penalty, y = utility, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility') +
    theme_bw()

plot_penalty

df_cash <- data.frame(incentive_cash = unique(data$incentive_cash)) %>% 
    mutate(
        utility = incentive_cash*coefs['incentive_cash'],
        upper =  incentive_cash*(coefs['incentive_cash']+2*ses['incentive_cash']),
        lower =  incentive_cash*(coefs['incentive_cash']-2*ses['incentive_cash'])
    )

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_cash$lower))
ymax <- ceiling(max(df_cash$upper))

plot_cash<- df_cash %>% 
    ggplot(aes(x = incentive_cash, y = utility, ymin=lower, ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Cash incentive?', y = 'Utility') +
    theme_bw()

plot_cash

df_grocery_store <- data.frame(incentive_grocery_store = unique(data$incentive_grocery_store)) %>% 
    mutate(
        utility = incentive_grocery_store*coefs['incentive_grocery_store'],
        upper =  incentive_grocery_store*(coefs['incentive_grocery_store']+2*ses['incentive_grocery_store']),
        lower =  incentive_grocery_store*(coefs['incentive_grocery_store']-2*ses['incentive_grocery_store'])
    )

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_grocery_store$lower))
ymax <- ceiling(max(df_grocery_store$upper))

plot_grocery_store <- df_grocery_store %>% 
    ggplot(aes(x = incentive_grocery_store, y = utility, ymin=lower, ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Grocery store incentive?', y = 'Utility') +
    theme_bw()

plot_grocery_store

df_internet <- data.frame(incentive_internet = unique(data$incentive_internet)) %>% 
    mutate(
        utility = incentive_internet*coefs['incentive_internet'],
        upper =  incentive_internet*(coefs['incentive_internet']+2*ses['incentive_internet']),
        lower =  incentive_internet*(coefs['incentive_internet']-2*ses['incentive_internet'])
    )

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_internet$lower))
ymax <- ceiling(max(df_internet$upper))

plot_internet <- df_internet %>% 
    ggplot(aes(x = incentive_internet , y = utility, ymin=lower, ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Internet bill incentive?', y = 'Utility') +
    theme_bw()

plot_internet


df_sport_tickets <- data.frame(incentive_sport_tickets  = unique(data$incentive_sport_tickets)) %>% 
    mutate(
        utility = incentive_sport_tickets *coefs['incentive_sport_tickets'],
        upper =  incentive_sport_tickets*(coefs['incentive_sport_tickets']+2*ses['incentive_sport_tickets']),
        lower =  incentive_sport_tickets*(coefs['incentive_sport_tickets']-2*ses['incentive_sport_tickets'])
    )

# Get upper and lower bounds (plots should have the same y-axis)
ymin <- floor(min(df_sport_tickets$lower))
ymax <- ceiling(max(df_sport_tickets$upper))

plot_sport_tickets  <- df_sport_tickets  %>% 
    ggplot(aes(x = incentive_sport_tickets , y = utility, ymin=lower, ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Sporting event tickets incentive?', y = 'Utility') +
    theme_bw()

plot_sport_tickets 

plot_continuous_attributes <- plot_grid(
    plot_value, plot_accessibility, plot_penalty,
    nrow = 3
)

plot_continuous_attributes 

# Save plots 
ggsave(
    filename = here('data', 'plot_continuous_attributes.png'), 
    plot = plot_continuous_attributes , 
    width = 10, height = 10)

plot_categorical_attributes <- plot_grid(
    plot_cash, plot_grocery_store, plot_internet, plot_sport_tickets,
    nrow = 4
)

plot_categorical_attributes 

# Save plots 
ggsave(
    filename = here('data', 'plot_categorical_attributes.png'), 
    plot = plot_categorical_attributes , 
    width = 10, height = 10)

