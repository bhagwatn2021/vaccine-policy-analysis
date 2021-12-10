# You should write code here to estimate preliminary models using your
# pilot data
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
data <- dummy_cols(data, c('accessibility','cbcAllSame'))
data <- data %>% mutate(
    accessibility_0 = ifelse(accessibility_0 == 1 & outsideGood == 0, 1,0)
)

# Validate change for door to door vaccinations with dummy columns
view(data %>% filter(accessibility_0 == 1 & outsideGood == 0))
view(data %>% filter(outsideGood == 1))
view(data %>% filter(accessibility_0 == 0 & outsideGood == 0))

view(data)
# Estimate the model
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
        "incentive_grocery_store",
        "incentive_internet",
        "incentive_sport_tickets",
        "outsideGood"
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

# Mixed logit model 
mxl_pref <- logitr(
    data    = data %>% filter(cbcAllSame == 0),
    outcome = "choice",
    obsID   = "obsID",
    pars    =  c(
        "value", 
        "penalty", 
        "accessibility_1", 
        "accessibility_3", 
        "accessibility_10", 
        "incentive_grocery_store", 
        "incentive_internet", 
        "incentive_sport_tickets", 
        "outsideGood"
    ),
    randPars = c(value = 'n', penalty = 'n', incentive_grocery_store='n', incentive_internet='n', incentive_sport_tickets='n'),
    numMultiStarts=50
)

# View summary of results
summary(mxl_pref)
# Hesitant vs. resistant - cbcAllSame = 0 is reference 
resistant_sessions <- choiceData %>% filter(cbcAllSame == 1 & outsideGood == 1) %>% select(session)

data_resistant <- data %>%
    mutate(
        outsideGood_resistant = outsideGood * ifelse(session %in% resistant_sessions$session,1,0),
        value_resistant = value * ifelse(session %in% resistant_sessions$session,1,0),
        penalty_resistant = penalty * ifelse(session %in% resistant_sessions$session,1,0),
        accessibility_1_resistant = accessibility_1 * ifelse(session %in% resistant_sessions$session,1,0),
        accessibility_3_resistant = accessibility_3 * ifelse(session %in% resistant_sessions$session,1,0),
        accessibility_10_resistant = accessibility_10 * ifelse(session %in% resistant_sessions$session,1,0),
        incentive_grocery_store_resistant = incentive_grocery_store * ifelse(session %in% resistant_sessions$session,1,0),
        incentive_internet_resistant = incentive_internet * ifelse(session %in% resistant_sessions$session,1,0),
        incentive_sport_tickets_resistant = incentive_sport_tickets * ifelse(session %in% resistant_sessions$session,1,0),
    )
view(data_resistant)

model <- logitr(
    data = data_resistant,
    outcome = "choice"
    obsID  = "obsID",
    pars   = c(
        "value", 
        "penalty",
        "accessibility_1",
        "accessibility_3",
        "accessibility_10",
        "incentive_grocery_store",
        "incentive_internet",
        "incentive_sport_tickets",
        "outsideGood",
        "outsideGood_resistant",
        "value_resistant",
        "penalty_resistant",
        "accessibility_1_resistant",
        "accessibility_3_resistant",
        "accessibility_10_resistant",
        "incentive_grocery_store_resistant",
        "incentive_internet_resistant",
        "incentive_sport_tickets_resistant"
    )
)

# Summary of model
summary(model)


# Conservative vs. liberal - moderate is reference 
data_politics <- dummy_cols(data, c('politics'))
view(data_politics)

data_politics <- data_politics%>%
    mutate(
        value_liberal =  value * politics_liberal + value * politics_leaning_liberal + value * politics_strongly_liberal,
        penalty_liberal = penalty * politics_liberal + penalty * politics_leaning_liberal + penalty * politics_strongly_liberal,
        accessibility_1_liberal = accessibility_1 * politics_liberal + accessibility_1 * politics_strongly_liberal + accessibility_1 * politics_strongly_liberal,
        accessibility_3_liberal = accessibility_3 * politics_liberal + accessibility_3 * politics_strongly_liberal + accessibility_3 * politics_strongly_liberal,
        accessibility_10_liberal = accessibility_10 * politics_liberal + accessibility_10 * politics_leaning_liberal + accessibility_10 * politics_strongly_liberal,
        incentive_grocery_store_liberal = incentive_grocery_store * politics_liberal + incentive_grocery_store * politics_leaning_liberal + incentive_grocery_store * politics_strongly_liberal,
        incentive_internet_liberal = incentive_internet * politics_liberal + incentive_internet * politics_leaning_liberal + incentive_internet * politics_strongly_liberal,
        incentive_sport_tickets_liberal = incentive_sport_tickets * politics_liberal + incentive_sport_tickets * politics_leaning_liberal + incentive_sport_tickets * politics_strongly_liberal,
        outsideGood_liberal = outsideGood * politics_liberal + outsideGood * politics_leaning_liberal + outsideGood * politics_strongly_liberal,
        value_conservative =  value * politics_conservative + value * politics_leaning_conservative + value * politics_strongly_conservative,
        penalty_conservative = penalty * politics_conservative + penalty * politics_leaning_conservative + penalty * politics_strongly_conservative,
        accessibility_1_conservative = accessibility_1 * politics_conservative + accessibility_1 * politics_leaning_conservative + accessibility_1 * politics_strongly_conservative,
        accessibility_3_conservative = accessibility_3 * politics_conservative + accessibility_3 * politics_leaning_conservative + accessibility_3 * politics_strongly_conservative,
        accessibility_10_conservative = accessibility_10 * politics_conservative + accessibility_10 * politics_leaning_conservative + accessibility_10 * politics_strongly_conservative,
        incentive_grocery_store_conservative = incentive_grocery_store * politics_conservative + incentive_grocery_store * politics_leaning_conservative + incentive_grocery_store * politics_strongly_conservative,
        incentive_internet_conservative = incentive_internet * politics_conservative + incentive_internet * politics_leaning_conservative + incentive_internet * politics_strongly_conservative,
        incentive_sport_tickets_conservative = incentive_sport_tickets * politics_conservative + incentive_sport_tickets * politics_leaning_conservative + incentive_sport_tickets * politics_strongly_conservative,
        outsideGood_conservative = outsideGood * politics_conservative + outsideGood * politics_leaning_conservative + outsideGood * politics_strongly_conservative,
        value_independent =  value * politics_independent,
        penalty_independent = penalty * politics_independent,
        accessibility_1_independent = accessibility_1 * politics_independent,
        accessibility_3_independent = accessibility_3 * politics_independent,
        accessibility_10_independent = accessibility_10 * politics_independent,
        incentive_grocery_store_independent = incentive_grocery_store * politics_independent,
        incentive_internet_independent = incentive_internet * politics_independent,
        incentive_sport_tickets_independent = incentive_sport_tickets * politics_independent,
        outsideGood_independent = outsideGood * politics_independent
        
    )

model <- logitr(
    data = data_politics,
    outcome = "choice",
    obsID  = "obsID",
    pars   = c(
        "value", 
        "penalty",
        "accessibility_1",
        "accessibility_3",
        "accessibility_10",
        "incentive_grocery_store",
        "incentive_internet",
        "incentive_sport_tickets",
        "outsideGood",
        "value_liberal",
        "penalty_liberal",
        "accessibility_1_liberal",
        "accessibility_3_liberal",
        "accessibility_10_liberal",
        "incentive_grocery_store_liberal",
        "incentive_internet_liberal",
        "incentive_sport_tickets_liberal",
        "outsideGood_liberal",
        "value_conservative",
        "penalty_conservative",
        "accessibility_1_conservative",
        "accessibility_3_conservative",
        "accessibility_10_conservative",
        "incentive_grocery_store_conservative",
        "incentive_internet_conservative",
        "incentive_sport_tickets_conservative",
        "outsideGood_conservative",
        "value_independent",
        "penalty_independent",
        "accessibility_1_independent",
        "accessibility_3_independent",
        "accessibility_10_independent",
        "incentive_grocery_store_independent",
        "incentive_internet_independent",
        "incentive_sport_tickets_independent",
        "outsideGood_independent"
    )
)

summary(model)

# Lower vs high income - middle class is reference
data_income <- dummy_cols(data, c('income'))
view(data_income)

data_income <- data_income%>%
    mutate(
        value_low =  value * ifelse(income_under_10 == 1 | income_inc_10_to_20 == 1 | income_inc_20_to_30 == 1 | income_inc_30_to_40 == 1 | income_inc_40_to_50 == 1,1,0),
        penalty_low =  penalty * ifelse(income_under_10 == 1 | income_inc_10_to_20 == 1 | income_inc_20_to_30 == 1 | income_inc_30_to_40 == 1 | income_inc_40_to_50 == 1,1,0),
        accessibility_1_low = accessibility_1 * ifelse(income_under_10 == 1 | income_inc_10_to_20 == 1 | income_inc_20_to_30 == 1 | income_inc_30_to_40 == 1 | income_inc_40_to_50 == 1,1,0),
        accessibility_3_low = accessibility_3 * ifelse(income_under_10 == 1 | income_inc_10_to_20 == 1 | income_inc_20_to_30 == 1 | income_inc_30_to_40 == 1 | income_inc_40_to_50 == 1,1,0),
        accessibility_10_low  = accessibility_10 * ifelse(income_under_10 == 1 | income_inc_10_to_20 == 1 | income_inc_20_to_30 == 1 | income_inc_30_to_40 == 1 | income_inc_40_to_50 == 1,1,0),
        incentive_grocery_store_low = incentive_grocery_store * ifelse(income_under_10 == 1 | income_inc_10_to_20 == 1 | income_inc_20_to_30 == 1 | income_inc_30_to_40 == 1 | income_inc_40_to_50 == 1,1,0),
        incentive_internet_low  = incentive_internet * ifelse(income_under_10 == 1 | income_inc_10_to_20 == 1 | income_inc_20_to_30 == 1 | income_inc_30_to_40 == 1 | income_inc_40_to_50 == 1,1,0),
        incentive_sport_tickets_low  = incentive_sport_tickets * ifelse(income_under_10 == 1 | income_inc_10_to_20 == 1 | income_inc_20_to_30 == 1 | income_inc_30_to_40 == 1 | income_inc_40_to_50 == 1,1,0),
        outsideGood_low  = outsideGood * ifelse(income_under_10 == 1 | income_inc_10_to_20 == 1 | income_inc_20_to_30 == 1 | income_inc_30_to_40 == 1 | income_inc_40_to_50 == 1,1,0),
        value_high =  value * ifelse(income_inc_100_to_150 == 1 | income_inc_150_to_200 == 1 | income_inc_over_200 == 1,1,0),
        penalty_high= penalty * ifelse(income_inc_100_to_150 == 1 | income_inc_150_to_200  == 1 | income_inc_over_200 == 1,1,0),
        accessibility_1_high = accessibility_1 *  ifelse(income_inc_100_to_150 == 1 | income_inc_150_to_200  == 1 | income_inc_over_200 == 1,1,0),
        accessibility_3_high = accessibility_3 * ifelse(income_inc_100_to_150 == 1 | income_inc_150_to_200  == 1 | income_inc_over_200 == 1,1,0),
        accessibility_10_high = accessibility_10 * ifelse(income_inc_100_to_150 == 1 | income_inc_150_to_200  == 1 | income_inc_over_200 == 1,1,0),
        incentive_grocery_store_high = incentive_grocery_store * ifelse(income_inc_100_to_150 == 1 | income_inc_150_to_200  == 1 | income_inc_over_200 == 1,1,0),
        incentive_internet_high = incentive_internet * ifelse(income_inc_100_to_150 == 1 | income_inc_150_to_200 == 1 | income_inc_over_200 == 1,1,0),
        incentive_sport_tickets_high = incentive_sport_tickets *  ifelse(income_inc_100_to_150 == 1 | income_inc_150_to_200 == 1 | income_inc_over_200 == 1,1,0),
        outsideGood_high = outsideGood * ifelse(income_inc_100_to_150 == 1 | income_inc_150_to_200 == 1 | income_inc_over_200 == 1,1,0)
 )
model <- logitr(
    data = data_income,
    outcome = "choice",
    obsID  = "obsID",
    pars   = c(
        "value", 
        "penalty",
        "accessibility_1",
        "accessibility_3",
        "accessibility_10",
        "incentive_grocery_store",
        "incentive_internet",
        "incentive_sport_tickets",
        "outsideGood",
        "value_low", 
        "penalty_low",
        "accessibility_1_low",
        "accessibility_3_low",
        "accessibility_10_low",
        "incentive_grocery_store_low",
        "incentive_internet_low",
        "incentive_sport_tickets_low",
        "outsideGood_low",
        "value_high", 
        "penalty_high",
        "accessibility_1_high",
        "accessibility_3_high",
        "accessibility_10_high",
        "incentive_grocery_store_high",
        "incentive_internet_high",
        "incentive_sport_tickets_high",
        "outsideGood_high"
    )
)

summary(model)

# Ethnicity - white is reference 
data_ethnicity <- dummy_cols(data, c('ethnicity'))
view(data_ethnicity)

data_ethnicity <- data_ethnicity %>%
    mutate(
        value_asian =  value * ethnicity_asian,
        penalty_asian =  penalty * ethnicity_asian,
        accessibility_1_asian = accessibility_1 * ethnicity_asian,
        accessibility_3_asian = accessibility_3 * ethnicity_asian,
        accessibility_10_asian  = accessibility_10 * ethnicity_asian,
        incentive_grocery_store_asian = incentive_grocery_store * ethnicity_asian,
        incentive_internet_asian  = incentive_internet * ethnicity_asian,
        incentive_sport_tickets_asian  = incentive_sport_tickets * ethnicity_asian,
        outsideGood_asian  = outsideGood * ethnicity_asian,
        value_black =  value * ethnicity_black,
        penalty_black = penalty * ethnicity_black,
        accessibility_1_black = accessibility_1 * ethnicity_black,
        accessibility_3_black = accessibility_3 * ethnicity_black,
        accessibility_10_black = accessibility_10 * ethnicity_black,
        incentive_grocery_store_black = incentive_grocery_store * ethnicity_black,
        incentive_internet_black = incentive_internet * ethnicity_black,
        incentive_sport_tickets_black = incentive_sport_tickets * ethnicity_black,
        outsideGood_black = outsideGood * ethnicity_black,
        value_hispanic =  value * ethnicity_hispanic,
        penalty_hispanic =  penalty * ethnicity_hispanic,
        accessibility_1_hispanic = accessibility_1 *ethnicity_hispanic,
        accessibility_3_hispanic = accessibility_3 * ethnicity_hispanic,
        accessibility_10_hispanic = accessibility_10 * ethnicity_hispanic,
        incentive_grocery_store_hispanic = incentive_grocery_store * ethnicity_hispanic,
        incentive_internet_hispanic  = incentive_internet * ethnicity_hispanic,
        incentive_sport_tickets_hispanic  = incentive_sport_tickets * ethnicity_hispanic,
        outsideGood_hispanic  = outsideGood * ethnicity_hispanic,
        value_native =  value * ethnicity_native,
        penalty_native = penalty * ethnicity_native,
        accessibility_1_native = accessibility_1 * ethnicity_native,
        accessibility_3_native = accessibility_3 * ethnicity_native,
        accessibility_10_native = accessibility_10 * ethnicity_native,
        incentive_grocery_store_native = incentive_grocery_store * ethnicity_native,
        incentive_internet_native = incentive_internet * ethnicity_native,
        incentive_sport_tickets_native = incentive_sport_tickets * ethnicity_native,
        outsideGood_native = outsideGood * ethnicity_native,
    )
model <- logitr(
    data = data_ethnicity,
    outcome = "choice",
    obsID  = "obsID",
    pars   = c(
        "value", 
        "penalty",
        "accessibility_1",
        "accessibility_3",
        "accessibility_10",
        "incentive_grocery_store",
        "incentive_internet",
        "incentive_sport_tickets",
        "outsideGood",
        "value_asian", 
        "penalty_asian",
        "accessibility_1_asian",
        "accessibility_3_asian",
        "accessibility_10_asian",
        "incentive_grocery_store_asian",
        "incentive_internet_asian",
        "incentive_sport_tickets_asian",
        "outsideGood_asian",
        "value_black", 
        "penalty_black",
        "accessibility_1_black",
        "accessibility_3_black",
        "accessibility_10_black",
        "incentive_grocery_store_black",
        "incentive_internet_black",
        "incentive_sport_tickets_black",
        "outsideGood_black",
        "value_hispanic", 
        "penalty_hispanic",
        "accessibility_1_hispanic",
        "accessibility_3_hispanic",
        "accessibility_10_hispanic",
        "incentive_grocery_store_hispanic",
        "incentive_internet_hispanic",
        "incentive_sport_tickets_hispanic",
        "outsideGood_hispanic",
        "value_native", 
        "penalty_native",
        "accessibility_1_native",
        "accessibility_3_native",
        "accessibility_10_native",
        "incentive_grocery_store_native",
        "incentive_internet_native",
        "incentive_sport_tickets_native",
        "outsideGood_native"
    )
)

summary(model)

# Education - Bachelor's degreee is reference 
data_education <- dummy_cols(data, c('education'))
view(data_education)

data_education <- data_education%>%
    mutate(
        value_low =  value * ifelse(education_no_hs == 1 | education_hs == 1 | education_degree_associate == 1 | education_college_some == 1,1,0),
        penalty_low =  penalty * ifelse(education_no_hs == 1 | education_hs == 1 | education_degree_associate == 1 | education_college_some == 1,1,0),
        accessibility_1_low = accessibility_1 * ifelse(education_no_hs == 1 | education_hs == 1 | education_degree_associate == 1 | education_college_some == 1,1,0),
        accessibility_3_low = accessibility_3 * ifelse(education_no_hs == 1 | education_hs == 1 | education_degree_associate == 1 | education_college_some == 1,1,0),
        accessibility_10_low  = accessibility_10 * ifelse(education_no_hs == 1 | education_hs == 1 | education_degree_associate == 1 | education_college_some == 1,1,0),
        incentive_grocery_store_low = incentive_grocery_store * ifelse(education_no_hs == 1 | education_hs == 1 | education_degree_associate == 1 | education_college_some == 1,1,0),
        incentive_internet_low  = incentive_internet * ifelse(education_no_hs == 1 | education_hs == 1 | education_degree_associate == 1 | education_college_some == 1,1,0),
        incentive_sport_tickets_low  = incentive_sport_tickets * ifelse(education_no_hs == 1 | education_hs == 1 | education_degree_associate == 1 | education_college_some == 1,1,0),
        outsideGood_low  = outsideGood * ifelse(education_no_hs == 1 | education_hs == 1 | education_degree_associate == 1 | education_college_some == 1,1,0),
        value_high =  value * education_degree_grad,
        penalty_high= penalty * education_degree_grad,
        accessibility_1_high = accessibility_1 * education_degree_grad,
        accessibility_3_high = accessibility_3 * education_degree_grad,
        accessibility_10_high = accessibility_10 * education_degree_grad,
        incentive_grocery_store_high = incentive_grocery_store * education_degree_grad,
        incentive_internet_high = incentive_internet * education_degree_grad,
        incentive_sport_tickets_high = incentive_sport_tickets * education_degree_grad,
        outsideGood_high = outsideGood * education_degree_grad
    )
    
model <- logitr(
    data = data_education,
    outcome = "choice",
    obsID  = "obsID",
    pars   = c(
        "value", 
        "penalty",
        "accessibility_1",
        "accessibility_3",
        "accessibility_10",
        "incentive_grocery_store",
        "incentive_internet",
        "incentive_sport_tickets",
        "outsideGood",
        "value_low", 
        "penalty_low",
        "accessibility_1_low",
        "accessibility_3_low",
        "accessibility_10_low",
        "incentive_grocery_store_low",
        "incentive_internet_low",
        "incentive_sport_tickets_low",
        "outsideGood_low",
        "value_high", 
        "penalty_high",
        "accessibility_1_high",
        "accessibility_3_high",
        "accessibility_10_high",
        "incentive_grocery_store_high",
        "incentive_internet_high",
        "incentive_sport_tickets_high",
        "outsideGood_high"
    )
)

summary(model)
