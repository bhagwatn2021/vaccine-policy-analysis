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

save(
    model,
    file = here("models", "model.RData")
)
# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values

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

model_resistant <- logitr(
    data = data_resistant,
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
summary(model_resistant)

coefs_resistant <- coef(model_resistant)
coefs

ses_resistant <- se(model_resistant)
ses

save(
    model_resistant,
    file = here("models", "model_resistant.RData")
)

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

model_politics <- logitr(
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

summary(model_politics)

coefs_politics <- coef(model_politics)
coefs_politics

ses_politics <- se(model_politics)
ses_politics

save(
    model_politics,
    file = here("models", "model_politics.RData")
)
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
model_income <- logitr(
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

summary(model_income)

coefs_income <- coef(model_income)
coefs_income

ses_income <- se(model_income)
ses_income

save(
    model_income,
    file = here("models", "model_income.RData")
)
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

model_ethnicity <- logitr(
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

summary(model_ethnicity)

coefs_ethnicity <- coef(model_ethnicity)
coefs_ethnicity

ses_ethnicity <- se(model_ethnicity)
ses_ethnicity

save(
    model_ethnicity,
    file = here("models", "model_ethnicity.RData")
)

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
    
model_education <- logitr(
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

summary(model_education)

coefs_education <- coef(model_education)
coefs_education

ses_education <- se(model_education)
ses_education

save(
    model_education,
    file = here("models", "model_education.RData")
)







# PLOTTING 
df_value <- data.frame(value = unique(data$value)) %>% 
    mutate(
        diff    = value - min(value),
        utility = diff*coefs['value'],
        upper =  diff*(coefs['value']+2*ses['value']),
        lower =  diff*(coefs['value']-2*ses['value'])
    )

df_accessibility <- data.frame(
    distance = c(0, 1, 3, 10),
    utility = c(0, coefs['accessibility_1'],coefs['accessibility_3'],coefs['accessibility_10']),
    upper = c(0, coefs['accessibility_1']+2*ses['accessibility_1'], 
              coefs['accessibility_3']+2*ses['accessibility_3'], 
              coefs['accessibility_10']+2*ses['accessibility_10']
    ),
    lower = c(0, coefs['accessibility_1']-2*ses['accessibility_1'], 
              coefs['accessibility_3']-2*ses['accessibility_3'], 
              coefs['accessibility_10']-2*ses['accessibility_10']
    )
)

df_penalty <- data.frame(penalty = unique(data$penalty)) %>% 
    mutate(
        diff    = penalty - min(penalty),
        utility = diff*coefs['penalty'],
        upper =  diff*(coefs['penalty']+2*ses['penalty']),
        lower =  diff*(coefs['penalty']-2*ses['penalty'])
    )

df_incentives <-  data.frame(
    incentive = c("cash", "grocery", "internet", "sports"),
    utility = c(0, coefs['incentive_grocery_store'], 
                coefs['incentive_internet'], 
                coefs['incentive_sport_tickets']
    ),
    upper = c(0, coefs['incentive_grocery_store']+2*ses['incentive_grocery_store'], 
              coefs['incentive_internet']+2*ses['incentive_internet'], 
              coefs['incentive_sport_tickets']+2*ses['incentive_sport_tickets']
    ),
    lower = c(0, coefs['incentive_grocery_store']-2*ses['incentive_grocery_store'], 
              coefs['incentive_internet']-2*ses['incentive_internet'], 
              coefs['incentive_sport_tickets']-2*ses['incentive_sport_tickets']
    )
)





df_resistant_value <- data.frame(value = unique(data_resistant$value)) %>% 
    mutate(
        diff    = value - min(value),
        utility = diff*coefs_resistant['value'],
        upper =  diff*(coefs_resistant['value']+2*ses_resistant['value']),
        lower =  diff*(coefs_resistant['value']-2*ses_resistant['value']),
        utility_resistant = diff*coefs_resistant['value_resistant'],
        upper_resistant =  diff*(coefs_resistant['value_resistant']+2*ses_resistant['value_resistant']),
        lower_resistant =  diff*(coefs_resistant['value_resistant']-2*ses_resistant['value_resistant'])
    )
df_resistant_value

df_resistant_accessibility <- data.frame(
    distance = c(0, 1, 3, 10),
    utility = c(0, coefs_resistant['accessibility_1'],coefs_resistant['accessibility_3'],coefs_resistant['accessibility_10']),
    utility_resistant = c(0, coefs_resistant['accessibility_1_resistant'],coefs_resistant['accessibility_3_resistant'],coefs_resistant['accessibility_10_resistant']),
    upper = c(0, coefs_resistant['accessibility_1']+2*ses_resistant['accessibility_1'], 
              coefs_resistant['accessibility_3']+2*ses_resistant['accessibility_3'], 
              coefs_resistant['accessibility_10']+2*ses_resistant['accessibility_10']
    ),
    upper_resistant = c(0, coefs_resistant['accessibility_1_resistant']+2*ses_resistant['accessibility_1_resistant'], 
                        coefs_resistant['accessibility_3_resistant']+2*ses_resistant['accessibility_3_resistant'], 
                        coefs_resistant['accessibility_10_resistant']+2*ses_resistant['accessibility_10_resistant']
    ),
    lower = c(0, coefs_resistant['accessibility_1']-2*ses_resistant['accessibility_1'], 
              coefs_resistant['accessibility_3']-2*ses_resistant['accessibility_3'], 
              coefs_resistant['accessibility_10']-2*ses_resistant['accessibility_10']
    ),
    lower_resistant = c(0, coefs_resistant['accessibility_1_resistant']-2*ses_resistant['accessibility_1_resistant'], 
                        coefs_resistant['accessibility_3_resistant']-2*ses_resistant['accessibility_3_resistant'], 
                        coefs_resistant['accessibility_10_resistant']-2*ses_resistant['accessibility_10_resistant']
    )
)
df_resistant_accessibility

df_resistant_penalty <- data.frame(penalty = unique(data_resistant$penalty)) %>% 
    mutate(
        diff    = penalty - min(penalty),
        utility = diff*coefs_resistant['penalty'],
        upper =  diff*(coefs_resistant['penalty']+2*ses_resistant['penalty']),
        lower =  diff*(coefs_resistant['penalty']-2*ses_resistant['penalty']),
        utility_resistant = diff*coefs_resistant['penalty_resistant'],
        upper_resistant =  diff*(coefs_resistant['penalty_resistant']+2*ses_resistant['penalty_resistant']),
        lower_resistant =  diff*(coefs_resistant['penalty_resistant']-2*ses_resistant['penalty_resistant'])
    )
df_resistant_penalty

df_resistant_incentives <-  data.frame(
    incentive = c("cash", "grocery", "internet", "sports"),
    utility = c(0, coefs_resistant['incentive_grocery_store'], 
                coefs_resistant['incentive_internet'], 
                coefs_resistant['incentive_sport_tickets']
    ),
    upper = c(0, coefs_resistant['incentive_grocery_store']+2*ses_resistant['incentive_grocery_store'], 
              coefs_resistant['incentive_internet']+2*ses_resistant['incentive_internet'], 
              coefs_resistant['incentive_sport_tickets']+2*ses_resistant['incentive_sport_tickets']
    ),
    lower = c(0, coefs_resistant['incentive_grocery_store']-2*ses_resistant['incentive_grocery_store'], 
              coefs_resistant['incentive_internet']-2*ses_resistant['incentive_internet'], 
              coefs_resistant['incentive_sport_tickets']-2*ses_resistant['incentive_sport_tickets']
    ),
    utility_resistant = c(0, coefs_resistant['incentive_grocery_store_resistant'], 
                          coefs_resistant['incentive_internet_resistant'], 
                          coefs_resistant['incentive_sport_tickets_resistant']
    ),
    upper_resistant = c(0, coefs_resistant['incentive_grocery_store_resistant']+2*ses_resistant['incentive_grocery_store_resistant'], 
                        coefs_resistant['incentive_internet_resistant']+2*ses_resistant['incentive_internet_resistant'], 
                        coefs_resistant['incentive_sport_tickets_resistant']+2*ses_resistant['incentive_sport_tickets_resistant']
    ),
    lower_resistant = c(0, coefs_resistant['incentive_grocery_store_resistant']-2*ses_resistant['incentive_grocery_store_resistant'], 
                        coefs_resistant['incentive_internet_resistant']-2*ses_resistant['incentive_internet_resistant'], 
                        coefs_resistant['incentive_sport_tickets_resistant']-2*ses_resistant['incentive_sport_tickets_resistant']
    )
)

df_resistant_incentives




df_politics_value <- data.frame(value = unique(data_politics$value)) %>% 
    mutate(
        diff    = value - min(value),
        utility = diff*coefs_politics['value'],
        upper =  diff*(coefs_politics['value']+2*ses_politics['value']),
        lower =  diff*(coefs_politics['value']-2*ses_politics['value']),
        utility_liberal = diff*coefs_politics['value_liberal'],
        upper_liberal =  diff*(coefs_politics['value_liberal']+2*ses_politics['value_liberal']),
        lower_liberal =  diff*(coefs_politics['value_liberal']-2*ses_politics['value_liberal']),
        utility_conservative = diff*coefs_politics['value_conservative'],
        upper_conservative  =  diff*(coefs_politics['value_conservative']+2*ses_politics['value_conservative']),
        lower_conservative  =  diff*(coefs_politics['value_conservative']-2*ses_politics['value_conservative']),
        utility_independent = diff*coefs_politics['value_independent'],
        upper_independent  =  diff*(coefs_politics['value_independent']+2*ses_politics['value_independent']),
        lower_independent  =  diff*(coefs_politics['value_independent']-2*ses_politics['value_independent'])
    )
df_politics_value

df_politics_accessibility <- data.frame(
    distance = c(0, 1, 3, 10),
    utility = c(0, coefs_politics['accessibility_1'],coefs_politics['accessibility_3'],coefs_politics['accessibility_10']),
    utility_liberal = c(0, coefs_politics['accessibility_1_liberal'],coefs_politics['accessibility_3_liberal'],coefs_politics['accessibility_10_liberal']),
    utility_conservative = c(0, coefs_politics['accessibility_1_conservative'],coefs_politics['accessibility_3_conservative'],coefs_politics['accessibility_10_conservative']),
    utility_independent = c(0, coefs_politics['accessibility_1_independent'],coefs_politics['accessibility_3_independent'],coefs_politics['accessibility_10_independent']),
    upper = c(0, coefs_politics['accessibility_1']+2*ses_politics['accessibility_1'], 
              coefs_politics['accessibility_3']+2*ses_politics['accessibility_3'], 
              coefs_politics['accessibility_10']+2*ses_politics['accessibility_10']
    ),
    upper_liberal = c(0, coefs_politics['accessibility_1_liberal']+2*ses_politics['accessibility_1_liberal'], 
                        coefs_politics['accessibility_3_liberal']+2*ses_politics['accessibility_3_liberal'], 
                        coefs_politics['accessibility_10_liberal']+2*ses_politics['accessibility_10_liberal']
    ),
    upper_conservative = c(0, coefs_politics['accessibility_1_conservative']+2*ses_politics['accessibility_1_conservative'], 
                           coefs_politics['accessibility_3_conservative']+2*ses_politics['accessibility_3_conservative'], 
                           coefs_politics['accessibility_10_conservative']+2*ses_politics['accessibility_10_conservative']
    ),
    upper_independent = c(0, coefs_politics['accessibility_1_independent']+2*ses_politics['accessibility_1_independent'], 
                           coefs_politics['accessibility_3_independent']+2*ses_politics['accessibility_3_independent'], 
                           coefs_politics['accessibility_10_independent']+2*ses_politics['accessibility_10_independent']
    ),
    lower = c(0, coefs_politics['accessibility_1']-2*ses_politics['accessibility_1'], 
              coefs_politics['accessibility_3']-2*ses_politics['accessibility_3'], 
              coefs_politics['accessibility_10']-2*ses_politics['accessibility_10']
    ),
    lower_liberal = c(0, coefs_politics['accessibility_1_liberal']-2*ses_politics['accessibility_1_liberal'], 
                        coefs_politics['accessibility_3_liberal']-2*ses_politics['accessibility_3_liberal'], 
                        coefs_politics['accessibility_10_liberal']-2*ses_politics['accessibility_10_liberal']
    ),
    lower_conservative = c(0, coefs_politics['accessibility_1_conservative']-2*ses_politics['accessibility_1_conservative'], 
                      coefs_politics['accessibility_3_conservative']-2*ses_politics['accessibility_3_conservative'], 
                      coefs_politics['accessibility_10_conservative']-2*ses_politics['accessibility_10_conservative']
    ),
    lower_independent = c(0, coefs_politics['accessibility_1_independent']-2*ses_politics['accessibility_1_independent'], 
                           coefs_politics['accessibility_3_independent']-2*ses_politics['accessibility_3_independent'], 
                           coefs_politics['accessibility_10_independent']-2*ses_politics['accessibility_10_independent']
    )
)
df_politics_accessibility

df_politics_penalty <- data.frame(penalty = unique(data_politics$penalty)) %>% 
    mutate(
        diff    = penalty - min(penalty),
        utility = diff*coefs_politics['penalty'],
        upper =  diff*(coefs_politics['penalty']+2*ses_politics['penalty']),
        lower =  diff*(coefs_politics['penalty']-2*ses_politics['penalty']),
        utility_liberal = diff*coefs_politics['penalty_liberal'],
        upper_liberal =  diff*(coefs_politics['penalty_liberal']+2*ses_politics['penalty_liberal']),
        lower_liberal =  diff*(coefs_politics['penalty_liberal']-2*ses_politics['penalty_liberal']),
        utility_conservative = diff*coefs_politics['penalty_conservative'],
        upper_conservative =  diff*(coefs_politics['penalty_conservative']+2*ses_politics['penalty_conservative']),
        lower_conservative =  diff*(coefs_politics['penalty_conservative']-2*ses_politics['penalty_conservative']),
        utility_independent = diff*coefs_politics['penalty_independent'],
        upper_independent =  diff*(coefs_politics['penalty_independent']+2*ses_politics['penalty_independent']),
        lower_independent =  diff*(coefs_politics['penalty_independent']-2*ses_politics['penalty_independent'])
        
    )
df_politics_penalty

df_politics_incentives <-  data.frame(
    incentive = c("cash", "grocery", "internet", "sports"),
    utility = c(0, coefs_politics['incentive_grocery_store'], 
                coefs_politics['incentive_internet'], 
                coefs_politics['incentive_sport_tickets']
    ),
    upper = c(0, coefs_politics['incentive_grocery_store']+2*ses_politics['incentive_grocery_store'], 
              coefs_politics['incentive_internet']+2*ses_politics['incentive_internet'], 
              coefs_politics['incentive_sport_tickets']+2*ses_politics['incentive_sport_tickets']
    ),
    lower = c(0, coefs_politics['incentive_grocery_store']-2*ses_politics['incentive_grocery_store'], 
              coefs_politics['incentive_internet']-2*ses_politics['incentive_internet'], 
              coefs_politics['incentive_sport_tickets']-2*ses_politics['incentive_sport_tickets']
    ),
    utility_liberal = c(0, coefs_politics['incentive_grocery_store_liberal'], 
                          coefs_politics['incentive_internet_liberal'], 
                          coefs_politics['incentive_sport_tickets_liberal']
    ),
    upper_liberal = c(0, coefs_politics['incentive_grocery_store_liberal']+2*ses_politics['incentive_grocery_store_liberal'], 
                        coefs_politics['incentive_internet_liberal']+2*ses_politics['incentive_internet_liberal'], 
                        coefs_politics['incentive_sport_tickets_liberal']+2*ses_politics['incentive_sport_tickets_liberal']
    ),
    lower_liberal = c(0, coefs_politics['incentive_grocery_store_liberal']-2*ses_politics['incentive_grocery_store_liberal'], 
                        coefs_politics['incentive_internet_liberal']-2*ses_politics['incentive_internet_liberal'], 
                        coefs_politics['incentive_sport_tickets_liberal']-2*ses_politics['incentive_sport_tickets_liberal']
    ),
    utility_conservative = c(0, coefs_politics['incentive_grocery_store_conservative'], 
                        coefs_politics['incentive_internet_conservative'], 
                        coefs_politics['incentive_sport_tickets_conservative']
    ),
    upper_conservative = c(0, coefs_politics['incentive_grocery_store_conservative']+2*ses_politics['incentive_grocery_store_conservative'], 
                      coefs_politics['incentive_internet_conservative']+2*ses_politics['incentive_internet_conservative'], 
                      coefs_politics['incentive_sport_tickets_conservative']+2*ses_politics['incentive_sport_tickets_conservative']
    ),
    lower_conservative = c(0, coefs_politics['incentive_grocery_store_conservative']-2*ses_politics['incentive_grocery_store_conservative'], 
                      coefs_politics['incentive_internet_conservative']-2*ses_politics['incentive_internet_conservative'], 
                      coefs_politics['incentive_sport_tickets_conservative']-2*ses_politics['incentive_sport_tickets_conservative']
    ),
    utility_independent = c(0, coefs_politics['incentive_grocery_store_independent'], 
                             coefs_politics['incentive_internet_independent'], 
                             coefs_politics['incentive_sport_tickets_independent']
    ),
    upper_independent = c(0, coefs_politics['incentive_grocery_store_independent']+2*ses_politics['incentive_grocery_store_independent'], 
                           coefs_politics['incentive_internet_independent']+2*ses_politics['incentive_internet_independent'], 
                           coefs_politics['incentive_sport_tickets_independent']+2*ses_politics['incentive_sport_tickets_independent']
    ),
    lower_independent = c(0, coefs_politics['incentive_grocery_store_independent']-2*ses_politics['incentive_grocery_store_independent'], 
                           coefs_politics['incentive_internet_independent']-2*ses_politics['incentive_internet_independent'], 
                           coefs_politics['incentive_sport_tickets_independent']-2*ses_politics['incentive_sport_tickets_independent']
    )
)

df_politics_incentives










df_income_value <- data.frame(value = unique(data_income$value)) %>% 
    mutate(
        diff    = value - min(value),
        utility = diff*coefs_income['value'],
        upper =  diff*(coefs_income['value']+2*ses_income['value']),
        lower =  diff*(coefs_income['value']-2*ses_income['value']),
        utility_low = diff*coefs_income['value_low'],
        upper_low =  diff*(coefs_income['value_low']+2*ses_income['value_low']),
        lower_low =  diff*(coefs_income['value_low']-2*ses_income['value_low']),
        utility_high = diff*coefs_income['value_high'],
        upper_high  =  diff*(coefs_income['value_high']+2*ses_income['value_high']),
        lower_high  =  diff*(coefs_income['value_high']-2*ses_income['value_high'])
    )

df_income_value

df_income_accessibility <- data.frame(
    distance = c(0, 1, 3, 10),
    utility = c(0, coefs_income['accessibility_1'],coefs_income['accessibility_3'],coefs_income['accessibility_10']),
    utility_low = c(0, coefs_income['accessibility_1_low'],coefs_income['accessibility_3_low'],coefs_income['accessibility_10_low']),
    utility_high = c(0, coefs_income['accessibility_1_high'],coefs_income['accessibility_3_high'],coefs_income['accessibility_10_high']),
    upper = c(0, coefs_income['accessibility_1']+2*ses_income['accessibility_1'], 
              coefs_income['accessibility_3']+2*ses_income['accessibility_3'], 
              coefs_income['accessibility_10']+2*ses_income['accessibility_10']
    ),
    upper_low = c(0, coefs_income['accessibility_1_low']+2*ses_income['accessibility_1_low'], 
                      coefs_income['accessibility_3_low']+2*ses_income['accessibility_3_low'], 
                      coefs_income['accessibility_10_low']+2*ses_income['accessibility_10_low']
    ),
    upper_high = c(0, coefs_income['accessibility_1_high']+2*ses_income['accessibility_1_high'], 
                          coefs_income['accessibility_3_high']+2*ses_income['accessibility_3_high'], 
                          coefs_income['accessibility_10_high']+2*ses_income['accessibility_10_high']
    ),
    lower = c(0, coefs_income['accessibility_1']-2*ses_income['accessibility_1'], 
              coefs_income['accessibility_3']-2*ses_income['accessibility_3'], 
              coefs_income['accessibility_10']-2*ses_income['accessibility_10']
    ),
    lower_low = c(0, coefs_income['accessibility_1_low']-2*ses_income['accessibility_1_low'], 
                      coefs_income['accessibility_3_low']-2*ses_income['accessibility_3_low'], 
                      coefs_income['accessibility_10_low']-2*ses_income['accessibility_10_low']
    ),
    lower_high = c(0, coefs_income['accessibility_1_high']-2*ses_income['accessibility_1_high'], 
                           coefs_income['accessibility_3_high']-2*ses_income['accessibility_3_high'], 
                           coefs_income['accessibility_10_high']-2*ses_income['accessibility_10_high']
    )
)
df_income_accessibility

df_income_penalty <- data.frame(penalty = unique(data_income$penalty)) %>% 
    mutate(
        diff    = penalty - min(penalty),
        utility = diff*coefs_income['penalty'],
        upper =  diff*(coefs_income['penalty']+2*ses_income['penalty']),
        lower =  diff*(coefs_income['penalty']-2*ses_income['penalty']),
        utility_low = diff*coefs_income['penalty_low'],
        upper_low =  diff*(coefs_income['penalty_low']+2*ses_income['penalty_low']),
        lower_low =  diff*(coefs_income['penalty_low']-2*ses_income['penalty_low']),
        utility_high = diff*coefs_income['penalty_high'],
        upper_high =  diff*(coefs_income['penalty_high']+2*ses_income['penalty_high']),
        lower_high =  diff*(coefs_income['penalty_high']-2*ses_income['penalty_high'])
    )

df_income_penalty

df_income_incentives <-  data.frame(
    incentive = c("cash", "grocery", "internet", "sports"),
    utility = c(0, coefs_income['incentive_grocery_store'], 
                coefs_income['incentive_internet'], 
                coefs_income['incentive_sport_tickets']
    ),
    upper = c(0, coefs_income['incentive_grocery_store']+2*ses_income['incentive_grocery_store'], 
              coefs_income['incentive_internet']+2*ses_income['incentive_internet'], 
              coefs_income['incentive_sport_tickets']+2*ses_income['incentive_sport_tickets']
    ),
    lower = c(0, coefs_income['incentive_grocery_store']-2*ses_income['incentive_grocery_store'], 
              coefs_income['incentive_internet']-2*ses_income['incentive_internet'], 
              coefs_income['incentive_sport_tickets']-2*ses_income['incentive_sport_tickets']
    ),
    utility_low = c(0, coefs_income['incentive_grocery_store_low'], 
                        coefs_income['incentive_internet_low'], 
                        coefs_income['incentive_sport_tickets_low']
    ),
    upper_low = c(0, coefs_income['incentive_grocery_store_low']+2*ses_income['incentive_grocery_store_low'], 
                      coefs_income['incentive_internet_low']+2*ses_income['incentive_internet_low'], 
                      coefs_income['incentive_sport_tickets_low']+2*ses_income['incentive_sport_tickets_low']
    ),
    lower_low = c(0, coefs_income['incentive_grocery_store_low']-2*ses_income['incentive_grocery_store_low'], 
                      coefs_income['incentive_internet_low']-2*ses_income['incentive_internet_low'], 
                      coefs_income['incentive_sport_tickets_low']-2*ses_income['incentive_sport_tickets_low']
    ),
    utility_high = c(0, coefs_income['incentive_grocery_store_high'], 
                             coefs_income['incentive_internet_high'], 
                             coefs_income['incentive_sport_tickets_high']
    ),
    upper_high = c(0, coefs_income['incentive_grocery_store_high']+2*ses_income['incentive_grocery_store_high'], 
                           coefs_income['incentive_internet_high']+2*ses_income['incentive_internet_high'], 
                           coefs_income['incentive_sport_tickets_high']+2*ses_income['incentive_sport_tickets_high']
    ),
    lower_high = c(0, coefs_income['incentive_grocery_store_high']-2*ses_income['incentive_grocery_store_high'], 
                           coefs_income['incentive_internet_high']-2*ses_income['incentive_internet_high'], 
                           coefs_income['incentive_sport_tickets_high']-2*ses_income['incentive_sport_tickets_high']
    )
)
df_income_incentives






df_education_value <- data.frame(value = unique(data_education$value)) %>% 
    mutate(
        diff    = value - min(value),
        utility = diff*coefs_education['value'],
        upper =  diff*(coefs_education['value']+2*ses_education['value']),
        lower =  diff*(coefs_education['value']-2*ses_education['value']),
        utility_low = diff*coefs_education['value_low'],
        upper_low =  diff*(coefs_education['value_low']+2*ses_education['value_low']),
        lower_low =  diff*(coefs_education['value_low']-2*ses_education['value_low']),
        utility_high = diff*coefs_education['value_high'],
        upper_high  =  diff*(coefs_education['value_high']+2*ses_education['value_high']),
        lower_high  =  diff*(coefs_education['value_high']-2*ses_education['value_high'])
    )

df_education_value

df_education_accessibility <- data.frame(
    distance = c(0, 1, 3, 10),
    utility = c(0, coefs_education['accessibility_1'],coefs_education['accessibility_3'],coefs_education['accessibility_10']),
    utility_low = c(0, coefs_education['accessibility_1_low'],coefs_education['accessibility_3_low'],coefs_education['accessibility_10_low']),
    utility_high = c(0, coefs_education['accessibility_1_high'],coefs_education['accessibility_3_high'],coefs_education['accessibility_10_high']),
    upper = c(0, coefs_education['accessibility_1']+2*ses_education['accessibility_1'], 
              coefs_education['accessibility_3']+2*ses_education['accessibility_3'], 
              coefs_education['accessibility_10']+2*ses_education['accessibility_10']
    ),
    upper_low = c(0, coefs_education['accessibility_1_low']+2*ses_education['accessibility_1_low'], 
                  coefs_education['accessibility_3_low']+2*ses_education['accessibility_3_low'], 
                  coefs_education['accessibility_10_low']+2*ses_education['accessibility_10_low']
    ),
    upper_high = c(0, coefs_education['accessibility_1_high']+2*ses_education['accessibility_1_high'], 
                   coefs_education['accessibility_3_high']+2*ses_education['accessibility_3_high'], 
                   coefs_education['accessibility_10_high']+2*ses_education['accessibility_10_high']
    ),
    lower = c(0, coefs_education['accessibility_1']-2*ses_education['accessibility_1'], 
              coefs_education['accessibility_3']-2*ses_education['accessibility_3'], 
              coefs_education['accessibility_10']-2*ses_education['accessibility_10']
    ),
    lower_low = c(0, coefs_education['accessibility_1_low']-2*ses_education['accessibility_1_low'], 
                  coefs_education['accessibility_3_low']-2*ses_education['accessibility_3_low'], 
                  coefs_education['accessibility_10_low']-2*ses_education['accessibility_10_low']
    ),
    lower_high = c(0, coefs_education['accessibility_1_high']-2*ses_education['accessibility_1_high'], 
                   coefs_education['accessibility_3_high']-2*ses_education['accessibility_3_high'], 
                   coefs_education['accessibility_10_high']-2*ses_education['accessibility_10_high']
    )
)
df_education_accessibility

df_education_penalty <- data.frame(penalty = unique(data_education$penalty)) %>% 
    mutate(
        diff    = penalty - min(penalty),
        utility = diff*coefs_education['penalty'],
        upper =  diff*(coefs_education['penalty']+2*ses_education['penalty']),
        lower =  diff*(coefs_education['penalty']-2*ses_education['penalty']),
        utility_low = diff*coefs_education['penalty_low'],
        upper_low =  diff*(coefs_education['penalty_low']+2*ses_education['penalty_low']),
        lower_low =  diff*(coefs_education['penalty_low']-2*ses_education['penalty_low']),
        utility_high = diff*coefs_education['penalty_high'],
        upper_high =  diff*(coefs_education['penalty_high']+2*ses_education['penalty_high']),
        lower_high =  diff*(coefs_education['penalty_high']-2*ses_education['penalty_high'])
    )

df_education_penalty

df_education_incentives <-  data.frame(
    incentive = c("cash", "grocery", "internet", "sports"),
    utility = c(0, coefs_education['incentive_grocery_store'], 
                coefs_education['incentive_internet'], 
                coefs_education['incentive_sport_tickets']
    ),
    upper = c(0, coefs_education['incentive_grocery_store']+2*ses_education['incentive_grocery_store'], 
              coefs_education['incentive_internet']+2*ses_education['incentive_internet'], 
              coefs_education['incentive_sport_tickets']+2*ses_education['incentive_sport_tickets']
    ),
    lower = c(0, coefs_education['incentive_grocery_store']-2*ses_education['incentive_grocery_store'], 
              coefs_education['incentive_internet']-2*ses_education['incentive_internet'], 
              coefs_education['incentive_sport_tickets']-2*ses_education['incentive_sport_tickets']
    ),
    utility_low = c(0, coefs_education['incentive_grocery_store_low'], 
                    coefs_education['incentive_internet_low'], 
                    coefs_education['incentive_sport_tickets_low']
    ),
    upper_low = c(0, coefs_education['incentive_grocery_store_low']+2*ses_education['incentive_grocery_store_low'], 
                  coefs_education['incentive_internet_low']+2*ses_education['incentive_internet_low'], 
                  coefs_education['incentive_sport_tickets_low']+2*ses_education['incentive_sport_tickets_low']
    ),
    lower_low = c(0, coefs_education['incentive_grocery_store_low']-2*ses_education['incentive_grocery_store_low'], 
                  coefs_education['incentive_internet_low']-2*ses_education['incentive_internet_low'], 
                  coefs_education['incentive_sport_tickets_low']-2*ses_education['incentive_sport_tickets_low']
    ),
    utility_high = c(0, coefs_education['incentive_grocery_store_high'], 
                     coefs_education['incentive_internet_high'], 
                     coefs_education['incentive_sport_tickets_high']
    ),
    upper_high = c(0, coefs_education['incentive_grocery_store_high']+2*ses_education['incentive_grocery_store_high'], 
                   coefs_education['incentive_internet_high']+2*ses_education['incentive_internet_high'], 
                   coefs_education['incentive_sport_tickets_high']+2*ses_education['incentive_sport_tickets_high']
    ),
    lower_high = c(0, coefs_education['incentive_grocery_store_high']-2*ses_education['incentive_grocery_store_high'], 
                   coefs_education['incentive_internet_high']-2*ses_education['incentive_internet_high'], 
                   coefs_education['incentive_sport_tickets_high']-2*ses_education['incentive_sport_tickets_high']
    )
)
df_education_incentives



df_ethnicity_value <- data.frame(value = unique(data_ethnicity$value)) %>% 
    mutate(
        diff    = value - min(value),
        utility = diff*coefs_ethnicity['value'],
        upper =  diff*(coefs_ethnicity['value']+2*ses_ethnicity['value']),
        lower =  diff*(coefs_ethnicity['value']-2*ses_ethnicity['value']),
        utility_asian = diff*coefs_ethnicity['value_asian'],
        upper_asian =  diff*(coefs_ethnicity['value_asian']+2*ses_ethnicity['value_asian']),
        lower_asian =  diff*(coefs_ethnicity['value_asian']-2*ses_ethnicity['value_asian']),
        utility_black = diff*coefs_ethnicity['value_black'],
        upper_black  =  diff*(coefs_ethnicity['value_black']+2*ses_ethnicity['value_black']),
        lower_black  =  diff*(coefs_ethnicity['value_black']-2*ses_ethnicity['value_black']),
        utility_hispanic = diff*coefs_ethnicity['value_hispanic'],
        upper_hispanic =  diff*(coefs_ethnicity['value_hispanic']+2*ses_ethnicity['value_hispanic']),
        lower_hispanic =  diff*(coefs_ethnicity['value_hispanic']-2*ses_ethnicity['value_hispanic']),
        utility_native = diff*coefs_ethnicity['value_native'],
        upper_native  =  diff*(coefs_ethnicity['value_native']+2*ses_ethnicity['value_native']),
        lower_native  =  diff*(coefs_ethnicity['value_native']-2*ses_ethnicity['value_native'])
    )

df_ethnicity_value

df_ethnicity_accessibility <- data.frame(
    distance = c(0, 1, 3, 10),
    utility = c(0, coefs_ethnicity['accessibility_1'],coefs_ethnicity['accessibility_3'],coefs_ethnicity['accessibility_10']),
    upper = c(0, coefs_ethnicity['accessibility_1']+2*ses_ethnicity['accessibility_1'], 
              coefs_ethnicity['accessibility_3']+2*ses_ethnicity['accessibility_3'], 
              coefs_ethnicity['accessibility_10']+2*ses_ethnicity['accessibility_10']
    ),
    lower = c(0, coefs_ethnicity['accessibility_1']-2*ses_ethnicity['accessibility_1'], 
              coefs_ethnicity['accessibility_3']-2*ses_ethnicity['accessibility_3'], 
              coefs_ethnicity['accessibility_10']-2*ses_ethnicity['accessibility_10']
    ),
    utility_asian = c(0, coefs_ethnicity['accessibility_1_asian'],coefs_ethnicity['accessibility_3_asian'],coefs_ethnicity['accessibility_10_asian']),
    upper_asian = c(0, coefs_ethnicity['accessibility_1_asian']+2*ses_ethnicity['accessibility_1_asian'], 
                    coefs_ethnicity['accessibility_3_asian']+2*ses_ethnicity['accessibility_3_asian'], 
                    coefs_ethnicity['accessibility_10_asian']+2*ses_ethnicity['accessibility_10_asian']
    ),
    lower_asian = c(0, coefs_ethnicity['accessibility_1_asian']-2*ses_ethnicity['accessibility_1_asian'], 
                    coefs_ethnicity['accessibility_3_asian']-2*ses_ethnicity['accessibility_3_asian'], 
                    coefs_ethnicity['accessibility_10_asian']-2*ses_ethnicity['accessibility_10_asian']
    ),
    utility_black = c(0, coefs_ethnicity['accessibility_1_black'],coefs_ethnicity['accessibility_3_black'],coefs_ethnicity['accessibility_10_black']),
    upper_black = c(0, coefs_ethnicity['accessibility_1_black']+2*ses_ethnicity['accessibility_1_black'], 
                    coefs_ethnicity['accessibility_3_black']+2*ses_ethnicity['accessibility_3_black'], 
                    coefs_ethnicity['accessibility_10_black']+2*ses_ethnicity['accessibility_10_black']
    ),
    lower_black = c(0, coefs_ethnicity['accessibility_1_black']-2*ses_ethnicity['accessibility_1_black'], 
                    coefs_ethnicity['accessibility_3_black']-2*ses_ethnicity['accessibility_3_black'], 
                    coefs_ethnicity['accessibility_10_black']-2*ses_ethnicity['accessibility_10_black']
    ),
    utility_hispanic = c(0, coefs_ethnicity['accessibility_1_hispanic'],coefs_ethnicity['accessibility_3_hispanic'],coefs_ethnicity['accessibility_10_hispanic']),
    upper_hispanic = c(0, coefs_ethnicity['accessibility_1_hispanic']+2*ses_ethnicity['accessibility_1_hispanic'], 
                    coefs_ethnicity['accessibility_3_hispanic']+2*ses_ethnicity['accessibility_3_hispanic'], 
                    coefs_ethnicity['accessibility_10_hispanic']+2*ses_ethnicity['accessibility_10_hispanic']
    ),
    lower_hispanic = c(0, coefs_ethnicity['accessibility_1_hispanic']-2*ses_ethnicity['accessibility_1_hispanic'], 
                    coefs_ethnicity['accessibility_3_hispanic']-2*ses_ethnicity['accessibility_3_hispanic'], 
                    coefs_ethnicity['accessibility_10_hispanic']-2*ses_ethnicity['accessibility_10_hispanic']
    ),
    utility_native = c(0, coefs_ethnicity['accessibility_1_native'],coefs_ethnicity['accessibility_3_native'],coefs_ethnicity['accessibility_10_native']),
    upper_native = c(0, coefs_ethnicity['accessibility_1_native']+2*ses_ethnicity['accessibility_1_native'], 
                       coefs_ethnicity['accessibility_3_native']+2*ses_ethnicity['accessibility_3_native'], 
                       coefs_ethnicity['accessibility_10_native']+2*ses_ethnicity['accessibility_10_native']
    ),
    lower_native = c(0, coefs_ethnicity['accessibility_1_native']-2*ses_ethnicity['accessibility_1_native'], 
                       coefs_ethnicity['accessibility_3_native']-2*ses_ethnicity['accessibility_3_native'], 
                       coefs_ethnicity['accessibility_10_native']-2*ses_ethnicity['accessibility_10_native']
    )
)

df_ethnicity_accessibility

df_ethnicity_penalty <- data.frame(penalty = unique(data_ethnicity$penalty)) %>% 
    mutate(
        diff    = penalty - min(penalty),
        utility = diff*coefs_ethnicity['penalty'],
        upper =  diff*(coefs_ethnicity['penalty']+2*ses_ethnicity['penalty']),
        lower =  diff*(coefs_ethnicity['penalty']-2*ses_ethnicity['penalty']),
        utility_asian = diff*coefs_ethnicity['penalty_asian'],
        upper_asian =  diff*(coefs_ethnicity['penalty_asian']+2*ses_ethnicity['penalty_asian']),
        lower_asian =  diff*(coefs_ethnicity['penalty_asian']-2*ses_ethnicity['penalty_asian']),
        utility_black = diff*coefs_ethnicity['penalty_black'],
        upper_black =  diff*(coefs_ethnicity['penalty_black']+2*ses_ethnicity['penalty_black']),
        lower_black =  diff*(coefs_ethnicity['penalty_black']-2*ses_ethnicity['penalty_black']),
        utility_hispanic = diff*coefs_ethnicity['penalty_hispanic'],
        upper_hispanic =  diff*(coefs_ethnicity['penalty_hispanic']+2*ses_ethnicity['penalty_hispanic']),
        lower_hispanic =  diff*(coefs_ethnicity['penalty_hispanic']-2*ses_ethnicity['penalty_hispanic']),
        utility_native = diff*coefs_ethnicity['penalty_native'],
        upper_native =  diff*(coefs_ethnicity['penalty_native']+2*ses_ethnicity['penalty_native']),
        lower_native =  diff*(coefs_ethnicity['penalty_native']-2*ses_ethnicity['penalty_native'])
    )

df_ethnicity_penalty

df_ethnicity_incentives <-  data.frame(
    incentive = c("cash", "grocery", "internet", "sports"),
    utility = c(0, coefs_ethnicity['incentive_grocery_store'], 
                coefs_ethnicity['incentive_internet'], 
                coefs_ethnicity['incentive_sport_tickets']
    ),
    upper = c(0, coefs_ethnicity['incentive_grocery_store']+2*ses_ethnicity['incentive_grocery_store'], 
              coefs_ethnicity['incentive_internet']+2*ses_ethnicity['incentive_internet'], 
              coefs_ethnicity['incentive_sport_tickets']+2*ses_ethnicity['incentive_sport_tickets']
    ),
    lower = c(0, coefs_ethnicity['incentive_grocery_store']-2*ses_ethnicity['incentive_grocery_store'], 
              coefs_ethnicity['incentive_internet']-2*ses_ethnicity['incentive_internet'], 
              coefs_ethnicity['incentive_sport_tickets']-2*ses_ethnicity['incentive_sport_tickets']
    ),
    utility_asian = c(0, coefs_ethnicity['incentive_grocery_store_asian'], 
                    coefs_ethnicity['incentive_internet_asian'], 
                    coefs_ethnicity['incentive_sport_tickets_asian']
    ),
    upper_asian = c(0, coefs_ethnicity['incentive_grocery_store_asian']+2*ses_ethnicity['incentive_grocery_store_asian'], 
                  coefs_ethnicity['incentive_internet_asian']+2*ses_ethnicity['incentive_internet_asian'], 
                  coefs_ethnicity['incentive_sport_tickets_asian']+2*ses_ethnicity['incentive_sport_tickets_asian']
    ),
    lower_asian = c(0, coefs_ethnicity['incentive_grocery_store_asian']-2*ses_ethnicity['incentive_grocery_store_asian'], 
                  coefs_ethnicity['incentive_internet_asian']-2*ses_ethnicity['incentive_internet_asian'], 
                  coefs_ethnicity['incentive_sport_tickets_asian']-2*ses_ethnicity['incentive_sport_tickets_asian']
    ),
    utility_black = c(0, coefs_ethnicity['incentive_grocery_store_black'], 
                      coefs_ethnicity['incentive_internet_black'], 
                      coefs_ethnicity['incentive_sport_tickets_black']
    ),
    upper_black = c(0, coefs_ethnicity['incentive_grocery_store_black']+2*ses_ethnicity['incentive_grocery_store_black'], 
                    coefs_ethnicity['incentive_internet_black']+2*ses_ethnicity['incentive_internet_black'], 
                    coefs_ethnicity['incentive_sport_tickets_black']+2*ses_ethnicity['incentive_sport_tickets_black']
    ),
    lower_black = c(0, coefs_ethnicity['incentive_grocery_store_black']-2*ses_ethnicity['incentive_grocery_store_black'], 
                    coefs_ethnicity['incentive_internet_black']-2*ses_ethnicity['incentive_internet_black'], 
                    coefs_ethnicity['incentive_sport_tickets_black']-2*ses_ethnicity['incentive_sport_tickets_black']
    ),
    utility_native = c(0, coefs_ethnicity['incentive_grocery_store_native'], 
                      coefs_ethnicity['incentive_internet_native'], 
                      coefs_ethnicity['incentive_sport_tickets_native']
    ),
    upper_native = c(0, coefs_ethnicity['incentive_grocery_store_native']+2*ses_ethnicity['incentive_grocery_store_native'], 
                    coefs_ethnicity['incentive_internet_native']+2*ses_ethnicity['incentive_internet_native'], 
                    coefs_ethnicity['incentive_sport_tickets_native']+2*ses_ethnicity['incentive_sport_tickets_native']
    ),
    lower_native = c(0, coefs_ethnicity['incentive_grocery_store_native']-2*ses_ethnicity['incentive_grocery_store_native'], 
                    coefs_ethnicity['incentive_internet_native']-2*ses_ethnicity['incentive_internet_native'], 
                    coefs_ethnicity['incentive_sport_tickets_native']-2*ses_ethnicity['incentive_sport_tickets_native']
    ),
    utility_hispanic = c(0, coefs_ethnicity['incentive_grocery_store_hispanic'], 
                      coefs_ethnicity['incentive_internet_hispanic'], 
                      coefs_ethnicity['incentive_sport_tickets_hispanic']
    ),
    upper_hispanic = c(0, coefs_ethnicity['incentive_grocery_store_hispanic']+2*ses_ethnicity['incentive_grocery_store_hispanic'], 
                   coefs_ethnicity['incentive_internet_hispanic']+2*ses_ethnicity['incentive_internet_hispanic'], 
                   coefs_ethnicity['incentive_sport_tickets_hispanic']+2*ses_ethnicity['incentive_sport_tickets_hispanic']
    ),
    lower_hispanic = c(0, coefs_ethnicity['incentive_grocery_store_hispanic']-2*ses_ethnicity['incentive_grocery_store_hispanic'], 
                   coefs_ethnicity['incentive_internet_hispanic']-2*ses_ethnicity['incentive_internet_hispanic'], 
                   coefs_ethnicity['incentive_sport_tickets_hispanic']-2*ses_ethnicity['incentive_sport_tickets_hispanic']
    )
)
df_ethnicity_incentives

# Get upper and lower bounds (plots should have the same y-axis)
utility <- c(
    df_value$upper, df_penalty$upper, 
    df_incentives$upper, df_accessibility$upper,
    df_value$lower, df_penalty$lower, 
    df_incentives$lower, df_accessibility$lower,
   
    
    df_resistant_value$upper, df_resistant_penalty$upper, 
    df_resistant_incentives$upper, df_resistant_accessibility$upper,
    df_resistant_value$lower, df_resistant_penalty$lower, 
    df_resistant_incentives$lower, df_resistant_accessibility$lower,
    
    df_resistant_value$upper_resistant, df_resistant_penalty$upper_resistant, 
    df_resistant_incentives$upper_resistant, df_resistant_accessibility$upper_resistant,
    df_resistant_value$lower_resistant, df_resistant_penalty$lower_resistant, 
    df_resistant_incentives$lower_resistant, df_resistant_accessibility$lower_resistant,
   
   
    
    df_politics_value$upper, df_politics_penalty$upper, 
    df_politics_incentives$upper, df_politics_accessibility$upper,
    df_politics_value$lower, df_politics_penalty$lower, 
    df_politics_incentives$lower, df_politics_accessibility$lower,
    
    df_politics_value$upper_liberal, df_politics_penalty$upper_liberal, 
    df_politics_incentives$upper_liberal, df_politics_accessibility$upper_liberal,
    df_politics_value$lower_liberal, df_politics_penalty$lower_liberal, 
    df_politics_incentives$lower_liberal, df_politics_accessibility$lower_liberal,
    
    df_politics_value$upper_conservative, df_politics_penalty$upper_conservative, 
    df_politics_incentives$upper_conservative, df_politics_accessibility$upper_conservative,
    df_politics_value$lower_conservative, df_politics_penalty$lower_conservative, 
    df_politics_incentives$lower_conservative, df_politics_accessibility$lower_conservative, 
    
    df_politics_value$upper_independent, df_politics_penalty$upper_independent, 
    df_politics_incentives$upper_independent, df_politics_accessibility$upper_independent,
    df_politics_value$lower_independent, df_politics_penalty$lower_independent, 
    df_politics_incentives$lower_independent, df_politics_accessibility$lower_independent,
    
    
    
    df_income_value$upper, df_income_penalty$upper, 
    df_income_incentives$upper, df_income_accessibility$upper,
    df_income_value$lower, df_income_penalty$lower, 
    df_income_incentives$lower, df_income_accessibility$lower,
    
    df_income_value$upper_low, df_income_penalty$upper_low, 
    df_income_incentives$upper_low, df_income_accessibility$upper_low,
    df_income_value$lower_low, df_income_penalty$lower_low, 
    df_income_incentives$lower_low, df_income_accessibility$lower_low,
    
    df_income_value$upper_high, df_income_penalty$upper_high, 
    df_income_incentives$upper_high, df_income_accessibility$upper_high,
    df_income_value$lower_high, df_income_penalty$lower_high, 
    df_income_incentives$lower_high, df_income_accessibility$lower_high, 
    
    
    
    df_ethnicity_value$upper, df_ethnicity_penalty$upper, 
    df_ethnicity_incentives$upper, df_ethnicity_accessibility$upper,
    df_ethnicity_value$lower, df_ethnicity_penalty$lower, 
    df_ethnicity_incentives$lower, df_ethnicity_accessibility$lower,
    
    df_ethnicity_value$upper_asian, df_ethnicity_penalty$upper_asian, 
    df_ethnicity_incentives$upper_asian, df_ethnicity_accessibility$upper_asian,
    df_ethnicity_value$lower_asian, df_ethnicity_penalty$lower_asian, 
    df_ethnicity_incentives$lower_asian, df_ethnicity_accessibility$lower_asian,
    
    df_ethnicity_value$upper_black, df_ethnicity_penalty$upper_black, 
    df_ethnicity_incentives$upper_black, df_ethnicity_accessibility$upper_black,
    df_ethnicity_value$lower_black, df_ethnicity_penalty$lower_black, 
    df_ethnicity_incentives$lower_black, df_ethnicity_accessibility$lower_black, 
    
    df_ethnicity_value$upper_hispanic, df_ethnicity_penalty$upper_hispanic, 
    df_ethnicity_incentives$upper_hispanic, df_ethnicity_accessibility$upper_hispanic,
    df_ethnicity_value$lower_hispanic, df_ethnicity_penalty$lower_hispanic, 
    df_ethnicity_incentives$lower_hispanic, df_ethnicity_accessibility$lower_hispanic,
    
    df_ethnicity_value$upper_native, df_ethnicity_penalty$upper_native, 
    df_ethnicity_incentives$upper_native, df_ethnicity_accessibility$upper_native,
    df_ethnicity_value$lower_native, df_ethnicity_penalty$lower_native, 
    df_ethnicity_incentives$lower_native, df_ethnicity_accessibility$lower_native,
    
    
    
    df_education_value$upper, df_education_penalty$upper, 
    df_education_incentives$upper, df_education_accessibility$upper,
    df_education_value$lower, df_education_penalty$lower, 
    df_education_incentives$lower, df_education_accessibility$lower,
    
    df_education_value$upper_low, df_education_penalty$upper_low, 
    df_education_incentives$upper_low, df_education_accessibility$upper_low,
    df_education_value$lower_low, df_education_penalty$lower_low, 
    df_education_incentives$lower_low, df_education_accessibility$lower_low,
    
    df_education_value$upper_high, df_education_penalty$upper_high, 
    df_education_incentives$upper_high, df_education_accessibility$upper_high,
    df_education_value$lower_high, df_education_penalty$lower_high, 
    df_education_incentives$lower_high, df_education_accessibility$lower_high
    
) 

ymin <- floor(min(utility))
ymax <- ceiling(max(utility))
ymin
ymax

# Plot the utility for each attribute
plot_value <- df_value %>% 
    ggplot(aes(x = value, y = utility,ymin=lower,ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility') +
    theme_bw()

plot_value

plot_accessibility <- df_accessibility %>% 
    ggplot(aes(x = distance, y = utility,ymin=lower,ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility') +
    theme_bw()

plot_accessibility

plot_penalty<- df_penalty %>% 
    ggplot(aes(x = penalty, y = utility, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility') +
    theme_bw()

plot_penalty

plot_incentives <- df_incentives %>% 
    ggplot(aes(x = incentive, y = utility, ymin=lower, ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility') +
    theme_bw()

plot_incentives


plot_continuous_attributes <- plot_grid(
    plot_value, plot_penalty,
    nrow = 1
)

plot_continuous_attributes 

# Save plots 
ggsave(
    filename = here('data', 'plot_continuous_attributes.png'), 
    plot = plot_continuous_attributes , 
    width = 10, height = 10)

plot_categorical_attributes <- plot_grid(
    plot_accessibility, plot_incentives,
    nrow = 1
)

plot_categorical_attributes 

# Save plots 
ggsave(
    filename = here('data', 'plot_categorical_attributes.png'), 
    plot = plot_categorical_attributes , 
    width = 10, height = 10)

# Plot the utility for each attribute
plot_resistant_value <- df_resistant_value %>% 
    ggplot(aes(x = value, y = utility,ymin=lower,ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility') +
    theme_bw()

plot_resistant_value

plot_resistant_value_resistant <- df_resistant_value %>% 
    ggplot(aes(x = value, y = utility_resistant,ymin=lower_resistant,ymax=upper_resistant)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility for resistant respondents') +
    theme_bw()

plot_resistant_value_resistant

plot_resistant_accessibility <- df_resistant_accessibility %>% 
    ggplot(aes(x = distance, y = utility,ymin=lower,ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility') +
    theme_bw()

plot_resistant_accessibility

plot_resistant_accessibility_resistant <- df_resistant_accessibility %>% 
    ggplot(aes(x = distance, y = utility_resistant,ymin=lower_resistant,ymax=upper_resistant)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility for resistant respondents') +
    theme_bw()

plot_resistant_accessibility_resistant

plot_resistant_penalty <- df_resistant_penalty %>% 
    ggplot(aes(x = penalty, y = utility, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility') +
    theme_bw()

plot_resistant_penalty

plot_resistant_penalty_resistant <- df_resistant_penalty %>% 
    ggplot(aes(x = penalty, y = utility_resistant, ymin=lower_resistant, ymax=upper_resistant)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance($)', y = 'Utility for resistant respondents') +
    theme_bw()

plot_resistant_penalty_resistant

plot_resistant_incentives <- df_resistant_incentives %>% 
    ggplot(aes(x = incentive, y = utility, ymin=lower, ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility') +
    theme_bw()

plot_resistant_incentives

plot_resistant_incentives_resistant <- df_resistant_incentives %>% 
    ggplot(aes(x = incentive, y = utility_resistant, ymin=lower_resistant, ymax=upper_resistant)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility for resistant respondents') +
    theme_bw()

plot_resistant_incentives_resistant

plot_resistant_continuous_attributes <- plot_grid(
    plot_resistant_value, plot_resistant_value_resistant, plot_resistant_penalty, plot_resistant_penalty_resistant,
    nrow = 2
)

plot_resistant_continuous_attributes

# Save plots 
ggsave(
    filename = here('data', 'plot_resistant_continuous_attributes.png'), 
    plot = plot_resistant_continuous_attributes, 
    width = 10, height = 10)

plot_resistant_categorical_attributes <- plot_grid(
    plot_resistant_accessibility, plot_resistant_accessibility_resistant, plot_resistant_incentives, plot_resistant_incentives_resistant,
    nrow = 2
)

plot_resistant_categorical_attributes 

# Save plots 
ggsave(
    filename = here('data', 'plot_resistant_categorical_attributes.png'), 
    plot = plot_resistant_continuous_attributes, 
    width = 10, height = 10)


# Plot the utility for each attribute
plot_politics_value <- df_politics_value %>% 
    ggplot(aes(x = value, y = utility,ymin=lower,ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility') +
    theme_bw()

plot_politics_value

plot_politics_value_liberal <- df_politics_value %>% 
    ggplot(aes(x = value, y = utility_liberal,ymin=lower_liberal,ymax=upper_liberal)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility for liberal respondents') +
    theme_bw()

plot_politics_value_liberal

plot_politics_value_conservative <- df_politics_value %>% 
    ggplot(aes(x = value, y = utility_conservative,ymin=lower_conservative,ymax=upper_conservative)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility for conservative respondents') +
    theme_bw()

plot_politics_value_conservative

plot_politics_value_independent <- df_politics_value %>% 
    ggplot(aes(x = value, y = utility_independent,ymin=lower_independent,ymax=upper_independent)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility for independent respondents') +
    theme_bw()

plot_politics_value_independent

plot_politics_accessibility <- df_politics_accessibility %>% 
    ggplot(aes(x = distance, y = utility,ymin=lower,ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility') +
    theme_bw()

plot_politics_accessibility

plot_politics_accessibility_liberal <- df_politics_accessibility %>% 
    ggplot(aes(x = distance, y = utility_liberal,ymin=lower_liberal,ymax=upper_liberal)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility for liberal respondents') +
    theme_bw()

plot_politics_accessibility_liberal

plot_politics_accessibility_conservative <- df_politics_accessibility %>% 
    ggplot(aes(x = distance, y = utility_conservative,ymin=lower_conservative,ymax=upper_conservative)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility for conservative respondents') +
    theme_bw()

plot_politics_accessibility_conservative

plot_politics_accessibility_independent <- df_politics_accessibility %>% 
    ggplot(aes(x = distance, y = utility_independent,ymin=lower_independent,ymax=upper_independent)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility  for independent respondents') +
    theme_bw()

plot_politics_accessibility_independent

plot_politics_penalty <- df_politics_penalty %>% 
    ggplot(aes(x = penalty, y = utility, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility') +
    theme_bw()

plot_politics_penalty

plot_politics_penalty_liberal <- df_politics_penalty %>% 
    ggplot(aes(x = penalty, y = utility_liberal, ymin=lower_liberal, ymax=upper_liberal)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility for liberal respondents') +
    theme_bw()

plot_politics_penalty_liberal

plot_politics_penalty_conservative <- df_politics_penalty %>% 
    ggplot(aes(x = penalty, y = utility_conservative, ymin=lower_conservative, ymax=upper_conservative)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility for conservative respondents') +
    theme_bw()

plot_politics_penalty_conservative

plot_politics_penalty_independent <- df_politics_penalty %>% 
    ggplot(aes(x = penalty, y = utility_independent, ymin=lower_independent, ymax=upper_independent)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility for independent respondents') +
    theme_bw()

plot_politics_penalty_independent



plot_politics_incentives <- df_politics_incentives %>% 
    ggplot(aes(x = incentive, y = utility, ymin=lower, ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility') +
    theme_bw()

plot_politics_incentives

plot_politics_incentives_liberal <- df_politics_incentives %>% 
    ggplot(aes(x = incentive, y = utility_liberal, ymin=lower_liberal, ymax=upper_liberal)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility for liberal respondents') +
    theme_bw()

plot_politics_incentives_liberal

plot_politics_incentives_conservative <- df_politics_incentives %>% 
    ggplot(aes(x = incentive, y = utility_conservative, ymin=lower_conservative, ymax=upper_conservative)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility for conservative respondents') +
    theme_bw()

plot_politics_incentives_conservative

plot_politics_incentives_independent <- df_politics_incentives %>% 
    ggplot(aes(x = incentive, y = utility_independent, ymin=lower_independent, ymax=upper_independent)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility for independent respondents') +
    theme_bw()

plot_politics_incentives_independent



plot_politics_continuous_attributes <- plot_grid(
    plot_politics_value, plot_politics_value_liberal, plot_politics_value_conservative, plot_politics_value_independent, 
    plot_politics_penalty, plot_politics_penalty_liberal, plot_politics_penalty_conservative, plot_politics_penalty_independent,
    nrow = 2
)

plot_politics_continuous_attributes


# Save plots 
ggsave(
    filename = here('data', 'plot_politics_continuous_attributes.png'), 
    plot = plot_politics_continuous_attributes, 
    width = 10, height = 10)

plot_politics_categorical_attributes <- plot_grid(
    plot_politics_accessibility, plot_politics_accessibility_liberal, plot_politics_accessibility_conservative, plot_politics_accessibility_independent, 
    plot_politics_incentives, plot_politics_incentives_liberal, plot_politics_incentives_conservative, plot_politics_incentives_independent,
    nrow = 2
)

plot_politics_categorical_attributes 

# Save plots 
ggsave(
    filename = here('data', 'plot_politics_categorical_attributes.png'), 
    plot = plot_politics_categorical_attributes, 
    width = 10, height = 10)





# Plot the utility for each attribute
plot_income_value <- df_income_value %>% 
    ggplot(aes(x = value, y = utility,ymin=lower,ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility') +
    theme_bw()

plot_income_value

plot_income_value_low <- df_income_value %>% 
    ggplot(aes(x = value, y = utility_low,ymin=lower_low,ymax=upper_low)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility for low income respondents') +
    theme_bw()

plot_income_value_low

plot_income_value_high <- df_income_value %>% 
    ggplot(aes(x = value, y = utility_high,ymin=lower_high,ymax=upper_high)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility for high income respondents') +
    theme_bw()

plot_income_value_high

plot_income_accessibility <- df_income_accessibility %>% 
    ggplot(aes(x = distance, y = utility,ymin=lower,ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility') +
    theme_bw()

plot_income_accessibility

plot_income_accessibility_low <- df_income_accessibility %>% 
    ggplot(aes(x = distance, y = utility_low,ymin=lower_low,ymax=upper_low)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility for low income respondents') +
    theme_bw()

plot_income_accessibility_low

plot_income_accessibility_high <- df_income_accessibility %>% 
    ggplot(aes(x = distance, y = utility_high,ymin=lower_high,ymax=upper_high)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility for high income respondents') +
    theme_bw()

plot_income_accessibility_high

plot_income_penalty <- df_income_penalty %>% 
    ggplot(aes(x = penalty, y = utility, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility') +
    theme_bw()

plot_income_penalty

plot_income_penalty_low <- df_income_penalty %>% 
    ggplot(aes(x = penalty, y = utility_low, ymin=lower_low, ymax=upper_low)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility for low income respondents') +
    theme_bw()

plot_income_penalty_low

plot_income_penalty_high <- df_income_penalty %>% 
    ggplot(aes(x = penalty, y = utility_high, ymin=lower_high, ymax=upper_high)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility for high income respondents') +
    theme_bw()

plot_income_penalty_high

plot_income_incentives <- df_income_incentives %>% 
    ggplot(aes(x = incentive, y = utility, ymin=lower, ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility') +
    theme_bw()

plot_income_incentives

plot_income_incentives_low <- df_income_incentives %>% 
    ggplot(aes(x = incentive, y = utility_low, ymin=lower_low, ymax=upper_low)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility for low income respondents') +
    theme_bw()

plot_income_incentives_low

plot_income_incentives_high <- df_income_incentives %>% 
    ggplot(aes(x = incentive, y = utility_high, ymin=lower_high, ymax=upper_high)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility for high income respondents') +
    theme_bw()

plot_income_incentives_high

plot_income_continuous_attributes <- plot_grid(
    plot_income_value, plot_income_value_low, plot_income_value_high, 
    plot_income_penalty, plot_income_penalty_low, plot_income_penalty_high,
    nrow = 2
)

plot_income_continuous_attributes


# Save plots 
ggsave(
    filename = here('data', 'plot_income_continuous_attributes.png'), 
    plot = plot_income_continuous_attributes, 
    width = 10, height = 10)

plot_income_categorical_attributes <- plot_grid(
    plot_income_accessibility, plot_income_accessibility_low, plot_income_accessibility_high, 
    plot_income_incentives, plot_income_incentives_low, plot_income_incentives_high,
    nrow = 2
)

plot_income_categorical_attributes 

# Save plots 
ggsave(
    filename = here('data', 'plot_income_categorical_attributes.png'), 
    plot = plot_income_categorical_attributes, 
    width = 10, height = 10)





# Plot the utility for each attribute
plot_education_value <- df_education_value %>% 
    ggplot(aes(x = value, y = utility,ymin=lower,ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility') +
    theme_bw()

plot_education_value

plot_education_value_low <- df_education_value %>% 
    ggplot(aes(x = value, y = utility_low,ymin=lower_low,ymax=upper_low)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility for less-educated respondents') +
    theme_bw()

plot_education_value_low

plot_education_value_high <- df_education_value %>% 
    ggplot(aes(x = value, y = utility_high,ymin=lower_high,ymax=upper_high)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility for more-educated respondents') +
    theme_bw()

plot_education_value_high

plot_education_accessibility <- df_education_accessibility %>% 
    ggplot(aes(x = distance, y = utility,ymin=lower,ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility') +
    theme_bw()

plot_education_accessibility

plot_education_accessibility_low <- df_education_accessibility %>% 
    ggplot(aes(x = distance, y = utility_low,ymin=lower_low,ymax=upper_low)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility for less-educated respondents') +
    theme_bw()

plot_education_accessibility_low

plot_education_accessibility_high <- df_education_accessibility %>% 
    ggplot(aes(x = distance, y = utility_high,ymin=lower_high,ymax=upper_high)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility for more-educated respondents') +
    theme_bw()

plot_education_accessibility_high

plot_education_penalty <- df_education_penalty %>% 
    ggplot(aes(x = penalty, y = utility, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility') +
    theme_bw()

plot_education_penalty

plot_education_penalty_low <- df_education_penalty %>% 
    ggplot(aes(x = penalty, y = utility_low, ymin=lower_low, ymax=upper_low)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility for less-educated respondents') +
    theme_bw()

plot_education_penalty_low

plot_education_penalty_high <- df_education_penalty %>% 
    ggplot(aes(x = penalty, y = utility_high, ymin=lower_high, ymax=upper_high)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility for more-educated respondents') +
    theme_bw()

plot_education_penalty_high

plot_education_incentives <- df_education_incentives %>% 
    ggplot(aes(x = incentive, y = utility, ymin=lower, ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility') +
    theme_bw()

plot_education_incentives

plot_education_incentives_low <- df_education_incentives %>% 
    ggplot(aes(x = incentive, y = utility_low, ymin=lower_low, ymax=upper_low)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility for less-educated respondents') +
    theme_bw()

plot_education_incentives_low

plot_education_incentives_high <- df_education_incentives %>% 
    ggplot(aes(x = incentive, y = utility_high, ymin=lower_high, ymax=upper_high)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility for more-educated respondents') +
    theme_bw()

plot_education_incentives_high

plot_education_continuous_attributes <- plot_grid(
    plot_education_value, plot_education_value_low, plot_education_value_high, 
    plot_education_penalty, plot_education_penalty_low, plot_education_penalty_high,
    nrow = 2
)

plot_education_continuous_attributes


# Save plots 
ggsave(
    filename = here('data', 'plot_education_continuous_attributes.png'), 
    plot = plot_education_continuous_attributes, 
    width = 10, height = 10)

plot_education_categorical_attributes <- plot_grid(
    plot_education_accessibility, plot_education_accessibility_low, plot_education_accessibility_high, 
    plot_education_incentives, plot_education_incentives_low, plot_education_incentives_high,
    nrow = 2
)

plot_education_categorical_attributes 

# Save plots 
ggsave(
    filename = here('data', 'plot_education_categorical_attributes.png'), 
    plot = plot_education_categorical_attributes, 
    width = 10, height = 10)







# Plot the utility for each attribute
plot_ethnicity_value <- df_ethnicity_value %>% 
    ggplot(aes(x = value, y = utility,ymin=lower,ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility') +
    theme_bw()

plot_ethnicity_value

plot_ethnicity_value_asian <- df_ethnicity_value %>% 
    ggplot(aes(x = value, y = utility_asian,ymin=lower_asian,ymax=upper_asian)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility for Asian respondents') +
    theme_bw()

plot_ethnicity_value_asian

plot_ethnicity_value_black <- df_ethnicity_value %>% 
    ggplot(aes(x = value, y = utility_black,ymin=lower_black,ymax=upper_black)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility for Black respondents') +
    theme_bw()

plot_ethnicity_value_black

plot_ethnicity_value_hispanic <- df_ethnicity_value %>% 
    ggplot(aes(x = value, y = utility_hispanic,ymin=lower_hispanic,ymax=upper_hispanic)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility for Hispanic respondents') +
    theme_bw()

plot_ethnicity_value_hispanic

plot_ethnicity_value_native <- df_ethnicity_value %>% 
    ggplot(aes(x = value, y = utility_native,ymin=lower_native,ymax=upper_native)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Value of incentive ($)', y = 'Utility for Native respondents') +
    theme_bw()

plot_ethnicity_value_native






plot_ethnicity_accessibility <- df_ethnicity_accessibility %>% 
    ggplot(aes(x = distance, y = utility,ymin=lower,ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility') +
    theme_bw()

plot_ethnicity_accessibility

plot_ethnicity_accessibility_asian <- df_ethnicity_accessibility %>% 
    ggplot(aes(x = distance, y = utility_asian,ymin=lower_asian,ymax=upper_asian)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility for Asian respondents') +
    theme_bw()

plot_ethnicity_accessibility_asian

plot_ethnicity_accessibility_black <- df_ethnicity_accessibility %>% 
    ggplot(aes(x = distance, y = utility_black,ymin=lower_black,ymax=upper_black)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility for Black respondents') +
    theme_bw()

plot_ethnicity_accessibility_black

plot_ethnicity_accessibility_hispanic <- df_ethnicity_accessibility %>% 
    ggplot(aes(x = distance, y = utility_hispanic,ymin=lower_hispanic,ymax=upper_hispanic)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility for Hispanic respondents') +
    theme_bw()

plot_ethnicity_accessibility_hispanic

plot_ethnicity_accessibility_native <- df_ethnicity_accessibility %>% 
    ggplot(aes(x = distance, y = utility_native,ymin=lower_native,ymax=upper_native)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Distance of nearest vaccination center (miles)', y = 'Utility for Native respondents') +
    theme_bw()

plot_ethnicity_accessibility_native




plot_ethnicity_penalty <- df_ethnicity_penalty %>% 
    ggplot(aes(x = penalty, y = utility, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility') +
    theme_bw()

plot_ethnicity_penalty

plot_ethnicity_penalty_asian <- df_ethnicity_penalty %>% 
    ggplot(aes(x = penalty, y = utility_asian, ymin=lower_asian, ymax=upper_asian)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility for Asian respondents') +
    theme_bw()

plot_ethnicity_penalty_asian

plot_ethnicity_penalty_black <- df_ethnicity_penalty %>% 
    ggplot(aes(x = penalty, y = utility_black, ymin=lower_black, ymax=upper_black)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility for Black respondents') +
    theme_bw()

plot_ethnicity_penalty_black

plot_ethnicity_penalty_hispanic <- df_ethnicity_penalty %>% 
    ggplot(aes(x = penalty, y = utility_hispanic, ymin=lower_hispanic, ymax=upper_hispanic)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility for Hispanic respondents') +
    theme_bw()

plot_ethnicity_penalty_hispanic

plot_ethnicity_penalty_native <- df_ethnicity_penalty %>% 
    ggplot(aes(x = penalty, y = utility_native, ymin=lower_native, ymax=upper_native)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Penalty for non-compliance ($)', y = 'Utility for Native respondents') +
    theme_bw()

plot_ethnicity_penalty_native

plot_ethnicity_incentives <- df_ethnicity_incentives %>% 
    ggplot(aes(x = incentive, y = utility, ymin=lower, ymax=upper)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility') +
    theme_bw()

plot_ethnicity_incentives

plot_ethnicity_incentives_asian <- df_ethnicity_incentives %>% 
    ggplot(aes(x = incentive, y = utility_asian, ymin=lower_asian, ymax=upper_asian)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility for Asian respondents') +
    theme_bw()

plot_ethnicity_incentives_asian

plot_ethnicity_incentives_black <- df_ethnicity_incentives %>% 
    ggplot(aes(x = incentive, y = utility_black, ymin=lower_black, ymax=upper_black)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility for Black respondents') +
    theme_bw()

plot_ethnicity_incentives_black

plot_ethnicity_incentives_hispanic <- df_ethnicity_incentives %>% 
    ggplot(aes(x = incentive, y = utility_hispanic, ymin=lower_hispanic, ymax=upper_hispanic)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility for Hispanic respondents') +
    theme_bw()

plot_ethnicity_incentives_hispanic

plot_ethnicity_incentives_native <- df_ethnicity_incentives %>% 
    ggplot(aes(x = incentive, y = utility_native, ymin=lower_native, ymax=upper_native)) +
    geom_point() +
    geom_errorbar(width=0.3)  +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(x = 'Incentive', y = 'Utility for Native respondents') +
    theme_bw()

plot_ethnicity_incentives_native

plot_ethnicity_continuous_attributes <- plot_grid(
    plot_ethnicity_value, plot_ethnicity_value_asian, plot_ethnicity_value_black,  plot_ethnicity_value_hispanic, plot_ethnicity_value_native, 
    plot_ethnicity_penalty, plot_ethnicity_penalty_asian, plot_ethnicity_penalty_black, plot_ethnicity_penalty_hispanic, plot_ethnicity_penalty_native,
    nrow = 2
)

plot_ethnicity_continuous_attributes


# Save plots 
ggsave(
    filename = here('data', 'plot_ethnicity_continuous_attributes.png'), 
    plot = plot_ethnicity_continuous_attributes, 
    width = 10, height = 10)

plot_ethnicity_categorical_attributes <- plot_grid(
    plot_ethnicity_accessibility, plot_ethnicity_accessibility_asian, plot_ethnicity_accessibility_black, plot_ethnicity_accessibility_hispanic, plot_ethnicity_accessibility_native, 
    plot_ethnicity_incentives, plot_ethnicity_incentives_asian, plot_ethnicity_incentives_black, plot_ethnicity_incentives_hispanic, plot_ethnicity_incentives_native,
    nrow = 2
)

plot_ethnicity_categorical_attributes 

# Save plots 
ggsave(
    filename = here('data', 'plot_ethnicity_categorical_attributes.png'), 
    plot = plot_ethnicity_categorical_attributes, 
    width = 10, height = 10)
