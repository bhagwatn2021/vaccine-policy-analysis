# Load libraries
library(conjointTools)
library(fastDummies)
library(here)
library(tidyverse)

# Define the attributes and levels
levels <- list(
    incentive = c(
        "grocery_store", "internet_phone", "sport_tickets", "cash"), 
    value = c(0, 50, 100, 200, 300, 500, 1000),
    penalty = c(0, 0, 0, 0, 0, 0, 50, 100, 200, 300, 500, 1000),
    accessibility   = c(0, 1, 3, 10) # Distance (miles) from a vaccination center
)

# # Make a full-factorial design of experiment
 doe <- makeDoe(levels)
 doe <- recodeDesign(doe, levels) %>% 
#     # Set cash value to 0 for non-monetary incentives
#     mutate(cash = ifelse(incentive != "cash", 0, cash)) %>% 
#     # Set fine_value to 0 when fine == FALSE 
#     mutate(cost = ifelse(free == 1, 0, cost)) %>% 
#     # No longer need free variable
#     select(-free) %>% 
#     # Drop repeated rows introduced by making some cash_value = 0
#     distinct() 

head(doe) # preview

survey <- makeSurvey(
    doe       = doe,  
    nResp     = 1000, 
    nAltsPerQ = 1,  
    nQPerResp = 8, 
    outsideGood = TRUE
)
# ) %>% 
#     # Don't need incentive_cash variable (accounted for with cash_value)
#     mutate(
#         incentive_label = ifelse(
#             incentive == "incentive_cash",
#             paste0("$", value, " in cash"), NA
#         )
#     )

head(survey) # preview
write.csv(survey, here('choice_questions.csv'))

data <- simulateChoices(
    survey = survey,
    altID = "altID",
    obsID = "obsID"
)
