
# Load libraries
library(conjointTools)
library(fastDummies)
library(here)
library(tidyverse)

# Define the attributes and levels
levels <- list(
    incentive = c("grocery_store", "internet", "sport_tickets", "cash"), 
    value = c(0, 50, 100, 200, 300, 500, 1000),
    penalty = c(0, 0, 0, 0, 0, 0, 50, 100, 200, 300, 500, 1000),
    accessibility = c(0, 1, 3, 10) # Distance (miles) from a vaccination center
)

# Make a full-factorial design of experiment
doe <- makeDoe(levels)
doe <- recodeDesign(doe, levels)

head(doe) # preview

survey <- makeSurvey(
    doe       = doe,
    nResp     = 1000, 
    nAltsPerQ = 1,
    nQPerResp = 8, 
    outsideGood = TRUE
) %>% 
    mutate(
        incentive_label = ifelse(
            incentive_cash == 1, 
            paste0("$", value, " in cash"), 
            ifelse(
            incentive_grocery_store == 1, 
            paste0("$", value, " gift card for your local grocery store"), 
            ifelse(
            incentive_internet == 1, 
            paste0("$", value, " gift card for your internet provider"), 
            ifelse(
            incentive_sport_tickets == 1, 
            paste0("$", value, " gift card for a tickets to a local professional sports event"), "")
    )))) %>% 
    mutate(
        incentive_label = ifelse(
            value == 0,
            "No incentives for compliance",
            incentive_label
        )
    ) %>% 
    mutate(
        penalty_label = ifelse(
            penalty == 0,
            "Vaccine not mandated, no penalty for non-compliance",
            paste0("Vaccine mandated, $", penalty, " fine for non-compliance")
        )
    ) %>% 
    mutate(
        accessibility_label = ifelse(
            accessibility == 0,
            "Door-door vaccinations",
            paste0("Vaccination clinic will be located within ", accessibility, " miles from your house")
        )
    ) %>% 
    mutate(
        no_vaccine_label = ifelse(
            outsideGood == 1,
            "I will not take the vaccine under this condition.",
            ""
        ),
        penalty_label = ifelse(
            outsideGood == 1,
            "",
            penalty_label
        ),
        accessibility_label = ifelse(
            outsideGood == 1,
            "",
           accessibility_label
        ),
        incentive_label = ifelse(
            outsideGood == 1,
            "",
            incentive_label
        )
    ) 

head(survey) # preview
head(survey$incentive_label) # preview incentive labels
head(survey$penalty_label) # preview incentive labels
head(survey$no_vaccine_label) # preview incentive labels
write.csv(survey, here('combined_policy_questions.csv'))

