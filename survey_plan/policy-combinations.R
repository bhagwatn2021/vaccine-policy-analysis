# Load libraries
library(conjointTools)
library(fastDummies)
library(here)

# Define the attributes and levels
levels <- list(
    incentive   = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, "6 month supply of multivitamins", "12 month supply of multivitamins", "18 month supply of multivitamins", "24 month supply of multivitamins",
                  "6 months of internet and phone coverage", "12 months of internet and phone coverage", "18 months of internet and phone coverage", "24 months of internet and phone coverage",
                  "1 ticket to a sporting event", "2 tickets to a sporting event", "3 tickets to a sporting event", "4 tickets to a sporting event", "5 tickets to a sporting event", "6 tickets to a sporting event", "7 tickets to a sporting event", "8 tickets to a sporting event", "9 tickets to a sporting event", "10 tickets to a sporting event",
                  "$100 TV", "$200 TV", "$300 TV", "$400 TV", "$500 TV", "$600 TV", "$700 TV", "$800 TV", "$900 TV", "$1000 TV"),
    fine = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),   # Price ($1)
    accessibility   = c(0, 3, 5, 10) # Distance (miles)
)

# Make a full-factorial design of experiment
doe <- makeDoe(levels)
doe <- recodeDesign(doe, levels)
doe # preview
df_json <- jsonlite::toJSON(doe)

df_json# Make a labeld survey with "powertrain" as the label
survey_labeled <- makeSurvey(
    doe       = doe,  
    nResp     = 1000, 
    nAltsPerQ = 1,  
    nQPerResp = 8,
    outsideGood = TRUE
)
head(survey_labeled) # preview
write.csv(survey_labeled, here('combined_policy_questions.csv'))
