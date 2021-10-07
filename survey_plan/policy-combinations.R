# Load libraries
library(conjointTools)
library(fastDummies)
library(here)

# Define the attributes and levels
levels <- list(
    incentive = c("multivitamin supply", "internet + phone coverage", "tickets to sporting events", "television unit","cash"),  # Categorical
    incentive_monetary_value = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),   # Price (USD)
    non_compliance_fine = c(TRUE,FALSE), # TRUE = fine the same monetary amount as incentive for non compliance
    accessibility   = c(0, 3, 5, 10) # Distance (miles) from a vaccination center
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
    nQPerResp = 8
)
head(survey_labeled) # preview
write.csv(survey_labeled, here('combined_policy_questions.csv'))
