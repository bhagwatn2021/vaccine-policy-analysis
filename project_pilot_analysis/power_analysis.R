# You should write code here to conduct a power analysis to inform the sample size you will need for your final survey

install.packages("remotes")
remotes::install_github("jhelvy/conjointTools")

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(janitor)
library(here)
library(conjointTools)
options(dplyr.width = Inf) # So you can see all of the columns

# Load the data set:
data <- read_csv(here("data", "choiceData.csv"))
View(data)

# Clean up names of created variables
data <- clean_names(data)
data <- data %>%
    mutate(obs_id = "obsID",
           q_id = "qID")

rename(data, obsID = obs_id)
rename(data, qID = q_id)

# Estimate the model
model <- estimateModels(
    nbreaks = 7,
    data   = data,
    pars   = c(
        "value", 
        "penalty",
        "accessibility",
        "incentive_cash",
        "incentive_grocery_store",
        "incentive_internet",
        "incentive_sport_tickets"
    ),
    outcome = "choice",
    obsID  = "obsID",
)

# Extract results
results <- getModelResults(model)
head(results)

# Plot results
ggplot(results) +
    geom_hline(yintercept = 0.05, color = "black", linetype = 2) +
    geom_point(aes(x = sampleSize, y = se, color = coef)) +
    expand_limits(y = 0) +
    theme_bw() + 
    labs(title = "Power Analysis",
        x = "Sample Size", 
        y = "Standard Error", 
        color = "Coefficient"
    )
