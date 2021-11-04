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

# Estimate MNL model

# First create some dummy coded variables for categorical variables
data <- dummy_cols(data, c("incentive_cash","incentive_grocery_store","incentive_internet","incentive_sport_tickets"))
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

# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values
