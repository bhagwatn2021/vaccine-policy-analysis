# You should write code here to clean your pilot data and export it as a
# CSV file to the "data" folder
library(here)
library(tidyverse)

p1 <- read_csv(here("data", "sheet1.csv"))
p2 <- read_csv(here("data", "sheet2.csv"))
p3 <- read_csv(here("data", "sheet3.csv"))

varList<- names(p2)[!(names(p2) %in% names(p1))] # get non common names
varList<- c(varList,"session","created") # appending key parameter

responses <- left_join(p1,p2 %>% select(varList),by='session')

varList<- names(responses)[!(names(responses) %in% names(p3))] # get non common names
varList<- c(varList,"session") # appending key parameter

responses <- left_join(responses,p3,by='session')

responses <- responses %>% filter(!grepl('2021-10-15',created)) %>% filter(consentVaccine == 1)

view(responses)
