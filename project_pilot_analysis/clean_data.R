# You should write code here to clean your pilot data and export it as a
# CSV file to the "data" folder
library(fastDummies)
library(here)
library(lubridate)
library(tidyverse)

p1 <- read_csv(here("data", "sheet1.csv"))
p2 <- read_csv(here("data", "sheet2.csv"))
p3 <- read_csv(here("data", "sheet3.csv"))

view(p1)
view(p2)
view(p3)

p1 <- p1 %>% 
    mutate(
        created = ymd_hms(created, tz = "EST"),
        ended =  ymd_hms(ended, tz = "EST"),
        time_sec_p1 = as.numeric(ended - created, units = "secs")) %>%
    # Select important columns
    select(session, time_sec_p1, consentAge, consentVaccine, consentUnderstand) %>% 
    filter(!is.na(session))

p2 <- p2 %>% 
    mutate(
        created = ymd_hms(created),
        ended =  ymd_hms(ended),
        time_sec_p2 = as.numeric(ended - created, units = "secs")) %>%
    # Select important columns
    select(session, time_sec_p2, respondentID, starts_with("cbc"), cbcAllSame) %>% 
    filter(!is.na(session))

p3 <- p3 %>% 
    mutate(
        created = ymd_hms(created),
        ended =  ymd_hms(ended),
        time_sec_p3 = as.numeric(ended - created, units = "secs")) %>%
    # Select important columns
    select(session, time_sec_p3, yearOfBirth:feedback,completionCode) %>% 
    filter(!is.na(session))

view(p1)
view(p2)
view(p3)

data <- p1 %>% 
    inner_join(p2, by = "session") %>% 
    inner_join(p3, by = "session") 
view(data)
data <- data %>% 
    filter(!is.na(cbc1)) %>% 
    filter(!is.na(cbc2)) %>% 
    filter(!is.na(cbc3)) %>% 
    filter(!is.na(cbc4)) %>% 
    filter(!is.na(cbc5)) %>% 
    filter(!is.na(cbc6)) %>% 
    filter(!is.na(cbc7)) %>% 
    filter(!is.na(cbc8))%>% 
    filter(!is.na(cbc9))%>% 
    filter(!is.na(cbc10)) %>% 
    filter(yearOfBirth != 'prefer_not_say') %>% 
    filter(consentVaccine == 1)%>% 
    filter(consentAge == 1) %>% 
    filter(consentUnderstand == 1)

data <- data %>% 
    pivot_longer(
        cols = cbc1:cbc10,
        names_to = "qID",
        values_to = "choice") %>% 
    # Convert the qID variable to a number
    mutate(qID = parse_number(qID))
survey <- read_csv("https://raw.githubusercontent.com/bhagwatn2021/vaccine-policy-analysis/pilot-analysis/project_pilot/combined_policy_questions.csv") %>% 
    select(-...1,-contains("_label"))
# Convert choice column to 1 or 0 based on if the alternative was chosen 
data <- data %>% 
    rename(respID = respondentID) %>% 
    left_join(survey, by = c("respID", "qID"))%>% 
    mutate(choice = ifelse(choice == altID, 1, 0)) %>% 
    # Drop unused variables
    select(-cbcPractice, -cbcAllSame)

view(survey)
view(data)


write_csv(data, here("data", "choiceData.csv"))
