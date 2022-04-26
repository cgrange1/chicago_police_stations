library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(lubridate)
library(readxl)
library(stringi)
library(stringr)

####CLEAN####

#L2
l2 <- fread("~/Documents/Brennan Center/R Files/Police Polling Places/X_1I0I08KPF_chi_CSV_CUSTOM.csv")

#POLLING PLACES
place <- read_excel("~/Documents/Brennan Center/R Files/Police Polling Places/Top 10 Cities & Counties (Police - Polling Place Locations)_11-15.xlsx",
                    sheet = "Police Office Polling Place All")
place <- place %>% 
  filter(Jurisdiction == "Cook/Chicago")

place$`Precinct ID Recent` <- str_pad(place$`Precinct ID Recent`, width=4, side="left", pad="0")
stri_sub(place$`Precinct ID Recent`, 3, 2) <- "-"
place$`Precinct ID Recent` <- paste0("CHICAGO ", place$`Precinct ID Recent`)

precinct_2018 <- place$`Precinct ID Recent`
print(precinct_2018)

precinct_2012 <- place %>% 
  select(`Precinct ID 2012`) %>% 
  separate_rows(`Precinct ID 2012`)
precinct_2012$`Precinct ID 2012` <- str_pad(precinct_2012$`Precinct ID 2012`, width=4, side="left", pad="0")
stri_sub(precinct_2012$`Precinct ID 2012`, 3, 2) <- "-"
precinct_2012$`Precinct ID 2012` <- paste0("CHICAGO ", precinct_2012$`Precinct ID 2012`)
precinct_2012 <- precinct_2012$`Precinct ID 2012`
print(precinct_2012)

precinct_2016 <- place %>% 
  select(`Precinct ID 2016`) %>% 
  separate_rows(`Precinct ID 2016`)
precinct_2016$`Precinct ID 2016` <- str_pad(precinct_2016$`Precinct ID 2016`, width=4, side="left", pad="0")
stri_sub(precinct_2016$`Precinct ID 2016`, 3, 2) <- "-"
precinct_2016$`Precinct ID 2016` <- paste0("CHICAGO ", precinct_2016$`Precinct ID 2016`)
precinct_2016 <- precinct_2016$`Precinct ID 2016`
print(precinct_2016)

precinct_2014 <- place %>% 
  select(`Precinct ID 2014`) %>% 
  separate_rows(`Precinct ID 2014`)
precinct_2014$`Precinct ID 2014` <- str_pad(precinct_2014$`Precinct ID 2014`, width=4, side="left", pad="0")
stri_sub(precinct_2014$`Precinct ID 2014`, 3, 2) <- "-"
precinct_2014$`Precinct ID 2014` <- paste0("CHICAGO ", precinct_2014$`Precinct ID 2014`)
precinct_2014 <- precinct_2014$`Precinct ID 2014`
print(precinct_2014)



#CHICAGO PRECINCT TURNOUT
my_fun <-  function(x) { 
  city <- (x) %>% 
    filter(!(is.na(.[[1]]))) %>% 
    mutate(election_year = 2012) %>% 
    rename(precinct = 1, registered_voters = 2, ballots_cast = 3, turnout = 4) %>% 
    filter(!(precinct %in% c("Precinct", "Total"))) %>% 
    slice(-(1:2)) %>% 
    mutate(precinct2 = case_when(!grepl('WARD', precinct) ~ precinct),
           precinct2 = str_pad(precinct2, width=2, side="left", pad="0"),
           precinct = ifelse(!startsWith(precinct, "WARD"), NA, precinct )) %>% 
    fill(precinct) %>% 
    filter(!is.na(precinct2)) %>% 
    mutate(precinct = gsub("WARD ", "", precinct), 
           precinct = str_pad(precinct, width=2, side="left", pad="0"),
           precinct = paste0("CHICAGO ", precinct)) %>% 
    unite(precinct, c(precinct,precinct2), sep = "-") %>% 
    mutate(turnout = as.numeric(turnout),
           turnout = turnout*100)
  
  return(data.frame(city))
}  

files <- list.files("~/Documents/Brennan Center/R Files/Police Polling Places/Chi Election Turnout Totals", full.names = T)
city_turnout <- lapply(files, read_excel)
result <- lapply(city_turnout, my_fun)
turnout2012 <- as.data.frame(result[[1]])
turnout2014 <- as.data.frame(result[[2]]) %>% 
  mutate(election_year = 2014)
turnout2016 <- as.data.frame(result[[3]]) %>% 
  mutate(election_year = 2016)
turnout2018 <- as.data.frame(result[[4]]) %>% 
  mutate(election_year = 2018)
turnout2020 <- as.data.frame(result[[5]]) %>% 
  mutate(election_year = 2020)
city_turnout <- bind_rows(turnout2012,turnout2014,turnout2016,turnout2018,turnout2020)
rm(turnout2012,turnout2014,turnout2016,turnout2018,turnout2020)


####CODING####
chi <- l2 %>% 
  rename(race = EthnicGroups_EthnicGroup1Desc, gender = Voters_Gender, age = Voters_Age, party = Parties_Description) %>% 
  mutate(police_2018 = ifelse(Precinct %in% precinct_2018, 1, 0),
         police_2012 = ifelse(Precinct %in% precinct_2012, 1, 0),
         police_2020 = 0) %>% 
  group_by(Precinct, General_2020_11_03) %>% 
  mutate(tot_precinct_2020 = sum(General_2020_11_03 == 1)) %>% 
  group_by(Precinct, General_2018_11_06) %>% 
  mutate(tot_precinct_2018 = sum(General_2018_11_06 == 1)) %>% 
  group_by(Precinct, General_2016_11_08) %>% 
  mutate(tot_precinct_2016 = sum(General_2016_11_08 == 1)) %>% 
  group_by(Precinct, General_2014_11_04) %>% 
  mutate(tot_precinct_2014 = sum(General_2014_11_04 == 1)) %>% 
  group_by(Precinct, General_2012_11_06) %>% 
  mutate(tot_precinct_2012 = sum(General_2012_11_06 == 1))

#city turnout dnd
dnd <- city_turnout %>% 
  filter(precinct != "CHICAGO 06-11") %>% 
  mutate(police = ifelse(precinct %in% precinct_2018, 1, 
                  ifelse(election_year == 2016 && precint %in% precinct_2016, 1,
                  ifelse(election_year == 2014 && precint %in% precinct_2014, 1,
                  ifelse(election_year == 2011 && precint %in% precinct_2012, 1, 
                         ifelse(election_year %in% c(2012,2014,2016) && precinct == 611, 0,
                         ifelse(election_year == 2012 && precinct == 3416, 0, 0)))))),
         time = ifelse(election_year == 2020, 0, 1), # makes no sense
         did = time*police,
         police = as.character(police)) %>% 
  mutate(election_year = ifelse(election_year == 2012, 1,
                                ifelse(election_year == 2014, 2,       
                                ifelse(election_year == 2016, 3,
                                ifelse(election_year == 2018, 5,       
                                ifelse(election_year == 2020, 6, 0))))))
         
didreg = lm(turnout ~ police + time + did, data = subset(dnd,election_year>=2018))
summary(didreg)

ggplot(dnd, aes(election_year, turnout, color = police)) +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 2018) +
  theme_minimal()
     
    


#####PANEL MATCH####
library(devtools)
install_github("insongkim/PanelMatch", dependencies=TRUE)
library(PanelMatch)
 
DisplayTreatment(unit.id = "time",
                 time.id = "election_year", legend.position = "none",
                 xlab = "election_year", ylab = "precinct",
                 treatment = "police", data = dnd)


