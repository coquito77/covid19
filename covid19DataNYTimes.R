data_Date <- Sys.Date()

US_CoronaVirus <- data.table::fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# save(US_CoronaVirus, file = "US_CoronaVirus.Rdata")

US_StCoronaVirus <- data.table::fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

#save(US_StCoronaVirus, data_Date, US_CoronaVirus, file = "US_CoronaVirus.Rdata")