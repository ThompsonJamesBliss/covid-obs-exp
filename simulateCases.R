library(tidyverse)


#############################
######## Reading Data
#############################

#CDC conversion of positive tests to positive cases
positiveTests2PositiveCases <- 6.5

#Positive tests for each team by date
df_priorPlayerPositives <- read_csv('raw data/fakePriorPlayerPositives.csv')

#Positives tests prior to 8/1 start date
df_teamTests <- read_csv('raw data/teamTests.csv')

#Positive tests of team's practice facility
df_teamCountyPositives <- read_csv('raw data/teamCountyPopulationPositives.csv')


#############################
######## calculating cumulative positive tests of Team County Positives
#############################


df_teamCountyPositives <- df_teamCountyPositives %>%
  
  #arranging by date
  arrange(observationDay) %>%
  
  #calculating county prob of getting positive test
  mutate(positiveProb = positives / population * positiveTests2PositiveCases) %>%
  
  #removing unecessary columns
  select(-c('positives', 'population'))


#adjusting for SF's move to ARI
df_teamCountyPositives <- bind_rows(
                              #removing all SF rows after 12/1/2020.
                              df_teamCountyPositives %>%
                                    filter(!((team == "SF") & (observationDay >= as.Date("2020-12-02")))),
                       
                              #replacing SF rows after 12/1/2020 with ARI rows.
                              df_teamCountyPositives %>%
                                     filter((team == "ARI") & (observationDay >= as.Date("2020-12-02"))) %>%
                                     mutate(team = "SF") )



#############################
######## Setting up simulation
#############################

df_teamCountyPositives <- df_teamCountyPositives  %>%
  
  #adding prior positives
  inner_join(df_priorPlayerPositives, by = 'team') %>%
  
  inner_join(df_teamTests)






numPlayersTraningCamp <- 80
numPlayersFinalRoster <- 74 


df_sim <- inner_join(df_teamCountyPositives,
                     
                    expand.grid(observationDay = unique(df_teamCountyPositives$observationDay),
                                team = unique(df_teamCountyPositives$team),
                                playerNum = seq(1, numPlayersTraningCamp)),
                   
                   by = c("team", "observationDay"))



#iterations
numIterations <- 500


#results of simulation
df_simResults <- data.frame()



set.seed(1)

#simulating
for(i in seq(1, numIterations)){

  df_simResults <- bind_rows(
    
    df_sim %>%
      
      #grouping by team
      group_by(team) %>%
      
      #randomly assigning players who will be cut and players who were positive prior to sim
      mutate(hasCOViDatStart = as.integer(playerNum %in% sample.int(n = numPlayersTraningCamp,
                                                                    size = priorPositives)),
             
             cutFromFinalRoster = as.integer(playerNum %in% sample.int(n = numPlayersFinalRoster,
                                                                       size = numPlayersTraningCamp - numPlayersFinalRoster))) %>%
      ungroup() %>%
      
      #player can have covid if they tested today, randomly hit threshold in sim, were not cut from roster or had covid from start
      mutate(hasCovid = as.integer( (testedToday & (runif(n = n()) < positiveProb) &
                                       
                                       !(cutFromFinalRoster & (observationDay >= as.Date('2020-09-07')))) |
                                      
                                      (hasCOViDatStart))) %>%
      
      #grouping by players and team
      group_by(team, playerNum) %>%
      
      #arranging by date
      arrange(observationDay) %>%
      
      #summing cumulative players with COViD
      mutate(hasCovid = cummax(hasCovid)) %>%
      
      #ungrouping
      ungroup() %>%
      
      #grouping by team and date
      group_by(team, observationDay) %>%
      
      #summing number of players above prior with COViD
      summarise(cumPos = sum(hasCovid) - first(priorPositives), .groups = 'keep') %>%
      
      #ungrouping
      ungroup() %>%
      
      #saving iteration number to results
      mutate(iteration = i),
    
    df_simResults)
  
    
  

}
