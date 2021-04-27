library(ggridges)
library(tidyverse)
library(extrafont)

#############################
######## Loading Fonts / Creating Plot Theme
#############################
loadfonts(device='win')


theme_nfl_1 <- theme_bw(16) + 
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(colour = "#013369", size = 2),
        axis.text = element_text(colour="#013369"), axis.ticks = element_line(colour = "#013369", size = 1.5),
        text = element_text(colour="#013369", family = "Endzone Sans Medium", size = 25),
        strip.background = element_blank(), strip.text = element_text(colour="#013369"), plot.subtitle = element_text(hjust = 0.5))




#############################
######## Reading Data
#############################

df_simResults <- read_csv('simResults.csv') #results of simulation (see simulateCases.R)

#Positive tests for each team by date
df_playerPositives <- read_csv('raw data/fakeDailyPlayerPositives.csv')


#############################
######## Aggergating Data
#############################

#taking mean of all simulations
df_simResultsAggregated <- df_simResults %>%
  
  #grouping by team and date
  group_by(team, observationDay) %>%
  
  #taking mean of cumulative positives accross simulation iterations
  summarise(cumPos = mean(cumPos), .groups = 'keep') %>%
  
  #ungrouping
  ungroup()







# #needed for greylines for plot
df_simResultsAggregatedByDate <- df_simResults %>%

  #grouping by date and iteration
  group_by(observationDay, iteration) %>%

  #taking mean accross dates
  summarise(cumPos = mean(cumPos), .groups = 'keep') %>%

  ungroup()


#############################
######## Plotting Average Accross League
#############################


df_simResultsAggregated %>%
  
  #joining observed positives
  inner_join(df_playerPositives, by = c("observationDay", "team"),
            suffix = c("_Expected", "_Observed")) %>%
  
  #gathering
  gather(key = "Type", value = "CumPos", cumPos_Expected, cumPos_Observed) %>%
  
  #remoivng `cumPos` from Type varialbe
  mutate(Type = gsub("cumPos_", "", Type)) %>%
  
  #grouping by type (observed or expected) and date
  group_by(observationDay, Type) %>%
  
  #taking mean accross all
  summarise(CumPos = mean(CumPos)) %>%
  
  #ungrouping
  ungroup() %>%
  
  #convertign day to date
  mutate(observationDay = as.Date(observationDay)) %>%
  
  #plotting
  ggplot() +
  
  #adding greylines for other sims
  geom_line(data = df_simResultsAggregatedByDate, aes(observationDay, cumPos, group = iteration), color = 'lightgrey', alpha = 0.2, lwd = 0.5) +
  
  #lines for actual / mean of expected
  geom_line(aes(observationDay, CumPos, color = Type), lwd = 2) +
  
  #setting theme
  theme_nfl_1 +
  
  #setting labels
  xlab("Date") +
  ylab("Avg. Cumulative Positve Tests") +
  ggtitle("Avg. Cumulative Positive Tests vs Date Across League") +
  
  #setting axis
  scale_x_date(limits = c(as.Date('2020-08-01'), NA)) +
  scale_color_manual(values = c('#013369', '#D50A0A'))





bandWidth <- 0.8


df_simResults  %>%
  
  #joining observed positives
  inner_join(df_playerPositives, by = c("observationDay", "team"),
             suffix = c("", "_Observed")) %>%
  
  #selecting last day
  filter(observationDay == '2021-01-02') %>%
  
  #grouping by team
  group_by(team) %>%
  
  #creating values for graph
  mutate(#used to order the teams - finding the x coordinate at maximum bandwidth
        order = density(cumPos, bw = bandWidth)$x[density(cumPos, bw = bandWidth)$y == max(density(cumPos, bw = bandWidth)$y)],
         
         #used for label
         colVal = 'Represents\nObserved\nValue',
         
         #used to fill the density curves
         fill_val = mean(cumPos >= cumPos_Observed)) %>%
  
  #ungrouping
  ungroup() %>%
  
  #reordering
  mutate(team = reorder(team, order)) %>%
  
  #plotting
  ggplot(aes(y = team, x = cumPos, fill = fill_val)) +
  
  #density ridgers
  geom_density_ridges(bandwidth = bandWidth) +
  
  #segement representing final team positives
  geom_segment(aes(y = as.integer(team), yend = as.integer(team) + 1, x = cumPos_Observed, xend = cumPos_Observed, color = colVal),
               lwd = 1) +
  
  #setting colors / theme
  scale_color_manual(values = 'black') +
  scale_fill_gradientn(colours = c('firebrick1', 'white', 'white', 'white', 'dodgerblue2'), limits = c(0,1),  guide = 'none') +
  theme_nfl_1 +
  
  #setting labels
  ylab('') +
  ggtitle('Distributions of Expected Covid-19 Cases, By Team') +
  labs(color = '') +
  xlab('Simulated Total Positive Cases')