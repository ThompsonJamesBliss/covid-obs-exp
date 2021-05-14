library(ggridges)
library(tidyverse)
library(extrafont)

#############################
######## Loading Fonts / Creating Plot Theme
#############################
loadfonts(device='win')


customTheme <- theme_bw(16) + 
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(colour = "#013369", size = 2),
        axis.text = element_text(colour="#013369"), axis.ticks = element_line(colour = "#013369", size = 1.5),
        text = element_text(colour="#013369", size = 25),
        strip.background = element_blank(), strip.text = element_text(colour="#013369"), plot.subtitle = element_text(hjust = 0.5))




#############################
######## Reading Data
#############################

df_simResults <- read_csv('sampleSim.csv') #results of simulation (see simulateCases.R)


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
  
  #grouping by type (observed or expected) and date
  group_by(observationDay) %>%
  
  #taking mean accross all
  summarise(cumPos = mean(cumPos)) %>%
  
  #un-grouping
  ungroup() %>%
  
  #converting day to date
  mutate(observationDay = as.Date(observationDay)) %>%
  
  #plotting
  ggplot() +
  
  #adding grey lines for other sims
  geom_line(data = df_simResultsAggregatedByDate, aes(observationDay, cumPos, group = iteration), color = 'lightgrey', alpha = 0.2, lwd = 0.5) +
  
  #lines for actual / mean of expected
  geom_line(aes(observationDay, cumPos), lwd = 2, color = '#013369') +
  
  #setting theme
  customTheme +
  
  #setting labels
  xlab("Date") +
  ylab("Avg. Cumulative Positve Tests") +
  ggtitle("Avg. Cumulative Positive Tests vs Date Across League") +
  
  #setting axis
  scale_x_date(limits = c(as.Date('2020-08-01'), NA))





bandWidth <- 0.8


df_simResults  %>%
  
  #selecting last day
  filter(observationDay == '2021-01-02') %>%
  
  #grouping by team
  group_by(team) %>%
  
  #creating values for graph
  mutate(#used to order the teams - finding the x coordinate at maximum bandwidth
        order = density(cumPos, bw = bandWidth)$x[density(cumPos, bw = bandWidth)$y == max(density(cumPos, bw = bandWidth)$y)]) %>%
  
  #un-grouping
  ungroup() %>%
  
  #reordering
  mutate(team = reorder(team, order)) %>%
  
  #plotting
  ggplot(aes(y = team, x = cumPos)) +
  
  #density ridges
  geom_density_ridges(bandwidth = bandWidth, fill = 'lightblue') +
  
  
  #setting colors / theme
  scale_color_manual(values = 'black') +
  scale_fill_gradientn(colours = c('firebrick1', 'white', 'white', 'white', 'dodgerblue2'), limits = c(0,1),  guide = 'none') +
  customTheme +
  
  #setting labels
  ylab('') +
  ggtitle('Distributions of Expected Covid-19 Cases, By Team') +
  labs(color = '') +
  xlab('Simulated Total Positive Cases')
