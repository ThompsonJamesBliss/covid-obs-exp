# Observed Versus Expected COVID-19 Infections Among National Football League Players During the 2020 Season

This Github repository corresponds with the "Observed Versus Expected COVID-19 Infections Among National Football League Players During the 2020 Season" paper and includes code/data with that allows one to go through the simulation process.

`simulateCases.R`: code used to run a series of COVID-19 simulations of the 2020 NFL season using data from the county of each team's practice facility.

`visualize.R`: code used to make the plots in the paper. 

`raw data/fakeDailyPlayerPositives.csv`: fake data of player positives of for each team on each date.
- `observationDay`: Date of observation
- `team`: NFL team of measurement
- `cumPos`: Cummulative positives for team to Date (all set to 0)

`raw data/fakeDailyPriorPositives.csv`: fake data of player positives prior to 08/01/2020 (first date of simulation).
- `team`: NFL team of measurement
- `priorPositives`: total positives for team (all set to 0)

`raw data/teamCountyPopulationPositives.csv`: population and positives in NFL team's practice facility county.
- `observationDay`: Date of observation
- `team`: NFL team
- `positives`: positives in NFL team's practice facilty county
- `population`: population of NFL team's practice facilty county

`raw data/teamTests.csv`: binary variable regarding whether a team took COVID-19 tests on a given day.
- `observationDay`: Date of observation.
- `team`: NFL team.
- `testedToday`: binary variable whether team tested on given day.
