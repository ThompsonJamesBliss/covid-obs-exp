# Observed Versus Expected COVID-19 Infections Among National Football League Players During the 2020 Season

This Github repository corresponds with the "Observed Versus Expected COVID-19 Infections Among National Football League Players During the 2020 Season" paper and includes code/data that allows one to go through the simulation and visualization process.

## Scripts

`simulateCases.R`: code used to run a series of COVID-19 simulations on the 2020 NFL season using data from the county of each team's practice facility.

`visualize.R`: code used to make the plots in the paper. 

## Data

`sampleSim.csv`: sample results of the 2020 NFL season COVID-19 simulation which is created by running from the simulation script.
- `team`: NFL team.
- `observationDay`: date of observation.
- `cumPos`: simulated cumulative season positives for given team on given date.
- `iteration`: simulation iteration number.

`raw data/fakePriorPlayerPositives.csv`: fake data of player positives prior to 08/01/2020 (first date of simulation).
- `team`: NFL team.
- `priorPositives`: total positives for team (all set to 0).

`raw data/teamCountyPopulationPositives.csv`: population and positives for each county near a given NFL team's practice facility.
- `observationDay`: date of observation.
- `team`: NFL team.
- `county`: county near NFL team.
- `practiceFacilityCounty`: binary variable for whether practice facility is in given county.
- `stadiumCounty`: binary variable for whether stadium is in given county.
- `positives`: positives in given county.
- `population`: population of given county.

`raw data/teamTests.csv`: information on whether an NFL team took COVID-19 tests on a given day.
- `observationDay`: date of observation.
- `team`: NFL team.
- `hadFirstFourWeeksGame`: binary variable for whether team had a game during the first four weeks of the regular season on given day.
