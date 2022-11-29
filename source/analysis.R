library(tidyverse)
library(usmap)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num = 6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- Data Summary
#----------------------------------------------------------------------------#
# Load incarceration data
incarceration_df <- get_data()

# Which urbanicity has the greatest difference between jail rated
# capacity and total jail population in 2018?

# A dataframe that looks at urbanicity and the total difference
# between prisoners housed and capacity for each urbanicity
urbanicity_capacity <- incarceration_df %>%
  filter(year == max(year)) %>%
  filter(!is.na(urbanicity)) %>%
  filter(!is.na(jail_rated_capacity)) %>%
  filter(!is.na(total_jail_pop)) %>%
  group_by(urbanicity) %>%
  summarize(total_capacity = sum(jail_rated_capacity), 
            total_prisoners = sum(total_jail_pop)) %>%
  mutate(difference = total_capacity - total_prisoners)

# Rural difference between jail population and jail capacity
rural_jail_capacity_difference <- urbanicity_capacity$difference[1]
# Small/middle difference between jail population and jail capacity
small_jail_capacity_difference <- urbanicity_capacity$difference[2]
# Suburban difference between jail population and jail capacity
suburb_jail_capacity_difference <- urbanicity_capacity$difference[3]
# Urban difference between jail population and jail capacity
urban_jail_capacity_difference <- urbanicity_capacity$difference[4]

# Which urbanicity incarcerates residents at the highest average rate in 2018?
# What are the values for each urbanicity?

# A dataframe that looks at the average incarceration rate
# across all the counties in an urbanicity
urbanicity_incarceration <- incarceration_df %>%
  filter(year == max(year)) %>%
  filter(!is.na(urbanicity)) %>%
  filter(!is.na(total_jail_pop_rate)) %>%
  group_by(urbanicity) %>%
  summarize(avg_jail_rate = mean(total_jail_pop_rate))

# Average incarceration rates in rural counties
rural_incarceration_rate <- urbanicity_incarceration$avg_jail_rate[1]
# Average incarceration rates in small/middle sized counties
small_incarceration_rate <- urbanicity_incarceration$avg_jail_rate[2]
# Average incarceration rates in suburban counties
suburban_incarceration_rate <- urbanicity_incarceration$avg_jail_rate[3]
# Average incarceration rates in urban counties
urban_incarceration_rate <- urbanicity_incarceration$avg_jail_rate[4]

# What is the total number of in pre-trial incarcerations in each urbanicity
# in 2018?

# A dataframe that sums the number of individuals
# that have been incarcerated pre-trial in each type of county
urbanicity_pre_trial <- incarceration_df %>%
  filter(year == max(year)) %>%
  filter(!is.na(urbanicity)) %>%
  filter(!is.na(total_jail_pretrial)) %>%
  group_by(urbanicity) %>%
  summarize(pretrial_sum = sum(total_jail_pretrial))

# Total pretrial prisoners in all rural counties
rural_pretrial <- urbanicity_pre_trial$pretrial_sum[1]
# Total pretrial prisoners in all small/mid-sized counties
small_pretrial <- urbanicity_pre_trial$pretrial_sum[2]
# Total pretrial prisoners in all suburban counties
suburban_pretrial <- urbanicity_pre_trial$pretrial_sum[3]
# Total pretrial prisoners in all urban counties
urban_pretrial <- urbanicity_pre_trial$pretrial_sum[4]

#----------------------------------------------------------------------------#

## Section 3  ---- Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# This function returns a dataframe that is appropriate
# for graphing and includes information on total prisoners
# in a year.
get_year_jail_pop <- function() {
  total_jail_by_year <- incarceration_df %>%
    group_by(year) %>%
    summarise(year_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(total_jail_by_year)
}

# This function plots and returns a bar chart that
# presents the trends of number of prisoners between 1970 and 2018.
plot_jail_pop_for_us <- function() {
  plot_total_jail_by_year <- ggplot(get_year_jail_pop(), 
                                    aes(x = year, y = year_jail_pop)) +
    geom_bar(stat = "identity") +
    labs(title = "Increase of Jail Population in U.S. (1970 - 2018)", 
         x = "Year", 
         y = "Total Jail Population")
  return(plot_total_jail_by_year)
}
#----------------------------------------------------------------------------#

## Section 4  ---- Growth of Prison Population by State
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
# This function returns a dataframe that is appropriate for
# graphing and includes information on the total jail population
# in a given vector of states from 1970 to 2018.
get_jail_pop_by_states <- function(states) {
  jail_pop_by_states <- incarceration_df %>%
    filter(!is.na(total_jail_pop)) %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarise(jail_pop = sum(total_jail_pop))
  return(jail_pop_by_states)
}

# This function plots and returns a line chart that presents
# the total jail population of all the given states in a given
# vector and the statistics are given between 1970 and 2018.
plot_jail_pop_by_state <- function(states) {
  plot_total_jail_by_state <- ggplot(get_jail_pop_by_states(states),
                                     aes(x = year, 
                                         y = jail_pop, 
                                         color = state)) +
    geom_line() +
    labs(title = "Increase of Jail Popualtion in Rural U.S. states (1970-2018)", 
         x = "Year", 
         y = "Total Jail Population")
  return(plot_total_jail_by_state)
}
#----------------------------------------------------------------------------#


## Section 5  ---- 
## Inequities Present in Population Growth of Counties and their Prisons
#----------------------------------------------------------------------------#
# Inequities Present in Population Growth of Counties and Prisons
# This function returns a dataframe containing the ratio of population growth
# in the county between 1970 and 2018, the ratio of jail growth between
# 1970 and 2018, urbanicity, and the fips identification of each county. This
# dataframe is appropriate for graphing as a scatterplot.
get_population_growth_rate <- function() {
  # Look at incarceration in each county in 2018, specifically
  # looking at the jail population and its urbanicity.
  current_population_jail <- incarceration_df %>%
    filter(year == max(year)) %>%
    select(fips, total_jail_pop, urbanicity) %>%
    rename(total_jail_pop_2018 = total_jail_pop)
  # Look at incarceration in each county in 1970, specifically
  # looking at the jail population and its urbanicity.
  old_population_jail <- incarceration_df %>%
    filter(year == min(year)) %>%
    select(fips, total_jail_pop, urbanicity) %>%
    rename(total_jail_pop_1970 = total_jail_pop)
  # Look at population in each county in 2018, specifically
  # looking at the total population and its urbanicity.
  current_population_county <- incarceration_df %>%
    filter(year == max(year)) %>%
    select(fips, total_pop) %>%
    rename(total_pop_2018 = total_pop)
  # Look at population in each county in 1970, specifically
  # looking at the total population and its urbanicity.
  old_population_county <- incarceration_df %>%
    filter(year == min(year)) %>%
    select(fips, total_pop) %>%
    rename(total_pop_1970 = total_pop)
  # Look at the ratio of jail populations of 2018 jail populations
  # to 1970 jail populations.
  ratio_of_jail_pop <- left_join(old_population_jail, 
                                 current_population_jail) %>%
    filter(!is.na(total_jail_pop_1970)) %>%
    filter(!is.na(total_jail_pop_2018)) %>%
    mutate(ratio_jail = total_jail_pop_2018 / total_jail_pop_1970) %>%
    filter(is.finite(ratio_jail))
  # Look at the ratio of county populations of 2018 county populations
  # to 1970 county populations.
  ratio_of_pop <- left_join(old_population_county, 
                            current_population_county) %>%
    filter(!is.na(total_pop_1970)) %>%
    filter(!is.na(total_pop_2018)) %>%
    mutate(ratio_pop = total_pop_2018 / total_pop_1970) %>%
    filter(is.finite(ratio_pop))
  # Join the two tables and look specifically at the two different
  # ratios and the urbanicity.
  ratio_table <- left_join(ratio_of_jail_pop, ratio_of_pop) %>%
    select(fips, ratio_jail, ratio_pop, urbanicity)
  return(ratio_table)
}

# This function plots and returns a scatter plot that presents
# the jail population growth as a ratio of 2018 jail population to 1970
# jail population as compared to the ratio of the same county's population
# from 2018 to 1970. Each point is then a county and its color is the 
# urbanicity.
plot_pop_growth_rate <- function() {
  scatterplt_of_jail_pop_growth <- ggplot(get_population_growth_rate(), 
                                          aes(x = ratio_pop, 
                                              y = ratio_jail, 
                                              color = urbanicity)) +
    geom_point() +
    labs(title = "Population Growth Ratios Between 2018 and 1970 in both Jails and Counties with Urbanicity", 
         x = "Ratio of County Population Between 2018 and 1970", 
         y = "Ratio of Jail Population Between 2018 and 1970") +
    xlim(0, 10) +
    ylim(0, 200)
  return(scatterplt_of_jail_pop_growth)
}
#----------------------------------------------------------------------------#


## Section 6  ---- 
## Geographical Inequities Present in Pretrial Populations and the Increase in Jail Capacity
## by State
#----------------------------------------------------------------------------#
# Geographical Inequities Present in Pretrial Populations and the Increase in Jail Capacity
# by State
# This function returns the increase state-by-state
# in the jail rated capacity as a ratio of the capacity
# from 2018 to 1970 in each of these states. It returns
# a graphable data frame of each state and the increase in
# capacity as a ratio.
get_increase_jail_capacity <- function() {
  # Look at the average jail capacity in each state
  # in 2018.
  current_jail_capacity <- incarceration_df %>%
    filter(year == max(year)) %>%
    group_by(state) %>%
    summarize(jail_rated_capacity = mean(jail_rated_capacity, 
                                         na.rm = TRUE)) %>%
    select(state, jail_rated_capacity) %>%
    rename(jail_rated_capacity_2018 = jail_rated_capacity)
  # Look at the average jail capacity in each state
  # in 1970.
  jail_capacity_1970 <- incarceration_df %>%
    filter(year == min(year)) %>%
    group_by(state) %>%
    summarize(jail_rated_capacity = mean(jail_rated_capacity, 
                                         na.rm = TRUE)) %>%
    select(state, jail_rated_capacity) %>%
    rename(jail_rated_capacity_1970 = jail_rated_capacity)
  # Look at the ratio of jail capacity 2018 to jail
  # capacity in 1970, state-to-state.
  ratio_jail_capacity <- left_join(jail_capacity_1970, 
                                   current_jail_capacity) %>%
    mutate(ratio = jail_rated_capacity_2018 / jail_rated_capacity_1970) %>%
    select(state, ratio)
  return(ratio_jail_capacity)
}

# This function returns the increase state-by-state
# in the pretrial population as a ratio of the population
# from 2018 to 1970 in each of these states. It returns
# a graphable data frame of each state and the increase in
# pretrial population as a ratio.
get_increase_pretrial <- function() {
  # Look at the average jail pretrial population in each state
  # in 2018.
  current_pretrial <- incarceration_df %>%
    filter(year == max(year)) %>%
    group_by(state) %>%
    summarize(total_jail_pretrial_2018 = mean(total_jail_pretrial,
      na.rm = TRUE)) %>%
    select(state, total_jail_pretrial_2018)
  # Look at the average jail pretrial population in each state
  # in 1970.
  pretrial_1970 <- incarceration_df %>%
    filter(year == min(year)) %>%
    group_by(state) %>%
    summarize(total_jail_pretrial_1970 = mean(total_jail_pretrial,
      na.rm = TRUE)) %>%
    select(state, total_jail_pretrial_1970)
  # Look at the ratio of pretrial population 2018 to pretrial
  # population in 1970, state-to-state.
  ratio_pretrial <- left_join(pretrial_1970, current_pretrial) %>%
    mutate(ratio = total_jail_pretrial_2018 / total_jail_pretrial_1970) %>%
    select(state, ratio)
  return(ratio_pretrial)
}

# This function plots and returns a chloropleth map that presents
# the jail pretrial population growth as a ratio of the statistic in 2018 to
# 1970 jail population in each state.
plot_ratio_pretrial <- function() {
  plt_ratio_pretrial <- plot_usmap(data = get_increase_pretrial(), 
             values = "ratio") +
    scale_fill_continuous(low = "white", 
                          high = "blue", 
                          name = "Pretrial Increases from 1970 as a ratio", 
                          label = scales::comma) +
    labs(title = "Pretrial Increases since 1970 by State", 
         subtitle = "Increases are Ratios of the Population from 2018 to 1970") +
    theme(legend.position = "right")
  return(plt_ratio_pretrial)
}

# This function plots and returns a chloropleth map that presents
# the jail capacity growth as a ratio of the statistic in 2018 to
# 1970 jail capacity in each state.
plot_ratio_capacity <- function() {
  plt_ratio_cap <- plot_usmap(data = get_increase_jail_capacity(), 
             values = "ratio") +
    scale_fill_continuous(low = "white",
                          high = "blue",
                          name = "Prison Capacity Increases from 1970 as a ratio",
                          label = scales::comma) +
    labs(title = "Prison Capacity Increases since 1970 by State", 
         subtitle = "Increases are Ratios of the Prison Capacity from 2018 to 1970") +
    theme(legend.position = "right")
  return(plt_ratio_cap)
}

#----------------------------------------------------------------------------#
