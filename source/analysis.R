#-----# Assignment 3 by Mason Howes #-----#

# Loading the necessary libraries for the code

library(dplyr)
library(ggplot2)
library(ggmap)
library(tidyr)
library(knitr)

# Reading the prison data set into something usable in R

prison <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

options(scipen = 999)

#View(prison)

#-----# COMPUTED VARIABLES #-----#

### 1. Percent of total black community imprisoned [black_imprisoned_percent]

# Pulls number of black inmates from the data frames' most recent record

black_imprisoned_recent <- prison %>%
                           filter(year == 2016) %>%
                           group_by(year) %>%
                           tally(black_prison_pop) %>%
                           round() %>%
                           pull(n)

# Pulls number of black inmates from the data frames' most recent record

black_total_recent <- prison %>%
                      filter(year == 2016) %>%
                      group_by(year) %>%
                      tally(black_pop_15to64) %>%
                      round() %>%
                      pull(n)

# Finds the percentage

black_imprisoned_percent <- (black_imprisoned_recent / black_total_recent) * 100
black_imprisoned_percent <- round(black_imprisoned_percent, digits = 2)


### 2. Percent of total white community imprisoned [white_imprisoned_percent]

# Pulls number of white inmates from the data frames' most recent record

white_imprisoned_recent <- prison %>%
                           filter(year == 2016) %>%
                           group_by(year) %>%
                           tally(white_prison_pop) %>%
                           round() %>%
                           pull(n)

# Pulls number of white inmates from the data frames' most recent record

white_total_recent <- prison %>%
                      filter(year == 2016) %>%
                      group_by(year) %>%
                      tally(white_pop_15to64) %>%
                      round() %>%
                      pull(n)

# Finds the percentage

white_imprisoned_percent <- (white_imprisoned_recent / white_total_recent) * 100
white_imprisoned_percent <- round(white_imprisoned_percent, digits = 2)


### 3. Regional Imprisonment Ratios of Black Populations in 2016

# Midwest Ratio -- Black Population

midwest_black_pop_2016 <- prison %>%
                          filter(year == 2016) %>%
                          group_by(region) %>%
                          summarise(black_pop_15to64) %>%
                          subset(region == "Midwest") %>%
                          tally(black_pop_15to64) %>%
                          pull(n) %>%
                          round()

midwest_black_prison_2016 <- prison %>%
                             filter(year == 2016) %>%
                             group_by(region) %>%
                             summarise(black_prison_pop) %>%
                             subset(region == "Midwest") %>%
                             tally(black_prison_pop) %>%
                             pull(n) %>%
                             round()

midwest_black_ratio_2016 <- (midwest_black_pop_2016 / midwest_black_prison_2016) %>%
                            round()


# Northeast Ratio -- Black Population

northeast_black_pop_2016 <- prison %>%
                            filter(year == 2016) %>%
                            group_by(region) %>%
                            summarise(black_pop_15to64) %>%
                            subset(region == "Northeast") %>%
                            tally(black_pop_15to64) %>%
                            pull(n) %>%
                            round()

northeast_black_prison_2016 <- prison %>%
                               filter(year == 2016) %>%
                               group_by(region) %>%
                               summarise(black_prison_pop) %>%
                               subset(region == "Northeast") %>%
                               tally(black_prison_pop) %>%
                               pull(n) %>%
                               round()

northeast_black_ratio_2016 <- (northeast_black_pop_2016 / northeast_black_prison_2016) %>%
                              round()


# South Ratio -- Black Population

south_black_pop_2016 <- prison %>%
                        filter(year == 2016) %>%
                        group_by(region) %>%
                        summarise(black_pop_15to64) %>%
                        subset(region == "South") %>%
                        tally(black_pop_15to64) %>%
                        pull(n) %>%
                        round()

south_black_prison_2016 <- prison %>%
                           filter(year == 2016) %>%
                           group_by(region) %>%
                           summarise(black_prison_pop) %>%
                           subset(region == "South") %>%
                           tally(black_prison_pop) %>%
                           pull(n) %>%
                           round()

south_black_ratio_2016 <- (south_black_pop_2016 / south_black_prison_2016) %>%
                          round()


# West Ratio -- Black Population

west_black_pop_2016 <- prison %>%
                       filter(year == 2016) %>%
                       group_by(region) %>%
                       summarise(black_pop_15to64) %>%
                       subset(region == "West") %>%
                       tally(black_pop_15to64) %>%
                       pull(n) %>%
                       round()

west_black_prison_2016 <- prison %>%
                          filter(year == 2016) %>%
                          group_by(region) %>%
                          summarise(black_prison_pop) %>%
                          subset(region == "West") %>%
                          tally(black_prison_pop) %>%
                          pull(n) %>%
                          round()

west_black_ratio_2016 <- (west_black_pop_2016 / west_black_prison_2016) %>%
                         round()



### 4. Regional Imprisonment Ratios of White Populations in 2016

# Midwest Ratio -- White Population

midwest_white_pop_2016 <- prison %>%
                          filter(year == 2016) %>%
                          group_by(region) %>%
                          summarise(white_pop_15to64) %>%
                          subset(region == "Midwest") %>%
                          tally(white_pop_15to64) %>%
                          pull(n) %>%
                          round()

midwest_white_prison_2016 <- prison %>%
                             filter(year == 2016) %>%
                             group_by(region) %>%
                             summarise(white_prison_pop) %>%
                             subset(region == "Midwest") %>%
                             tally(white_prison_pop) %>%
                             pull(n) %>%
                             round()

midwest_white_ratio_2016 <- (midwest_white_pop_2016 / midwest_white_prison_2016) %>%
                            round()


# Northeast Ratio -- White Population

northeast_white_pop_2016 <- prison %>%
                            filter(year == 2016) %>%
                            group_by(region) %>%
                            summarise(white_pop_15to64) %>%
                            subset(region == "Northeast") %>%
                            tally(white_pop_15to64) %>%
                            pull(n) %>%
                            round()

northeast_white_prison_2016 <- prison %>%
                               filter(year == 2016) %>%
                               group_by(region) %>%
                               summarise(white_prison_pop) %>%
                               subset(region == "Northeast") %>%
                               tally(white_prison_pop) %>%
                               pull(n) %>%
                               round()

northeast_white_ratio_2016 <- (northeast_white_pop_2016 / northeast_white_prison_2016) %>%
                              round()


# South Ratio -- White Population

south_white_pop_2016 <- prison %>%
                        filter(year == 2016) %>%
                        group_by(region) %>%
                        summarise(white_pop_15to64) %>%
                        subset(region == "South") %>%
                        tally(white_pop_15to64) %>%
                        pull(n) %>%
                        round()

south_white_prison_2016 <- prison %>%
                           filter(year == 2016) %>%
                           group_by(region) %>%
                           summarise(white_prison_pop) %>%
                           subset(region == "South") %>%
                           tally(white_prison_pop) %>%
                           pull(n) %>%
                           round()

south_white_ratio_2016 <- (south_white_pop_2016 / south_white_prison_2016) %>%
                          round()


# West Ratio -- White Population

west_white_pop_2016 <- prison %>%
                       filter(year == 2016) %>%
                       group_by(region) %>%
                       summarise(white_pop_15to64) %>%
                       subset(region == "West") %>%
                       tally(white_pop_15to64) %>%
                       pull(n) %>%
                       round()

west_white_prison_2016 <- prison %>%
                          filter(year == 2016) %>%
                          group_by(region) %>%
                          summarise(white_prison_pop) %>%
                          subset(region == "West") %>%
                          tally(white_prison_pop) %>%
                          pull(n) %>%
                          round()

west_white_ratio_2016 <- (west_white_pop_2016 / west_white_prison_2016) %>%
                         round()


### 5. Regional Imprisonment Ratios of Black Populations in 1990

# Midwest Ratio -- Black Population

midwest_black_pop_1990 <- prison %>%
                          filter(year == 1990) %>%
                          group_by(region) %>%
                          summarise(black_pop_15to64) %>%
                          subset(region == "Midwest") %>%
                          tally(black_pop_15to64) %>%
                          pull(n) %>%
                          round()

midwest_black_prison_1990 <- prison %>%
                             filter(year == 1990) %>%
                             group_by(region) %>%
                             summarise(black_prison_pop) %>%
                             subset(region == "Midwest") %>%
                             tally(black_prison_pop) %>%
                             pull(n) %>%
                             round()

midwest_black_ratio_1990 <- (midwest_black_pop_1990 / midwest_black_prison_1990) %>%
                            round()


# Northeast Ratio -- Black Population

northeast_black_pop_1990 <- prison %>%
                            filter(year == 1990) %>%
                            group_by(region) %>%
                            summarise(black_pop_15to64) %>%
                            subset(region == "Northeast") %>%
                            tally(black_pop_15to64) %>%
                            pull(n) %>%
                            round()

northeast_black_prison_1990 <- prison %>%
                               filter(year == 1990) %>%
                               group_by(region) %>%
                               summarise(black_prison_pop) %>%
                               subset(region == "Northeast") %>%
                               tally(black_prison_pop) %>%
                               pull(n) %>%
                               round()

northeast_black_ratio_1990 <- (northeast_black_pop_1990 / northeast_black_prison_1990) %>%
                              round()


# South Ratio -- Black Population

south_black_pop_1990 <- prison %>%
                        filter(year == 1990) %>%
                        group_by(region) %>%
                        summarise(black_pop_15to64) %>%
                        subset(region == "South") %>%
                        tally(black_pop_15to64) %>%
                        pull(n) %>%
                        round()

south_black_prison_1990 <- prison %>%
                           filter(year == 1990) %>%
                           group_by(region) %>%
                           summarise(black_prison_pop) %>%
                           subset(region == "South") %>%
                           tally(black_prison_pop) %>%
                           pull(n) %>%
                           round()

south_black_ratio_1990 <- (south_black_pop_1990 / south_black_prison_1990) %>%
                          round()


# West Ratio -- Black Population

west_black_pop_1990 <- prison %>%
                       filter(year == 1990) %>%
                       group_by(region) %>%
                       summarise(black_pop_15to64) %>%
                       subset(region == "West") %>%
                       tally(black_pop_15to64) %>%
                       pull(n) %>%
                       round()

west_black_prison_1990 <- prison %>%
                          filter(year == 1990) %>%
                          group_by(region) %>%
                          summarise(black_prison_pop) %>%
                          subset(region == "West") %>%
                          tally(black_prison_pop) %>%
                          pull(n) %>%
                          round()

west_black_ratio_1990 <- (west_black_pop_1990 / west_black_prison_1990) %>%
                         round()


### 6. Regional Imprisonment Ratios of White Populations in 1990

# Midwest Ratio -- White Population

midwest_white_pop_1990 <- prison %>%
                          filter(year == 1990) %>%
                          group_by(region) %>%
                          summarise(white_pop_15to64) %>%
                          subset(region == "Midwest") %>%
                          tally(white_pop_15to64) %>%
                          pull(n) %>%
                          round()

midwest_white_prison_1990 <- prison %>%
                             filter(year == 1990) %>%
                             group_by(region) %>%
                             summarise(white_prison_pop) %>%
                             subset(region == "Midwest") %>%
                             tally(white_prison_pop) %>%
                             pull(n) %>%
                             round()

midwest_white_ratio_1990 <- (midwest_white_pop_1990 / midwest_white_prison_1990) %>%
  round()

# Northeast Ratio -- White Population

northeast_white_pop_1990 <- prison %>%
                            filter(year == 1990) %>%
                            group_by(region) %>%
                            summarise(white_pop_15to64) %>%
                            subset(region == "Northeast") %>%
                            tally(white_pop_15to64) %>%
                            pull(n) %>%
                            round()

northeast_white_prison_1990 <- prison %>%
                               filter(year == 1990) %>%
                               group_by(region) %>%
                               summarise(white_prison_pop) %>%
                               subset(region == "Northeast") %>%
                               tally(white_prison_pop) %>%
                               pull(n) %>%
                               round()

northeast_white_ratio_1990 <- (northeast_white_pop_1990 / northeast_white_prison_1990) %>%
                              round()


# South Ratio -- White Population

south_white_pop_1990 <- prison %>%
                        filter(year == 1990) %>%
                        group_by(region) %>%
                        summarise(white_pop_15to64) %>%
                        subset(region == "South") %>%
                        tally(white_pop_15to64) %>%
                        pull(n) %>%
                        round()

south_white_prison_1990 <- prison %>%
                           filter(year == 1990) %>%
                           group_by(region) %>%
                           summarise(white_prison_pop) %>%
                           subset(region == "South") %>%
                           tally(white_prison_pop) %>%
                           pull(n) %>%
                           round()

south_white_ratio_1990 <- (south_white_pop_1990 / south_white_prison_1990) %>%
                          round()


# West Ratio -- White Population

west_white_pop_1990 <- prison %>%
                       filter(year == 1990) %>%
                       group_by(region) %>%
                       summarise(white_pop_15to64) %>%
                       subset(region == "West") %>%
                       tally(white_pop_15to64) %>%
                       pull(n) %>%
                       round()

west_white_prison_1990 <- prison %>%
                          filter(year == 1990) %>%
                          group_by(region) %>%
                          summarise(white_prison_pop) %>%
                          subset(region == "West") %>%
                          tally(white_prison_pop) %>%
                          pull(n) %>%
                          round()

west_white_ratio_1990 <- (west_white_pop_1990 / west_white_prison_1990) %>%
                         round()


### 7. Percent of prison population that is Black

prison_total <- prison %>%
                filter(year == 2016) %>%
                group_by(year) %>%
                tally(total_prison_pop) %>%
                round() %>%
                pull(n)

number_black <- prison %>%
                filter(year == 2016) %>%
                group_by(year) %>%
                tally(black_prison_pop) %>%
                round() %>%
                pull(n)

percent_black <- (number_black / prison_total) * 100
percent_black <- round(percent_black, digits = 2)

### 8. Percent of prison population that is White

number_white <- prison %>%
                filter(year == 2016) %>%
                group_by(year) %>%
                tally(white_prison_pop) %>%
                round() %>%
                pull(n)

percent_white <- (number_white / prison_total) * 100
percent_white <- round(percent_white, digits = 2)

#-----# TIME TREND CHART #----#

### Calculating Populations by Year
#
# Prison populations per race

blackprison_byyear <- prison %>%
                    group_by(year) %>%
                    tally(black_prison_pop) %>%
                    round()

whiteprison_byyear <- prison %>%
                    group_by(year) %>%
                    tally(white_prison_pop) %>%
                    round()

latinxprison_byyear <- prison %>%
                     group_by(year) %>%
                     tally(latinx_prison_pop) %>%
                     round()

# Total populations per race

blacktotal_byyear <- prison %>%
                     group_by(year) %>%
                     tally(black_pop_15to64) %>%
                     round()

whitetotal_byyear <- prison %>%
                     group_by(year) %>%
                     tally(white_pop_15to64) %>%
                     round()

latinxtotal_byyear <- prison %>%
                      group_by(year) %>%
                      tally(black_pop_15to64) %>%
                      round()

# Combining corresponding data frames by calculating the ratio as a percent

black_ratio <- (blackprison_byyear$n / blacktotal_byyear$n) * 100
white_ratio <- (whiteprison_byyear$n / whitetotal_byyear$n) * 100
latinx_ratio <- (latinxprison_byyear$n / latinxtotal_byyear$n) * 100

years_in_question <- blackprison_byyear$year # Keeps covered years consistent

# Joins the vectors into a single data frame in order to create a legend with

joined_data <- data_frame(years_in_question, black_ratio, white_ratio,latinx_ratio)
joined_data <- subset(joined_data, years_in_question >= 1990)
joined_data <- subset(joined_data, years_in_question <= 2016)

value_data <- joined_data %>%
              gather(key = "race", value = "ethnicity", -years_in_question)

time_chart <- ggplot(value_data, aes(x = years_in_question, y = ethnicity, color = race)) +
                geom_line(size = 1, alpha = 1) +
                labs(x = "Year",
                     y = "Percent of Ethnic Individuals Within A Population Imprisioned",
                     title = "Ethnic Group Imprisonment Percentage Over Time",
                     colour = "Race",
                     tag = "Time Trend Chart") +
                scale_color_manual(labels = 
                                     c("Black", "Latinx", "White"),
                                   values =
                                     c("red", "green", "blue"))

#-----# VARIABLE COMPARISON CHART #-----#

black_prison_avg_2016 <- (west_black_prison_2016 +
                         south_black_prison_2016 +
                         northeast_black_prison_2016 +
                         midwest_black_prison_2016)

black_prison_avg_2016 <- round(black_prison_avg_2016, digits = 2)

black_pop_avg_2016 <- (west_black_pop_2016 +
                      south_black_pop_2016 +
                      northeast_black_pop_2016 +
                      midwest_black_pop_2016)

black_pop_avg_2016 <- round(black_pop_avg_2016, digits = 2)

white_prison_avg_2016 <- (west_white_prison_2016 +
                         south_black_prison_2016 +
                         northeast_black_prison_2016 +
                         midwest_black_prison_2016)

white_prison_avg_2016 <- round(white_prison_avg_2016, digits = 2)

white_pop_avg_2016 <- (west_white_pop_2016 +
                      south_white_pop_2016 +
                      northeast_white_pop_2016 +
                      midwest_white_pop_2016)

white_pop_avg_2016 <- round(white_pop_avg_2016, digits = 2)

combined_prison_avg <- c(white_prison_avg_2016, black_prison_avg_2016)
combined_pop_avg <- c(white_pop_avg_2016, black_pop_avg_2016)
race_identifier <- c("White", "Black")
combined_white_black_avg <- data_frame(race_identifier,
                                       combined_prison_avg, 
                                       combined_pop_avg)

#View(combined_white_black_avg)

white_black_pop_graph <- ggplot(data = combined_white_black_avg) +
                         geom_col(mapping = aes(x = race_identifier, y = combined_pop_avg),
                                  fill = "blue") +
                         labs(x = "Race",
                              y = "Total number of individuals",
                              title = "Total population of individuals within an ethnicity in 2016",
                              tag = "Variable Comparison Chart")

white_black_prison_graph <- ggplot(data = combined_white_black_avg) +
                            geom_col(mapping = aes(x = race_identifier, y = combined_prison_avg),
                            fill = "red") +
                            labs(x = "Race",
                                 y = "Total number of individuals",
                                 title = "Imprisoned population of individuals within an ethnicity in 2016",
                                 tag = "Variable Comparison Chart")

#white_black_pop_graph
#white_black_prison_graph

combined_graph <- ggplot(data = combined_white_black_avg) +
                  geom_col(mapping = aes(x = race_identifier, y = (combined_pop_avg/10)),
                           fill = "red") +
                  geom_col(mapping = aes(x = race_identifier, y = combined_prison_avg),
                           fill = "blue") +
                  labs(x = "Race",
                       y = "Total number of individuals",
                       title = "Imprisoned Population to Total Population Ratio, 2016",
                       tag = "Variable Comparison Chart") +
                  theme(legend.position = "right")
  

combined_graph

#-----# MAP #-----#

View(prison)

map_info <- prison %>%
            filter(year == 2016) %>%
            group_by(state) %>%
            summarise(black_pop_15to64 = mean(black_pop_15to64, na.rm = TRUE),
                      black_prison_pop = mean(black_prison_pop, na.rm = TRUE),
                      white_pop_15to64 = mean(white_pop_15to64, na.rm = TRUE),
                      white_prison_pop = mean(white_prison_pop, na.rm = TRUE)) %>%
            mutate(black_prison_percent = (black_prison_pop / black_pop_15to64) * 100) %>%
            mutate(white_prison_percent = (white_prison_pop / white_pop_15to64) * 100)
            
map_info$state <- state.name[match(map_info$state, state.abb)]

map_info <- mutate(map_info, state = tolower(state))

state_info <- map_data("state") %>%
              rename(state = region) %>%
              left_join(map_info, by = "state")

blank_theme <- theme_bw() +
               theme(
                 axis.line = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank()
               )

black_imprison_states <- ggplot(state_info) +
                         geom_polygon(
                           mapping = aes(
                             x = long,
                             y = lat,
                             group = group,
                             fill = black_prison_percent),
                             color = "white",
                             size = .1) +
                         coord_map() +
                         scale_fill_continuous(low = "#132B43", high = "red") +
                         labs(
                           title = "Percent of Each State's Black Population Imprisoned in 2016",
                           fill = "Percent of the Black Population"
                         ) +
                         blank_theme

white_imprison_states <- ggplot(state_info) +
                         geom_polygon(
                           mapping = aes(
                             x = long,
                             y = lat,
                             group = group,
                             fill = white_prison_percent),
                           color = "white",
                           size = .1) +
                         coord_map() +
                         scale_fill_continuous(low = "#132B43", high = "red") +
                         labs(
                           title = "Percent of Each State's White Population Imprisoned in 2016",
                           fill = "Percent of the White Population"
                         ) +
                         blank_theme

black_imprison_states
white_imprison_states
  