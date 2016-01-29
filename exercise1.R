### Exercise 1 ###

# Install the nycflights13 package and read it in.  Require the dplyr package
# install.packages("nycflights13")
library(nycflights13)
library(dplyr)

# The data.frame flights should now be accessible to you.  View it, 
# and get some basic information about the number of rows/columns
View(flights)
nrow(flights)
ncol(flights)
dim(flights)

# Add a column that is the amount of time gained in the air
flights2 <- mutate(flights, gained_time = arr_delay - dep_delay)

# Sort your data.frame desceding by the column you just created
flights2 <- arrange(flights2, desc(gained_time))

# Try doing the last 2 steps in a single operation using the pipe operator
flights2 <- flights2 %>% 
            mutate(flights, gained_time = arr_delay - dep_delay) %>%
            arrange(desc(gained_time))

# Make a histogram of the amount of gain using the `hist` command
hist(flights2$gained_time)

# On average, did flights gain or lose time?
average_gain <- mean(flights2$gained_time, na.rm = TRUE)

# Create a data.frame of flights headed to seatac ('SEA'), 
# and only include the column you just created
flights_sea <- flights2 %>% filter(dest == 'SEA')

# On average, did flights to seatac gain or loose time?
average_gain_sea <- mean(flights_sea$gained_time, na.rm = TRUE)

# Write a function that allows you to specify an origin, a destination, and a column of interest
# that returns a data.frame of flights from the origin to the destination and only the column of interest
## Hint: see slides on standard evaluation
flight_info <- function(from, destination, col_interest) {
   to_from <- flights2 %>% 
             filter(dest == destination, origin == from) %>% 
             select_(col_interest)
   return(to_from)
}

# Retireve the air_time column for flights from JFK to SEA
JFK_to_SEA_airtime <- flight_info('JFK', 'SEA', 'air_time')

# What was the average air time of those flights (in hours)?  What was the min/max?
mean(JFK_to_SEA_airtime$air_time, na.rm = TRUE)

### Bonus ###
# Rewrite the function above to return a list of the min, max, and mean values of the column of interest


# Calculate the departure delays from JFK to DEN

