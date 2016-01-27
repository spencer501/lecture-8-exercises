### Exercise 1 ###

# Install the nycflights13 package and read it in.  Require the dplyr package
# install.packages("nycflights13")
library(nycflights13)
library(dplyr)

# The data.frame flights should now be accessible to you.  View it, 
# and get some basic information about the number of rows/columns
View(flights)
dim(flights)
nrow(flights)
ncol(flights)
?flights

# Add a column that is the amount of time gained in the air
flights <- mutate(flights, gain = arr_delay - dep_delay)

# Sort your data.frame desceding by the column you just created
flights <- arrange(flights, desc(gain))

# Try doing the last 2 steps in a single operation using the pipe operator
flights <- flights %>% mutate(gain = arr_delay - dep_delay) %>% arrange(desc(gain))

# Make a histogram of the amount of gain using the `hist` command
hist(flights$gain)

# On average, did flights gain or lose time?
mean(flights$gain, na.rm = TRUE) # Lost 5 minutes!

# Create a data.frame of flights headed to seatac ('SEA'), 
# and only include the column you just created
to_sea <- flights %>% select(gain, dest) %>% filter(dest == 'SEA') 

# On average, did flights to seatac gain or loose time?
mean(to_sea$gain, na.rm = TRUE) # Lost 11 minutes!

# Write a function that allows you to specify an origin, a destination, and a column of interest
# that returns a data.frame of flights from the origin to the destination and only the column of interest
## Hint: see slides on standard evaluation
origin_dest_interest <- function(my_origin, my_dest, interest) {
  ret <- flights %>% filter(origin == my_origin, dest == my_dest) %>% select_(interest)
  return(ret)
}

# Retireve the air_time column for flights from JFK to SEA
jfk_to_sea <- origin_dest_interest('JFK', 'SEA', 'air_time')

# What was the average air time of those flights (in hours)?  What was the min/max?
mean(jfk_to_sea$air_time, na.rm = TRUE)/60
min(jfk_to_sea$air_time, na.rm = TRUE)/60
max(jfk_to_sea$air_time, na.rm = TRUE)/60

### Bonus ###
# Rewrite the function above to return a list of the min, max, and mean values of the column of interest
origin_dest_interest_info <- function(my_origin, my_dest, interest) {
  df <- flights %>% filter(origin == my_origin, dest == my_dest) %>% select_(interest)
  info <- list()
  
  # Write functions needed to calculate
  mean_fn <- paste0('mean(', interest, ', na.rm=TRUE)')
  min_fn <- paste0('min(', interest, ', na.rm=TRUE)')
  max_fn <- paste0('max(', interest, ', na.rm=TRUE)')
  
  # Calculate info
  info$mean <- summarise_(df, mean_fn)[[1]]
  info$min <- summarise_(df, min_fn)[[1]]
  info$max <- summarise_(df, max_fn)[[1]]
  return(info)
}

# Calculate the departure delays from JFK to DEN
jfk_to_den_info <- origin_dest_interest_info('JFK', 'DEN', 'dep_delay')
