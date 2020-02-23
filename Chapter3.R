### Chapter 3: Data Transformation with dplyr ###

# filter, arrange, select, mutate, transmute, summarize
library(tidyverse)
library(nycflights13)
filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)

sqrt(2) ^ 2 == 2
near(sqrt(2) ^ 2, 2)

# Only selects month one, since month == 11 | month == 12
# evaluates to TRUE
nov_dec <- filter(flights, month == 11 | month == 12)
# Apparently this works though...

nov_dec <- filter(flights, month %in% c(11, 12))

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)
df <- tibble(x = c(1, NA, 3),
             y = c(5,10,3))
filter(df, y > 3)

# Find all fights that:
# had an arrival delay of two or more hours
filter(flights, arr_delay >= 120)

# flew two houston (IAH or HOU)
filter(flights, dest %in% c("IAH", "HOU"))

# operated by United, American, or Delta
filter(flights, carrier %in% c("UA", "AA", "DL"))

# departed in summer
filter(flights, month %in% c(7, 8, 9))

# arrived more than two hours late, but didn't leave late
filter(flights, arr_delay > 120, dep_delay <= 0)

# were delayed by at least an hour, but made up 
# over 30 minutes in a flight
filter(flights, dep_delay >= 60, air_time - (sched_arr_time -
    sched_dep_time) <= -30)

filter(flights, dep_time <= 360 | dep_time == 2400)

# Using between
filter(flights, between(month, 7, 9))

# Couting na
sum(is.na(flights$dep_time))

# Arrange
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

# Sort all na to the beginning
arrange(flights, desc(is.na(arr_delay)))
arrange(flights, desc(dep_delay))
arrange(flights, arr_delay)
arrange(flights, air_time)

# Select
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))
select(flights, ends_with("delay"))

# Rename
rename(flights, tail_num = tailnum)
select(flights, time_hour, air_time, everything())
select(flights, air_time, air_time)

vars <- c(
  "year", "month", "day", "dep_delay", "arr_delay"
)
select(flights, vars)

flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
)
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)

mutate(flights_sml, gain = arr_delay - dep_delay,
       speed = distance / air_time * 60,
       hours = air_time / 60,
       gain_per_hour = gain / hours)

# Only keeping new variables
transmute(flights, gain = arr_delay - dep_delay,
           speed = distance / air_time * 60,
           hours = air_time / 60,
           gain_per_hour = gain / hours)

transmute(flights, air_time, arr_time - dep_time)

summarize(flights, delay = mean(dep_delay, na.rm = F))
by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = T))

by_dest <- group_by(flights, dest)
delay <- summarize(by_dest, count = n(),
          dist = mean(distance, na.rm = T),
          delay = mean(arr_delay, na.rm = T))
delay <- filter(delay, count > 20, dest != "HNL")
ggplot(delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = F)

delay <- flights %>%
  group_by(dest) %>%
  summarize(count = n(),
            dist = mean(distance, na.rm = T),
            delay = mean(arr_delay, na.rm = T)
            ) %>%
  filter(count > 20, dest != "HNL")

batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
  group_by(playerID) %>%
  summarize(ab = sum(AB, na.rm = T),
            ba = sum(H, na.rm = T) / sum(AB, na.rm = T))
batters %>%
  filter(ab > 20) %>%
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() +
  geom_smooth()

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0])
  )

not_cancelled %>%
  group_by(dest) %>%
  summarize(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    first = min(dep_time),
    last = max(dep_time))
  )

not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))

not_cancelled %>%
  group_by(dest) %>%
  summarize(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))

not_cancelled %>%
  count(tailnum, wt = distance)

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(n_early = sum(dep_time < 500))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(n_late = mean(arr_delay > 60))

group_by(flights, year, month, day) %>%
  summarize(flights = n()) %>%
  arrange(desc(flights))

flights %>%
  group_by(year, month) %>%
  summarize(flights = n()) %>%
  arrange(desc(flights))

flights %>%
  group_by(year) %>%
  summarize(flights = n()) %>%
  arrange(desc(flights))

daily %>%
  ungroup() %>%
  summarize(flights = n())

not_cancelled %>%
  group_by(flight) %>%
  summarize(flights = n()) %>%
  arrange(desc(flights))

flights %>% filter(flight == 15) %>%
  select(origin, dest)

not_cancelled %>% 
  group_by(flight) %>%
  summarize(prop = mean(arr_delay > 300)) %>%
  arrange(desc(prop))

not_cancelled %>% count(dest)
not_cancelled %>% 
  group_by(dest) %>%
  summarize(n = n())

not_cancelled %>% count(tailnum, wt = distance)
not_cancelled %>%
  group_by(tailnum) %>%
  summarize(distance = sum(distance))

flights %>%
  group_by(year, month, day) %>%
  summarize(cancelled = sum(is.na(arr_delay)),
            avg_delay = mean(arr_delay, na.rm = T)) %>%
  ggplot(mapping = aes(x = avg_delay, y = cancelled)) +
  geom_point() +
  geom_smooth(se = F)

flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)

flights %>%
  group_by(year, month) %>%
  count(dest) %>%
  filter(rank(desc(n)) <= 3)

popular_dests %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)

not_cancelled %>%
  group_by(tailnum) %>%
  summarize(bad = mean(arr_delay > 0))

ggplot(not_cancelled, mapping = aes(x = sched_dep_time, y = arr_delay)) + 
  geom_jitter() +
  geom_smooth(se = F)

not_cancelled %>%
  mutate(block_hour = sched_dep_time %/% 100) %>%
  group_by(block_hour) %>%
  summarize(avg_delay = mean(arr_delay)) %>%
  ggplot(mapping = aes(x = block_hour, y = avg_delay)) +
  geom_point()
