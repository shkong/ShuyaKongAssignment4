# Assignment 4

# 0:
print(paste(
  "Shuya",
  "Kong",
  "1505077",
  sep = " "
  )
)

# 1:
library(foreign)

flights <- read.csv(
  url("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/flights.csv"),
  stringsAsFactors = FALSE
)


planes <- read.csv(
  file = "https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/planes.csv",
  stringsAsFactors = FALSE
)

weather <- read.csv(
  url("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/weather.csv"),
  stringsAsFactors = FALSE
)

airports <- read.csv(
  url("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/airports.csv"),
  stringsAsFactors = FALSE
)

# 2:
flights$date <- as.Date(flights$date)
weather$date <- as.Date(weather$date)

# 3:
require(dplyr)

# flights that went to SFO or OAK
flights.2a <- flights %>%
  dplyr::filter(
    dest == "SFO" | dest == "OAK"
  )

print(nrow(flights.2a))

# flights that delayed by 1 hour or more
flights.2b <- flights %>%
  dplyr::filter(
    dep_delay >= 60 | arr_delay >= 60
  )

print(nrow(flights.2b))

# flights in which arr_delay more than twice as much as dep_delay
flights.2c <- flights %>%
  dplyr::filter(
    arr_delay > 2*dep_delay
  )

print(nrow(flights.2c))


# 4:
# select delay variables in three different ways

flights.4a <- flights %>%
  select(
    ends_with("delay")
  )

flights.4b <- flights %>%
  select(
    contains("delay")
  )

flights.4c <- flights %>%
  select(
    matches("._.")
  )


# 5:
# top 5 most departure delayed flights
flights.5a <- flights %>%
  arrange(desc(dep_delay)) %>%
  head(5)

print(flights.5a$flight)
  
# top 5 caught up the most
flights.5b <- flights %>%
  arrange(desc((dep_delay) - (arr_delay))) %>%
  head(5)

print(flights.5b$flight)


#6
# new column: speed in mph
flights <- flights %>%
  mutate(speed = dist/(time/60))

# new column: delta
flights <- flights %>%
  mutate(delta = dep_delay - arr_delay)

# top 5 highest speed flights
flights.6a <- flights %>%
  arrange(desc(speed)) %>%
  head(5)

print(flights.6a$flight)

# top 5 made up the most
flights.6b <- flights %>%
  arrange(desc(delta)) %>%
  head(5)

print(flights.6b$flight)
# which matches 5b


# top 5 lost the most time
flights.6c <- flights %>%
  arrange(delta) %>%
  head(5)

print(flights.6c$flight)


# 7:
# create individual tables first:

cancelled_flights <- flights %>%
  group_by(carrier) %>%
  summarize(
    min_cancelled = min(cancelled, na.rm=T),
    q1_cancelled = quantile(cancelled, .25, na.rm=T),
    avg_cancelled = mean(cancelled, na.rm=T),
    med_cancelled = median(cancelled, na.rm=T),
    q3_cancelled = quantile(cancelled, .75, na.rm=T),
    quan90_cancelled = quantile(cancelled, .90, na.rm=T),
    max_cancelled = max(cancelled, na.rm=T),
    total_flights = n()
  )
    
  
percent_cancelled <- flights %>%
  group_by(carrier) %>%
  summarize(
    min_percent = min(cancelled/n(), na.rm=T),
    q1_percent = quantile(cancelled/n(), .25, na.rm=T),
    avg_percent = mean(cancelled/n(), na.rm=T),
    med_percent = median(cancelled/n(), na.rm=T),
    q3_percent = quantile(cancelled/n(), .75, na.rm=T),
    quan90_percent = quantile(cancelled/n(), .90, na.rm=T),
    max_percent = max(cancelled/n(), na.rm=T)
  )

delta <- flights %>%
  group_by(carrier) %>%
  summarize(
    min_delta = min(delta, na.rm=T),
    q1_delta = quantile(delta, .25, na.rm=T),
    avg_delta = mean(delta, na.rm=T),
    med_delta = median(delta, na.rm=T),
    q3_delta = quantile(delta, .75, na.rm=T),
    quan90_delta = quantile(delta, .90, na.rm=T),
    max_delta = max(delta, na.rm=T)
  )

# join tables together:
join1 <- left_join(cancelled_flights, percent_cancelled, by = "carrier")
flights.7a <- left_join(join1, delta, by = "carrier")

# written answer:
print("the code summarizes the mean value of flights'time of departure delay,
      excluding NAs, grouping by date")

# rewrite code:
cat("
  day_delay <- flights %>%
    group_by(date) %>%
    summarize(
      delay = mean(dep_delay, na.rm=T),
      n = n()
      ),
  n > 10
")


# 9:
dest_delay <- flights %>%
  group_by(dest) %>%
  summarize(
    arr_delay = mean(arr_delay),
    n = n()
  )

airports <- airports %>%
  select(dest=iata, name=airport, city, state, lat, long)

# left join
df.9a <- left_join(
  dest_delay,
  airports,
  by = "dest"
)

# top five city and states
table <- df.9a %>%
    arrange(desc(arr_delay)) %>%
    head(5)
  
print(paste(
  table$city, 
  table$state
  )
)


# inner join
df.9b <- inner_join(
  dest_delay,
  airports,
  by = "dest"
)

print(paste(
  nrow(df.9a),
  nrow(df.9b)
  )
)

# answer
print("the numbers of observations do not match")

# right join
df.9c <- right_join(
  dest_delay,
  airports,
  by = "dest"
)

# number of obsr
print(nrow(df.9c))

# NAs
print("yes, there are NAs in arr_delay")

# full join
df.9d <- full_join(
  dest_delay,
  airports,
  by = "dest"
)

# NAs
print("yes, there are NAs in arr_delay")

# 11 a):
library(tidyr)
df <- data.frame(treatment = c("a", "b"), subject1 = c(3,4), subject2 = c(5,6))

df1 <- df %>%
  mutate(subject = c(1,1)) %>%
  select(subject, treatment, value = subject1)

df2 <- df %>%
  mutate(subject = c(2,2)) %>%
  select(subject, treatment, value = subject2)

df.11a <- full_join(
  df1,
  df2
)

print(df.11a)

# 11 b):
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)

df.11b <- df %>%
  spread(subject, value)
  
colnames(df.11b)[2] <- "subject1"
colnames(df.11b)[3] <- "subject2"

print(df.11b)

# 11 c):
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)

df.11c <- df %>%
  separate(demo, c("sex", "age", "state"), remove=T)

print(df.11c)

df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)

df.11d <- df %>%
  unite(demo, sex, age, city, sep=".", remove=T)

df.11d$demo[4] <- NA

print(df.11d)
