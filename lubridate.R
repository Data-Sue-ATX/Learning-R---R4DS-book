#R4DS Date and times with Lubridate

#Working through the book and trying to also learn how to use GitHub.  Had some issues with the code in this 
#will check online to see if updates were made to the code and add to the file. 
library(tidyverse)
library(lubridate)
library(nycflights13)


today()
now()
ymd("2018-01-20")
mdy("January 20, 2018")
dmy("20-Jan-2018")

ymd_hms("2018-01-20 17:05:05")

ymd(20180120, tz = "UTC")



flights %>%
  select(year, month, day, hour, minute)

flights %>%
  select(year, month, day, hour, minute)
mutate(
  departure = make_datetime(year, month, day, hour, minute, tz="UTC")
)

flights_dt <-  flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

flights_dt %>%
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400)

#within single day

flights_dt %>%
  filter(dep_time < ymd(20130102)) %>%
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600) #10 minutes

flights_dt %>%
  filter(dep_time < ymd(20130102)) %>%
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 1200) #20 minutes

#From Other Types

as_datetime(today())

as_date(now())


as_datetime(60*60*10)

as_date(365 * 10 + 2)

#Exercises 

ymd(c("2010-10-10", "bananas"))  #fails to parse

today(tzone = "EST")

datetime  <-  ymd_hms("2018-01-20 12:34:56")
year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)

month(datetime, label = TRUE)
wday(datetime, label = TRUE)


flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>%
  ggplot(aes(x=wday)) + 
  geom_bar()



flights_dt %>%
  mutate(minute = minute(dep_time)) %>%
  group_by(minute) %>%
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>%
  ggplot(aes(minute, avg_delay)) + 
  geom_line()


#Code matches book but doesnt work..... 
sched_dep <- flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE), 
    n = n())


#Rounding 
flights_dt %>%
  count(week - floor_date(dep_time, "week")) %>%
  ggplot(aes(week, n)) + geom_line()


head(flights_dt)


#Having issues with many of the time values, will have to do some research and see if any edits since book print have been done 

#Age

s_age <-  today() - ymd(19671110)
s_age

as.duration(s_age)

s_age_seconds <- (as.duration(s_age))

flights_dt %>%
  filter(arr_time < dep_time)
flights_dt %>%
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight *1)
  )

#Intervals

years(1)/days(1)

next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)

