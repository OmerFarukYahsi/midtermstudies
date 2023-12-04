# Chapter 3 ---------------------------------------------------------------

library(nycflights13)
library(tidyverse)

flights

class(flights)

glimpse(flights)

flights |>
  filter(dest == "IND") |>
  group_by(month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )



# Sample data frame
sales_data <- data.frame(
  Region = c("North", "South", "North", "South", "North", "South"),
  Month = c("Jan", "Jan", "Feb", "Feb", "Mar", "Mar"),
  Sales = c(100, 150, 120, 180, 130, 160)
)

# Group by Region
grouped_sales <- group_by(sales_data, Region)

# Summarize total sales for each region
summarized_data <- summarize(sales_data, Total_Sales = sum(Sales))

print(summarized_data)


flights |>
  filter(dep_delay > 120)

flights |>
  filter(month == 1 & day == 1)

flights |>
  filter(month == 1 | month == 2)

flights |>
  filter(month %in% c(1, 2))

JanFeb <- flights |>
  filter(month %in% c(1, 2))
head(JanFeb)

flights |>
  arrange(year, month, day, dep_delay)

flights |>
  distinct()

# Sample data frame
students <- data.frame(
  student_id = c(1, 2, 3, 4, 5, 6, 7, 8),
  name = c("John", "Jane", "Bob", "John", "Jane", "Alice", "Bob", "John"),
  age = c(22, 23, 22, 22, 23, 21, 22, 22)
)

# Extract distinct rows based on name and age
unique_students <- distinct(students, name, age)

print(unique_students)

flights |>
  distinct(origin, dest, .keep_all = TRUE)

flights |>
  count(origin, dest, sort = TRUE)

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60
  )

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  )

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

flights |>
  select(year, month, day)
flights |>
  select(year:day)

flights |>
  select(!year:day)

flights |>
  select(where(is.character))

flights |>
  select(tail_num = tailnum)

flights |>
  rename(tail_num = tailnum)

flights |>
  relocate(time_hour, air_time)

flights |>
  relocate(year:dep_time, .after = time_hour)

flights |>
  relocate(starts_with("arr"), .before = dep_time)

flights |>
  filter(dest == "IND") |>
  mutate(speed = distance / air_time * 60) |>
  select(year:day, dep_time, carrier, flight, speed) |>
  arrange(desc(speed))


# # chapter 4 -------------------------------------------------------------

flights |>
  filter(carrier == "UA", dest %in% c("IAH", "HOU"), sched_dep_time >
    0900, sched_arr_time < 2000) |>
  group_by(flight) |>
  summarize(delay = mean(
    arr_delay,
    na.rm = TRUE
  ), cancelled = sum(is.na(arr_delay)), n = n()) |>
  filter(n > 10)

head(flights)
str(flights)



# Lengthening Data --------------------------------------------------------

billboard_longer <- billboard |> 
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week)
  )

billboard_longer |> 
  ggplot(aes(week, rank, group=track)) +
  geom_line(alpha = 0.25) +
  scale_y_reverse()

df <- tribble(
  ~id, ~bp1, ~bp2,
  "A", 100,  120,
  "B", 140,  115,
  "C", 120,  125
)  
df |> 
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )

who2
View(who2)

who2 |> 
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count"
  )

household
View(household)

household |> 
  pivot_longer(
    cols = !family,
    names_to = c(".value", "child"),
    names_sep = "_",
    values_drop_na = TRUE
  )

# missing data ------------------------------------------------------------


# Missing Data

library(tidyverse)
library(nycflights13)

a <- NA
a
9 > NA
NA == NA

bof_selin <- NA
bof_pelin <- NA
bof_pelin == bof_selin

flights |> 
  filter(dep_time == NA)

vec <- c(2, FALSE, NA)
vec == NA

is.na(vec)

flights |> 
  filter(is.na(dep_time))

flights |> 
  filter(month == 1 & day == 1) |> 
  arrange(dep_time)

flights |> 
  filter(month == 1 & day == 1) |> 
  arrange(desc(is.na(dep_time)), dep_time)

# %in% combine == and |

NA %in% NA

flights |> 
  filter(month %in% c(11, 12))

flights |> 
  filter(dep_time %in% c(NA, 0800))

# Explicit Missing Values

treatment <- tribble(
  ~person, ~treatment, ~response,
  "Derrick Whitmore", 1, 7,
  NA, 2, 10,
  NA, 3, NA,
  "Katherine Burke", 1, 4
)
treatment

# Last observation carried forward 

# tidyr::fill()
treatment |> 
  fill(everything())

# Fixed 
# dplyr::coalesce()
x <- c(1, 4, 5, 7, NA)
x
coalesce(x, 0)

x <- c(1, 4, 5, 7, -99)
na_if(x, -99)

x[is.na(x)] = 0
x

# Nan
x <- c(NA, NaN)
x * 10
x == 1
is.na(x)
is.nan(x)
0/0
0*Inf
Inf - Inf
sqrt(-1)

# Implicit missing values 

stocks <- tibble(
  year = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr = c(1, 2, 3, 4, 2, 3, 4),
  price = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)
stocks
# Pivoting
stocks |> 
  pivot_wider(
    names_from = qtr,
    values_from = price
  )

# Complete
stocks |> 
  complete(year, qtr)

stocks |> 
  complete(year = 2019:2021, qtr)

# Joins 
flights |> 
  distinct(faa = dest) |> 
  anti_join(airports)

airports
View(airports)

flights |> 
  distinct(tailnum) |> 
  anti_join(planes)
planes


# Exercise: Can you find any relationship between the carrier
# and the rows that appear to be missing from planes?

flights |> 
  anti_join(planes, by = "tailnum") |> 
  count(carrier, sort = TRUE) |> 
  mutate(p = n/sum(n))

# Factors and empty groups
health <- tibble(name   = c("Ikaia", "Oletta", "Leriah", "Dashay", "Tresaun"), 
                 smoker = factor(c("no", "no", "no", "no", "no"), levels = c("yes", "no")),
                 age    = c(34, 88, 75, 47, 56),
)
health |> 
  count(smoker, .drop = FALSE)

ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

health |> 
  group_by(smoker) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  ) |> 
  complete(smoker)

# Regular expressions -----------------------------------------------------


babynames |> 
  count(name) |> 
  mutate(
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

babynames |> 
  count(name) |> 
  mutate(
    name = str_to_lower(name),
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

x <- c("apple", "pear", "banana")
str_replace_all(x, "[aeiou]", "-")


x <- c("apple", "pear", "banana")
str_remove_all(x, "[^aeiou]")


df <- tribble(
  ~str,
  "<Sheryl>-F_34",
  "<Kisha>-F_45", 
  "<Brandon>-N_33",
  "<Sharon>-F_38", 
  "<Penny>-F_58",
  "<Justin>-M_41", 
  "<Patricia>-F_84", 
)

df |> 
  separate_wider_regex(
    str,
    patterns = c(
      "<", 
      name = "[A-Za-z]+", 
      ">-", 
      gender = ".",
      "_",
      age = "[0-9]+"
    )
  )

df1 <- tibble(x = c("a,b,c", "d,e", "f"))
df1

df1 |>
  separate_longer_delim(x, delim = ",")


df2 <- tibble(x = c("1211", "131", "21"))
df2 |>
  separate_longer_position(x, width = 1)

df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))
df3 |>
  separate_wider_delim(x, delim = ".", names = c("code", "edition", "year"))


df3 |>
  separate_wider_delim(
    x, delim = ".",
    names = c("code", NA, "year"))

df4 <- tibble(x = c("202215TX", "202122LA", "202325CA"))
df4 |>
  separate_wider_position(
    x,widths = c(year = 4, age = 2, state = 2))

df <- tibble(x = c("1-1-1", "1-1-2", "1-3", "1-3-2", "1"))
df |>
  separate_wider_delim(
    x, delim = "-",
    names = c("x", "y", "z")
  )

debug <- df |>
  separate_wider_delim(
    x, delim = "-",
    names = c("x", "y", "z"),
    too_few = "debug"
  )

df |>
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "align_start"
  )

df <- tibble(x = c("1-1-1", "1-1-2", "1-3-5-6", "1-3-2", "1-3-5-7-9"))
df

df |>
  separate_wider_delim(
    x, delim = "-",
    names = c("x", "y", "z")
  )

debug <- df |>
  separate_wider_delim(
    x, delim = "-",
    names = c("x", "y", "z"),
    too_many = "debug"
  )

debug |> filter(!x_ok)

df |>
  separate_wider_delim(
    x, delim = "-",
    names = c("x", "y", "z"),
    too_many = "drop"
  )

df |>
  separate_wider_delim(
    x, delim = "-",
    names = c("x", "y", "z"),
    too_many = "merge"
  )

str_length(c("a", "R for data science", NA))

babynames |>
  count(length = str_length(name), wt=n)
