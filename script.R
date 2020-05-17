# load packages (and install before if not yet installed)
pacman::p_load(jsonlite, lubridate, tidyverse)
# jsonlite for scraping json files
# lubridate for handling dates
# tidyverse for data handling and plotting

# scrape covid-19 data from Zeit online
data <- fromJSON("https://interactive.zeit.de/cronjobs/2020/corona/germany.json")

# get numbers of total infections, dead and recovered cases for Landkreis Göttingen 
count <- data$kreise$items$historicalStats$count[[25]]
dead <- data$kreise$items$historicalStats$dead[[25]]
recovered <- data$kreise$items$historicalStats$recovered[[25]]

# Get start and end dates
date_start <- ymd(data$kreise$meta$historicalStats$start)
date_end <- ymd(data$kreise$meta$historicalStats$end)

# Build daily sequence from start to end date
day <- seq(from = date_start, to = date_end, by = "day")

# get vector with lengths
lengths <- c(date = length(day),
             count = length(count), 
             dead = length(dead), 
             recovered = length(recovered))

# unite data in a tibble, reduced to shortest available vector length
data1 <- tibble(
  day = day[1:min(lengths)],
  count = count[1:min(lengths)],
  dead = dead[1:min(lengths)],
  recovered = recovered[1:min(lengths)]
  )

# visualize
data1 %>%
  gather(type, value, -day) %>%
  ggplot(aes(x = day, y = value, col = type)) +
  geom_line()


  