# load packages (and install before if not yet installed)
pacman::p_load(jsonlite, lubridate, extrafont, tidyverse)
# jsonlite for scraping json files
# lubridate for handling dates
# emoGG for froggos
# tidyverse for data handling and plotting

# scrape covid-19 data from Zeit online
data <- fromJSON("https://interactive.zeit.de/cronjobs/2020/corona/germany.json")

# Göttingen ----------------------------------------------------

# get numbers of total infections for Landkreis Göttingen 
count_g <- data$kreise$items$historicalStats$count[[26]]

# Get start and end dates
date_start <- ymd(data$kreise$meta$historicalStats$start)
date_end <- ymd(data$kreise$meta$historicalStats$end)

# Build daily sequence from start to end date
day <- seq(from = date_start, to = date_end, by = "day")

# get vector with lengths
lengths <- c(date = length(day),
             count = length(count_g))

# Landkreis Göttingen population
pop_g <- 328074

# unite data in a tibble, reduced to shortest available vector length
data_g <- tibble(
  day = day[1:min(lengths)],
  count = count_g[1:min(lengths)]
  ) %>%
  mutate(count_7dago = lag(count, 7),
         newcases_7d = count-count_7dago,
         nc_7d_perht = newcases_7d*(100000/pop_g))

# visualize
data_g %>%
  filter(day > ymd("2020-03-31")) %>%
  ggplot(aes(x = day, y = nc_7d_perht)) +
  geom_line(col = "red") +
  geom_point(aes(x,y), data = tibble(x = seq(ymd("2020-04-01"), date_end, by = "days"), y = 18), 
             shape = "\U0001F438", size = 4, col = "darkgreen") +
  theme_bw() +
  labs(x = NULL, y = NULL, 
       title = "Neuinfektionen mit Covid-19 in 7 Tagen pro 100.000 Einwohner im Landkreis Göttingen")

# save
ggsave("plot_g.png", width = 10, height = 5)  

# LK Würzburg ----------------------------------------------------------------


# get numbers of total infections for Landkreis Würzburg 
count_w <- data$kreise$items$historicalStats$count[[304]]

pop_w <- 161834 
# unite data in a tibble, reduced to shortest available vector length
data_w <- tibble(
  day = day[1:min(lengths)],
  count = count_w[1:min(lengths)]
) %>%
  mutate(count_7dago = lag(count, 7),
         newcases_7d = count-count_7dago,
         nc_7d_perht = newcases_7d*(100000/pop_w))

data_w


# visualize
data_w %>%
  filter(day > ymd("2020-03-31")) %>%
  ggplot(aes(x = day, y = nc_7d_perht)) +
  geom_line(col = "red") +
  geom_point(aes(x,y), data = tibble(x = seq(ymd("2020-04-01"), date_end, by = "days"), y = 18), 
             shape = "\U0001F438", size = 4, col = "darkgreen") +
  theme_bw() +
  labs(x = NULL, y = NULL, 
       title = "Neuinfektionen mit Covid-19 in 7 Tagen pro 100.000 Einwohner im Landkreis Würzburg")

# save
ggsave("plot_w.png", width = 10, height = 5)  


# Stadt Würzburg ----------------------------------------------------------------


# get numbers of total infections for Landkreis Würzburg 
count_sw <- data$kreise$items$historicalStats$count[[295]]

pop_sw <- 127880
# unite data in a tibble, reduced to shortest available vector length
data_sw <- tibble(
  day = day[1:min(lengths)],
  count = count_sw[1:min(lengths)]
) %>%
  mutate(count_7dago = lag(count, 7),
         newcases_7d = count-count_7dago,
         nc_7d_perht = newcases_7d*(100000/pop_w))

data_sw


# visualize
data_sw %>%
  filter(day > ymd("2020-03-31")) %>%
  ggplot(aes(x = day, y = nc_7d_perht)) +
  geom_line(col = "red") +
  geom_point(aes(x,y), data = tibble(x = seq(ymd("2020-04-01"), date_end, by = "days"), y = 18), 
             shape = "\U0001F438", size = 4, col = "darkgreen") +
  theme_bw() +
  labs(x = NULL, y = NULL, 
       title = "Neuinfektionen mit Covid-19 in 7 Tagen pro 100.000 Einwohner in der Stadt Würzburg")

# save
ggsave("plot_sw.png", width = 10, height = 5)  


# all ---------------------------------------------------------------------

data_w$county <- "LK Würzburg"
data_g$county <- "LK Göttingen"
data_sw$county <- "Stadt Würzburg"

bind_rows(data_w, data_g, data_sw) %>%
  filter(day > ymd("2020-03-31")) %>%
  ggplot(aes(x = day, y = nc_7d_perht, col = county)) +
  geom_line() +
  geom_point(aes(x,y), data = tibble(x = seq(ymd("2020-04-01"), date_end, by = "days"), y = 18), 
             shape = "\U0001F438", size = 4, col = "darkgreen") +
  theme_bw() +
  labs(x = NULL, y = NULL, col = NULL,
       title = "Neuinfektionen mit Covid-19 in 7 Tagen pro 100.000 Einwohner")

# save
ggsave("plot_all.png", width = 10, height = 5) 
