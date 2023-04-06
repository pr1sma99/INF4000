# include packages
library(readr)
library(ggplot2)
library(tidyverse)
library(arrow)
library(data.table)
library(lubridate)
library(GGally)
library(rgdal)
library(RColorBrewer)
library(ggmap)
library(ggforce)
library(ggthemes)
library(showtext)
library(gridExtra)


# load fonts from Google
font_add_google("Lora")
showtext_auto()

# set time display to English
Sys.setlocale("LC_TIME", "English")


# read files from list and bind into data frames
yellow_list <-
  list.files(path = "./", pattern = "yellow*") %>%
  lapply(read_parquet)

green_list <-
  list.files(path = "./", pattern = "green*") %>%
  lapply(read_parquet)

yellow <- rbindlist(yellow_list) %>%
  select(c(2:5, 8:11, 14)) %>%
  rename(pickup_datetime = tpep_pickup_datetime) %>%
  rename(dropoff_datetime = tpep_dropoff_datetime) %>%
  mutate(type = "yellow") %>%
  drop_na()

green <- rbindlist(green_list) %>%
  select(c(2:3, 6:10, 13, 18)) %>%
  rename(pickup_datetime = lpep_pickup_datetime) %>%
  rename(dropoff_datetime = lpep_dropoff_datetime) %>%
  mutate(type = "green") %>%
  drop_na()

# filter and cleanse data
taxi <- rbind(yellow, green, fill = TRUE) %>%
  subset(
    pickup_datetime > as.POSIXct("2022-01-01 00:00:00.00") &
      dropoff_datetime < as.POSIXct("2022-09-30 23:59:59.00")
  ) %>% # drop entry before 2022
  subset(payment_type == 1 |
           payment_type == 2) %>% # drop unknown payments
  subset(passenger_count > 0 &
           passenger_count < 7) %>% # normal passenger
  subset(trip_distance > 0 &
           trip_distance < 25) %>% # eliminate outliers
  subset(fare_amount > 0 & fare_amount < 75) %>%
  subset(tip_amount < fare_amount) %>%
  mutate(
    date = as.Date(pickup_datetime),
    month = month(pickup_datetime),
    hour = hour(pickup_datetime),
    duration = difftime(dropoff_datetime, pickup_datetime,
                        units = "mins")
  )

rm(yellow, green, yellow_list, green_list)


# summarise monthly data
taxi_month_summary <- taxi %>%
  group_by(type, month) %>%
  summarise(
    order_month = n(),
    fare_month = sum(fare_amount),
    trip_month = sum(trip_distance),
    passenger_month = sum(passenger_count)
  )

# plot hourly count of orders
taxi_hour <- taxi %>%
  group_by(hour) %>%
  count()

# plot descriptive distributions
ggplot(taxi, aes(x = fare_amount, fill = factor(payment_type))) +
  geom_histogram(bins = 30, position = "dodge", alpha = 0.9, col = 'white') +
  labs(x = "Fare amount $",
       y = "Count #") +
  scale_fill_tableau(name = "Payment type",
                     labels = c("Credit card", "Cash")) +
  theme_hc() +
  theme(text = element_text(family = "Lora", size = 14)) 

payment_proportion <- taxi %>%
  group_by(payment_type) %>%
  summarise(total = sum(fare_amount))

type_proportion <- taxi %>%
  group_by(type) %>%
  count()

ggplot(payment_proportion, aes(
  x = '',
  y = total,
  fill = factor(payment_type)
)) +
  geom_bar(stat = "identity",
           width = 1,
           color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_tableau(name = "",
                     labels = c("79.4%", "20.6%")) +
  theme_void() +
  theme(text = element_text(family = "Lora", size = 14)) +
  theme(legend.position = 'bottom')

ggplot(type_proportion, aes(
  x = '',
  y = n/sum(n),
  fill = factor(type)
)) +
  geom_bar(stat = "identity", 
           color ='white',
           width = 1,
           alpha = 0.9) +
  coord_polar("y", start = 0) +
  scale_fill_manual(name = "",
                    labels = c("1.9%", "98.1%"),
                    values = c("#7EB77F", "#F7b731")) +
  theme_void() +
  theme(text = element_text(family = "Lora", size = 14)) +
  theme(legend.position = 'bottom')

ggplot(taxi_hour, aes(x = hour, y = n / 273)) +
  geom_col(position = "dodge", fill = "#7EB77F", alpha = 0.9) +
  labs(x = "24 hours",
       y = "Daily average order #") +
  theme_hc() +
  theme(text = element_text(family = "Lora", size = 14))

ggplot(taxi, aes(x = tip_amount / fare_amount * 100)) +
  geom_histogram(bins = 25, fill = "#7EB77F", color = "white", lwd = 1, alpha = 0.9) +
  labs(x = "Tip percentage %",
       y = "Count #") +
  theme_hc() +
  theme(text = element_text(family = "Lora", size = 14))


date_count <- taxi %>% 
  group_by(date) %>% 
  count() %>% 
  mutate(month = month(date, label = TRUE, abbr = TRUE))

ggplot(date_count, aes(x = date, y = n)) +
  geom_line(color = "#F7b731") +
  geom_point(color = "#7EB77F") +
  labs(x = '', y = 'Order #') +
  theme_hc() +
  theme(text = element_text(family = "Lora")) +
  facet_wrap(~ month, scales = "free")


ggplot(taxi_month_summary, aes(x = factor(month), y = order_month)) +
  geom_col(aes(fill = type), position = "stack", alpha = 0.9, width = 0.6) +
  labs(x = "Month",
       y = "Order #") +
  scale_fill_manual(name = "Taxi type",
                    labels = c("Green", "Yellow"),
                    values = c("#7EB77F", "#F7b731")) +
  scale_x_discrete(labels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep')) +
  theme_hc() +
  theme(text = element_text(family = "Lora", size = 14)) +
  facet_wrap(~ type, scales = "free")


# count occurrence of trips between zones
link_data <- taxi %>%
  group_by(PULocationID) %>%
  count(DOLocationID, sort = TRUE) %>%
  filter(PULocationID < 264 & DOLocationID < 264)

# plot heatmap of links
ggplot(link_data, aes(PULocationID, DOLocationID)) +
  geom_tile(aes(fill = log(n))) +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_light()

location_count <-
  cbind(
    count(taxi, PULocationID, name = "PU_count"),
    count(taxi, DOLocationID,  name = "DO_count")
  ) %>%
  mutate(n = PU_count + DO_count) %>%
  select(PULocationID, n)

location_count <- location_count %>%
  add_row(PULocationID = 103, n = location_count[PULocationID == 105]$n) %>%
  add_row(PULocationID = 104, n = location_count[PULocationID == 105]$n) %>%
  add_row(PULocationID = 110, n = location_count[PULocationID == 109]$n) %>%
  filter(PULocationID < 264) %>%
  arrange(PULocationID)

taxi_zones <- readOGR(dsn = "nyc_taxi_zones", layer = "taxi_zones")

summary(fortify(taxi_zones))

taxi_zones@data$count <- location_count$n

taxi_zones_df <- fortify(taxi_zones)

values <-
  data.frame(
    id = c(0:262),
    count = taxi_zones$count,
    borough = taxi_zones$borough
  )

taxi_zones_df <- merge(taxi_zones_df, values, by = c("id"))

qmplot(long,
       lat,
       data = taxi_zones_df,
       geom = "blank",
       maptype = "toner-lite") +
  geom_polygon(aes(group = group, fill = log10(count)), alpha = 0.8) +
  scale_fill_gradient2_tableau(name = "Log scale\nof order #",
                               palette = "Orange-Blue-White Diverging", 
                               guide = 'legend',
                               trans = "reverse")

taxi_zones_coordinate <- data.frame(long = double(), lat = double())

for (polygon in taxi_zones@polygons) {
  taxi_zones_coordinate <- taxi_zones_coordinate %>%
    add_row(long = polygon@labpt[1], lat = polygon@labpt[2])
}

plot_link <- function(link) {
  df <- data.frame(
    x = double(),
    y = double(),
    xend = double(),
    yend = double(),
    count = integer()
  )
  for (i in 1:nrow(link)) {
    df <- df %>%
      add_row(
        x = taxi_zones_coordinate[as.integer(link[i, 1]), 1],
        y = taxi_zones_coordinate[as.integer(link[i, 1]), 2],
        xend = taxi_zones_coordinate[as.integer(link[i, 2]), 1],
        yend = taxi_zones_coordinate[as.integer(link[i, 2]), 2],
        count = as.integer(link[i, 3])
      )
  }
  plot <- qmplot(x,
                 y,
                 data = df,
                 geom = "blank",
                 maptype = "toner-lite") +
    geom_link(
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        color = count,
        alpha = scales::rescale(count, to = c(0, 0.75))
      ),
      show.legend = FALSE,
      lineend = "round"
    ) +
    scale_color_gradient(low = "coral", high = "darkred") +
    geom_point(
      aes(x = x, y = y, size = count),
      show.legend = FALSE,
      alpha = 0.75,
      color = "red3"
    )
  
  return(plot)
}


link_above_10k <- subset(link_data, n > 10000)
link_inter_borough <-
  subset(link_above_10k, taxi_zones$borough[PULocationID] != taxi_zones$borough[DOLocationID])

link_plot <- plot_link(link_inter_borough)
link_plot
