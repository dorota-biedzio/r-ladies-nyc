library(meetupr)
library(tidyverse)

# getting meetup events data using meetupr package
urlname    <- c("rladies-newyork")
events     <- get_events(urlname)
dat.events <- dplyr::bind_rows(events)

# data on meetup joins downloaded manually from meetup.com stats page
file.dir    <- dirname(rstudioapi::getActiveDocumentContext()$path)
dat.joins   <- read.csv(file = file.path(file.dir,"R-Ladies New York Group Joins.csv"))
total.joins <- sum(dat.joins$value)

# cleaning "joins" data
dat.joins <- dat.joins %>%
  
  # adding value for starting membership numbers (current membership minus total in the "joins" data)
  # adding event dates that are missing from "joins" data and 
  # adding dates for months that had no new joins and are therefore missing from "joins" data
  add_row(date = "2021-05-01", value = 3155-total.joins) %>% 
  add_row(date = "2024-02-29", value = 0) %>% 
  add_row(date = "2024-03-18", value = 0) %>% 
  add_row(date = "2024-03-30", value = 0) %>% 
  add_row(date = "2024-04-01", value = 0) %>% 
  add_row(date = "2024-05-05", value = 0) %>% 
  
  arrange(date) %>% 
  rename(date_char = date) %>% 
  mutate(date = lubridate::ymd(date_char),
         csum = cumsum(value)) 

# cleaning "events" data
dat.events <- dat.events %>% 
  mutate(date = lubridate::date(time))

# merging "events" onto "joins" data
dat.all <- left_join(dat.joins,
                     dat.events,
                     by = join_by(date))

# graphing membership over time
plot <- ggplot() +
  geom_line(data = dat.all,
            mapping = aes(x = date, y = csum)) +
  scale_x_date(date_breaks = "2 months", 
               date_labels = "%Y \n %b  ") +
  geom_point(data = filter(dat.all, !is.na(id)),
             mapping = aes(x = date, y = csum)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 3,
                                          color = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_text(color = "black"),
        plot.caption = element_text(hjust = 0)) +
  labs(title = "R-Ladies NYC Meetup Membership Growth Over Time",
       subtitle = "Points Represent Meetup Events \n",
       caption = "\n Data from Meetup.com for the last three years as of May 5th, 2024.")

ggsave(filename = file.path(file.dir,"rladies_nyc_membership.png"), plot = plot)