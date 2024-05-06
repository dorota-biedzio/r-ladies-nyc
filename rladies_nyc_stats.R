library(meetupr)
library(tidyverse)

# getting location of this R script
file.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# getting meetup events data using meetupr package
urlname <- c("rladies-newyork")
events  <- get_events(urlname)
members <- get_members(urlname)

# cleaning "members" data
dat.members <- members %>% 
  mutate(date = lubridate::date(created)) %>% 
  group_by(date) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  distinct(date,n) %>% 
  arrange(date) %>% 
  mutate(csum = cumsum(n))

# cleaning "events" data
dat.events <- dplyr::bind_rows(events) %>% 
  mutate(date = lubridate::date(time))

# getting total events and members
tot.members <- max(dat.members$csum)
tot.events <- n_distinct(dat.events$id)

# merging "events" onto "members" data
dat.all <- left_join(dat.members,
                     dat.events,
                     by = join_by(date))

# graphing membership over time
plot <- ggplot() +
  geom_line(data = dat.all,
            mapping = aes(x = date, y = csum)) +
  scale_x_date(date_breaks = "4 months", 
               date_labels = "%Y \n %b  ") +
  geom_point(data = filter(dat.all, !is.na(id)),
             mapping = aes(x = date, y = csum)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = 3,
                                          color = "darkgrey"),
        axis.title = element_blank(),
        axis.text = element_text(color = "black"),
        plot.caption = element_text(hjust = 0)) +
  labs(title = "R-Ladies NYC Meetup Membership Over Time",
       subtitle = paste(paste("Total members:", tot.members),
                        paste("Total events:", tot.events),
                      sep = "\n"),
       caption = "\n Data pulled from Meetup.com using the `meetupr` R package. \n Points represent individual events.")

ggsave(filename = file.path(file.dir,"rladies_nyc_membership.png"), 
       plot = plot,
       height = 4,
       width = 7.5,
       units = "in")
