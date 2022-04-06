library(jsonlite)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggrepel)

streams_life <- data.frame()
for(i in 1:7){
  filepath <- paste("Data2/endsong_",i-1,".json", sep="")
  x <- fromJSON(filepath)
  x <- data.frame(x)
  streams_life <- rbind(streams_life, x)
}
streams_life <- streams_life %>% 
  rename(track = master_metadata_track_name, artist = master_metadata_album_artist_name, album = master_metadata_album_album_name, uri = spotify_track_uri)
streams21 <- streams_life %>% 
  filter(year(ts)=="2021", ms_played >= 60000) %>% #only 2021, only songs listened to for at least one minute
  select(ts, platform, ms_played, track, artist, album, uri, reason_start, reason_end, shuffle, skipped) %>% 
  mutate(sec = ms_played / 1000)
streams21$ts <- as.POSIXct(streams21$ts, tz="UTC", format = "%Y-%m-%dT%H:%M:%OSZ")
streams21$date <- as.Date(streams21$ts)
streams21$date <- gsub("2021-","",streams21$date)
streams21$time <- streams21$ts
streams21$time <- format(as.POSIXct(streams21$ts), format = "%H:%M")

#first graph
tswizzle <- streams21 %>% 
  filter(artist == "Taylor Swift") %>% 
  group_by(track) %>% 
  summarize(plays = n()) %>% 
  arrange(desc(plays))
tswizzle[1:20,] %>% 
  kbl() %>% 
  kable_styling()
ggplot(tswizzle[1:20,]) +
  geom_bar(aes(x=track, y=plays), stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Top 20 Taylor Swift Songs")
ggsave("Graphs/plot1.png")

#second graph
ggplot(tswizzle[1:20,]) +
  geom_bar(aes(x=track, y=plays, fill="light red"), stat="identity") +
  geom_text(aes(x=track, y=plays, label=plays), vjust="top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),panel.background = element_rect(fill="white"), axis.line=element_line(colour="black")) +
  labs(title = "Top 20 Taylor Swift Songs", x="Song", y="Number of Plays") +
  ggplot2::annotate("text", x=18, y = 68, label="Data: Spotify", color= "light gray") +
  ggplot2::annotate("text", x=18, y=65, label = "Viz: @KaraGodsey", color = "light gray") +
  guides(fill = FALSE)
ggsave("Graphs/plot2.png")

#third graph
streams_noskips <- streams21 %>% 
  mutate(day = yday(ts), month= month(ts))
byday <- streams_noskips %>% 
  group_by(day) %>% 
  summarize(plays = n())
bymonth <- streams_noskips %>% 
  group_by(month) %>% 
  summarize(plays = n())
ggplot(byday, aes(day, plays)) +
  geom_point(color="green") +
  geom_line() +
  labs(title = "Daily Song Totals", x = "Day", y = "Number of Songs") +
  theme(panel.background = element_rect(fill="white"), axis.line=element_line(colour="black"))
ggsave("Graphs/plot3.png")

#fourth graph
ggplot(bymonth, aes(month, plays)) +
  geom_point(color="green") +
  geom_line(color="green") +
  labs(title = "Plays by Month", x = "Month", y = "Number of Plays") +
  theme(panel.background = element_rect(fill="black"), axis.line=element_line(colour="green"), panel.grid = element_line("black"))
ggsave("Graphs/plot4.png")

#fifth graph
ggplot(bymonth, aes(month, plays)) +
  geom_bar(color="green3",stat="identity", fill="green3") +
  labs(title = "Plays by Month", x = "Month", y = "Number of Plays") +
  theme(panel.background = element_rect(fill="black"), axis.line=element_line(color="green3"), panel.grid = element_line("black"), plot.background =  element_rect(fill="black"), axis.text = element_text(colour = "white"), axis.title = element_text(colour = "white"), title = element_text(colour = "white", hjust=0.5), plot.title=element_text(hjust=0.5)) +
  geom_text(aes(x=month, y=plays, label=plays), vjust="top", color = "black") +
  scale_x_continuous(breaks=seq(1,12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  coord_cartesian(clip = "off")
ggsave("Graphs/plot5.png")

#sixth graph
taylor <- streams21 %>% 
  filter(artist == "Taylor Swift") %>% 
  select(track, album, ms_played) %>% 
  group_by(track, album) %>% 
  summarize(plays = n()) %>% 
  arrange(desc(plays))
taylor2 <- taylor %>% 
  mutate(track = gsub(" \\(Taylor’s Version\\)", "",track),
         album = gsub(" \\(Taylor's Version\\)","",album))
taylor[1:20,] %>% 
  kbl() %>% 
  kable_styling()
ts_albums <- taylor %>% 
  group_by(album) %>% 
  summarize(plays = sum(plays)) %>% 
  mutate(color = 
           case_when(album=="Fearless" ~ "gold",
                     album=="Fearless (Taylor's Version)" ~ "gold",
                     album=="Speak Now" ~ "purple",
                     album=="Red" ~ "red",
                     album=="Red (Taylor's Version)" ~ "red",
                     album=="1989" ~ "lightblue",
                     album=="eputation" ~ "black",
                     album=="Lover" ~ "hotpink",
                     album=="folklore" ~ "gray",
                     album=="evermore" ~ "goldenrod4",
                     album=="All Too Well (Sad Girl Autumn Version) - Recorded at Long Pond Studios" ~ "Red",
                     TRUE ~ "green"
           )
  ) 
library(gplots)
ts_albums$hex <- gplots::col2hex(ts_albums$color)
tsColors <- ts_albums$hex
names(tsColors) <- ts_albums$album
taylor <- taylor %>% 
  left_join(ts_albums, by=c("album"="album")) %>% 
  rename(tot_album_plays = plays.y, plays = plays.x)
ggplot(taylor[1:20,]) +
  geom_bar(aes(x=track, y=plays, fill=album), stat="identity") +
  scale_fill_manual(values=tsColors, limits=c("Red","Fearless (Taylor's Version)","evermore","folklore","Lover","Red (Taylor's Version)","All Too Well (Sad Girl Autumn Version) - Recorded at Long Pond Studios")) +
  geom_text(aes(x=track, y=plays, label=plays), vjust=-0.25) +
  labs(title = "Top 20 Taylor Swift Songs", x="Song", y="Number of Plays") +
  ggplot2::annotate("text", x=18, y=68, label="Data: Spotify", color="light gray") +
  ggplot2::annotate("text", x=18, y=65, label="Viz: @KaraGodsey", color="light gray") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1), panel.background = element_rect(fill="white"), axis.line=element_line(colour="black"),title=element_text(hjust=0.5))
ggsave("Graphs/plot6.png")

#seventh graph
s21cal <- streams21
s21cal$day_num <- day(s21cal$ts)
s21cal$month <- month(s21cal$ts)
s21cal$year <- year(s21cal$ts)
s21cal$weekday <- wday(s21cal$ts, label=TRUE)
s21cal$month_name <- factor(format(s21cal$ts, "%B"), levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
s21cal$day <- factor(s21cal$weekday, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
s21cal$week_num <- strftime(s21cal$ts, format="%V")
s21cal[s21cal$week_num>=52 & s21cal$month==1,"week_num"]=00
s21cal$week_num <- as.integer(s21cal$week_num)
week_month <- s21cal %>% 
  group_by(year, month_name, week_num) %>% 
  summarise() %>% 
  mutate(week_month_num=row_number())
s21cal <- s21cal %>% 
  left_join(week_month, by=c("month_name"="month_name", "week_num"="week_num", "year"="year"))
s21cal1 <- s21cal %>% filter(month==1)
ggplot(s21cal) +
  geom_tile(mapping=aes(x=day,y=week_month_num), fill=NA) +
  geom_text(mapping=aes(x=day,y=week_month_num, label=day_num), color="black") +
  geom_point(mapping=aes(x=day,y=week_month_num),color="green3", size=8) +
  geom_text(mapping=aes(x=day, y=week_month_num, label=day_num), color="white") +
  scale_y_reverse() +
  coord_fixed() +
  facet_wrap(~month_name) +
  theme(panel.background = element_rect(fill="black"), axis.line=element_line(color="green3"), panel.grid = element_line("black"), plot.background =  element_rect(fill="black"), axis.text = element_text(colour = "white"), axis.title = element_text(colour = "white"), title = element_text(colour = "white", hjust=0.5), plot.title=element_text(hjust=0.5))
ggplot(s21cal1, aes(x=day_num,y=ms_played)) + 
  geom_bar(stat="identity") +
  coord_polar(theta="y") +
  facet_wrap(~day_num)

#time series
library(TSA)

#tsdata <- streams21 %>%
#  mutate(Date = as.Date(ts)) %>%
#  complete(Date = seq.Date(min(Date), max(Date), by="day"))
tsdata <- streams21 %>% 
  group_by(date) %>% 
  summarize(songs = n())
songs <- ts(tsdata, start=)

sequence <- seq.POSIXt(as.POSIXct("2021-01-01 0:00",'%m/%d/%y %H:%M', tz="UTC"), as.POSIXct("2021-12-31 23:59",'%m/%d/%y %H:%M', tz="UTC"), by="day")
df <- data.frame(timestamp=sequence)
tsdata <- full_join(df,tsdata$songs)

#total songs listened/unique songs listened
eachsong <- streams21 %>%
  group_by(artist, track) %>% 
  summarize(listens = n())
uniquesongs <- eachsong %>% 
  group_by(artist) %>% 
  summarize(songs = n())
totalsongs <- eachsong %>% 
  group_by(artist) %>% 
  summarize(listens = sum(listens))
songs <- uniquesongs %>% 
  left_join(totalsongs, by=c("artist"))
ggplot(filter(songs, artist != "Taylor Swift"), aes(x=songs, y=listens, label=artist)) +
  geom_point() +
  geom_text_repel(data=filter(songs, artist != "Taylor Swift" & (songs >= 50 | listens >= 300)),
                              min.segment.length = unit(0, 'lines')) + #hjust=0.75,vjust=1 
  labs(title = "Distinct vs Total Listens", x = "Unique Songs", y = "Total Songs")
songs2 <- songs %>% 
  mutate(replays = listens - songs, replaypct = listens/replays)
ggplot(songs2, aes(x=replays, y=listens, label=artist)) +
  geom_point() +
  geom_text() +
  geom_abline(aes(intercept=0, slope=1))
