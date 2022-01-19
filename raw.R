#install.packages("jsonlite")
library(jsonlite)
x1 <- fromJSON("Data/StreamingHistory0.json")
x1 <- data.frame(x1)
x2 <- fromJSON("Data/StreamingHistory1.json")
x2 <- data.frame(x2)
x3 <- fromJSON("Data/StreamingHistory2.json")
x3 <- data.frame(x3)
streams <- rbind(x1,x2,x3)
View(streams)
library <- fromJSON("Data/YourLibrary.json", flatten=TRUE)
#View(library)
str(library)
lib.tracks <- data.frame(library$tracks)
lib.albums <- data.frame(library$albums)
lib.artists <- data.frame(library$artists)
playlist <- fromJSON("Data/Playlist1.json")
playlists <- playlist$playlists
View(lib.albums)
View(lib.tracks)
streams <- streams %>% 
  mutate(sec = msPlayed / 1000)
streams$endTime <- as.POSIXct(streams$endTime)
tswizzle <- streams %>% 
  filter(format(endTime, "%Y") == 2021, artistName == "Taylor Swift", msPlayed > 10000) %>% 
  group_by(trackName) %>% 
  summarize(plays = n()) %>% 
  arrange(desc(plays))
tswizzle[1:20,] 
ggplot(tswizzle[1:20,]) +
  geom_bar(aes(x=trackName, y=plays), stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
tswizzle <- tswizzle %>%
  left_join(subset(lib.tracks, artist == "Taylor Swift"), by = c("trackName" = "track")) %>% 
  select(trackName, album, plays)
head(tswizzle)

#importing lifetime streams
library(jsonlite)
streams_life <- data.frame()
for(i in 1:7){
  filepath <- paste("Data2/endsong_",i-1,".json", sep="")
  x <- fromJSON(filepath)
  x <- data.frame(x)
  streams_life <- rbind(streams_life, x)
}
streams_life <- streams_life %>% 
  rename(track = master_metadata_track_name, artist = master_metadata_album_artist_name, album = master_metadata_album_album_name, uri = spotify_track_uri)

#top taylor swift albums
tsalbums <- streams_life %>% 
  filter(artist == "Taylor Swift", ms_played > 10000) %>% 
  mutate(album = replace(album, album == "Red (Taylor's Version)", "Red"),
         album = replace(album, album %in% c("Fearless Platinum Edition", "Fearless (Taylor's Version)"), "Fearless")) %>% 
  group_by(album) %>% 
  summarize(plays = n()) %>% 
  arrange(desc(plays)) %>% 
  head(9) %>% 
  mutate(percent = round((plays / sum(plays))*100,1))

#favorite song
favsong <- streams_life %>% 
  filter(ms_played > 10000) %>% 
  group_by(track, artist) %>% 
  summarize(plays = n()) %>% 
  arrange(desc(plays)) %>% 
  head(10)
