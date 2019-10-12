#download lyrics

df <- get_artist_audio_features(artist = "bruce springsteen")

glimpse(df)
unique(df$album_name)

# some albums only have one song, some are alternate versions

remove_albums <- c("Greatest Hits",
                   "Hammersmith Odeon, London 75",
                   "The Essential Bruce Springsteen (Bonus Disc)",
                   "The Ties That Bind: The River Collection",
                   "Chapter and Verse",
                   "The Promise",
                   "Tracks")

df %<>% 
  filter(!album_name %in% remove_albums) %>%
  filter(!grepl("live|Live",album_name)) %>%
  mutate(album_name = str_to_title(album_name))

d = df %>% select(artist_name,album_name) %>%
  distinct()

tracks = NULL
track= NULL
for (i in seq_along(d$album_name)){
  
  track[i] = genius_album(d$artist_name[i],d$album_name[i])
  tracks = rbind(tracks,track)
}

a = genius_album(rep("Bruce Springsteen",2),c("Born in the U.S.A.","Born To Run"))


