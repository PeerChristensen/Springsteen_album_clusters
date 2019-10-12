
#devtools::install_github('charlie86/spotifyr')

library(spotifyr)
library(tidyverse)
library(magrittr)
library(ggridges)
library(ggcorrplot)
library(viridisLite)
library(factoextra)
library(ggbiplot)
library(corrplot)
library(FactoMineR)
library(ggiraphExtra)

#df <- get_artist_audio_features(artist = "bruce springsteen")

#write_csv(df,"springstten_albums.csv")

df <- read_csv("springsteen_albums.csv")

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

df$album_name <- gsub(":.*","",df$album_name)
df$album_name[grepl("Innocent",df$album_name)] = "The Wild, The Innocent.."


# key_mode
df %>% 
  select(key_mode) %>%
  count() %>%
  arrange(desc(freq)) %>%
  top_n(10) %>%
  dplyr::mutate(ordered = row_number()) %>%
  ggplot(aes(x = reorder(key_mode,desc(ordered)), y = freq, fill = freq)) +
  geom_col() +
  coord_flip() +
  ggtitle("Ten most common keys") +
  scale_fill_viridis_c(option="B", direction = -1) +
  theme_minimal() +
  labs(y="n",x="key")
    
# danceability
df %>% 
  group_by(album_name) %>%
  ggplot(aes(x=danceability, y=reorder(album_name,desc(album_release_year)),fill=reorder(album_name,desc(album_release_year)))) +
  geom_density_ridges(colour="snow") +
  scale_fill_viridis_d(option="B", begin = .05, direction = -1, guide=F) +
  theme_minimal() +
  ggtitle("Danceability") +
  labs(y="album")

# all together
df %>% 
  gather(key = feature, value = measure, 
         danceability, energy, loudness, valence, tempo, acousticness) %>%
  group_by(album_name) %>%
  ggplot(aes(x=measure, y = reorder(album_name,desc(album_release_year)), fill=album_release_date)) +
  geom_density_ridges(rel_min_height = 0.005, legend = F, alpha=.9, size = .2,colour="snow") +
  facet_wrap(~feature, scales = "free", ncol = 3) +
  scale_fill_viridis_d(option="B" ,begin = .05) +
  theme_minimal() +
  theme(axis.text.y = element_text(size=7)) +
  labs(y = "album name") +
  ggtitle("Oasis albums in five features",
          subtitle = "Acousticness, danceability, energy, loudness, tempo and valence") +
  guides(fill=FALSE)

# Correlations
sign_test <- df  %>% 
  select(acousticness,danceability,energy,loudness,tempo,valence) %>%
  cor_pmat()

df  %>% 
  select(acousticness,danceability,energy,loudness,tempo,valence) %>%
  cor() %>%
  ggcorrplot(type   = "lower", 
             p.mat  = sign_test,
             colors = c(inferno(5)[2], "snow", inferno(5)[4])) +
  ggtitle("Correlations between features",
          subtitle = "Non-significant correlations marked with X")

# Distance
dfScale <- df %>%
  dplyr::select(album_name,acousticness,danceability,energy,loudness,tempo,valence) %>%
  group_by(album_name) %>%
  dplyr::summarise(acousticness = mean(scale(acousticness)),
            danceability = mean(scale(danceability)),
            energy       = mean(scale(energy)),
            loudness     = mean(scale(loudness)),
            tempo        = mean(scale(tempo)),
            valence      = mean(scale(valence))) %>%
  data.frame()

row.names(dfScale) <- dfScale$album_name
dfScale %<>% 
  dplyr::select(-album_name)

df_dist <- get_dist(dfScale, stand = TRUE)
fviz_dist(df_dist,gradient = list(low = inferno(5)[2], mid = "white", high = inferno(5)[4])) +
  theme_minimal() +
  ggtitle("Distance matrix",
          subtitle  = "Similarity between albums based on the five features") +
  theme(axis.text.x = element_text(hjust=1,angle=45))

# H-CLUSTERING
# Compute hierarchical clustering
fviz_nbclust(dfScale, hcut)


df.hc <- hclust(dist(scale(dfScale)))

fviz_dend(df.hc, k = 2, # Cut in groups
          cex = .5, # label size
          k_colors = inferno(10)[c(4,7)],
          color_labels_by_k = TRUE, 
          rect = TRUE) +
  ggtitle("Hierachical Clustering")

# K-MEANS
# n clusters
fviz_nbclust(dfScale, kmeans)

set.seed(324789)
km.res <- kmeans(dfScale, 2, nstart = 25)

fviz_cluster(km.res, data = dfScale,
             ellipse.type = "convex",
             repel = T,
             palette = inferno(10)[c(4,8)],
             ggtheme = theme_minimal(),
             main = "K-means Clustering") 

# PCA

dfPCA <- df %>%
  dplyr::select(album_name,acousticness,danceability,energy,loudness,tempo,valence) %>%
  group_by(album_name) %>%
  dplyr::summarise(acousticness = mean(scale(acousticness)),
            danceability = mean(scale(danceability)),
            energy       = mean(scale(energy)),
            loudness     = mean(scale(loudness)),
            tempo        = mean(scale(tempo)),
            valence      = mean(scale(valence))) %>%
  data.frame()

row.names(dfPCA) <- dfPCA$album_name
dfPCA %<>% 
  dplyr::select(-album_name) %>% 
  prcomp(center = TRUE,scale = TRUE)
summary(dfPCA)

ggbiplot(dfPCA,
         ellipse=T,
         labels=row.names(dfPCA), 
         groups = factor(km.res$cluster)) +
  scale_colour_manual(values = inferno(10)[c(2,7)]) +
  xlim(c(-3,3.2)) +
  ggtitle("Principal components analyse")


eig <-get_eigenvalue(dfPCA)
format(eig,digits=5)
fviz_eig(dfPCA, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(dfPCA)
var

corrplot(var$cos2, is.corr=FALSE)
corrplot(var$contrib, is.corr=FALSE)  

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(dfPCA, choice = "var", axes = 1:2)

fviz_contrib(dfPCA, choice = "var", axes = 1, top = 10)
fviz_contrib(dfPCA, choice = "var", axes = 2, top = 10)

dfPCA2 <- PCA(dfScale, graph = FALSE)
pcaDesc <- dimdesc(dfPCA2, axes = c(1,2), proba = 0.05)
# Description of dimension 1,2
pcaDesc$Dim.1
pcaDesc$Dim.2

fviz_contrib(dfPCA2, choice = "ind", axes = 1:2)


fviz_pca_ind(dfPCA2, 
             col.ind = factor(km.res$cluster), 
             palette=inferno(9)[c(2,7)],
             repel = TRUE,
             addEllipses = TRUE, # for få punkter i gr. 3
             legend.title="Group")

fviz_pca_biplot(dfPCA2, 
                fill.ind = factor(km.res$cluster), 
                col.ind = factor(km.res$cluster),
                palette=inferno(9)[c(2,7)],
                repel = TRUE,
                addEllipses = TRUE,
                #ellipse.type="confidence",
                legend.title="Group",
                alpha.var="contrib",
                col.var = "gray47",
                title=NULL)

######
# radar plots

dfScale %>%
    mutate(albums = row.names(dfScale)) %>%
    ggRadar(aes(group = albums), 
        rescale = FALSE, legend.position = "none",
        size = 1, interactive = FALSE, use.label = TRUE) +
  facet_wrap(~albums) + 
  scale_y_discrete(breaks = NULL) + # don't show ticks 
  theme(axis.text.x = element_text(size = 10)) + # larger label sizes
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option="B") +
  scale_colour_viridis_d(option="B")
  # adjust colors of radar charts to uniform colors
 # scale_fill_manual(values = rep(mycolor, nrow(df))) +
  #scale_color_manual(values = rep(mycolor, nrow(df))) 
  NULL

######
# radar plots of cluster centroids
set.seed(324789)
km.res <- kmeans(dfScale, 2, nstart = 25)

centroids <- km.res$centers %>% 
  as_tibble() %>%
  mutate(Cluster = row_number()) %>%
  select(Cluster, everything())

centroids %>%
  ggRadar(aes(group = Cluster), 
          rescale = FALSE,
          size = 1, interactive = FALSE, use.label = TRUE) +
  scale_y_discrete(breaks = NULL) + # don't show ticks 
  theme(axis.text.x = element_text(size = 10)) + # larger label sizes
  theme_minimal() 
  