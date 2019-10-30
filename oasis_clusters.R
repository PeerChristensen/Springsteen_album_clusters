
devtools::install_github('charlie86/spotifyr')

library(spotifyr)
library(tidyverse)
library(magrittr)
library(ggridges)
library(ggcorrplot)
library(viridisLite)
library(factoextra)
library(ggbiplot)

Sys.setenv(SPOTIFY_CLIENT_ID     = "124f0a42539c4129916a773c0a6aebe3")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "a763c9bdb6cd4422910cb04644d96ce9")

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

df$album_name <- gsub(":.*","",df$album_name)
df$album_name[grepl("Innocent",df$album_name)] = "The Wild, The Innocent.."


# key_mode
df %>% 
  count(key_mode) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  mutate(ordered = row_number()) %>%
  ggplot(aes(x = reorder(key_mode,desc(ordered)), y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  ggtitle("Ten most common keys") +
  scale_fill_viridis_c(option="B", direction = -1) +
  theme_minimal()
    
# danceability
df %>% 
  group_by(album_name) %>%
  ggplot(aes(x=danceability, y=reorder(album_name,desc(album_release_year)))) +
  geom_density_ridges()

# all together
df %>% 
  gather(key = feature, value = measure, 
         danceability, energy, loudness, valence, tempo, acousticness) %>%
  group_by(album_name) %>%
  ggplot(aes(x=measure, y = reorder(album_name,desc(album_release_year)), fill=album_release_date)) +
  geom_density_ridges(rel_min_height = 0.005, legend = F, alpha=.7, size = .2) +
  facet_wrap(~feature, scales = "free", ncol = 3) +
  scale_fill_viridis_d(option="B") +
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
             colors = c(inferno(5)[2], "white", inferno(5)[4])) +
  ggtitle("Correlations between features",
          subtitle = "Non-significant correlations marked with X")

# Distance
df_scale <- df %>%
  select(album_name,acousticness,danceability,energy,loudness,tempo,valence) %>%
  group_by(album_name) %>%
  summarise(acousticness = mean(scale(acousticness)),
            danceability = mean(scale(danceability)),
            energy       = mean(scale(energy)),
            loudness     = mean(scale(loudness)),
            tempo        = mean(scale(tempo)),
            valence      = mean(scale(valence))) %>%
  data.frame()

row.names(df_scale) <- df_scale$album_name
df_scale %<>% 
  select(-album_name)

df_dist <- get_dist(df_scale, stand = TRUE, method = "pearson")
fviz_dist(df_dist,gradient = list(low = inferno(5)[2], mid = "white", high = inferno(5)[4])) +
  theme_minimal() +
  ggtitle("Distance matrix",
          subtitle  = "Similarity between albums based on the five features") +
  theme(axis.text.x = element_text(hjust=1,angle=45))

# H-CLUSTERING
# Compute hierarchical clustering
df.hc <- hclust(dist(scale(df_scale)), method = "complete")

fviz_dend(df.hc, k = 2, # Cut in four groups
          cex = .5, # label size
          k_colors = inferno(10)[c(4,7)],
          color_labels_by_k = TRUE, 
          rect = TRUE) +
  ggtitle("Hierachical Clustering")

# K-MEANS
# n clusters
fviz_nbclust(df_scale, kmeans)

set.seed(324789)
km.res <- kmeans(df_scale, 2, nstart = 25)

fviz_cluster(km.res, data = df_scale,
             ellipse.type = "convex",
             repel = T,
             palette = inferno(10)[c(4,8)],
             ggtheme = theme_minimal(),
             main = "K-means Clustering") 

# PCA

df_pca <- df %>%
  select(album_name,acousticness,danceability,energy,loudness,tempo,valence) %>%
  group_by(album_name) %>%
  summarise(acousticness = mean(scale(acousticness)),
            danceability = mean(scale(danceability)),
            energy       = mean(scale(energy)),
            loudness     = mean(scale(loudness)),
            tempo        = mean(scale(tempo)),
            valence      = mean(scale(valence))) %>%
  data.frame()

row.names(df_pca) <- df_pca$album_name
df_pca %<>% 
  select(-album_name) %>% 
  prcomp(center = TRUE,scale = TRUE)
summary(df_pca)

ggbiplot(df_pca,
         ellipse=T,
         labels=row.names(df_pca), 
         groups = factor(km.res$cluster)) +
  scale_colour_manual(values = inferno(10)[c(1,4,8)]) +
  xlim(c(-2,3.2)) +
  ggtitle("Principal components analyse")


eig <-get_eigenvalue(df_pca)
format(eig,digits=5)
fviz_eig(df_pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(df_pca)
var

library(corrplot)
corrplot(var$cos2, is.corr=FALSE)
corrplot(var$contrib, is.corr=FALSE)  

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(df_pca, choice = "var", axes = 1:2)

fviz_contrib(df_pca, choice = "var", axes = 1, top = 10)
fviz_contrib(df_pca, choice = "var", axes = 2, top = 10)

library(FactoMineR)
df_pca2 <- PCA(df_scale, graph = FALSE)
pca_desc <- dimdesc(df_pca2, axes = c(1,2), proba = 0.05)
# Description of dimension 1,2
pca_desc$Dim.1
pca_desc$Dim.2

fviz_contrib(df_pca2, choice = "ind", axes = 1:2)


fviz_pca_ind(df_pca2, 
             col.ind = factor(km.res$cluster), 
             palette=viridis(9)[c(2,5,8)],
             repel = TRUE,
             addEllipses = TRUE, # for få punkter i gr. 3
             legend.title="Gruppe")

fviz_pca_biplot(df_pca2, 
                fill.ind = factor(km.res$cluster), 
                col.ind = factor(km.res$cluster),
                palette=viridis(9)[c(2,5,8)],
                repel = TRUE,
                addEllipses = TRUE,
                #ellipse.type="confidence", # for få punkter i gr. 3
                legend.title="Gruppe",
                alpha.var="contrib",
                col.var = "gray47",
                title=NULL)

