library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("viridis")
library(viridis)
install.packages("reshape2")
library(reshape2)
install.packages("networkD3")
library(networkD3)
install.packages("webshot")
library(webshot)
library(htmlwidgets)
install.packages("ggalluvial")
library(ggalluvial)
install.packages("fmsb")
library(fmsb)

#data pre-processing
spotify_data<-read.csv("D:/google download/dataset.csv")
spotify_data<-filter(spotify_data,spotify_data$popularity!=0)

top_genres <- spotify_data %>%
  group_by(track_genre) %>%
  summarise(mean_popularity = mean(popularity, na.rm = TRUE)) %>%
  arrange(desc(mean_popularity)) %>%
  slice(1:10) %>%
  pull(track_genre)

filtered_data <- spotify_data %>%
  filter(track_genre %in% top_genres)

summary(filtered_data$popularity)
IQR(filtered_data$popularity)

#classify popularity into three levels
filtered_data$popularity_level <- cut(filtered_data$popularity,
                                      breaks = c(-Inf, 51, 69, Inf),
                                      labels = c("Low", "Medium", "High"))

#Section 2 Heatmap

audio_features <- filtered_data[, c("danceability","loudness","energy","speechiness","valence", "liveness", "acousticness","tempo", "instrumentalness")]
cor_matrix <- cor(audio_features)
cor_data <- melt(cor_matrix)
cor_data_filtered <- cor_data[abs(cor_data$value) > 0.3 & abs(cor_data$value) <1, ]
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "lightblue", high = "brown", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap of Audio Features",
       x = "Audio Features",
       y = "Audio Features") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(data = cor_data_filtered, aes(label = round(value, 2)), color = "black", size = 2)

#Section 3 Part1: Sankey graph

sankey_data <- filtered_data %>%
  group_by(track_genre, popularity_level) %>%
  summarise(value = n(), .groups = "drop") %>%
  rename(source = track_genre, target = popularity_level)

sankey_ggplot<-ggplot(sankey_data,
       aes(axis1 = source, axis2 = target, y = value)) +
  geom_alluvium(aes(fill = source), width = 0.1) + 
  geom_stratum(width = 0.3, fill = "white", color = NA) +  
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + 
  scale_x_discrete(limits = c("Genre", "Popularity Level"), expand = c(0.1, 0.1)) +
  theme_minimal() +
  labs(title = "Sankey Diagram of Genre and Popularity Levels",
       x = "Categories",
       y = "Count") +
  theme(
    axis.text.x = element_text(hjust = 0.5),
    axis.ticks = element_blank(), 
    axis.text = element_text(size = 10),  
    panel.grid = element_blank(),  
    panel.border = element_blank(),  
    plot.background = element_blank(),  
    legend.position = "right" ) +
  scale_fill_viridis(discrete = TRUE, option = "D")

sankey_ggplot

#Section 3 Part2: Interactive Sankey diagram

links <- data.frame(
  source=sankey_data$source, 
  target=sankey_data$target, 
  value=sankey_data$value)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique())

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

sankey_interactive <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
sankey_interactive
saveWidget(sankey_interactive, "sankey_interactive.html", selfcontained = TRUE)


#Section 4 Density Plot: Popularity and Audio features
filtered_data$instrumentalness_sqrt <- sqrt(filtered_data$instrumentalness)

data_long <- filtered_data %>%
  pivot_longer(cols = c(instrumentalness_sqrt, danceability, energy, speechiness, liveness,tempo, valence),
               names_to = "feature",
               values_to = "value")
radar_plot <- ggplot(data_long, aes(x = value, fill = factor(popularity_level))) +
  geom_density(alpha = 0.5) + 
  facet_wrap(~ feature, scales = "free") + 
  labs(title = "Distribution of Audio Features by Popularity Level",
       x = "Value",
       y = "Density",
       fill = "Popularity Level") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(radar_plot)

#Section5 Radar Plot
radar_data <- filtered_data %>%
  group_by(popularity_level) %>%
  reframe(
    danceability = danceability,
    energy = energy,
    valence = valence,
    liveness = liveness,
    speechiness = speechiness,
    tempo = tempo,
    instrumentalness_sqrt = instrumentalness_sqrt)

radar_data <- radar_data %>%
  mutate(tempo_data = (tempo - min(tempo)) / (max(tempo) - min(tempo)))

radar_data <- radar_data %>%
  group_by(popularity_level) %>%
  summarise(
    danceability = mean(danceability),
    energy = mean(energy),
    valence = mean(valence),
    liveness = mean(liveness),
    speechiness= mean(speechiness),
    tempo_data=mean(tempo_data),
    instrumentalness_sqrt=mean(instrumentalness_sqrt))

radar_data <- rbind(
  apply(radar_data[, -1], 2, max), 
  apply(radar_data[, -1], 2, min),  
  radar_data[, -1])

colors <- c(  rgb(0.7,0.5,0.1,0.9),rgb(0.2,0.5,0.5,0.9),rgb(0.8,0.2,0.5,0.9) )  
fill_colors <- c( rgb(0.7,0.5,0.1,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.8,0.2,0.5,0.4) ) 

radarchart(radar_data,
           axistype = 1,
           pcol = colors,  
           pfcol = fill_colors,  
           plwd = 2, 
           cglcol = "grey", 
           cglty = 1,  
           axislabcol = "black",  
           cglwd = 0.8,  
           vlcex = 0.8, 
           title = "Radar Chart of Audio Features by Popularity Level")

legend("topleft", 
       legend = c("Low", "Medium", "High"),
       col = colors, 
       lty = 1, 
       lwd = 2, 
       pt.bg = fill_colors, 
       bty = "n", 
       cex = 0.8)


