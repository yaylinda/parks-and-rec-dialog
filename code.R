setwd("~/Developer/parks-and-rec-dialog")

library(ggplot2)
library(treemapify)
library(RColorBrewer)
library(stringr)

#==========================================================
# Read data
#==========================================================

# Get filenames of all files in /scripts
filenames = list.files(
  path = "~/Developer/parks-and-rec-dialog/scripts", 
  pattern = ".csv"
)

# Initialize dataframe
data = data.frame(
  Season = integer(),
  Episode = integer(),
  Character = character(),
  Line = character()
)

# Read data from each CSV file and append to dataframe
for (file_name in filenames) {
  file_data = read.csv(paste("scripts/", file_name, sep = ""))
  
  file_data$Season = as.numeric(strsplit(
    strsplit(file_name, 's')[[1]][2], 'e')[[1]][1])
  
  file_data$Episode = as.numeric(strsplit(
    strsplit(file_name, 'e')[[1]][2], '.csv')[[1]][1])
  
  data = rbind(data, file_data)
}

data$NumWords = sapply(strsplit(data$Line, " "), length)

#==========================================================
# Aggregate data
#==========================================================

num_words_by_character = aggregate(
  data$NumWords,
  by = list(
    Charater = data$Character
  ),
  sum
)

top_characters = c(
  "Leslie Knope",
  "Tom Haverford",
  "Ron Swanson",
  "Ben Wyatt",
  "Andy Dwyer",
  "Ann Perkins",
  "April Ludgate",
  "Chris Traeger",
  "Donna Meagle",
  "Jerry Gergich"
)

num_words_by_character_by_episode = aggregate(
  data$NumWords,
  by = list(
    Season = data$Season,
    Episode = data$Episode,
    Charater = data$Character
  ),
  sum
)

num_words_by_character_by_episode$Label = ifelse(
  num_words_by_character_by_episode$Charater %in% top_characters,
  num_words_by_character_by_episode$Charater,
  "Other"
)

num_words_by_label_by_episode = aggregate(
  num_words_by_character_by_episode$x,
  by = list(
    Season = num_words_by_character_by_episode$Season,
    Episode = num_words_by_character_by_episode$Episode,
    Label = num_words_by_character_by_episode$Label
  ),
  sum
)

num_words_by_label_by_episode$Season = paste(
  "Season ", 
  num_words_by_label_by_episode$Season, 
  sep = ""
)

num_words_by_label_by_episode$Episode = paste(
  "Episode ", 
  num_words_by_label_by_episode$Episode, 
  sep = ""
)
num_words_by_label_by_episode$Episode = factor(
  num_words_by_label_by_episode$Episode,
  levels = str_sort(unique(num_words_by_label_by_episode$Episode), numeric = TRUE)
)

# num episodes per season
num_episodes_per_season = aggregate(
  num_words_by_label_by_episode$Episode,
  by = list(
    num_words_by_label_by_episode$Season
  ),
  max
)

colnames(num_episodes_per_season) = c(
  "Season",
  "NumEpisodes"
)

#==========================================================
# Plots
#==========================================================

#--------------------------------------
# Helper function to plot one episode
#--------------------------------------
plot_one_episode = function(season, episode) {
  episode_data = subset(
    num_words_by_label_by_episode,
    num_words_by_label_by_episode$Season == season 
      & num_words_by_label_by_episode$Episode == episode
  )
  
  ggplot(
    episode_data,
    aes(
      area = x,
      label = Label,
      fill = factor(
        Label, 
        levels = c(top_characters, "Other")
      )
    )
  ) + 
    geom_treemap(
      
    ) +
    geom_treemap_text(
      family = "mono",
      color = "white",
      place = "center"
    )
}

#--------------------------------------
# Make one plot for each episode
#--------------------------------------

all_plots = c()

for (season_num in num_episodes_per_season$Season) {
  num_episodes = subset(
    num_episodes_per_season,
    num_episodes_per_season$Season == season_num
  )$NumEpisodes
  
  for (episode_num in num_episodes) {
    episode_plot = plot_one_episode(
      season_num,
      episode_num
    )
    all_plots = c(all_plots, episode_plot)
  }
}

#--------------------------------------
# Arrange plots in a grid
#--------------------------------------

top_characters = c(
  "Leslie",
  "Tom",
  "Ron",
  "Ben",
  "Andy",
  "Ann",
  "April",
  "Chris",
  "Donna",
  "Jerry"
)

num_words_by_label_by_episode$Label = word(num_words_by_label_by_episode$Label, 1)
  
ggplot(
  num_words_by_label_by_episode,
  aes(
    area = x,
    label = factor(
      Label, 
      levels = c(top_characters, "Other")
    ),
    fill = factor(
      Label, 
      levels = c(top_characters, "Other")
    )
  )
) + 
  geom_treemap(
    
  ) +
  geom_treemap_text(
    family = "mono",
    color = "black",
    place = "center"
  ) + 
  facet_grid(
    Episode ~ Season,
    switch = "y", 
    space = "free", 
  ) +
  labs(
    title = "Parks and Recreation: Proportion of Words Spoken",
    fill = "Character"
  ) +
  scale_fill_manual(
    values = brewer.pal(
      n = length(top_characters) + 1, 
      name = "Set3"
    ),
    breaks = c(top_characters, "Other"),
    labels = c(top_characters, "Other")
  ) +
  theme(
    text = element_text(family = "mono"),
    plot.title = element_text(size = rel(2), margin = margin(b = 10), face = "bold"),
    plot.margin = margin(t = 10, b = 10, l = 10, r = 10),
    strip.text = element_text(face = "bold"),
    strip.text.y.left = element_text(angle = 0),
    legend.title = element_text(size = rel(1.5), face = "bold"),
    legend.text = element_text(size = rel(1.5))
  )

ggsave(
  paste("plot.png", sep = ""),
  path = "~/Developer/parks-and-rec-dialog",
  dpi = 320,
  width = 10,
  height = 16,
  device = "png",
  units = "in"
)

