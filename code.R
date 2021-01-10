setwd("/Users/lindazheng/Developer/the-office-dialog")

library(schrute)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)

theoffice = schrute::theoffice

data = data.frame(
  season = theoffice$season,
  episode = theoffice$episode,
  character = theoffice$character,
  text = theoffice$text,
  num_words = sapply(strsplit(theoffice$text, " "), length),
  num_line = 1
)

#==========================================================
# Lines
#==========================================================
num_lines_per_character = aggregate(
  data$num_line,
  by = list(
    character = data$character
  ),
  sum
)

lines_per_character_per_episode = aggregate(
  data$num_line,
  by = list(
    character = data$character,
    season = data$season,
    episode = data$episode
  ),
  sum
)

most_lines_per_episode = data.frame(c(), c(), c(), c())

for (seasonNum in 1:max(data$season)) {
  for (episodeNum in 1:max(data[which(data$season == seasonNum), ]$episode)) {
    
    season_subset = lines_per_character_per_episode[
      which(lines_per_character_per_episode$season == seasonNum), 
    ]
    
    season_episode_subset = season_subset[
      which(season_subset$episode == episodeNum), 
    ]
    
    max_lines = max(season_episode_subset$x)
    
    character = season_episode_subset[
      which(season_episode_subset$x == max_lines), 
    ]$character
    
    most_lines_per_episode = rbind(
      most_lines_per_episode, 
      c(seasonNum, episodeNum, max_lines, character)
    )
  }
}

colnames(most_lines_per_episode) = c("season", "episode", "lines_spoken", "character")

most_lines_per_episode$season = as.numeric(most_lines_per_episode$season)
most_lines_per_episode$episode = as.numeric(most_lines_per_episode$episode)
most_lines_per_episode$lines_spoken = as.numeric(most_lines_per_episode$lines_spoken)
most_lines_per_episode$character = factor(
  most_lines_per_episode$character,
  levels = num_lines_per_character[order(-num_lines_per_character$x),]$character
)
most_lines_per_episode = subset(
  most_lines_per_episode, 
  most_lines_per_episode$lines_spoken > 0
)

#--------------------------------------
# Plot
#--------------------------------------

ggplot(
  most_lines_per_episode, 
  aes(
    x = factor(episode, levels = sort(unique(episode))), 
    y = factor(season, levels = rev(sort(unique(season)))), 
    fill = character
  )
) + 
  geom_tile(color = "white", alpha = 0.8) + 
  geom_text(
    aes(label = paste(character, lines_spoken, sep = "\n")), 
    size = 2
  ) +
  coord_equal(ratio = 1) + 
  labs(
    x = "Episode\n",
    y = "Season",
    title = "title",
    subtitle = "subtitle",
    fill = "Character (Total Lines Spoken)",
    caption = "* The number within each tile represents the number of lines spoken by the most loquatious character."
  ) + 
  scale_x_discrete(position = "top") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.ticks = element_blank(),
    strip.text.y = element_text(size = 12, angle = 180),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size = 20),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
    axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
    plot.subtitle = element_text(margin = margin(t = 5, r = 0, b = 20, l = 0)),
  )

#==========================================================
# Words
#==========================================================

num_words_per_character = aggregate(
  data$num_words,
  by = list(
    character = data$character
  ),
  sum
)

words_per_character_per_episode = aggregate(
  data$num_words,
  by = list(
    character = data$character,
    season = data$season,
    episode = data$episode
  ),
  sum
)

most_words_per_episode = data.frame(c(), c(), c(), c())

for (seasonNum in 1:max(data$season)) {
  for (episodeNum in 1:max(data[which(data$season == seasonNum), ]$episode)) {
    
    season_subset = words_per_character_per_episode[
      which(words_per_character_per_episode$season == seasonNum), 
    ]
    
    season_episode_subset = season_subset[
      which(season_subset$episode == episodeNum), 
    ]
    
    max_words = max(season_episode_subset$x)
    
    character = season_episode_subset[
      which(season_episode_subset$x == max_words), 
    ]$character
    
    most_words_per_episode = rbind(
      most_words_per_episode, 
      c(seasonNum, episodeNum, max_words, character)
    )
  }
}

colnames(most_words_per_episode) = c("season", "episode", "words_spoken", "character")

most_words_per_episode$season = as.numeric(most_words_per_episode$season)
most_words_per_episode$episode = as.numeric(most_words_per_episode$episode)
most_words_per_episode$words_spoken = as.numeric(most_words_per_episode$words_spoken)
most_words_per_episode$character = factor(
  most_words_per_episode$character,
  levels = num_words_per_character[order(-num_words_per_character$x),]$character
)
most_words_per_episode = subset(
  most_words_per_episode, 
  most_words_per_episode$words_spoken > 0
)


cumulative_words_per_character = aggregate(
  data$num_words,
  by = list(
    character = data$character
  ),
  sum
)
cumulative_words_per_character = subset(
  cumulative_words_per_character,
  cumulative_words_per_character$character %in% unique(most_words_per_episode$character)
)
cumulative_words_per_character = cumulative_words_per_character[order(-cumulative_words_per_character$x),]

#--------------------------------------
# Plot
#--------------------------------------

ggplot(
  most_words_per_episode, 
  aes(
    x = factor(episode, levels = sort(unique(episode))), 
    y = factor(season, levels = rev(sort(unique(season)))), 
    fill = factor(character, levels = cumulative_words_per_character$character)
  )
) + 
  geom_tile(color = "white", alpha = 0.8) + 
  geom_text(
    aes(label = paste(character, words_spoken, sep = "\n")), 
    size = 1.5,
    family = "mono"
  ) +
  coord_equal(ratio = 1) + 
  labs(
    x = "Episode",
    y = "Season",
    title = "Which Character Talks the Most in The Office?",
    subtitle = "",
    fill = "",
    caption = ""
  ) + 
  scale_fill_manual(
    values = brewer.pal(
      n = length(cumulative_words_per_character$character), 
      name = "Accent"
    ),
    breaks = cumulative_words_per_character$character,
    labels = cumulative_words_per_character$character
    # labels = paste(
    #   cumulative_words_per_character$character,
    #   " (",
    #   cumulative_words_per_character$x,
    #   ")",
    #   sep = ""
    # )
  ) + 
  theme_economist() +
  theme(
    text = element_text(family = "mono"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.ticks = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    plot.title = element_text(size = rel(2), margin = margin(t = 20, r = 0, b = 20, l = 0)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
    plot.subtitle = element_blank(),
  )

ggsave(
  paste("plot.png", sep = ""),
  path = "~/Developer/the-office-dialog",
  dpi = 320,
  width = 10,
  height = 8,
  device = "png",
  units = "in"
)
