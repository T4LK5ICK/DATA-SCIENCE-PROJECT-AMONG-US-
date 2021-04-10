library(tidyverse)

setwd("C:/Users/TALKSICK/Desktop/Pokémon Analysis")
# Importing the dataset and converting variables
pokedex <- read_csv("datasets/pokedex.csv", 
                    col_types = cols(name = col_factor(), 
                                     type = col_factor(),
                                     is_legendary = col_factor()))

head(pokedex)

# Examine the structure
str(pokedex)

# How many Pokémon are legendary?
legendary_pokemon <- pokedex %>% 
  count(is_legendary) %>% 
  mutate(prop = n / nrow(pokedex))

#Printing the data frame
legendary_pokemon

#Legendary Pokémon by height and weight
# Preparing the plot
legend_by_heightweight_plot <- pokedex %>% 
  ggplot(aes(x=height_m, y=weight_kg)) +
  geom_point(aes(color=is_legendary), size = 2) +
  geom_text(aes(label = ifelse(height_m > 7.5 | weight_kg > 600, as.character(name), '')), 
            vjust = 0, hjust = 0) +
  geom_smooth(method = "lm", se = FALSE, col = "black", linetype = "dashed") +
  expand_limits(x=16) +
  labs(title = "Legendary Pokemon by height and weight",
       x = "Height (m)",
       y = "Weight (kg)") +
  guides(color = guide_legend(title = "Pokémon status")) +
  scale_color_manual(labels = c("Non-Legendary", "Legendary"),
                     values = c("#F8766D", "#00BFC4"))

#Printing the plot
legend_by_heightweight_plot


# Legendary Pokémon by type
legend_by_type <- pokedex %>% 
  group_by(type) %>% 
  mutate(is_legendary = as.numeric(is_legendary) - 1) %>% 
  summarise(prop_legendary = mean(is_legendary)) %>% 
  ungroup() %>% 
  mutate(type = fct_reorder(type, prop_legendary))

# Preparing the plot
legend_by_type_plot <- legend_by_type %>% 
  ggplot(aes(x = type, y = prop_legendary, fill = prop_legendary)) + 
  geom_col() +
  labs(title = "Legendary Pokemon by type") +
  coord_flip() +
  guides(fill = FALSE)

# Printing the plot
legend_by_type_plot



# Legendary Pokémon by fighter stats
# Prepare the data
legend_by_stats <- pokedex  %>% 
  select(is_legendary, attack, sp_attack, defense, sp_defense, hp, speed)  %>% 
  gather(key = "fght_stats", value = "value", -is_legendary) 

# Prepare the plot
legend_by_stats_plot <- legend_by_stats %>% 
  ggplot(aes(x = is_legendary, y = value, fill = is_legendary)) +
  geom_boxplot(varwidth = TRUE) +
  facet_wrap(~fght_stats) +
  labs(title = "Pokemon fight statistics",
       x = "Legendary status") +
  guides(fill = FALSE)

# Print the plot
legend_by_stats_plot
