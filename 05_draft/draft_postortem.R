library(tidyverse)
library(janitor)
library(tidytext)

data <-
  read_csv("05_draft/draft-summary-Goat league-2021-08-26.csv") %>% 
  clean_names() %>% 
  mutate(amount = if_else(str_detect(player, "Hurts"), 1,amount))

data %>% 
  group_by(team) %>% 
  summarise(spent = sum(amount))

data %>% 
  group_by(player_position) %>% 
  summarise(across(.cols = amount, .fns = list(sum = sum, mean = mean)))
  

data %>% 
  ggplot(aes(x = fct_reorder(player, amount), y = amount, fill = player_position)) +
  geom_col() +
  facet_wrap(~team, scales = "free_y") +
  coord_flip() +
  labs(fill = "Position", y = "", x = "",
       title = "Amount spent for each player")


data %>% 
  ggplot(aes(x = fct_reorder(player, amount), y = amount, fill = team)) +
  geom_col() +
  facet_wrap(~player_position, scales = "free_y") +
  coord_flip() +
  labs(fill = "Team", y = "", x = "",
       title = "Amount spent by position")
  

get_top_n_players_by_position <- function(data = data, position = "RB",n = 2){
  data %>% 
    filter(str_detect(player_position, position)) %>% 
    # filter(player_position %in% position) %>% 
    group_by(team) %>% 
    slice_max(order_by = amount, n = n, with_ties = F) %>% 
    mutate(slot = position, min = min(amount), max = max(amount)) %>% 
    ungroup() %>% 
    arrange(-amount) 
}

starts_pre <-
  map2_dfr(
    .x = c("QB", "RB", "WR", "TE"),
    .y = c(1, 2, 2, 1),
    .f = ~ get_top_n_players_by_position(data = data, position = .x, n = .y)
  )

starters <-
data %>% 
  anti_join(starts_pre) %>% 
  get_top_n_players_by_position(position = "RB|WR", n = 1) %>% 
  bind_rows(starts_pre) %>% 
  arrange(team, -amount)

bench <-
data %>% 
  anti_join(starters) %>% 
  group_by(team) %>% 
  mutate(min = min(amount), max = max(amount)) 

starter_bench <-
  list(starters = starters, bench = bench) %>% bind_rows() %>% 
  mutate(slot = if_else(is.na(slot), "BE", slot))

starter_bench %>% 
  ggplot(aes(x = fct_reorder(player, amount), y = amount, fill = fct_relevel(slot, "BE", after = Inf))) +
  geom_col() +
  facet_wrap(~team, scales = "free_y") +
  coord_flip() +
  labs(fill = "Slot", y = "", x = "", title = "Amount paid for each position at each position for each team")

starter_bench %>% 
  # filter(str_detect(team, "Acute")) %>%
  mutate(slot = if_else(slot == "bench", "bench", slot)) %>% 
  mutate(slot = if_else(str_length(slot)==5, player_position, slot)) %>% 
  group_by(team, slot) %>%
  summarise(mean = mean(amount), max = max(max), min = min(min)) %>%  
  ggplot(aes(x = reorder_within(team, mean, slot), y = mean, fill = team)) +
  geom_col(show.legend = F) +
  geom_errorbar(aes(ymax = max, ymin = min), show.legend = F) +
  # geom_point(aes(y = min), show.legend = F, color = "blue") +
  facet_wrap(~fct_relevel(slot, "QB","RB","WR","TE","BE"), scales = "free_y") +
  scale_x_reordered() + 
  coord_flip() +
  labs(fill = "Team", x = "", y = "",
       title = "Average Money Spent on the 7 Starters and Bench", 
       subtitle = "Error bars represent max and min spent for starters at that position on your team")


starter_bench %>% 
  arrange(round_pick) %>% 
  separate(round_pick, into = c("round", "pick"), sep = "\\.", convert = T) %>% 
  arrange(round,pick) %>%
  mutate(pick_number = row_number()) %>% 
  group_by(team) %>% 
  mutate(team_pick_number = row_number()) %>% 
  ungroup() %>% 
  mutate(round_id = floor(pick_number/10)*10) %>% 
  ggplot(aes(x = pick_number, y = team_pick_number, color = team)) +
  geom_vline(aes(xintercept = round_id), color = "grey") +
  geom_line(size = 2) +
  scale_y_continuous(breaks = 1:11) +
  scale_x_continuous(breaks = seq(10,121,10)) +
  labs(x = "Pick Number", y = "Team Pick Number", color = "Team",
       title = "Who got their picks first?")


first_add <-
starter_bench %>% group_by(team) %>% 
  slice(1) %>% 
  separate(round_pick, into = c("round", "pick"), sep = "\\.", convert = T) %>% 
  mutate(round = 0, amount = 0)

starter_bench %>% 
  arrange(round_pick) %>% 
  separate(round_pick, into = c("round", "pick"), sep = "\\.", convert = T) %>% 
  bind_rows(first_add) %>% 
  arrange(round,pick) %>%
  mutate(pick_number = row_number()) %>% 
  group_by(team) %>% 
  mutate(team_pick_number = row_number()) %>%
  mutate(money_spent = cumsum(amount)) %>% 
  ungroup() %>% 
  mutate(round_id = floor(pick_number/10)*10) %>% 
  ggplot(aes(x = pick_number, y = money_spent, color = team)) +
  geom_vline(aes(xintercept = round_id), color = "grey") +
  geom_line(size = 2) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(breaks = seq(10,121,10)) +
  labs(x = "Pick Number", y = "Money Spent", color = "Team",
       title = "Who spent their moeny first?")
  