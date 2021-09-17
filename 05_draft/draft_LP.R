library(tidyverse)
library(ggrepel)
library(tesseract)
library(lpSolve)


ff_player_rankings_function <- function(sheet, file_name = "05_draft/draft_data.xlsx"){
  out <-
    readxl::read_xlsx(file_name, sheet = sheet) %>%
    janitor::clean_names() %>% 
    select(layer, fpts) %>% 
    mutate(row = sort(rep(1:50,3))) %>%
    fill(fpts) %>% 
    group_by(row) %>% 
    slice(-1) %>% 
    mutate(across(.cols = everything(), .fns = ~replace_na(., " "))) %>% 
    summarise(layer = str_c(layer, collapse = T), fpts = fpts[1], row = row[1]) %>% 
    mutate(across(.cols = everything(), .fns = ~str_remove_all(.,"TRUE"))) %>% 
    mutate(across(.cols = everything(), .fns = ~str_squish(.))) %>% 
    mutate(fpts = as.numeric(fpts)) %>% 
    mutate(player = str_sub(layer, 1,-3)) %>% 
    mutate(position = str_sub(layer, -2,-1)) %>% 
    select(-layer) 
  return(out)
}



ff_player_rankings <-
  map_dfr(.x = 1:6, .f = ~ff_player_rankings_function(sheet = .x)) %>% 
  distinct() 

img_file <- pdftools::pdf_convert("05_draft/NFLDK2021_CS_PPR300_v2.pdf", format = 'tiff',  dpi = 400)
text <- ocr(img_file)

ff_player_value <-
  tibble(text = text) %>% 
  separate_rows(text, sep = "\\(") %>% 
  separate(col = text, "\\)", into = c("position", "player"))  %>% 
  separate(col = player, "\\,", into = c("player", "team")) %>% 
  separate(col = team, "\\$", into = c("team", "Value")) %>% 
    # filter(str_detect(str_to_lower(player),"hend"))
  mutate(Value = as.numeric(str_squish(str_sub(Value, 1,2)))) %>% 
  mutate(team = str_squish(team)) %>% 
  mutate(player = str_squish(player)) %>% 
  filter(!is.na(player)) %>% 
  mutate(position = str_sub(position, 1,2)) %>% 
  arrange(-Value) %>% 
  distinct() %>% 
  mutate(player = str_c(player,team)) %>% 
  # filter(str_detect(str_to_lower(player),"mcl"))  %>% 
  mutate(player = if_else(str_detect(player, "Terry McL"),"Terry McLaurinWAS", player)) 

ff_player_value %>% filter(str_detect(str_to_lower(player),"jeffer"))
ff_player_rankings %>% filter(str_detect(str_to_lower(player),"jeffer"))

joined <-
  ff_player_rankings %>% 
  fuzzyjoin::stringdist_right_join(ff_player_value, by = "player") %>%  
    # filter(str_detect(str_to_lower(player.y),"mcl"))
  # print(n = Inf) %>% 
  filter(complete.cases(.)) %>% 
  # mutate(points_per_dollar = Value / fpts) %>%
  mutate(points_per_dollar = fpts / Value) %>%
  arrange(-points_per_dollar) %>% 
  filter(Value != 0) %>% 
  arrange(position.x) %>% 
  fastDummies::dummy_cols(select_columns = "position.x") %>% 
  mutate(position.x_FLEX = position.x_RB + position.x_WR) %>% 
  # select(position.x_FLEX) %>% arrange(-position.x_FLEX) %>% 
  mutate(position.x_FLEX = if_else(position.x_FLEX == 0,0,1)) %>% 
  select(fpts, player = player.x, position = position.x, team, Value, points_per_dollar, contains("position.x"))

write_csv(x = joined, file = "05_draft/fantasy_lp_data.csv")




joined %>% 
  filter(position == "RB") %>% 
  ggplot(aes(x = fct_reorder(player, points_per_dollar), y = points_per_dollar)) +
  geom_point() +
  geom_text(aes(label = str_c(fpts, " / ", Value)), nudge_y = +.03) +
  # geom_text(aes(label = fpts), nudge_y = -20) +
  # scale_y_log10() +
  facet_wrap(~position, scales = "free_y", nrow = 1) +
  coord_flip() +
  labs(x = "", y = "Dollars per Point")
# labs(x = "", y = "Points Per Dollar")


f_obj <- joined %>% pull(fpts) 

f_con <- matrix(c(joined %>% pull(position.x_QB),
                  joined %>% pull(position.x_RB),
                  joined %>% pull(position.x_WR),
                  joined %>% pull(position.x_TE),
                  joined %>% pull(position.x_FLEX),
                  joined %>% pull(Value)
                  
),nrow = 6, byrow = T)

f_dir <- c("=", # qb
           "<=", # rb
           "<=", # wr
           "=", # te
           "=", # flex
           "<=" # extra players
           )

f_rhs <- c(1,3,3,1,6,200-6)
# f_rhs <- c(1,2,2,1,6,200-6)


solution <-
  lp(
    direction = "max",
    objective.in =  f_obj,
    const.mat =  f_con,
    const.dir =  f_dir,
    const.rhs =  f_rhs, 
    all.bin = TRUE
  )


joined %>% 
  mutate(solution = solution$solution) %>% 
  # filter(solution > 0)
  filter(solution == 1) %>% 
  # print(n = Inf) %>% 
  select(fpts, player.x, position.x, Value, points_per_dollar) %>% 
  mutate(dollars_per_point = fpts/Value)



ff_player_rankings %>% 
  ggplot(aes(x = fct_reorder(player,-fpts), y = fpts, color = position)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

