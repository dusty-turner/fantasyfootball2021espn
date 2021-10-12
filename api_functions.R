library(tidyverse)
library(ggrepel)

get_dashboard_data <- function(leagueID = 89417258, per_id = 5){

  
get_data <- function(leagueID = leagueID, per_id = per_id){
base = "http://fantasy.espn.com/apis/v3/games/ffl/seasons/"
year = "2021"
mid = "/segments/0/leagues/"
# leagueID = "847888" # jim
# leagueID = "35354777" # caa
# leagueID = "89417258" # dusty 
# leagueID = "206814" # twitter guy
# per_id <- 1
tail = str_c("?view=mDraftDetail",
             "&view=mLiveScoring",
             "&view=mMatchupScore,",
             "&view=mPendingTransactions",
             "&view=mPositionalRatings",
             "&view=mRoster",
             "&view=mSettings",
             "&view=mTeam",
             "&view=modular",
             "&view=mNav",
             "&view=mMatchupScore",
             "&scoringPeriodId="
             )

# set_league <- function(league){leagueID <- league}
# set_per_id <- function(period){per_id <- period}
# per_id=1

url = paste0(base,year,mid,leagueID,tail,per_id)

ESPNGet <- httr::GET(url = url)
ESPNRaw <- rawToChar(ESPNGet$content)
ESPNFromJSON <- jsonlite::fromJSON(ESPNRaw)

ESPNFromJSON %>% listviewer::jsonedit()

# roster_size <- 16
number_of_teams <- length(ESPNFromJSON$teams$id)
team_ids <- ESPNFromJSON$teams$id

## one players stats
player_extract <- function(team_number = 1, player_number = 1, per_id = per_id, seasonId = 2021){
  player_week <-
    tibble(
      team = str_c(ESPNFromJSON$teams$location[team_number]," ",ESPNFromJSON$teams$nickname[team_number]),
      teamId = ESPNFromJSON$teams$id[team_number],
      fullName = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$fullName[player_number],
      appliedTotal = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$appliedTotal,
      seasonId = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$seasonId,
      scoringPeriodId = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$scoringPeriodId,
      statsplitTypeId = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$statSplitTypeId,
      externalId = ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats[[player_number]]$externalId,
      lineupSlot_id = ESPNFromJSON$teams$roster$entries[[team_number]]$lineupSlotId[player_number],
      eligibleSlots = list(ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$eligibleSlots[[player_number]])
    ) %>% 
    filter(seasonId==seasonId) %>% 
    filter(scoringPeriodId != 0) %>% 
    filter(scoringPeriodId == per_id)
  return(player_week)
}

schedule <-
  tibble(
    home = ESPNFromJSON$schedule$away$teamId,
    away = ESPNFromJSON$schedule$home$teamId,
    scoringPeriodId = ESPNFromJSON$schedule$matchupPeriodId,
    gameId = ESPNFromJSON$schedule$id
  ) %>% 
  pivot_longer(cols = c(home,away), values_to = "teamId")
# ESPNFromJSON$schedule$away$totalPoints

#Get the roster slots for each team
get_roster_slots <- function(team_number=1){
  return(tibble(team_number = team_number, player_slot = 1:length(ESPNFromJSON$teams$roster$entries[[team_number]]$playerPoolEntry$player$stats)))
}

team_player_slots <- purrr::map_dfr(1:number_of_teams,~get_roster_slots(team_number = .x))

team_list <- purrr::map2_dfr(team_player_slots$team_number,team_player_slots$player_slot, ~player_extract(team_number = .x,player_number = .y, per_id = per_id)) %>% 
  left_join(schedule) %>% 
  mutate(points_type = if_else(str_length(externalId) > 6, "actual", "projected")) %>% 
  relocate(team:appliedTotal, points_type)

# team_list %>% filter(scoringPeriodId == per_id) %>%   

return(list(team_list = team_list, ESPNFromJSON = ESPNFromJSON))

}

ESPNFromJSON <- get_data(leagueID = leagueID,per_id = per_id)[[2]]



n_qb <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`0`
n_rb <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`2`
n_wr <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`4`
n_flex <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`23`
n_te <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`6`
n_dst <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`16`
n_k <- ESPNFromJSON$setting$rosterSettings$lineupSlotCounts$`17`


# team_list %>% filter(team=="Syntax Error") %>% 
#   filter(points_type=="actual") %>% 
#   filter(lineupSlot_id!=20) %>% 
#   filter(scoringPeriodId==1)

team_list <-
  1:per_id %>% 
  purrr::map_dfr(~get_data(leagueID = leagueID,per_id = .x)[[1]]) %>% 
  distinct(teamId,fullName,appliedTotal,scoringPeriodId,.keep_all = T)

team_ids <- unique(team_list$teamId)

# team_list <-
# get_data(leagueID = leagueID,per_id = 1) %>% 
#   bind_rows(get_data(leagueID = leagueID,per_id = 2)) %>% 
#   distinct(teamId,fullName,appliedTotal,scoringPeriodId,.keep_all = T)

# team_list %>% filter(team =="'R'm Chair Quarterback") %>% 
#   # as.data.frame() %>%
#   filter(points_type=="actual") %>% arrange(fullName)

# player_slot <- rep(1:roster_size,number_of_teams)
# team_number <- rep(1:number_of_teams,roster_size) %>% sort()

# team_list <- purrr::map2_dfr(player_slot,team_number, .f = ~player_extract(team_number = .y,player_number = .x)) %>%
#   left_join(schedule) %>% 
#   mutate(points_type = if_else(str_length(externalId) > 6, "actual", "projected")) %>% 
#   relocate(team:appliedTotal, points_type)

# team_list %>% 
#   filter(lineupSlot_id != 20) %>% filter(lineupSlot_id==21)
  

schedule_prep <-
team_list %>% as.data.frame() %>% 
  # filter(team %in% c("'R'm Chair Quarterback")) %>%
  # filter(team %in% c("Palindrome Tikkit")) %>%
  filter(points_type=="actual")  %>%
  filter(!lineupSlot_id %in% c(20,21)) %>%  # remove bench players
  filter(scoringPeriodId <= per_id) %>% 
  group_by(team, scoringPeriodId, points_type)  %>% 
  # filter(scoringPeriodId==1) %>% 
  summarise(points = sum(appliedTotal), gameId = gameId[1]) %>% 
  arrange(scoringPeriodId,gameId) %>%
  filter(points_type == "actual") %>% 
  group_by(scoringPeriodId,gameId) 

# schedule_prep %>% as.data.frame() %>% arrange(team)

standings <-
schedule_prep %>% 
  mutate(Win_week = points==max(points)) %>% 
  ungroup() %>% 
  group_by(team) %>% summarise(Wins = sum(Win_week)) %>%
  mutate(Losses = per_id - Wins) %>% 
  group_by(team) %>% 
  mutate(win_perc = Wins / (Wins + Losses))

# schedule_prep %>% as.data.frame() %>% arrange(scoringPeriodId,points)

week_win_standings <-
schedule_prep %>% 
  group_by(scoringPeriodId) %>% 
  mutate(week_wins = rank(points)-1) %>% 
  mutate(week_losses = max(week_wins)-week_wins) %>% 
  group_by(team) %>% 
  summarise(week_wins = sum(week_wins), week_losses = sum(week_losses)) %>% 
  mutate(week_win_perc = week_wins / (week_wins + week_losses))

total_standings <-
  standings %>% 
  left_join(week_win_standings) %>% 
  relocate(contains("perc"), .after = last_col()) %>% 
  ungroup() %>% 
  mutate(luck = (-win_perc + week_win_perc)/sqrt(2)) %>% 
  mutate(across(.cols = c(win_perc, week_win_perc), .fns = ~round(.,3)))
  

luck_help_df = tibble(win_perc = c(0.3,0.3,.7,.7), week_win_perc = c(0,1,1,0), labs = c("Bad","Unlucky","Good","Lucky"))

plot_luck_chart <- function(total_standings = total_standings){
  total_standings %>% 
    arrange(-luck) %>% 
    ggplot(aes(x=win_perc,y=week_win_perc, color = luck)) +
    geom_point(size = 3) +
    geom_abline(aes(intercept = 0,slope = 1)) +
    xlim(0,1) + ylim(0,1) +
    scale_color_gradient2(low = "green",mid = "grey" ,high = "red",midpoint = 0,limits=c(range(total_standings$luck))) +
    # scale_color_continuous(low = "green", high = "red",limits=c(range(total_standings$luck))) +
    geom_label_repel(aes(label = str_c(team)), color = "Black",max.iter = 10000) +
    geom_label(data = luck_help_df,mapping = aes(x=win_perc,y=week_win_perc,label = labs), color = "Black", label.size = 1) +
    labs(title = "How Lucky Is Your Team?",subtitle = "Are You Winning As Much As You Should?",
         x = "Traditional Win Percentage", y = "Every Game Every Week Win Percentage", color = "Luck") +
    theme(legend.position = "none")
  
}

luck_chart <- plot_luck_chart(total_standings = total_standings)

players_letting_down_week <-
team_list %>% 
  filter(scoringPeriodId == per_id) %>% 
  filter(lineupSlot_id != 20) %>% 
  group_by(team, scoringPeriodId, points_type) %>% 
  summarise(points = sum(appliedTotal)) %>% 
  mutate(this_week = scoringPeriodId == per_id) %>% 
  pivot_wider(names_from = points_type,values_from = points) %>% 
  mutate(net_points = actual - projected) %>% 
  ggplot(aes(x=projected, y = net_points, color = net_points)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  scale_color_gradient2(low = "red",mid = "grey" ,high = "green",midpoint = 0) +
  geom_label_repel(aes(label = str_c(team)), color = "Black",max.iter = 10000) +
  labs(title = "Are your players letting you down?",subtitle = str_c("How many more (or less) points did your team score than projected in week ", per_id, "?"),
       x = "Projected Points", y = "Net Points (Actual - Projected)", 
       caption = "Over the line is overperformance.  Under the line is underperformance.") +
  theme(legend.position = "none") +
  ylim(-50,50)

players_letting_down_overall <-
team_list %>%  
  # filter(scoringPeriodId == per_id) %>%
  filter(lineupSlot_id != 20) %>% 
  group_by(team, points_type) %>% 
  # group_by(team, scoringPeriodId, points_type) %>% 
  summarise(points = sum(appliedTotal)) %>% as.data.frame() %>%
  # mutate(this_week = scoringPeriodId == per_id) %>% 
  pivot_wider(names_from = points_type,values_from = points) %>% 
  mutate(net_points = actual - projected) %>% 
  ggplot(aes(x=projected, y = net_points, color = net_points)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  scale_color_gradient2(low = "red",mid = "grey" ,high = "green",midpoint = 0) +
  geom_label_repel(aes(label = str_c(team)), color = "Black",max.iter = 10000) +
  labs(title = "Are your players letting you down?",subtitle = str_c("How many more (or less) points did your team score over the season?"),
       x = "Projected Points", y = "Net Points (Actual - Projected)", 
       caption = "Over the line is overperformance.  Under the line is underperformance.") +
  theme(legend.position = "none") +
  ylim(-50,50)

# 2 rb
# 4 wr
# 23 flex
# 0 qb
# 6 TE
# 16 def
# 17 kicker

best_roster <- function(team_num = 1){
  base <-
    team_list %>% 
    filter(points_type == "actual") %>%
    filter(teamId == team_num) %>%
    unnest(eligibleSlots) %>% 
    group_by(scoringPeriodId)
    
  rbs <-
    base %>% 
    filter(eligibleSlots == 2) %>%
    slice_max(appliedTotal, n = n_rb)

  wrs <-
    base %>% 
    filter(eligibleSlots == 4) %>%
    slice_max(appliedTotal, n = n_wr)

  tes <-
    base %>% 
    filter(eligibleSlots == 6) %>%
    slice_max(appliedTotal, n = n_te)

  qbs <-
    base %>% 
    filter(eligibleSlots == 0) %>%
    slice_max(appliedTotal, n = n_qb)

  def <-
    base %>% 
    filter(eligibleSlots == 16) %>%
    slice_max(appliedTotal, n = n_dst)

  kik <-
    base %>% 
    filter(eligibleSlots == 17) %>%
    slice_max(appliedTotal, n = n_k)
  
  flex <-
  base %>% 
    filter(!fullName %in% c(rbs$fullName,wrs$fullName,tes$fullName,qbs$fullName,def$fullName,kik$fullName)) %>% 
    filter(eligibleSlots == 23) %>% 
    slice_max(appliedTotal,n= n_flex)
  
  best_roster <-
  bind_rows(rbs,wrs,tes,qbs,def,kik,flex)  
  
 return(best_roster)
}

best_points <-
team_ids %>% 
  map_dfr(~best_roster(team_num = .x)) %>% 
  group_by(scoringPeriodId, team) %>% 
  summarise(best_points = sum(appliedTotal))

best_points_season <-
team_ids %>% 
  map_dfr(~best_roster(team_num = .x)) %>% 
  group_by(team) %>% 
  summarise(best_points = sum(appliedTotal))

week_points <-
team_list %>% 
  # filter(str_detect(team, "Quart")) %>%
  # as.data.frame() %>% filter(scoringPeriodId==5) %>%
  filter(lineupSlot_id != 20) %>% 
  filter(lineupSlot_id != 21) %>% 
  filter(points_type == "actual") %>%
  # arrange(lineupSlot_id)
  group_by(scoringPeriodId, team) %>% 
  summarise(week_points = sum(appliedTotal))



total_points <-
team_list %>% 
  # filter(team == "Syntax Error") %>%
  # as.data.frame() %>% filter(scoringPeriodId==1) %>%
  filter(lineupSlot_id != 20) %>% 
  filter(points_type == "actual") %>%
  # arrange(lineupSlot_id)
  group_by(team) %>% 
  summarise(week_points = sum(appliedTotal))

letting_players_down_week <-
best_points %>% 
  left_join(week_points) %>% 
  filter(scoringPeriodId == per_id) %>% #
  mutate(net_points = best_points - week_points) %>% 
  ggplot(aes(x=week_points, y = net_points, color = net_points)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  scale_color_gradient2(low = "red",mid = "grey" ,high = "green",midpoint = 0) +
  geom_label_repel(aes(label = str_c(team)), color = "Black",max.iter = 10000) +
  labs(title = "Are you letting your players down?",
       subtitle = str_c("How many more points could you have scored if you picked your best lineup in week ", per_id, "?"),
  # labs(title = "Are you letting your players down?",
  #      subtitle = str_c("How many more points could you have scored if you picked your best lineup in week ", per_id, "?"),
       x = "Actual Points", y = "Meat left on the bone (Actual Points - Potential Points)",
       caption = "Higher on the Y Axis: Bad at picking the right players to start \n Higher on the X Axis: Players started scoring more points") +
  theme(legend.position = "none")

letting_players_down_season <-
best_points_season %>% 
  left_join(total_points) %>% 
  # filter(scoringPeriodId == per_id) %>% #
  mutate(net_points = best_points - week_points) %>% 
  ggplot(aes(x=week_points, y = net_points, color = net_points)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  scale_color_gradient2(low = "red",mid = "grey" ,high = "green",midpoint = 0) +
  geom_label_repel(aes(label = str_c(team)), color = "Black",max.iter = 10000) +
  labs(title = "Are you letting your players down?",
       subtitle = str_c("How many more points could you have scored if you picked your best lineup in week ", per_id, "?"),
       x = "Actual Points", y = "Meat left on the bone (Actual Points - Potential Points)",
       caption = "Higher on the Y Axis: Bad at picking the right players to start \n Higher on the X Axis: Players started scoring more points") +
  theme(legend.position = "none")

# week_points %>% as.data.frame()

mug <-
week_points %>% 
  filter(week_points == max(week_points))

mugtally <-
  mug %>% 
  ungroup() %>% 
  count(team)

plunger <-
week_points %>% 
  filter(week_points == min(week_points))

plungertally <-
  plunger %>% 
  ungroup() %>% 
  count(team) 


coach_let_down <-
  best_points %>% 
  left_join(week_points) %>% 
  mutate(net_points = best_points - week_points) %>% 
  filter(scoringPeriodId == max(scoringPeriodId)) %>% 
  slice_max(net_points)

best_coach <-
  best_points %>% 
  left_join(week_points) %>% 
  mutate(net_points = best_points - week_points) %>% 
  filter(scoringPeriodId == max(scoringPeriodId)) %>% 
  slice_min(net_points)

biggest_letdown <-
team_list %>% 
  filter(lineupSlot_id != 20) %>% 
  group_by(team, scoringPeriodId, points_type) %>% 
  summarise(points = sum(appliedTotal)) %>% 
  mutate(this_week = scoringPeriodId == per_id) %>% 
  pivot_wider(names_from = points_type,values_from = points) %>% 
  mutate(net_points = actual - projected) %>% 
  arrange(net_points) %>% 
  group_by(scoringPeriodId) %>% 
  slice_min(net_points) %>% 
  mutate(projected = round(projected,2))

outperformance <-
team_list %>% 
  filter(lineupSlot_id != 20) %>% 
  group_by(team, scoringPeriodId, points_type) %>% 
  summarise(points = sum(appliedTotal)) %>% 
  mutate(this_week = scoringPeriodId == per_id) %>% 
  pivot_wider(names_from = points_type,values_from = points) %>% 
  mutate(net_points = actual - projected, projected = round(projected,2)) %>% 
  arrange(net_points) %>% 
  group_by(scoringPeriodId) %>% 
  slice_max(net_points)



team_performance <- function(data = team_list, per_id_now = per_id, team_no = 1){
  
  team_name <- data %>% filter(teamId == team_no) %>% slice_head() %>% select(team) %>% pull
  
  plot <-
data %>% 
  filter(scoringPeriodId == per_id_now) %>% 
  filter(teamId == team_no) %>% 
  select(team,fullName,appliedTotal,points_type) %>% 
  pivot_wider(names_from = points_type,values_from = appliedTotal) %>%
  mutate(y = 0) %>% 
  mutate(color = actual-projected > 0) %>% 
  ggplot(aes(x=projected, y = y)) +
  geom_point(size = 5) +
  geom_point(aes(x = actual)) +
  ylim(-5,5) +
  scale_color_manual(values = c("Red","Green")) +
  geom_segment(aes(x=projected, y = y, xend = actual, yend = y, color = color), arrow = arrow()) +
  facet_wrap(~forcats::fct_reorder(fullName,projected),ncol = 1) +
  # facet_wrap(~forcats::fct_reorder(fullName,projected),ncol = 1, labeller = labeller(size = 10)) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
        # strip.text = element_text(size = 3)) +
  labs(x = "Points", title = str_c("How well each player on ", team_name, " performed.") ,subtitle = "Big dots represent projected score.  Small dots represent actual score.",
      caption = "Arranged by projected score")

  plot
  
return(list(team = team_name, plot = plot))

}

plots <- purrr::map(.x = team_ids, .f = ~team_performance(team_no = .x))


# 2 rb
# 4 wr
# 23 flex
# 0 qb
# 6 TE
# 16 def
# 17 kicker

player_predictions_hist <-
team_list %>% 
  # filter(fullName == "49ers D/ST") %>% 
  unnest(eligibleSlots) %>% 
  filter(eligibleSlots %in% c(2,4,0,6,16,17)) %>% 
  mutate(position = case_when(eligibleSlots == 0 ~ "Quarter Back",
                              eligibleSlots == 2 ~ "Running Back",
                              eligibleSlots == 4 ~ "Wide Receiver",
                              eligibleSlots == 6 ~ "Tight End",
                              eligibleSlots == 16 ~ "Defense",
                              eligibleSlots == 17 ~ "Kicker",
                              )) %>% 
  select(fullName,appliedTotal,points_type,scoringPeriodId,eligibleSlots, position)  %>% 
  distinct(fullName,appliedTotal,points_type,scoringPeriodId,eligibleSlots,position) %>% 
  # group_by(scoringPeriodId,fullName) %>%
  pivot_wider(names_from = points_type, values_from = appliedTotal)  %>% 
  mutate(overperformance = actual - projected) %>% 
  ggplot(aes(x=overperformance, fill = position)) +
  geom_histogram() +
  geom_vline(aes(xintercept = 0)) +
  facet_wrap(~forcats::fct_reorder(position,eligibleSlots), ncol = 1) +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = "Actual - Predicted Points",title = "How Good Are ESPN's Predictions?",caption = "Positive Means ESPN Underpredicted Performance.")
  

# team_list %>% 
#   unnest(eligibleSlots) %>% 
#   filter(eligibleSlots %in% c(2,4,0,6,16,17)) %>% 
#   select(fullName,appliedTotal,points_type,scoringPeriodId,eligibleSlots) %>% 
#   pivot_wider(names_from = points_type, values_from = appliedTotal) %>% 
#   mutate(overperformance = actual - projected) %>% 
#   ggplot(aes(x=projected, y = actual, color = as.factor(eligibleSlots))) +
#   geom_point()

all_list <- list(
  mug = mug, 
  plunger = plunger, 
  total_standings = total_standings,
  luck_chart = luck_chart,
  biggest_letdown = biggest_letdown,
  outperformance = outperformance,
  coach_let_down = coach_let_down,
  best_coach = best_coach,
  players_letting_down_week = players_letting_down_week,
  players_letting_down_overall = players_letting_down_overall,
  letting_players_down_week = letting_players_down_week,
  plots = plots,
  player_predictions_hist = player_predictions_hist,
  mugtally = mugtally,
  plungertally = plungertally
  )

return(all_list)

}



# dashboard_data <- get_dashboard_data()
