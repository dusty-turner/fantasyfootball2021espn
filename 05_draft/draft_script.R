library(tidyverse)
library(ggrepel)

base = "http://fantasy.espn.com/apis/v3/games/ffl/seasons/"
year = "2021"
mid = "/segments/0/leagues/"
leagueID = "89417258"
tail = str_c("?view=mDraftDetail",
             "&view=mLiveScoring&view=mMatchupScore",
             "&view=mPendingTransactions",
             "&view=mPositionalRatings",
             "&view=mSettings",
             "&view=mTeam",
             "&view=modular",
             "&view=mNav",
             "&view=mMatchupScore",
             "&view=players_wl",
             "&view=mTransactions2",
             "&view=mStatus",
             "&view=mPositionalRankingsStats"
)
url = paste0(base,year,mid,leagueID,tail)


ESPNGet <- httr::GET(url = url)
ESPNGet$status_code

ESPNRaw <- rawToChar(ESPNGet$content)
ESPNFromJSON <- jsonlite::fromJSON(ESPNRaw)


## team info
team_info <-
  ESPNFromJSON$teams %>% 
  select(abbrev,currentProjectedRank,divisionId,draftDayProjectedRank,id,location,nickname,playoffSeed,points,pointsAdjusted,pointsDelta,rankCalculatedFinal,waiverRank) %>% 
  bind_cols(ESPNFromJSON$teams$transactionCounter %>% select(acquisitions))

## game info
game_df <-
  ESPNFromJSON$schedule$away %>% mutate(location = "Away") %>% bind_cols(ESPNFromJSON$schedule %>% select(id,matchupPeriodId,winner)) %>% 
  bind_rows(ESPNFromJSON$schedule$home %>% mutate(location = "Away") %>% bind_cols(ESPNFromJSON$schedule %>% select(id,matchupPeriodId,winner)))  %>% 
  as_tibble() %>% 
  rename(game_id = id)

game_and_team_info <-
  game_df %>% 
  left_join(team_info, by = c("teamId"="id")) 





team <- 
  game_and_team_info %>%
  mutate(team_name = str_c(location.y," ",nickname)) %>% 
  select(team_name) %>% 
  distinct() 

### Everything above this line is just me being extra to get a vector of team names

### The code below is what does the randomization

draft_order <-
  team %>% 
  mutate(rand = runif(10,0,1)) %>% 
  arrange(rand) %>% 
  mutate(draft_pick = row_number())

### The code below is just me being extra in how we revealed it



reveal_draft_order <- function(n, wait = TRUE){
  n <- 11-n
  draft <-
    draft_order %>% 
    slice(n)
  message(str_c("The number ", n, " pick in the 2020 OA Fantasy Football Draft is: "))
  if(wait){Sys.sleep(5)}
  message(str_c(draft$team_name, praise::praise(template = ", ${Exclamation}!")))
  if(wait){Sys.sleep(10)}
}
# reveal_draft_order(1, wait = TRUE)

purrr::walk(1:10, ~reveal_draft_order(.x,wait = T))




