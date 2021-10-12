library(tidyverse)


leagueID <- list(89417258)
# leagueID <- list(847888,35354777,89417258,206814)
# names <- list("jim","headshed","OA","Twitter_Guy")
# is_dusty = TRUE
names <- list("OA")
per_id <- 5

run_reports <- function(leagueID, per_id = per_id, names) {
  # leagueID=89417258
  # per_id=1
  # unlink("ff2020_reports",recursive = T,force = T)
  
  if(!dir.exists("03_ff2021_reports")){
    dir.create("03_ff2021_reports")
  } 
  
  rmarkdown::render("ff2021.Rmd",params=list(
    per_id=per_id, 
    leagueID=leagueID))

  file.rename(from="ff2021.html", to = paste0("ffdashboard_",names,"_",per_id,".html"))

  file.copy(from=str_c("ffdashboard_",names,"_",per_id,".html"),
            to=str_c("03_ff2021_reports/ffdashboard_",names,"_",per_id,".html" ),overwrite = T)

  file.remove(paste0(getwd(),"/","ffdashboard_",names,"_",per_id,".html"))

  unlink(x = "ff2020_cache*",recursive = T, force = T)
  
  
  if(Sys.info()[[6]]=="dusty.s.turner" & names %in% c("headshed","OA")){
    file.copy(from=str_c("03_ff2020_reports/ffdashboard_",names,"_",per_id,".html"),
              to=str_c("../blog/static/ffdashboard_",names,"_",per_id,".html")
    )
  }

}

purrr::walk2(.x = leagueID, .y = names,.f = ~run_reports(leagueID = .x,names = .y,per_id = per_id))

system("git status")
system("git add .")
system('git commit -m "auto commit"')

