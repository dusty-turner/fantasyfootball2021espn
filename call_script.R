library(tidyverse)


# leagueID <- list(89417258)
leagueID <- list(847888,35354777,89417258,206814)
names <- list("jim","headshed","OA","Twitter_Guy")
is_dusty = TRUE
per_id <- 2

run_reports <- function(leagueID, per_id = per_id, names) {
  # leagueID=89417258
  # per_id=1
  # unlink("ff2020_reports",recursive = T,force = T)
  
  if(!dir.exists("03_ff2020_reports")){
    dir.create("03_ff2020_reports")
  } 
  
  rmarkdown::render("ff2020.Rmd",params=list(
    per_id=per_id, 
    leagueID=leagueID))

  file.rename(from="ff2020.html", to = paste0("ffdashboard_",names,"_",per_id,".html"))

  file.copy(from=str_c("ffdashboard_",names,"_",per_id,".html"),
            to=str_c("03_ff2020_reports/ffdashboard_",names,"_",per_id,".html" ),overwrite = T)

  file.remove(paste0(getwd(),"/","ffdashboard_",names,"_",per_id,".html"))

  unlink(x = "ff2020_cache*",recursive = T, force = T)
  
  
  if(Sys.info()[[6]]=="dusty.s.turner" & names %in% c("headshed","OA")){
    file.copy(from=str_c("03_ff2020_reports/ffdashboard_",names,"_",per_id,".html"),
              to=str_c("../blog/static/ffdashboard_",names,"_",per_id,".html")
    )
    setwd("../blog")
    # getwd()
    blogdown::serve_site()
    blogdown::stop_server()
    # setwd("../fantasyfootball2020/")
    # system("cd ../blog")
    # system("git status")
    shell(str_c(getwd(),"/09_personal/shell1.sh"))
    
    # system("bash -l", input = str_c(getwd(),"/09_personal/shell1.sh"))
    # shell(cmd = "cd ../blog", shell = "git status")
    # shell(cmd = "git status", shell = "git")

  }

}


# map2(leagueIDs,rep(per_id_number,length(leagueIDs)),run_reports)
leagueID %>% 
purrr::walk2(names,.f = ~run_reports(leagueID = .x,names = .y,per_id = per_id))

# purrr::map(leagueID, ~run_reports(leagueID = .x,per_id = 1))
# 
# run_reports(leagueID = leagueID[1],per_id = 1)

