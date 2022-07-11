library(DBI)
library(RSQLite)
library(nflfastR)
library(tidyverse)
library(furrr)
# if (!requireNamespace("remotes", quietly = TRUE)) {install.packages("remotes")}
# remotes::install_github("mrcaseb/nflfastR")

source("https://raw.githubusercontent.com/nflverse/nflfastR/d0a0009237938d8fe5a1aaaae2fbec5447467828/R/helper_add_xyac.R")
source("https://raw.githubusercontent.com/nflverse/nflfastR/fd8a24580dfda70253eea1e85b4b22101d5a743f/R/helper_add_nflscrapr_mutations.R")
source("https://raw.githubusercontent.com/nflverse/nflfastR/a02724d9ef2a802b41b799a37d88702081cead08/R/helper_additional_functions.R")
source("https://raw.githubusercontent.com/nflverse/nflfastR/57638c4c6ae1286b226e255570d0024c0df9d2ce/R/utils.R")
source("https://raw.githubusercontent.com/christianlohr9/Expected-Fantasy-Points/main/R/xyac-ryoe-helper-functions.R")

# Sys.setenv(http_proxy = "172.30.15.242:8080")
# Sys.setenv(https_proxy = "172.30.7.242:8080")

act_season <- 2021
week <- 1:17
scoring <- c("preset_ppr","139712","173881")
art <- c("receiving","passing")

options(digits = 2,scipen = 9999)

update_db()
connection <- dbConnect(SQLite(), "./pbp_db")
rosters <- fast_scraper_roster(act_season)

pbp_db <- tbl(connection, "nflfastR_pbp")


##### Own xFP model
#####

pbp_all <- pbp_db %>% 
  filter(season==act_season) %>% collect() %>% 
  add_xyac %>% 
  add_ryoe
  
###########
# Everything below is a fucking mess!
###########


xFP <- avg_exp_fp_df %>% 
  select(season, week, game_id, play_id, posteam, receiver_id, rusher_id, yardline_100, air_yards, 
         actual_yards_gained = yards_gained, complete_pass, cp, xyac_prob, xyac_gain, x_rush_yards) %>% 
  mutate(
    gain = ifelse(yardline_100==air_yards, yardline_100, xyac_gain),
    #oben bei gain einfach first down einf?gen du Held
    yac_prob = ifelse(yardline_100==air_yards, 1, xyac_prob),
    PPR_points = 0.5 + gain/10 + ifelse(gain == yardline_100, 6, 0),
    catch_run_prob = cp * xyac_prob,
    exp_PPR_points = PPR_points * catch_run_prob,
    exp_yards = gain * catch_run_prob,
    actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
    actual_PPR_points = ifelse(actual_outcome==1, PPR_points, 0),
    target = 0,
    game_played = 0
  )  %>% 
  group_by(game_id, receiver_id) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(game_id, play_id, receiver_id) %>% 
  mutate(target = ifelse(row_number()==1,1,0)) %>% 
  ungroup %>% 
  group_by(season, week, posteam, receiver_id) %>% 
  summarize(
    games = sum(game_played, na.rm = T),
    targets = sum(target, na.rm = T),
    catches = sum(actual_outcome, na.rm = T),
    yards = sum(ifelse(actual_outcome==1, gain, 0), na.rm = T),
    td = sum(ifelse(gain==yardline_100, actual_outcome, 0), na.rm = T),
    PPR_pts = sum(actual_PPR_points, na.rm = T),
    exp_catches = sum(ifelse(target==1, cp, NA), na.rm = T),
    exp_yards = sum(exp_yards, na.rm = T),
    exp_td = sum(ifelse(gain==yardline_100, catch_run_prob, 0), na.rm = T),
    exp_PPR_pts = sum(exp_PPR_points, na.rm = T)
  ) %>% 
  ungroup

####

rec_actual_raw_xFP <- rec_actual_raw %>% 
  left_join(rec_actual_xFP %>% select(gsis_id=receiver_id,xFP=exp_PPR_pts,FP=PPR_pts,season,week), by=c("gsis_id"="gsis_id","week"="week"))

# write.csv2(rec_actual_raw_xFP,"C:/Users/clohr/Downloads/rec_actual_w10.csv")

# PFF snapcount data

get_snaps_rec <- function(week){
  raw <- httr::GET(paste0(
    "https://premium.pff.com/api/v1/facet/receiving/summary?league=nfl&season=2021&week=",
    week),
    httr::set_cookies(
      "seerses"="e",
      "seerid"="u_883230244543415300",
      "mailmunch_second_pageview"="true",
      "_mailmunch_visitor_id"="0ba6adf9-d968-4064-9804-1acc99b8be8a",
      "_gcl_au"="1.1.454177847.1632744079",
      "_fbp"="fb.1.1633110538604.1350247760",
      "_ga"="GA1.1.605246541.1609166519",
      #
      "_premium_key"="SFMyNTY.g3QAAAABbQAAABZndWFyZGlhbl9kZWZhdWx0X3Rva2VubQAAAmpleUpoYkdjaU9pSklVelV4TWlJc0luUjVjQ0k2SWtwWFZDSjkuZXlKaGRXUWlPaUpRY21WdGFYVnRJaXdpWlhod0lqb3hOak0yTlRRMU9UQTJMQ0pwWVhRaU9qRTJNelkxTkRJek1EWXNJbWx6Y3lJNklsQnlaVzFwZFcwaUxDSnFkR2tpT2lKbVlUQTRZV1JrWmkwM05UVmtMVFExWWpndFlUWTVOQzB5WVRBek1EZzFabVE0WVRBaUxDSnVZbVlpT2pFMk16WTFOREl6TURVc0luQmxiU0k2ZXlKaFlXWWlPakVzSW01bWJDSTZNU3dpZUdac0lqb3hmU3dpYzNWaUlqb2llMXdpWlcxaGFXeGNJanBjSW1ac2J5NWlhV1ZzYldWcFpYSkFaMjFoYVd3dVkyOXRYQ0lzWENKbVpXRjBkWEpsYzF3aU9sdGRMRndpWm1seWMzUmZibUZ0WlZ3aU9sd2lSbXh2Y21saGJsd2lMRndpYkdGemRGOXVZVzFsWENJNlhDSkNhV1ZzYldWcFpYSmNJaXhjSW5WcFpGd2lPbHdpWWpKbU9EZGhObUl0TVdKa055MDBNMlkyTFRrM09EVXROakl5WVRoaVpqazBZVFUxWENJc1hDSjJaWEowYVdOaGJGd2lPbHdpUTI5dWMzVnRaWEpjSW4waUxDSjBlWEFpT2lKaFkyTmxjM01pZlEuay1jZURrcWhiSUU0U1dzbGR4cnhQeVRKSFpuQTdIQnlRX3NMaUtpMUo5N0wtQW10M0hiRDdTNmM4RHFWSGpzQlh4ZS10RVcyRkdJYmtjRVdyUW1HaEE.yO0Gv4LGwrBwbuKJYSuwB39WjZB668ufCcPsl_00rPk",
      "_gid"="GA1.2.2085606972.1636542308",
      "_ga_8Y6RN784SW"="GS1.1.1636542307.349.1.1636542311.56",
      "c_groot_access_token"="Zcv8htDpjRC6G3ipjGC2OI5_RszNzgHn5_dXl5fbPAqu71a5k7QEiu9-lCVBMGrN",
      # "c_groot_access_ts"="2020-12-07T15:33:44Z",
      "c_groot_refresh_token"="ZoQTKWQ_mq_enbz3xHPWfuKunhbz6M4ZhXiqgHzE5kF0q83l9rJ-ed9jCzIS7UN0"
    )
  )
  
  content <- httr::content(raw) %>% 
    purrr::pluck("receiving_summary") %>% 
    purrr::map_dfr(function(x){
      x %>% purrr::discard(is.list)
    }) %>% 
    mutate(season=act_season,
           week=week)
  
  return(content)
  
}

snapcount_rec <- purrr::map_dfr(week, get_snaps_rec)

snapcount_team_rec <- snapcount_rec %>% 
  mutate(offense = pass_plays) %>% 
  select(player_id,offense,routes,week,team) %>% 
  group_by(week,team) %>% 
  summarise(snaps = max(offense)) %>% 
  as_tibble()

snapcount_player_rec <- snapcount_rec %>% 
  mutate(offense = pass_plays) %>% 
  select(player_id,offense,routes,week,team) %>% 
  as_tibble()

snapcount_actual_rec <- snapcount_player_rec %>%
  left_join(snapcount_team_rec, by = c("team"="team",
                                       "week"="week")) %>% 
  mutate(snapshare = offense/snaps) %>% 
  select(player_id,routes,week,offense,snapshare)

rec_actual_tidy <- snapcount_actual_rec %>% 
  right_join(rec_actual_raw, by = c("player_id" = "pff_id",
                                    "week" = "week")) %>% 
  mutate(trr = targets/routes,
         yrr = yards/routes,
         season = act_season) # for some reason the season from PFF isn't taken for some rookies (missing data in nflfastR?)

# PFF xFP data

get_xFP <- function(week){
  raw <- httr::GET(paste0(
    "https://www.pff.com/api/fantasy/expected-points?scoring=preset_half_ppr&report=receiving&season=2021&weeks=",
    week),
    httr::set_cookies(
      "seerses"="e",
      "seerid"="u_883230244543415300",
      "mailmunch_second_pageview"="true",
      "_mailmunch_visitor_id"="0ba6adf9-d968-4064-9804-1acc99b8be8a",
      "_gcl_au"="1.1.454177847.1632744079",
      "_fbp"="fb.1.1633110538604.1350247760",
      "_ga"="GA1.1.605246541.1609166519",
      #
      "_merlin_key"="SFMyNTY.g3QAAAAEbQAAAAtfY3NyZl90b2tlbm0AAAAYcy1RcnBvVlFOOUpXZGlGUnZwLWpoTm0zbQAAABZndWFyZGlhbl9kZWZhdWx0X3Rva2VubQAAAlRleUpoYkdjaU9pSklVelV4TWlJc0luUjVjQ0k2SWtwWFZDSjkuZXlKaGRXUWlPaUpOWlhKc2FXNGlMQ0psZUhBaU9qRTJNelkxTkRVd09UY3NJbWxoZENJNk1UWXpOalUwTXpJNU55d2lhWE56SWpvaVRXVnliR2x1SWl3aWFuUnBJam9pTkRnd05qWXdNell0TURFd01TMDBNVFE1TFRrd1pURXRPVFZoT1RnNVpHVTFPVE5pSWl3aWJtSm1Jam94TmpNMk5UUXpNamsyTENKd1pXMGlPbnNpWld4cGRHVWlPakY5TENKemRXSWlPaUo3WENKbGJXRnBiRndpT2x3aVpteHZMbUpwWld4dFpXbGxja0JuYldGcGJDNWpiMjFjSWl4Y0ltWmxZWFIxY21WelhDSTZXMTBzWENKbWFYSnpkRjl1WVcxbFhDSTZYQ0pHYkc5eWFXRnVYQ0lzWENKc1lYTjBYMjVoYldWY0lqcGNJa0pwWld4dFpXbGxjbHdpTEZ3aWRXbGtYQ0k2WENKaU1tWTROMkUyWWkweFltUTNMVFF6WmpZdE9UYzROUzAyTWpKaE9HSm1PVFJoTlRWY0lpeGNJblpsY25ScFkyRnNYQ0k2WENKRGIyNXpkVzFsY2x3aWZTSXNJblI1Y0NJNkltRmpZMlZ6Y3lKOS5MUFAydi1OSzFQV0hyOEhKS3JwNkJTVUpVLUZqdnBBRnN5am15ZXNhSWJIZ29YUno4bHhMOWVlX1JrQ1ZaeklDeVl3ajYwVjBDVC1YWDlsOEY5eGVtd20AAAAJcmV0dXJuX3RvbQAAABgvZmFudGFzeS9leHBlY3RlZC1wb2ludHNtAAAAFXlhaG9vX3NpdGVfbGVhZ3VlX3VybGQAA25pbA.sTYbTs2krOFJ8IF-e5v3Ytcx60p5rwLj_3AK6XX3jk4",
      "_gid"="GA1.2.2085606972.1636542308",
      "_ga_8Y6RN784SW"="GS1.1.1636542307.349.1.1636543298.60",
      "c_groot_access_token"="Zcv8htDpjRC6G3ipjGC2OI5_RszNzgHn5_dXl5fbPAqu71a5k7QEiu9-lCVBMGrN",
      # "c_groot_access_ts"="2020-12-07T15:33:44Z",
      "c_groot_refresh_token"="ZoQTKWQ_mq_enbz3xHPWfuKunhbz6M4ZhXiqgHzE5kF0q83l9rJ-ed9jCzIS7UN0",
      "AWSALB"="5SI9sz+sUUZ13HdEI+XnJXtbBSOoZFtmosgjlwAga8rBrdg6GdWKzYcboVzXZ5VuqnDGSoMM0EyCjCf3stKdGAwGM9iYBdqGrwn/vnHE3IVYYkYH7fi1zfuhJY2y",
      "AWSALBCORS"="5SI9sz+sUUZ13HdEI+XnJXtbBSOoZFtmosgjlwAga8rBrdg6GdWKzYcboVzXZ5VuqnDGSoMM0EyCjCf3stKdGAwGM9iYBdqGrwn/vnHE3IVYYkYH7fi1zfuhJY2y"
    )
  )
  
  content <- httr::content(raw) %>% 
    purrr::pluck("points") %>% 
    purrr::map_dfr(function(x){
      x %>% purrr::discard(is.list)
    }) %>% 
    mutate(season=act_season,
           week=week)
  
  return(content)
  
}

xFP_rec <- purrr::map_dfr(week, get_xFP)

rec_actual <- xFP_rec %>% 
  mutate(xFP_rec = expected_receiving_yards*0.1 + expected_receptions*0.5 + expected_receiving_tds*6) %>% 
  select(player_id,season,week,xFP_rec) %>% 
  right_join(rec_actual_tidy) %>% 
  mutate(xFPrr = xFP_rec/routes)

rec_actual %>% 
  select(full_name,posteam,position,offense,snapshare,targets,rec,yards,td,
         firstdown,redzonetgt,airyards,yac,aDot,epa,wpa,targetshare,
         airyardshare,skillplayshare,trr,yrr,RACR,WOPR,SPORT,xFPrr,season,week) %>% 
  saveRDS("Z:/GitHub/UpsideShiny/data/rec_actual_2021.rds")
#saveRDS("C:/Users/Christian/Desktop/stats/data/rec_actual.rds")


####################################################################