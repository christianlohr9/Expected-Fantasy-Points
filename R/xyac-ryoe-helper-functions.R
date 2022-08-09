
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_ep_wp.R")
source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_cp_cpoe.R")

add_xyac <- function(pbp, ...) {
  if (nrow(pbp) == 0) {
    user_message("Nothing to do. Return passed data frame.", "info")
  } else {
    # testing only
    # pbp <- g
    
    pbp <- pbp %>% dplyr::select(-tidyselect::any_of(drop.cols.xyac))
    
    # for joining at the end
    pbp <- pbp %>%
      dplyr::mutate(index = 1:dplyr::n())
    
    # prepare_xyac_data helper function shown below
    passes <- prepare_xyac_data(pbp) %>%
      dplyr::filter(.data$valid_pass == 1, .data$distance_to_goal != 0)
    
    if (!nrow(passes) == 0) {
      user_message("Computing xyac...", "todo")
      join_data <- passes %>%
        dplyr::select(
          "index", "distance_to_goal", "season", "week", "home_team", "posteam", "roof",
          "half_seconds_remaining", "down", "ydstogo",
          "posteam_timeouts_remaining", "defteam_timeouts_remaining",
          "original_spot" = "yardline_100", "original_ep" = "ep", "air_epa", "air_yards"
        ) %>%
        dplyr::mutate(
          down = as.integer(.data$down),
          ydstogo = as.integer(.data$ydstogo),
          original_ydstogo = .data$ydstogo
        ) %>%
        dplyr::select("index":"ydstogo", "original_ydstogo", dplyr::everything())
      
      xyac_vars <-
        stats::predict(
          fastrmodels::xyac_model,
          as.matrix(passes %>% xyac_model_select())
        ) %>%
        tibble::as_tibble() %>%
        dplyr::rename(prob = "value") %>%
        dplyr::bind_cols(
          tibble::tibble(
            "yac" = rep_len(-5:70, length.out = nrow(passes) * 76),
            "index" = rep(passes$index, times = rep_len(76, length.out = nrow(passes)))
          ) %>%
            dplyr::left_join(join_data, by = "index") %>%
            dplyr::mutate(
              half_seconds_remaining = dplyr::if_else(
                .data$half_seconds_remaining <= 6,
                0,
                .data$half_seconds_remaining - 6
              )
            )
        ) %>%
        dplyr::group_by(.data$index) %>%
        dplyr::mutate(
          max_loss = dplyr::if_else(.data$distance_to_goal < 95, -5, .data$distance_to_goal - 99),
          max_gain = dplyr::if_else(.data$distance_to_goal > 70, 70, .data$distance_to_goal),
          cum_prob = cumsum(.data$prob),
          prob = dplyr::case_when(
            # truncate probs at loss greater than max loss
            .data$yac == .data$max_loss ~ .data$cum_prob,
            # same for gains bigger than possible
            .data$yac == .data$max_gain ~ 1 - dplyr::lag(.data$cum_prob, 1),
            TRUE ~ .data$prob
          ),
          # get end result for each possibility
          yardline_100 = .data$distance_to_goal - .data$yac
        ) %>%
        dplyr::filter(.data$yac >= .data$max_loss, .data$yac <= .data$max_gain) %>%
        dplyr::select(-.data$cum_prob) %>%
        dplyr::mutate(
          posteam_timeouts_pre = .data$posteam_timeouts_remaining,
          defeam_timeouts_pre = .data$defteam_timeouts_remaining,
          gain = .data$original_spot - .data$yardline_100,
          turnover = dplyr::if_else(.data$down == 4 & .data$gain < .data$ydstogo, as.integer(1), as.integer(0)),
          down = dplyr::if_else(.data$gain >= .data$ydstogo, 1, .data$down + 1),
          ydstogo = dplyr::if_else(.data$gain >= .data$ydstogo, 10, .data$ydstogo - .data$gain),
          # possession change if 4th down failed
          down = dplyr::if_else(.data$turnover == 1, as.integer(1), as.integer(.data$down)),
          ydstogo = dplyr::if_else(.data$turnover == 1, as.integer(10), as.integer(.data$ydstogo)),
          # flip yardline_100 and timeouts for turnovers
          yardline_100 = dplyr::if_else(.data$turnover == 1, as.integer(100 - .data$yardline_100), as.integer(.data$yardline_100)),
          posteam_timeouts_remaining = dplyr::if_else(
            .data$turnover == 1,
            .data$defeam_timeouts_pre,
            .data$posteam_timeouts_pre
          ),
          defteam_timeouts_remaining = dplyr::if_else(
            .data$turnover == 1,
            .data$posteam_timeouts_pre,
            .data$defeam_timeouts_pre
          ),
          # ydstogo can't be bigger than yardline
          ydstogo = dplyr::if_else(.data$ydstogo >= .data$yardline_100, as.integer(.data$yardline_100), as.integer(.data$ydstogo)),
          ###
          gain = ifelse(yardline_100==air_yards, yardline_100, gain),
          yac_prob = ifelse(yardline_100==air_yards, 1, prob),
          PPR_points = 1 + gain/10 + ifelse(gain == yardline_100, 6, 0),
          catch_run_prob = cp * yac_prob,
          exp_PPR_points = PPR_points * catch_run_prob,
          exp_yards = gain * catch_run_prob,
          actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
          actual_PPR_points = ifelse(actual_outcome==1, PPR_points, 0),
          target = 0,
          game_played = 0
        ) %>%
        group_by(game_id, receiver) %>% 
        mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
        ungroup %>% 
        group_by(game_id, play_id, receiver) %>% 
        mutate(target = ifelse(row_number()==1,1,0)) %>% 
        ungroup %>% 
        group_by(.data$index) %>% 
        summarize(
          exp_catches = sum(ifelse(target==1, cp, NA), na.rm = T),
          exp_yards = sum(exp_yards, na.rm = T),
          exp_td = sum(ifelse(gain==yardline_100, catch_run_prob, 0), na.rm = T),
          exp_PPR_pts = sum(exp_PPR_points, na.rm = T)
        ) %>% 
        dplyr::ungroup() %>% filter(.data$index==1)
      
      pbp <- pbp %>%
        dplyr::left_join(xyac_vars, by = "index") %>%
        dplyr::select(-.data$index)
      
      message_completed("added xyac variables", ...)
    } else { # means no valid pass plays in the pbp
      pbp <- pbp %>%
        dplyr::mutate(
          xyac_epa = NA_real_,
          xyac_mean_yardage = NA_real_,
          xyac_median_yardage = NA_real_,
          xyac_success = NA_real_,
          xyac_fd = NA_real_
        ) %>%
        dplyr::select(-.data$index)
      user_message("No non-NA values for xyac calculation detected. xyac variables set to NA", "info")
    }
  }
  
  return(pbp)
}


add_ryoe <- function(pbp, ...) {
  if (nrow(pbp) == 0) {
    user_message("Nothing to do. Return passed data frame.", "info")
  } else {
    
    # pbp <- pbp %>% dplyr::select(-tidyselect::any_of(drop.cols.xyac))
    
    # for joining at the end
    pbp <- pbp %>%
      dplyr::mutate(index = 1:dplyr::n())
    
    # prepare_ryoe_data helper like you did with prepare_xyac_data function (according to Tejs Gist)
    pbp_rp <- pbp %>%
      filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")
    pbp_rp <- pbp_rp %>%
      mutate(
        pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
        rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
        success = ifelse(epa>0, 1 , 0)
      ) 

    pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)
    
    pbp_rp <- pbp_rp %>%
      mutate(
        posteam = case_when(
          posteam == 'OAK' ~ 'LV',
          posteam == 'SD' ~ 'LAC',
          posteam == 'STL' ~ 'LA',
          TRUE ~ posteam
        )
      )

    pbp_rp <- pbp_rp %>%
      mutate(
        defteam = case_when(
          defteam == 'OAK' ~ 'LV',
          defteam == 'SD' ~ 'LAC',
          defteam == 'STL' ~ 'LA',
          TRUE ~ defteam
        )
      )

    rush_attempts <- pbp_rp %>%
      filter(rush_attempt == 1, qb_scramble == 0, qb_dropback == 0)

    def_ypc <- rush_attempts %>%
      group_by(season, defteam) %>%
      summarize(def_ypc = mean(yards_gained),
                count = n()) %>%
      filter(count >= 100) %>%
      select(-count)

    rush_attempts <- rush_attempts %>%
      left_join(def_ypc, by = c("season", "defteam"))

    rush_attempts2 <- rush_attempts %>%
      mutate(yards_rushed = case_when(yards_gained > 20 ~ 20L,
                                      yards_gained < -5 ~ -5L,
                                      TRUE ~ as.integer(yards_gained)),
             label = yards_rushed + 5L)

    rush_attempts3 <- rush_attempts2 %>%
      mutate(run_left_end = if_else((run_gap == "end" & run_location == "left"), 1, 0),
             run_left_guard = if_else((run_gap == "guard" & run_location == "left"), 1, 0),
             run_left_tackle = if_else((run_gap == "tackle" & run_location == "left"), 1, 0),
             run_right_end = if_else((run_gap == "end" & run_location == "right"), 1, 0),
             run_right_guard = if_else((run_gap == "guard" & run_location == "right"), 1, 0),
             run_right_tackle = if_else((run_gap == "tackle" & run_location == "right"), 1, 0),
             run_middle = if_else((run_location == "middle"), 1, 0))

    
########## insert the model here
    ryoe_model1 <- xgb.load("H:/GitHub/Expected-Fantasy-Points/R/ryoe_model")
##########
    
    rushes <- rush_attempts3 %>%
      select(index,yardline_100, quarter_seconds_remaining, half_seconds_remaining,
             game_seconds_remaining, qtr, down, goal_to_go, ydstogo, shotgun, no_huddle,
             no_score_prob, ep, wp, def_ypc) #%>%
      # mutate(index = 1:n())

    join_data <- rushes %>%
      dplyr::select(
        "index", "yardline_100", "quarter_seconds_remaining", "half_seconds_remaining",
        "game_seconds_remaining", "qtr", "down", "goal_to_go", "ydstogo", "shotgun", "no_huddle",
        "no_score_prob", "ep", "wp", "def_ypc"
      ) %>%
      dplyr::mutate(
        down = as.integer(.data$down),
        ydstogo = as.integer(.data$ydstogo),
        original_ydstogo = .data$ydstogo
      ) %>%
      dplyr::select("index":"ydstogo", "original_ydstogo", dplyr::everything())

    ryoe_vars <- stats::predict(ryoe_model1,
                           as.matrix(rushes %>%
                                       select(yardline_100, quarter_seconds_remaining, half_seconds_remaining,
                                              game_seconds_remaining, qtr, down, goal_to_go, ydstogo, shotgun, no_huddle,
                                              no_score_prob, ep, wp, def_ypc))) %>%
      tibble::as_tibble() %>%
      dplyr::rename(prob = "value") %>%
      dplyr::bind_cols(
        tibble::tibble(
          "xyds_rushed" = rep_len(-5:20, length.out = nrow(rushes)*26),
          "index" = rep(rushes$index, times = rep_len(26, length.out = nrow(rushes))) 
      ) %>%
        dplyr::left_join(join_data, by = "index") %>%
        dplyr::mutate(
          half_seconds_remaining = dplyr::if_else(
            .data$half_seconds_remaining <= 6,
            0,
            .data$half_seconds_remaining - 6
          )
        )
      ) %>%
      dplyr::group_by(.data$index) %>%
      dplyr::mutate(max_loss = dplyr::if_else(.data$yardline_100 < 95, -5L, as.integer(.data$yardline_100 - 99L)),
                    max_gain = dplyr::if_else(.data$yardline_100 > 20, 20L, as.integer(.data$yardline_100)),
                    cum_prob = cumsum(.data$prob),
                    prob = dplyr::case_when(.data$xyds_rushed == .data$max_loss ~ .data$prob,
                                            .data$xyds_rushed == .data$max_gain ~ 1 - dplyr::lag(.data$cum_prob, 1),
                                            TRUE ~ .data$prob),
                    yardline_100 = .data$yardline_100 - .data$xyds_rushed) %>%
      dplyr::filter(.data$xyds_rushed >= .data$max_loss, .data$xyds_rushed <= .data$max_gain) %>%
      dplyr::select(-.data$cum_prob) %>%
      dplyr::summarise(x_rush_yards = sum(.data$prob * .data$xyds_rushed)) %>%
      ungroup()

    pbp <- pbp %>%
      dplyr::left_join(ryoe_vars, by = "index") %>%
      dplyr::select(-.data$index)
    
  }
  
  return(pbp)
  
}
