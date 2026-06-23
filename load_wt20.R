# load_wt20.R
# Load the ball-by-ball Women's T20I dataset produced by parse_wt20.py,
# enforce sensible column types, summarise, and run a sanity check that
# no innings exceeds 120 legal deliveries.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

wt20 <- read_csv(
  "wt20_ball_by_ball.csv",
  col_types = cols(
    match_id           = col_factor(),
    date               = col_date(format = "%Y-%m-%d"),
    venue              = col_factor(),
    city               = col_factor(),
    match_type         = col_factor(),
    gender             = col_factor(),
    team_1             = col_factor(),
    team_2             = col_factor(),
    toss_winner        = col_factor(),
    toss_decision      = col_factor(),
    winner             = col_factor(),
    win_by_runs        = col_integer(),
    win_by_wickets     = col_integer(),
    player_of_match    = col_factor(),
    innings            = col_integer(),
    batting_team       = col_factor(),
    bowling_team       = col_factor(),
    over               = col_integer(),
    ball               = col_integer(),
    global_ball        = col_integer(),
    batter             = col_factor(),
    non_striker        = col_factor(),
    bowler             = col_factor(),
    runs_batter        = col_integer(),
    runs_extras        = col_integer(),
    runs_total         = col_integer(),
    extras_type        = col_factor(),
    is_wide            = col_integer(),
    is_noball          = col_integer(),
    is_legal_delivery  = col_integer(),
    cumulative_runs    = col_integer(),
    cumulative_wickets = col_integer(),
    balls_bowled       = col_integer(),
    balls_remaining    = col_integer(),
    current_run_rate   = col_double(),
    target             = col_integer(),
    runs_required      = col_integer(),
    required_run_rate  = col_double(),
    is_wicket          = col_integer(),
    wicket_kind        = col_factor(),
    player_out         = col_factor(),
    fielder            = col_factor()
  )
)

# Sanity check: no regular innings (1 or 2) should have more than 120
# legal deliveries. Super overs (innings 3) are excluded.
violations <- wt20 %>%
  filter(innings %in% c(1, 2)) %>%
  group_by(match_id, innings) %>%
  summarise(legal_deliveries = sum(is_legal_delivery), .groups = "drop") %>%
  filter(legal_deliveries > 120)

if (nrow(violations) == 0) {
  cat("\nNo innings exceeds 120 legal deliveries.\n")
} else {
  cat("\nInnings exceeding 120 legal deliveries:\n")
  print(violations)
}

#We want to add two functions; one to split game by game,
#one to split over by over
#Static data (same throughout the game)
# match_id, Date, Venue, City, match_type, Gender,
#Team_1, Team_2, Toss_winner, Toss_decision
# Winner, Win_by_runs, Win_by_wickets, Player_of_match

game_split <- function(data) {
  games <- split(data, data$match_id, drop = TRUE)
  games <- lapply(games, function(match) {
    static <- list(
      game_id = as.character(match$match_id[1]),
      info = match[1, c(
        "date", "venue", "city", "match_type", "gender",
        "team_1", "team_2", "toss_winner", "toss_decision",
        "winner", "win_by_runs", "win_by_wickets",
        "player_of_match"
      )]
    )
    inns <- split(match, match$innings)
    names(inns) <- paste0("innings_", names(inns))
    c(static, inns)
  })
  return(games)
}

wt20_data <- game_split(wt20)

collect_teams <- function(data = wt20) {
  #Re-organise the data by team. Each team maps to a game_split() result
  #containing every match that team played in, on either side. A team can
  #appear as team_1 or team_2, so the set of teams is the union of both
  #columns and each match is selected with team_1 == team | team_2 == team.
  teams <- sort(unique(c(as.character(data$team_1),
                         as.character(data$team_2))))
  team_games <- lapply(teams, function(team) {
    matches <- dplyr::filter(data, team_1 == team | team_2 == team)
    game_split(matches)
  })
  names(team_games) <- teams
  team_games
}

teams_data <- collect_teams(wt20)
