#We are first going to look into conditional wins
#This should answer the question if we see observation A how many of the games
# in the dataset does A occur in and what is the distribution of wins to losses
# of the games in the dataset with event A

valid_inputs <- list(
  "Binary_inputs" = c("Won Toss", "Batted First", "Bowled First"),
  "Cts_inputs" = c("No Balls", "Wides", "Sixes", "Runs", "Wickets"),
  "Parameter" = c("Lesser", "Greater"),
  "inns" = c("First", "Second")
)

binary_condition <- function(match, event) {
  info <- match$info
  switch(event,
    "Won Toss" = as.character(info$toss_winner),
    "Batted First" = if (info$toss_decision == "bat") {
      as.character(info$toss_winner)
    } else {
            #the setdiff function will return the odd one out of the two
            setdiff(c(as.character(info$team_1),
                      as.character(info$team_2)),
                    as.character(info$toss_winner))},
    "Bowled First" = if(info$toss_decision == "bowl") {
      as.character(info$toss_winner)
    } else {
      setdiff(c(as.character(info$team_1),
                as.character(info$team_2)),
              as.character(info$toss_winner))
    }
  )
}

cts_condition <- function(match, event, parameter, modifyer, inns) {
  codes <- c("First" = "innings_1",
    "Second" = "innings_2",
  )
  param_codes <- c("Lesser" = "<=",
    "Greater" = ">="
  )
  index <- match[[codes[[inns]]]]
  #Switch is going to gather the events where the condition is met
  switch(event,
    "No Balls" = if (param_codes(sum(index$is_noball), parameter) == TRUE) {
      as.character(index$bowling_team[1]) #bowling team is constant throughout
    }else {
           NULL },
    "Wides" = if (param_codes(sum(index$is_wide), parameter) == TRUE) {
      as.character(index$bowling_team[1])
    } else {
            NULL },
    "Sixes" = if (param_codes(sum(index$runs_batter == 6),
                              parameter) == TRUE) {
      as.character(index$batting_team[1])
      #batting team is constant
    } else {
            NULL },
    "Runs" = if (param_codes(tail(index$cumulative_runs, n = 1),
                             parameter) == TRUE) {
      as.character(index$batting_team[1])
    }else {
           NULL },
    "Wickets" = if (param_codes(tail(index$cumulative_wickets, n = 1),
                                parameter) == TRUE) {
      as.character(index$bowling_team[1])
    } else {
      NULL
    }
  )
}

match_outcome <- function(match, team){
  winner <- as.character(match$info$winner)
  if (is.na(winner) || winner == "") {
    return("tied")
  }
  if (winner == team) {
    "won"
  } else{
    "lost"
  }
}

conditional <- function(event, parameter = NULL, modifier = NULL,
                        inns = NULL, data = wt20_data) {
  #catch errors in input
  if (event %in% valid_inputs$Binary_inputs ||
      event %in% valid_inputs$Cts_inputs) {
  } else {
    cat("Error: Invalid Input\n ,event, is not a valid Event (See Events)")
    return(invisible(NULL))
  }
  if (parameter %in% valid_inputs$Parameter || is.null(parameter)) {
  } else {
    cat("Error: Invalid Input\n, parameter, is not a valid Parameter",
        "(See Parameters)")
    return(invisible(NULL))
  }
  if (inns %in% valid_inputs$inns || is.null(inns)) {
  } else {
    cat("Error: Invalid Input\n, inns, is not a valid Innings (See Innings)")
    return(invisible(NULL))
  }

  results <- list()
  if (event %in% valid_inputs$Binary_inputs){
    for (game in data){
      results <- c(results, match_outcome(game, binary_condition(game, event)))
    }
  } else {
    next
  }

  #print statement:
  wins   <- sum(results == "won")
  losses <- sum(results == "lost")
  ties   <- sum(results == "tied")
  total  <- length(results)
  cat("Condition:", event, "\n",
      "Occurrences:", total, "\n",
      "----------------------\n",
      "Outcomes Won:", wins, "\n",
      "Outcomes Lost:", losses, "\n",
      "Outcomes Tied:", ties, "\n\n",
      "Win Rate of Condition:",
      if (total > 0) round(wins / total, 3) else NA, "\n")
}
