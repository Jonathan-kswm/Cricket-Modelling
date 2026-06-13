#We are first going to look into conditional wins
#This should answer the question if we see observation A how many of the games
# in the dataset does A occur in and what is the distribution of wins to losses
# of the games in the dataset with event A
source("load_wt20.R") #This ensures that wt20_data is always loaded for this file

valid_inputs <- list(
  "Binary_inputs" = c("Won Toss", "Batted First", "Bowled First"),
  "Cts_inputs" = c("No Balls", "Wides", "Sixes", "Runs", "Wickets"),
  "Modifier" = c("Lesser", "Greater"),
  "inns" = c("First", "Second")
)

binary_condition <- function(match, event) {
  info <- match$info
  toss_winner <- as.character(info$toss_winner)
  #the setdiff function will return the odd one out of the two
  other_team <- setdiff(c(as.character(info$team_1),
                          as.character(info$team_2)),
                        toss_winner)
  #Each branch returns the team that met the condition and, for batted/bowled
  #first, whether it was that team's own decision (i.e. they won the toss and
  #chose it). by_decision is unused for "Won Toss".
  switch(event,
    "Won Toss" = list(team = toss_winner, by_decision = NA),
    "Batted First" = if (info$toss_decision == "bat") {
      list(team = toss_winner, by_decision = TRUE)
    } else {
      list(team = other_team, by_decision = FALSE)
    },
    "Bowled First" = if (info$toss_decision == "field") {
      list(team = toss_winner, by_decision = TRUE)
    } else {
      list(team = other_team, by_decision = FALSE)
    }
  )
}

cts_condition <- function(match, event, parameter, modifier, inns) {
  codes <- c("First" = "innings_1",
    "Second" = "innings_2"
  )
  compare <- function(a, b) if (modifier == "Lesser") a <= b else a >= b
  index <- match[[codes[[inns]]]]
  #Guard against games that never had this innings (abandoned/no-result).
  #Without this, sum(NULL) == 0 would spuriously satisfy a "Lesser" test and
  #the team lookup on a NULL innings would later crash match_outcome.
  if (is.null(index) || nrow(index) == 0) {
    return(NULL)
  }
  #Switch is going to gather the events where the condition is met
  switch(event,
    "No Balls" = if (compare(sum(index$is_noball), parameter)) {
      as.character(index$bowling_team[1]) #bowling team is constant throughout
    } else {
      NULL },
    "Wides" = if (compare(sum(index$is_wide), parameter)) {
      as.character(index$bowling_team[1])
    } else {
      NULL },
    "Sixes" = if (compare(sum(index$runs_batter == 6), parameter)) {
      as.character(index$batting_team[1])
      #batting team is constant
    } else {
      NULL },
    "Runs" = if (compare(tail(index$cumulative_runs, n = 1), parameter)) {
      as.character(index$batting_team[1])
    } else {
      NULL },
    "Wickets" = if (compare(tail(index$cumulative_wickets, n = 1), parameter)) {
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
                        inns = NULL, data = wt20_data, print = TRUE) {
  #catch errors in input
  if (event %in% valid_inputs$Binary_inputs ||
      event %in% valid_inputs$Cts_inputs) {
  } else {
    cat("Error: Invalid Input\n ", event, "is not a valid Event (See Events)")
    return(invisible(NULL))
  }
  if (modifier %in% valid_inputs$Modifier || is.null(modifier)) {
  } else {
    cat("Error: Invalid Input\n", modifier, "is not a valid Parameter",
        "(See Parameters)")
    return(invisible(NULL))
  }
  if (inns %in% valid_inputs$inns || is.null(inns)) {
  } else {
    cat("Error: Invalid Input\n", inns, "is not a valid Innings (See Innings)")
    return(invisible(NULL))
  }
  #Continuous events need a numeric threshold, a modifier and an innings.
  if (event %in% valid_inputs$Cts_inputs) {
    if (is.null(parameter) || !is.numeric(parameter)) {
      cat("Error: Invalid Input\n", event,
          "requires a numeric parameter (threshold)")
      return(invisible(NULL))
    }
    if (is.null(modifier)) {
      cat("Error: Invalid Input\n", event,
          "requires a modifier (Lesser/Greater)")
      return(invisible(NULL))
    }
    if (is.null(inns)) {
      cat("Error: Invalid Input\n", event,
          "requires an innings (First/Second)")
      return(invisible(NULL))
    }
  }

  results   <- character()
  decisions <- logical()
  if (event %in% valid_inputs$Binary_inputs) {
    for (game in data) {
      outcome   <- binary_condition(game, event)
      results   <- c(results, match_outcome(game, outcome$team))
      decisions <- c(decisions, outcome$by_decision)
    }
    total <- length(results)
    if (print) {
      cat("Condition:", event, "\n",
          "Occurrences:", total, "\n")
    }

    #helper to print one block of outcomes for a subset of the games
    print_block <- function(label, keep) {
      res    <- results[keep]
      n      <- length(res)
      wins   <- sum(res == "won")
      losses <- sum(res == "lost")
      ties   <- sum(res == "tied")
      #Base the win rate on decisive games only so ties don't dilute it.
      decisive <- wins + losses
      if (print) {
        cat(" -----------------------------\n",
            label, "\n",
            "Occurrences:", n, " (", 100 * round(n / total, 3), "%)", "\n",
            "Outcomes Won:", wins, "\n",
            "Outcomes Lost:", losses, "\n",
            "Outcomes Tied:", ties, " (", 100 * round(ties / total, 3), "%)",
            "\n\n",
            "Win Rate (decisive games): ",
            if (decisive > 0) round(wins / decisive, 3) * 100 else NA, "% \n\n")
      }
    }

    if (event == "Won Toss") {
      #Won Toss stays as a single block
      print_block(event, rep(TRUE, total))
    } else {
      #split by whether the team chose to bat/bowl first (i.e. won the toss)
      print_block(paste(event, "after winning the toss:"), decisions)
      print_block(paste(event, "after loosing the toss:"), !decisions)
    }
  } else {
    results <- character()
    for (game in data) {
      team <- cts_condition(game, event, parameter, modifier, inns)
      if (!is.null(team)) {
        results <- c(results, match_outcome(game, team))
      }
    }
    total <- length(results)
    wins <- sum(results == "won")
    losses <- sum(results == "lost")
    ties <- sum(results == "tied")
    #Base the win rate on decisive games only so ties don't dilute it.
    decisive <- wins + losses
    winrate <- if (decisive > 0) 100 * (wins / decisive) else NA
    if (print) {
      cat("Condition:", event, modifier, "than", parameter, "in the", inns,
          "innings resulted in the following\n",
          "Occurrences:", total, "\n",
          "-------------------------\n",
          "Wins: ", wins, "\n",
          "Losses:", losses, "\n",
          "Ties: ", ties, "\n",
          "\n",
          "Win rate (decisive games):", round(winrate, 3), "%")
    }
    return(invisible(winrate))
  }
}