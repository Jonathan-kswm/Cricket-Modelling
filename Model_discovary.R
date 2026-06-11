#We are first going to look into conditional wins
#This should answer the question if we see observation A how many of the games 
# in the dataset does A occur in and what is the distribution of wins to losses
# of the games in the dataset with event A

binary_condition <- function(match, event){
  info <- match$info
  switch(event,
         "Won Toss" = as.character(info$toss_winner),
         "Batted First" = if (info$toss_decision == "bat")
                            as.character(info$toss_winner)
                          else
                            setdiff(c(as.character(info$team_1), as.character(info$team_2), as.character(info$toss_winner))),
         "Bowled First" = if(info$toss_decision == "bat")
                            as.character(info$toss_winner)
                          else
                            setdiff(c(as.character(info$team_1), as.character(info$team_2), as.character(info$toss_winner)))
         )
}

cts_condition <- function(match, event, parameter, modifyer, inns) {
  codes <- c("First" = "innings_1",
             "Second" = "innings_2",
             )
  mod_codes <- c("Lesser" = '<=',
                   "Greater" = '>='
                 )
  index <- match$codes[[inns]]
  switch(event,
         "No Balls" = if (param_codes(sum(index$is_noball), parameter) == TRUE)
                        as.character(match$info$),
         "Wides" = Pass,
         "Sixes" = Pass,
         "Run Rate" = Pass,
         "Wickets" = Pass
          )
}

match_outcome <- function(match, team){
  winner <- as.character(match$info$winner)
  if (is.na(winner) || winner == "") return("tied")
  if (winner == team) "won" else "lost"
  
}

conditional <- function(event = c("Won Toss", "Bowled First", "Batted First", "No balls", "Wides", "Sixes", "Run Rate", "Runs", "Wickets"), parameter, modifier = c("Greater", "Lesser"), inns = c("First", "Second")) {
  #codes will tell the function what to do when a condition is called
  codes <- c(
    "Won Toss", #binary
    "Bowled First", #binary
    "Batted First", #binary
    "No balls", #cts
    "Wides", #cts
    "Sixes", #cts
    "Run Rate", #cts
    "Runs", #cts
    "Wickets",#cts
    "Greater", 
    "Lesser"
  )
  
  
  
  #print statement:
  print <- cat("Condition: [something]\n
              Occurences:[something]\n
               ----------------------\n
               Outcomes Won: [something]\n
               Outcomes lost: [something]\n
               Outcomes tied: [something]\n \n
               Win Rate of Condition: [something]")
}

