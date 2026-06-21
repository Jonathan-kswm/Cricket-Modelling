# Cricket-Modelling

## Set-up
I used **<a href="https://cricsheet.org/" target="_blank">Cricsheet</a>** for the ball by ball data. 
The T20 data for international games comes in YAML format with both mens and womens games.

### Parsing the YAML folder
The parsed YAML folder for the womens games are in this repo in the wt20 file. 
If you want to create this file from scratch use the [Data_base_creation.py](https://github.com/Jonathan-kswm/Cricket-Modelling/blob/main/Data_base_creation.py)
file. At the moment it is only set up to create a separate folder of womens games.

### Creating CSV file
There are 1960 games in the wt20 folder at the time of writing the model, and the CSV file is too large to add here.
You need to parse the wt20 folder using [parse_wt20.py](https://github.com/Jonathan-kswm/Cricket-Modelling/blob/main/parse_wt20.py).
This creates a CSV file in your directory containing the ball by ball information for the games.
If there are any errors in the YAML files they are noted in [parse_errors.log](https://github.com/Jonathan-kswm/Cricket-Modelling/blob/main/parse_errors.log)

### Loading the CSV into R
The CSV file needs to be loaded into R using [load_wt20](https://github.com/Jonathan-kswm/Cricket-Modelling/blob/main/load_wt20.R).
This creates two variables. The first variable is wt20 which is a dataframe in the same format as the CSV file.
The CSV file has repeated information throughout a game, the second wt20_data splits wt20 dataframe into a list of things that are 
constant throughout a game and a dataframe that contains just the information that changes between innings and balls.
This is sourced at the start of R files where it is required.

## Model Discovery
[Model_discovery.R](https://github.com/Jonathan-kswm/Cricket-Modelling/blob/main/Model_discovery.R) contains functions to help you discover information about what has happened in the games in the 
dataset.
- valid_inputs
  - this is a dictionary of inputs that are used within the file
- binary_condition
  - this is a function that takes a match and an event in the match (either "Won Toss", "Batted First", or "Bowled First)
  - the function then records who won the toss and the other team, and records who won the game]
- cts_condition
  - this function measures weather a specific event occurs in a specific innings of a game
  - shows the outcome if in an innings an event occurs more or less than a number of times
- match_outcome
  - returns who won the game
- summarize_block
  - returns the variables for conditional
- conditional
  - this is the main function that strings the first section of the code together
  - it takes inputs: event, parameter, modifier, inns, data
    - event is what conditional is looking to see the result of. This is either:
      - "Won Toss"
      - "Batted First"
      - "Bowled First"
    - for binary inputs. For these no other inputs are required. Or for continuous data:
      - "No Balls"
      - "Wides"
      - "Sixes"
      - "Runs"
      - "Wickets"
    - These events requre additional inputs.
    - Parameter
      - this is a number that you are using to measure the cts data with
    - Modifier
      - this is either:
        - "Lesser" (Weak inequality $\leq$)
        - "Greater" (Strict inequality $>$)
      - this is measuring weather the event occurs less or more than the parameter.
    - inns
      - the innings where the event you are measuring occurs
        - "First"
        - "Second"

### Examples
```doctest 
> conditional("Won Toss")
Condition: Won Toss 
 Occurrences: 1960 
 -----------------------------
 Won Toss 
 Occurrences: 1960  ( 100 %) 
 Outcomes Won: 994 
 Outcomes Lost: 923 
 Outcomes Tied: 43  ( 2.2 %) 

 Win Rate (decisive games):  51.9 %

> won_when_won_toss <- conditional("Won Toss")
> won_when_won_toss$win_rate
[1] 51.85185
> conditonal("No Balls", 3, "Greater", "Second")
Condition: No Balls Greater than 3 in the Second innings resulted in the following
 Occurrences: 94 
 -------------------------
 Wins:  42 
 Losses: 51 
 Ties:  1 
 
 Win rate (decisive games): 45.161 %
> won_after_three_no_balls_in_second_innings <- conditional("No Balls", 3, "Greater", "Second")
> won_after_three_no_balls_in_second_innings$win_rate
[1] 45.16129
```

## Modeling Doc
This is used to visualise and explain the results from [Model_discovery.R](https://github.com/Jonathan-kswm/Cricket-Modelling/blob/main/Model_discovery.R)