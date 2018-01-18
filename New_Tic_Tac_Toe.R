library(gridExtra)
library(dplyr)

winning_triples <- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7)
)

#########################################################################
# Displays the numbers of each available position in a table in the console
display <- function(game_board){
  cat("", game_board[1], "|", game_board[2], "|", game_board[3],"\n", "---+---+---\n", 
      game_board[4], "|", game_board[5], "|", game_board[6],"\n", "---+---+---\n", 
      game_board[7], "|", game_board[8], "|", game_board[9])
}

# Creates a gtable showing empty spaces and Xs and Os 
display_table <- function(game_board){
  game_board <- as.character(game_board)
  game_board <- recode(game_board, "1" = " ", "2" = " ", "3" = " ", "4" = " ",
                       "5" = " ", "6" = " ", "7" = " ", "8" = " ", "9" = " ")
  display_board <- as.data.frame(game_board[c(1, 4, 7)])
  display_board <- cbind(display_board, game_board[c(2, 5, 8)])
  display_board <- cbind(display_board, game_board[c(3, 6, 9)])
  tab <- tableGrob(display_board, cols = NULL, rows = NULL)
  grid.arrange(tab)
 }

# 1st player is X 
# This function only allows players to input an available space
# The function loops if an invalid space is input
# The output is the number of the space the player has chosen 
player1_turn <- function(valid_positions, spaces_used){
  cat("\nPlayer X Turn \n")
  response <- readline(prompt = "Player X, please type the number of the position you choose. \n")
  repeat {
    if (any(valid_positions == response) && all(spaces_used != response)) break
    response <- readline(prompt = "Player X, I'm sorry that is not a valid position. \nPlease choose an available number from the board. \n")
  }
  spaces_used <<- c(spaces_used, response)
  return(response)
} 

# For multiplayer mode - 2nd player is O 
# This function only allows players to input an available space
# The function loops if an invalid space is input
# The output is the number of the space the player has chosen 
player2_turn <- function(valid_positions, spaces_used){
  cat("\nPlayer O Turn \n")
  response <- readline(prompt = "Player O, please type the number of the position you choose. \n")
  repeat {
    if (any(valid_positions == response) && all(spaces_used != response)) break
    response <- readline(prompt = "Player O, I'm sorry that is not a valid position. \nPlease choose an available number from the board. \n")
  }
  spaces_used <<- c(spaces_used, response)
  return(response)
}

# For single player mode - Computer is O 
# This function checks which spaces are available
# The function also uses the about_to_win function 
# If the about_to_win function returns NA, the computer will randomly sample available spaces
# The output is the number of the space the computer has chosen 
computer_turn <- function(game_board, valid_positions, spaces_used){
  cat("\nComputer Turn \n")
  Sys.sleep(1)
  if (length(valid_positions) == 9) {
    computer_options <- valid_positions
  } else {
    spaces_used <- as.integer(spaces_used)
    computer_options <- valid_positions[!valid_positions %in% spaces_used]
  }
  if (is.double(about_to_win(game_board, almost.win.pos, winning_triples, computer_options))) {
    response <- about_to_win(game_board, almost.win.pos, winning_triples, computer_options)
    spaces_used <<- c(spaces_used, response)
  } else {
    response <- sample(computer_options, 1)
    spaces_used <<- c(spaces_used, response)
  }
  return(response)
}

# This function replaces the response from the player or computer function with an X or O
update_board <- function(response, game_board, current_player){
    if (current_player == 1) {
      game_board[as.numeric(response)] <- "X"
    } else {
      game_board[as.numeric(response)] <- "O"
    }
  return(game_board)
} 

# This function is for the computer_turn function
# This checks if either the computer or player is about to win 
# The computer will take the position if there are either two Xs or two Os in a row
# If there aren't two Xs or Os in a row, the function returns NA
about_to_win <- function(game_board, almost.win.pos, winning_triples, computer_options){
  dummy_board <- recode(game_board, `1` = "0", `2` = "0", `3` = "0", `4` = "0", `5` = "0", `6` = "0", `7` = "0", `8` = "0", `9` = "0")
  for (i in 1:length(winning_triples)) {
    if (sum(dummy_board[winning_triples[[i]]] == c("0", "O", "O")) == 3) {
      b <- which(dummy_board[winning_triples[[i]]] == "0")
      c <- winning_triples[[i]]
      possible.pos <- c[b]
      if (any(computer_options == possible.pos)) {
        almost.win.pos <- possible.pos
        almost_win <- TRUE
      }
    } else if (sum(dummy_board[winning_triples[[i]]] == c("O", "0", "O")) == 3) {
      b <- which(dummy_board[winning_triples[[i]]] == "0")
      c <- winning_triples[[i]]
      possible.pos <- c[b]
      if (any(computer_options == possible.pos)) {
        almost.win.pos <- possible.pos
        almost_win <- TRUE
      }
    } else if (sum(dummy_board[winning_triples[[i]]] == c("O", "O", "0")) == 3) {
      b <- which(dummy_board[winning_triples[[i]]] == "0")
      c <- winning_triples[[i]]
      possible.pos <- c[b]
      if (any(computer_options == possible.pos)) {
        almost.win.pos <- possible.pos
        almost_win <- TRUE
      }
    } else if (sum(dummy_board[winning_triples[[i]]] == c("0", "X", "X")) == 3) {
      b <- which(dummy_board[winning_triples[[i]]] == "0")
      c <- winning_triples[[i]]
      possible.pos <- c[b]
      if (any(computer_options == possible.pos)) {
        almost.win.pos <- possible.pos
        almost_win <- TRUE
      }
    } else if (sum(dummy_board[winning_triples[[i]]] == c("X", "0", "X")) == 3) {
      b <- which(dummy_board[winning_triples[[i]]] == "0")
      c <- winning_triples[[i]]
      possible.pos <- c[b]
      if (any(computer_options == possible.pos)) {
      almost.win.pos <- possible.pos
      almost_win <- TRUE
      }
    } else if (sum(dummy_board[winning_triples[[i]]] == c("X", "X", "0")) == 3) {
      b <- which(dummy_board[winning_triples[[i]]] == "0")
      c <- winning_triples[[i]]
      possible.pos <- c[b]
      if (any(computer_options == possible.pos)) {
        almost.win.pos <- possible.pos
        almost_win <- TRUE
      }
    } else {
      almost_win <- FALSE
    }
    if (almost_win) break
  }
  if (almost_win) {
    return(almost.win.pos)
  } else {
    return(NA)
  }
}

# This function checks if any player has won 
# If no player has won, it returns FALSE 
check_if_winner <- function(game_board){
  for (i in 1:length(winning_triples)) {
    if (sum(game_board[winning_triples[[i]]] == c("X", "X", "X")) == 3) {
      win = TRUE 
    } else if (sum(game_board[winning_triples[[i]]] == c("O", "O", "O")) == 3) {
      win = TRUE
    } else {
      win = FALSE
    }
    if (win) break
  }
  if (win) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# This function outputs text displaying if a player has won or if the game resulted in a tie.
# The function runs if check_if_winner returns TRUE
display_winner <- function(game_board, winning_triples, computer){
  new <- game_board
  for (i in 1:length(winning_triples)) {
    if (sum(new[winning_triples[[i]]] == c("X", "X", "X")) == 3) {
      who_wins <- 1
    } else if (sum(new[winning_triples[[i]]] == c("O", "O", "O")) == 3) { 
      who_wins <- 0
    } else {
      who_wins <- 2
    }
    if (who_wins == 1 || who_wins == 0 ) break
  } 
  if (computer == FALSE) {
    if (who_wins == 0) {
      cat("\nPlayer O, you have won!")
    } else if (who_wins == 1) {
      cat("\nPlayer X, you have won!")
    } else {
      cat("\nThe game has resulted in a tie.")
    }  
  } else {
    if (who_wins == 0) {
      cat("\nComputer has won.")
    } else if (who_wins == 1) {
      cat("\nPlayer X, you have won!")
    } else {
      cat("\nThe game has resulted in a tie.")
    }
  }
}

# This function alternates players after each turn
# Player 1 is X and player 2 is O
switch_players <- function(current_player){
  if (current_player == 1) {
    return(0)
  }else{
    return(1)
  }
}

# This function corresponds to playing the game
# The player must type "play()" to execute the game
play <- function(){
  almost_win <<- FALSE
  almost.win.pos <<- NA
  comp.pos <<- NA
  current_player <<- 0
  game_board <<- 1:9
  valid_positions <<- 1:9
  spaces_used <<- 0
  
  who_is_playing <- readline(prompt = "Would you like to play the computer or another player? Please type 'computer' or 'player'. \n")
  repeat {
    if (who_is_playing == "computer" || who_is_playing == "player") break
    who_is_playing <- readline(prompt = "Would you like to play the computer or another player? Please type 'computer' or 'player'. \n")
  }
  if (who_is_playing == "computer") computer = TRUE
  else if (who_is_playing == "player") computer = FALSE
  
  if (computer == FALSE) {
    while (check_if_winner(game_board) == FALSE) {
      current_player <- switch_players(current_player)
      display_table(game_board)
      display(game_board)
      
      if (current_player == 1) {
        response <- player1_turn(valid_positions, spaces_used)
      } else {
        response <- player2_turn(valid_positions, spaces_used)
      }
      
      game_board <- update_board(response, game_board, current_player)
    }
    display_table(game_board)
    display(game_board)
    display_winner(game_board, winning_triples, computer)
  } else {
      while (check_if_winner(game_board) == FALSE) {
        current_player <- switch_players(current_player)
        display_table(game_board)
        display(game_board)
        
        if (current_player == 1) {
          response <- player1_turn(valid_positions, spaces_used)
        } else {
          response <- computer_turn(game_board, valid_positions, spaces_used)
        }
        game_board <- update_board(response, game_board, current_player)
      }
    display_table(game_board)
    display(game_board)
    display_winner(game_board, winning_triples, computer)
  }
}


