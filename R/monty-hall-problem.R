#' @title
#'   Create a new Monty Hall Problem Game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#' The Contestant Selects a Door out of the Three.
#'
#' @description
#' `select_door()`randomly select ONE of the three doors and returns the number
#' corresponding to the selection.
#'
#' @details
#' The game setup consists of 3 doors, 2 goats and 1 car. At this stage of the
#' game, the contestant is required to select one of the three doors. What the
#' function does is that it creates a vector of doors numbered 1,2,3
#' and returns one door that is randomly selected out of the three.
#'
#' @param
#' No arguments are used by the function.
#'
#' @return
#' The function returns the number corresponding to the selected door.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' The Host Opens a Goat Door
#'
#' @description
#' `open_goat_door()` return a goat door that is not selected by the contestant
#' in the previous step.
#'
#' @details
#' The host will always open a door with a goat behind it. But the opened door
#' should not be a door that the contestant has already selected. So it must be
#' a door that is not a car and not a current contestant selection.
#'
#' @param
#' This function require two input arguments: The game and the picked door.
#'
#' @return
#' The function returns a door with a goat behind it. However, the returned
#' door should not be the one selected by the contestant.
#'
#' @examples
#' open_goat_door( game, a.pick )
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Change Selected Door.
#' @description
#' `change_door()` represents the contestant's choice to either maintain the
#' initially selected door, or opt for the one that is still closed.
#'
#' @details
#' The contestant is given the option to change from their initial selection to
#' the other door that is still closed.
#'
#' @param
#' This function requires three input arguments. The first one is stay=TRUE or
#' stay=FALSE, depending on whether the contestant will maintain the initial
#' choice or will switch to the closed door.
#'
#' @return
#' The function returns the contestant choice. The output of the change_door()
#' function is a number corresponding to the contestant final selection.
#'
#' @examples
#' change_door( stay=T, opened.door, a.pick )
#' change_door( stay=F, opened.door, a.pick )
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine if the contestant has won the game or not.
#'
#' @description
#' `determine_winner()`returns the outcome of the game and displays if the
#' contestant has won or lost the game.
#'
#' @details
#' This function returns either the word "WIN" or "LOSE", depending on the final
#' pick of the contestant. If the contestant's final choice turns out to be a
#' car, the program will display the word "Won", otherwise, the function will
#' return "LOST".
#'
#' @param
#' This function includes two input arguments: the contestant final pick and the
#' game that has been played.
#'
#' @return
#' The function returns either "WIN" or "LOSE" depending on whether the
#' contestant's final choice contains a car or a goat.
#'
#' @examples
#' determine_winner( final.pick, game )
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play a New Game.
#'
#' @description
#' `play_game()` executes all the steps of the game in order and returns the
#' game outcome.
#'
#' @details
#' This function wraps up all the steps comprised in the game and gathers all
#' the functions into a single play_game().
#'
#' @param
#' No arguments are used by the function.
#'
#' @return
#' the function returns the game results. Therefore, depending on the outcome,
#' it returns either "WIN" or "LOSE".
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}



#' @title
#' Play the Game n Times
#'
#' @description
#' `play_n_games()` executes all the steps of the game in order and returns the
#' game outcome for the n games.
#'
#' @details
#' This function wraps up all the steps comprised in the game and gathers all
#' the functions into a single play_game() that the program is running n times.
#'
#' @param
#' The only input argument that the function uses is n, which is the number of
#' games that the contestant wants to play.
#'
#' @return
#' The function returns the game results. It displays the outcome of the n
#' games.
#'
#' @examples
#' play_n_games( n=100 )
#' play_n_games( n=200 )
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}


