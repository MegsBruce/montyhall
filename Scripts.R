#' @title
#'   Create a new Monty Hall Problem game.
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
#' The contestant picks one of three available doors.
#'
#' @description
#' a.pick allows contestant's to pick a door (at random)
#'
#' @details
#' This function is the part of the game where the contestant
#' chooses what door they would like.
#'
#'
#' @param
#' This function has 1 arguements:
#' -the input of a.pcik through the select_door fuction
#'
#'
#'
#' @return
#' The function returs a lenght of 1 numeric vector (doors #1, #2, or #3)
#'
#'
#' @examples
#' a.pick == 1
#'
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' The host reveals a goat door.
#'
#' @description
#' open_goat_door allows the host selects one of two remaining doors, but it will always be a
#' goat door.
#'
#' @details
#' This function replicates the part of the game where the host picks one of
#' the two doors the contestant did not choose. However, this door has to be a
#' goat door. The host will not reveal a car.
#'
#' @param
#' This function has 2 argumentens
#' If the contestant picks a car, the host does not pick a car.
#' If the contestant picks a goat, the host picks a goat door but not the same
#' one the contestant does.
#'
#' @return
#' This function returns 1 numeric vector that represents the host's choice. It
#' is dependent on what door the contestant picked.
#'
#' @examples
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
#' The contestant stays with their door choice or switches.
#'
#' @description
#' change_door lets the contestant will decide if they are going to stay with
#' their original choice or if they are going to switch their door pick to the
#' one that hasn't yet been opened.
#'
#' @details
#' This function replicates the part of the game where the contestant
#' is allowed to stay with their initial pick or switch their pick to
#' the unopened door.
#'
#' @param
#' This function has three different arguments: stay == True or stay == False,
#' opened.door, which is inputted by the open_goat_door function, and
#' a.pick, which is inputted by the select_door function.
#'
#' @return
#' This function returns a length of one vector (numeric) that represents the
#' contestants final door choice.
#'
#' @examples
#' change_door(stay==F, opened.door, a.pick) or
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
#' The contestant wins or looses.
#'
#' @description
#' If the contestant's final choice is a car, they win.
#' If the contestant's final choice is a goat, they lose.
#'
#' @details
#' This function replicates the part of the game where the contestant
#' finds out if they have one the game or lost the game.
#'
#'
#' @param
#' This functon has two arugments:
#' If the final pick is a car, the contestant wins: (if game[ final.pick ]
#'  == "car" ).
#'  If the final pick is a goat, the contestant loses:  if( game[ final.pick ]
#'   == "goat" )
#'
#'
#' @return
#' This function returns one vector: either win or lose
#'
#'
#' @examples
#'   "WIN"
#'
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
#' Monty Hall Game (start to finish)
#'
#' @description
#' This combines the previous steps into a singe function.
#'
#' @details
#' This function replicates the full game.
#'
#' @param
#' There are multiple arguments for this function, as it is made up of the
#' previous steps.
#' Selecting a door, choosing a goat door, the contestant choosing to switch
#' or stay, and the contestant winning or losing
#'
#' @return
#' This function returns one vector: win or lose
#'
#' @examples
#'   LOSE
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
#' Running the game on a loop.
#'
#' @description
#' This function plays the Monty Hall game on a loop, 100 times.
#'
#' @details
#' This function shows the results (percentages) of winning and losing over
#' the 100 games.
#'
#' @param
#' This function has hour arguments.
#' The loop count, the iterator, and the results table.
#'
#' @return
#' This function returns
#'
#' @examples
#' An examle would be a WIN LOSE table with percentages.
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
#' @title
#'   Create a new Monty Hall Problem game.
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
#' The contestant picks one of three available doors.
#'
#' @description
#' a.pick allows contestant's to pick a door (at random)
#'
#' @details
#' This function is the part of the game where the contestant
#' chooses what door they would like.
#'
#'
#' @param
#' This function has 1 arguements:
#' -the input of a.pcik through the select_door fuction
#'
#'
#'
#' @return
#' The function returs a lenght of 1 numeric vector (doors #1, #2, or #3)
#'
#'
#' @examples
#' a.pick == 1
#'
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' The host reveals a goat door.
#'
#' @description
#' open_goat_door allows the host selects one of two remaining doors, but it will always be a
#' goat door.
#'
#' @details
#' This function replicates the part of the game where the host picks one of
#' the two doors the contestant did not choose. However, this door has to be a
#' goat door. The host will not reveal a car.
#'
#' @param
#' This function has 2 argumentens
#' If the contestant picks a car, the host does not pick a car.
#' If the contestant picks a goat, the host picks a goat door but not the same
#' one the contestant does.
#'
#' @return
#' This function returns 1 numeric vector that represents the host's choice. It
#' is dependent on what door the contestant picked.
#'
#' @examples
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
#' The contestant stays with their door choice or switches.
#'
#' @description
#' change_door lets the contestant will decide if they are going to stay with
#' their original choice or if they are going to switch their door pick to the
#' one that hasn't yet been opened.
#'
#' @details
#' This function replicates the part of the game where the contestant
#' is allowed to stay with their initial pick or switch their pick to
#' the unopened door.
#'
#' @param
#' This function has three different arguments: stay == True or stay == False,
#' opened.door, which is inputted by the open_goat_door function, and
#' a.pick, which is inputted by the select_door function.
#'
#' @return
#' This function returns a length of one vector (numeric) that represents the
#' contestants final door choice.
#'
#' @examples
#' change_door(stay==F, opened.door, a.pick) or
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
#' The contestant wins or looses.
#'
#' @description
#' If the contestant's final choice is a car, they win.
#' If the contestant's final choice is a goat, they lose.
#'
#' @details
#' This function replicates the part of the game where the contestant
#' finds out if they have one the game or lost the game.
#'
#'
#' @param
#' This functon has two arugments:
#' If the final pick is a car, the contestant wins: (if game[ final.pick ]
#'  == "car" ).
#'  If the final pick is a goat, the contestant loses:  if( game[ final.pick ]
#'   == "goat" )
#'
#'
#' @return
#' This function returns one vector: either win or lose
#'
#'
#' @examples
#'   "WIN"
#'
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
#' Monty Hall Game (start to finish)
#'
#' @description
#' This combines the previous steps into a singe function.
#'
#' @details
#' This function replicates the full game.
#'
#' @param
#' There are multiple arguments for this function, as it is made up of the
#' previous steps.
#' Selecting a door, choosing a goat door, the contestant choosing to switch
#' or stay, and the contestant winning or losing
#'
#' @return
#' This function returns one vector: win or lose
#'
#' @examples
#'   LOSE
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
#' Running the game on a loop.
#'
#' @description
#' This function plays the Monty Hall game on a loop, 100 times.
#'
#' @details
#' This function shows the results (percentages) of winning and losing over
#' the 100 games.
#'
#' @param
#' This function has hour arguments.
#' The loop count, the iterator, and the results table.
#'
#' @return
#' This function returns
#'
#' @examples
#' An examle would be a WIN LOSE table with percentages.
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

setwd( "montyhall" )
getwd()   # 'C:/Users/username/Documents/montyhall'
devtools::document()

setwd( ".." )
devtools::install( "montyhall" )
library( montyhall )
create_game()
