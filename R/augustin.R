



# player class ------------------------------------------------------------

newPlayer <- function(id){
  structure(list(id=id,
                 cards=list()),
            class="Player")
  }


# game class
#' @export
newGame <- function(nPl=3,
                    nDice=5,
                    maxOPen=3,
                    symSet=c("O", "I", "R")){
  structure(list(players=lapply(1:nPl, function(i) newPlayer(i)),
                 finished=FALSE,
                 openCards=list(),
                 stack=list(),#for drawing, might be NULL if cards are continuously generatred
                 round=0,
                 attempt=0,
                 currRoll=NULL,
                 currentPl=1,
                 symbolSet=symSet,
                 nDice=nDice,
                 maxOpen=maxOPen,
                 history=list()
  ), class = "Game")
}

#' @export
print.Game <- function(x, ...){
  # player info
  cat(length(x$players), " players, ",
      x$currentPl, "'s turn. Attempt ", x$attempt, ".\n", 
      sep = "")
  # open cards
  nopen <- length(x$openCards)
  if(nopen == 0)
  cat("No open cards.\n")
  else{
    cat(nopen, "open cards:\n")
    for(i in 1:length(x$openCards)){
      cat(" ", i, ": ", x$openCards[[i]], "\n")
    }
  }
  # current roll
  cat("Current roll:\n  ", x$currRoll, "\n")
  
} 


#' @export
drawNewCard <- function(gg, source="rng"){
  stopifnot(inherits(gg, "Game"))
  stopifnot(length(gg$openCards) < gg$maxOpen)
  stopifnot(source=="rng")
  gg$openCards <- append(gg$openCards,
                         list(sample(gg$symbolSet,
                                     gg$nDice,
                                     replace = T
                                     )
                              )
                         )
  gg
}

#' @export
dieRoll <- function(...){
  UseMethod("dieRoll")
}
  

#' @export
#' @method dieRoll default
dieRoll.default <- function(sset=c("O", "I", "R"), nDice=5, ...){
  structure(sample(sset, size = nDice, replace = TRUE),
            class="dieRoll"
  )
}


#' @export
#' @method dieRoll Game
dieRoll.Game <- function(gm, ...){
  stopifnot(gm$attempt < 3)
  gm$currRoll <- dieRoll()
  gm$attempt <- gm$attempt + 1
  return(gm)
}

#' @export
#' @method print dieRoll
print.dieRoll <- function(x, ...){
  cat("A roll of", length(x), "dice:\n")
  cat(x, "\n")
}

#' @export
reroll <- function(gm, inds){
  # split at commas and trim whitespace
  nums <- trimws(strsplit(inds, ",")[[1]])
  nums <- as.numeric(nums)
  stopifnot(all(nums %in% 1:gm$nDice))
  stopifnot(length(nums) >= 1)
  gm$attempt <- gm$attempt + 1 # increment attempt counter
  gm$currRoll[nums] <- dieRoll(gm$symbolSet, nDice=length(inds))
  gm
}

rollCardMatch <- function(card, roll){
  n <- length(card)
  stopifnot(n == length(roll))
  cc <- card
  matchBool <- rep(FALSE, n)
  # loop over die indices
  for(d in 1:length(roll)){
    ind <- which(cc==roll[d])
    if(length(ind) > 0){
      matchBool[d] <- TRUE
      cc <- cc[-ind[1]]
    }
    
  }
  
  return(matchBool)
}

# this is the player's job, need not expose to user. But needed for automation.
#' @export
rollMatch <- function(game){
  stopifnot(length(game$openCards) > 0)
  stopifnot(!is.null(game$currRoll))
  ll <- lapply(game$openCards, function(cc) rollCardMatch(cc, game$currRoll))
  class(ll) <- "rollMatch"
  ll
}

#' @export
isPerfectMatch <- function(game){
  rm <- rollMatch(game)
  sapply(rm, sum) == game$nDice
}


#' @export
#' @method print rollMatch
print.rollMatch <- function(x, ...){
  cat("Match of", length(x[[1]]), "dice to", length(x), "open cards.\n")
  TF <- lapply(x, function(x) ifelse(x, "T", "F"))
  for(i in TF){
    cat(i, "\n")
  }
}

# card class
list(symbols=list(),
     pic=NULL)
