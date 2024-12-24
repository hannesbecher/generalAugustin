
symbolSet <- c("O", "I", "R")#c(LETTERS[1:3])
nJuggled <- 5
maxOpen <- 3

# game class
newGame <- function(){
  structure(list(players=list(),
                 finished=FALSE,
                 openCards=list(),
                 stack=list(),#for drawing, might be NULL if cards are continuously generatred
                 round=0
  ), class = "Game")
}

print.Game <- function(x, ...){
  nopen <- length(x$openCards)
  if(nopen == 0)
  cat("No open cards.")
  else{
    cat(nopen, "open cards:\n")
    for(i in 1:length(x$openCards)){
      cat(i, ": ", x$openCards[[i]], "\n")
    }
  }
} 


drawNewCard <- function(gg, source="rng"){
  stopifnot(length(gg$openCards) < maxOpen)
  stopifnot(source=="rng")
  gg$openCards <- append(gg$openCards, list(sample(symbolSet, nJuggled, replace = T)))
  gg
}

dieRoll <- function(sset=symbolSet, nDice=nJuggled){
  structure(sample(sset, size = nDice, replace = TRUE),
            class="dieRoll"
  )
}

print.dieRoll <- function(dr){
  cat("A roll of", length(dr), "dice:\n")
  cat(dr, "\n")
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

rollMatch <- function(game, roll){
  stopifnot(length(game$openCards) > 0)
  ll <- lapply(game$openCards, function(cc) rollCardMatch(cc, roll))
  class(ll) <- "rollMatch"
  ll
}

print.rollMatch <- function(rm){
  cat("Match of", length(rm[[1]]), "dice to", length(rm), "cards.\n")
  TF <- lapply(rm, function(x) ifelse(x, "T", "F"))
  for(i in TF){
    cat(i, "\n")
  }
}

# player class
list(cards=list())

# card class
list(symbols=list(),
     pic=NULL)
