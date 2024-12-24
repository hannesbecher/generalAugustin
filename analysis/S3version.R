library(here)
#source(here("codeBase", "augustin.R"))



gg <- newGame()
str(gg)
gg
gg <- drawNewCard(gg, source="rng")
gg <- drawNewCard(gg, source="rng")
gg <- drawNewCard(gg, source="rng")
gg # seems to call print function for S3 objects
str(gg)
class(gg)
rollMatch(gg)
sloop::s3_dispatch(dieRoll(gg))
gg <- dieRoll(gg)
class(gg)
gg
gg$currRoll
rollMatch(gg)

isPerfectMatch(gg)
which(isPerfectMatch(gg))
gg1 <- reroll(gg, "3,4")
rollMatch(gg1)
gg2 <- reroll(gg1, "4")
rollMatch(gg2)
isPerfectMatch(gg2)
