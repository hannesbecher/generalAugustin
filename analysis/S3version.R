library(here)
source(here("codeBase", "augustin.R"))



gg <- newGame()
str(gg)
gg
gg <- drawNewCard(gg, source="rng")
gg <- drawNewCard(gg, source="rng")
gg <- drawNewCard(gg, source="rng")
gg # seems to call print function for S3 objects

rr <- dieRoll()
rr
rollMatch(gg, rr)
