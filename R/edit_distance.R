

edit_dist <- function(s, t){
  adist(s,t)[1,1]
}

edit_sim <- function(s, t){
  1 - edit_dist(s, t)/max(nchar(s), nchar(t))
}
