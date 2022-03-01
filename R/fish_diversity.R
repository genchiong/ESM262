#' Describe diversity based on a list of fish species 
#' 
#' Compute the most common fish, the rarest fish, and the total number of fish
#' @param fish_species list of fish (names or code)
#' @return list with the following items 
#' \describe{
#' \item{common}{Most common fish}
#' \item{rarest}{Rarest fish}
#' \item{count}{Total number of fish}
#' }
#' @examples 
#' fish_diversity(c("salmon", "tuna", "steelhead", "cod", "shark", "salmon", "cod"))

# function definition
fish_diversity = function(fish_species) {
  
  fish_species = as.factor(fish_species$x)
  
  # most common fish
  common = names(which.max(summary(fish_species)))
  
  # rarest fish
  rarest = names(which.min(summary(fish_species)))
  
  # total number of fish
  count = length(fish_species)
  
  # output from function
  return(list(common = common, rarest = rarest, count = count))
}


