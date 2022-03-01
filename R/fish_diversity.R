#' Describe diversity based on a list of fish species 
#' 
#' Compute the most common fish, the rarest fish, and the total number of fish
#' @param fish_species list of fish (names or code)
#' @return list with the following items 
#' \describe{
#' \item{common}{Most common fish}
#' \item{rarest}{Rarest fish}
#' \item{number}{Total number of fish}
#' }
#' @examples 
#' fish_diversity(c("salmon", "tuna", "steelhead", "cod", "shark", "salmon", "cod"))

# function definition
fish_diversity = function(fish_species) {
  
  fish_species = as.factor(fish_species)
  
  # most common fish
  common_r = which.max(summary(fish_species))
  common = names(summary(fish_species)[common_r])
  
  #common = names(which.max(summary(fish_species)))
  
  # rarest fish
  rarest_r = which.min(summary(fish_species))
  rarest = names(summary(fish_species)[rarest_r])
  #rarest = names(which.min(summary(fish_species)))
  
  # total number of fish
  number = length(fish_species)
  
  # output from function
  return(list(common = common, rarest = rarest, number = number))
}


