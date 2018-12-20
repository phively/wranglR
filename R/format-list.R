### Functions to format and reshape lists

#' Extract named elements from a nested list
#'
#' Given a list of lists, this function goes through each element and copies
#' the named element from each sub-list into a top-level element of a new list.
#' @param list List to be reformatted
#' @param name Quoted name of sub-list item to extract
#' @param name.elements Whether to name the new list elements based on the sub-list
#' item's name and their order in the ; defaults to \code{TRUE}
#' @export
#' @examples
#'
#' # Create a nested list
#' a <- list(letter = 'a', number = 1)
#' b <- list(letter = 'b', number = 2)
#' c <- list(letter = 'c', number = 3)
#' nested <- list(first = a, second = b, third = c)
#'
#' # Extract the letter element
#' ListExtract(nested, 'letter')
#'
#' # Extract the number element
#' ListExtract(nested, 'number')
#'
#' # Extract the number element into an unnamed list
#' ListExtract(nested, 'number', FALSE)
ListExtract <- function(list, name, name.elements = TRUE) {
  list.out <- list()
  for(i in 1:length(list)) {
    list.out[[i]] <- list[[i]][name]
    if (name.elements) {
      names(list.out)[i] <- paste0(name, '_', i)
    }
  }
  return(list.out)
}
