string_to_dataframe <- function(const){
  x <- strsplit(const, "") 
  x[is.na(x)] <- NULL
  matrix(unlist(x), ncol=6, byrow=TRUE)
}

#' Extract the strain name from a vector of strings
#'
#' We assume the strain name follows the pattern 'A/.../<year>' For example:
#' 'A/swine/Oklahoma/A01785514/2018'. We further assume that there are no '|'
#' characters in the sequence (since these are used as separators in FASTA
#' headers).
#'
#' @param x character vector (e.g., of FASTA headers)
#' @return character vector with the same length as x (with possible missing values)
#' @export
extract_strain_name <- function(x){
  sub(".*(A\\/[^|]*\\/\\d\\d\\d\\d).*", "\\1", x)
}

#' Extract tip data from a table based on strain name
#'
#' If there is an existing 'data' slot in the \code{tre} object, then it will
#' be replaced. The tree is expected to have tip labels that contain, but may
#' not be equal to, the strain name. Strain names are extracted from the tip
#' labels using the \code{extract_strain_name} function. After the data are
#' added, the tip labels are set to the strain names and the original labels
#' are stored in a new 'header' column.
#'
#' @param tre treedata object
#' @param dat data.frame containing the column \code{by}.
#' @param by the name of the column containing strain names in \code{dat} table
#' @return treedata object with new 'data' slot
#' @export
add_tip_data_by_strain_name <- function(tre, dat, by="Strain"){
  stopifnot(is.data.frame(dat))
  if(! by %in% names(dat)){
    stop("Expected '", by, "' to be a column name in the input data.frame")
  }
  by.x <- extract_strain_name(tre@phylo$tip.label)
  if(sum(is.na(by.x)) != 0){
    stop("Failed to extract strain name from tree tip labels")
  }

  d <- dat[match(by.x, dat[[by]]), ]
  new_tip_names <- d[[by]]

  # Assert that all strains in the tree are found in the data.  This is
  # required, since the mapping from node to data is based on row number.
  stopifnot(nrow(d) == length(by.x))

  m <- as.data.frame(matrix(
    ncol=ncol(d),
    nrow=tre@phylo$Nnode
  ))
  names(m) <- names(d)
  d <- rbind(d, m)

  d$node <- 1:nrow(d)
  d$header <- c(tre@phylo$tip.label, tre@phylo$node.label)
  tre@phylo$tip.label = new_tip_names 
  tre@data <- tibble::as_data_frame(d)
  tre
}


