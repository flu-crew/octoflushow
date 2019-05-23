string_to_dataframe <- function(const){
  x <- strsplit(const, "") 
  x[is.na(x)] <- NULL
  matrix(unlist(x), ncol=6, byrow=TRUE)
}

#' Load and pre-process tree from a filename
#' 
#' This function will
#'   1) load the tree from a file
#'   2) find the midpoint
#'   3) label the internal nodes (n<node_id>)
#'   4) create a default data slot that contains the node/tip
#'      names and the node number
#'   5) cast the tree as a treedata object
#'
#' @param filename path to the tree file
#' @return treedata object
#' @export
load_tree <- function(filename){
  label_nodes <- function(tre){
    if(is.null(tre$node.label)){
      tre$node.label <- paste0("n", seq(from=length(tre$tip.label) + 1, length.out=tre$Nnode))
    }
    tre
  }
  tre <- treeio::read.tree(filename) %>%
    phangorn::midpoint() %>%
    label_nodes %>%
    treeio::as.treedata()
  all_names <- c(tre@phylo$tip.label, tre@phylo$node.label)
  tre@data <- tibble::data_frame(
    node = 1:length(all_names),
    name = all_names
  )
  tre
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

#' Update table data based on string matches
#'
#' @param tre treedata object
#' @param dat data.frame containing the column \code{by}.
#' @param by character vector of names from the \code{dat} that are passed to \code{fun.dat}.
#' matches to substrings of 0 or 1 node or tip labels in the input tree. 
#' @param fun function for transforming tree names and data columns. The
#' default is \code{paste0}. For the tree, this default simply returns the
#' original names (i.e., is equivalent to the identity function). For
#' \code{dat}, which is a data.frame, this will paste together elements from
#' each column specified in \code{by}.
#' @param fun.dat function of the \code{by} column that creates a new character vector of equal length (default = \code{fun})
#' @param fun.tre function of the tre tip labels that creates a new character vector of equal length (default = \code{fun})
#' @return treedata object with an updated 'data' slot 
#' @export
add_data <- function(
  tre,
  dat,
  by,
  fun=paste0,
  fun.dat=fun,
  fun.tre=fun
){
  stopifnot(is.data.frame(dat))
  if(! any(by %in% names(dat))){
    stop("Expected '", by, "' to be a column name in the input data.frame")
  }

  by.x <- c(tre@phylo$tip.label, tre@phylo$node.label)
  # generate internal labels for unlabeled nodes
  by.x[is.na(by.x)] <- paste0("_noname_", 1:length(by.x))[is.na(by.x)]
  
  if(nrow(tre@data) != length(by.x)){
    stop("The treedata object is expected to have an initialized data slot")
  }

  dat$node <- match(do.call(fun.dat, unname(as.list(dat[, by]))), fun.tre(by.x))
  treedat <- dplyr::left_join(tre@data, dat, by="node")
  if(all("node" %in% names(tre@data))){
    treedat <- dplyr::left_join(treedat, tre@data, by="node")
  }
  tre@data <- treedat

  return(tre)
}

#' Create a new column in the tree data that groups
#' 
#' @param tre treedata object
#' @param by string a column in the treedata@data slot
#' @param to string the name of the new column that will be created
#' @param na.rm logical should NA's be ignored in the \code{by} column?
#' @return tre treedata object
#' @export
group_by_nested_clade <- function(tre, by, to, na.rm=TRUE){
  # get the unique, non-NA clade names
  factors <- if(na.rm){
    as.character(unique(tre@data[[by]][! is.na(tre@data[[by]])]))
  } else {
    as.character(unique(tre@data[[by]]))
  }
  # get the node ID of the most recent common ancestor for all members of each clade
  mrcas <- lapply(factors, function(x){ggtree::MRCA(tre, tre@data$node[which(tre@data[[by]] == x)])})
  names(mrcas) <- factors
  # get clade names ordered by decreasing depth in the tree
  depths <- sapply(factors, function(x){length(tidytree::ancestor(tre@phylo, mrcas[[x]]))})
  names(depths) <- factors
  ordered_clades <- names(sort(depths, decreasing=FALSE))
  # create nested groups of clades
  tre@data[[to]] <- ordered_clades[1]
  for(n in ordered_clades){
    tre@data[[to]][tidytree::offspring(tre@phylo, mrcas[[n]], tiponly=FALSE, self_include=TRUE)] <- n
  }
  tre
}

plot_tree_2color <- function(){
  
}

#' Default tree theme for wilbur figures
#' 
#' @return ggplot2 theme
wilbur_theme <- function(){
  ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )
}