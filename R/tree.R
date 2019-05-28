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
#' @param filetype string the tree file type (can be nexus, beast, newick or raxml)
#' @return treedata object
#' @export
load_tree <- function(filename, filetype="nexus"){
  label_nodes <- function(x){
    if(is.null(x$node.label)){
      x$node.label <- paste0("n", seq(from=length(x$tip.label) + 1, length.out=x$Nnode))
    }
    x
  }

  # because treeio parses nexus labels in single quotes ...
  unquote_labels <- function(x){
    x$tip.label <- gsub("^'|'$", "", x$tip.label, perl=TRUE)
    x
  }

  treereader <- switch(filetype,
    nexus = treeio::read.nexus,
    beast = treeio::read.beast,
    newick = treeio::read.tree,
    raxml = treeio::read.raxml,
    treeio::read.tree
  )

  tre <- treereader(filename) %>%
    phangorn::midpoint() %>%
    label_nodes %>%
    unquote_labels %>%
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

  # list(tre=tre, by=by, to=to, mrcas=mrcas, factors=factors, depths=depths)

  ordered_clades <- names(sort(depths, decreasing=FALSE))
  # create nested groups of clades
  tre@data[[to]] <- ordered_clades[1]
  for(n in ordered_clades){
    tre@data[[to]][tidytree::offspring(tre@phylo, mrcas[[n]], tiponly=FALSE, self_include=TRUE)] <- n
  }
  tre
}

load_segment_tree <- function(
  segment,
  filename=NULL,
  reference_data=NULL,
  fun.tre=function(x){
    x <- sub("REF|", "", x)
    sub("([^|]+\\|[^|]+).*", "\\1", x, perl=TRUE)
  },
  fun.dat=function(gbid, strain){ paste0(gbid, "|", strain) }
){
  if(is.null(filename)){
    filename <- system.file("app-data", paste0(segment, ".tre"), package="wilbur")
  }
  if(! file.exists(filename)){
    stop("Could not find tree file '", filename, "'")
  }

  tre <- load_tree(filename)

  dat <- if(is.null(reference_data)){
    clean_data(load_current())
  } else {
    reference_data
  }

  tre <- add_data(
    tre,
    dat,
    by=c("H_Genbank", "Strain"), # pass these two columns to fun.dat
    fun.dat=fun.dat,             # paste the values together separated by '|'
    fun.tre=fun.tre              # extract '<genbank>|<strain>'
  )

  group_by_nested_clade(tre=tre, by=segment, to="clade", na.rm=TRUE)
}

#' Make tree with branches colored by clade and names by some column
#'
#' @param segment string The name of a flu segment (e.g., H1, H3, M)
#' @param colname string Either a column in the tree data or a column that will
#' be created by the \code{f} function
#' @param tre optional treedata object. If this argument is not included, the
#' default tree for this segment will be loaded. 
#' @param f function a function of the tree that will create the column \code{colname}
#' @param subset_with function of the tree that specifies which labels to include
#' @param ggpalette A color palette for the tip labels. This may be 1) the name
#' of a segment in the config color list, 2) a literal list of colors, 3) a
#' "gg" object storing a color scale.
#' @param output_nexus_filename filename if given, create a colored nexus file
#' @param ... Additional argument passed to \code{load_segment_tree}
#' @return gg tree object
#' @export
#' @examples
#' # Plot an H1 tree with branches colored by H1 clade and labels colored by
#' # PB2 clade. Do not write labels for strains with missing PB2 annotations.
#' tre <- load_segment_tree("H1")
#' plot_segment_against(
#'  "H1", "PB2", tre=tre,
#'  subset_with=function(x) !is.na(x@data$PB2),
#'  ggpalette = "PB2"
#' )
#'
#' # You can filter out the most common TRIG strain, and just see where
#' # pandemic and TX98 appear:
#' plot_segment_against(
#'  "H1", "PB2", tre=tre,
#'  subset_with=function(x) { !is.na(x@data$PB2) & (x@data$PB2 != "TRIG") },
#'  ggpalette = c("red", "blue")
#' )
plot_segment_against <- function(
  segment,
  colname,
  tre=NULL,
  f=NULL,
  subset_with=function(x){ rep(TRUE, nrow(x@data)) },
  ggpalette=viridis::scale_color_viridis(discrete=TRUE),
  output_nexus_filename=NULL,
  ...
){
  config <- yaml::read_yaml(system.file("config.yaml", package="wilbur"))
  segment_palette <- unname(unlist(config$colors[[segment]]))

  if(all(class(ggpalette) == "character")){
    ggpalette <- if(all(ggpalette %in% names(config$colors))){
      ggplot2::scale_color_manual(values=unname(unlist(config$colors[[ggpalette]])))
    } else {
      ggplot2::scale_color_manual(values=ggpalette)
    }
  }

  if(is.null(tre)){
    tre <- load_segment_tree(segment, ...)
  }
  tre@data$clade <- factor(tre@data$clade, levels=names(config$colors[[segment]]))

  group <- if(!is.null(f)){
    tre@data[[colname]] <- f(tre)
  } else {
    colname
  }

  g <- ggplot2::ggplot(tre, ggplot2::aes(x,y)) +
    ggtree::geom_tree(ggplot2::aes(color = clade)) +
    ggplot2::scale_color_manual(values=segment_palette) +
    ggnewscale::new_scale_color() +
    ggtree::geom_tiplab(aes(subset=subset_with(tre), label=name, color=!!sym(colname)), size=1) +
    ggpalette +
    wilbur_theme()

  if(!is.null(output_nexus_filename)){

    g <- ggplot2::ggplot_build(g)

    cdat1 <- dplyr::left_join(g$data[[1]], tre@data[, c("node", "name")], by="node")
    colmap1 <- cdat1$colour
    names(colmap1) <- cdat1$name
    tre@phylo$node.label <- paste0(tre@phylo$node.label, "[&!color=", colmap1[tre@phylo$node.label], "]")

    cdat3 <- dplyr::left_join(g$data[[3]], tre@data[, c("node", "name")], by="node")
    colmap3 <- cdat3$colour
    names(colmap3) <- cdat3$name
    tre@phylo$tip.label <- paste0(tre@phylo$tip.label, "[&!color=", colmap3[tre@phylo$tip.label], "]")

    raw <- capture.output(ape::write.nexus(tre@phylo, file=""))
    leafX = "(\\d+):([0-9.]+)" # "2544:0.00061"
    nodeX = "(n\\d+)" # n1234
    colorX = "(.&!color=#.......)" # [&!color=#c7aea5]
    pat = glue::glue("{leafX}([^[]*){colorX}")

    # Repeatedly replace uncolored nodes with color of the nearest ancestor
    while(any(grepl(glue::glue("TREE.*{leafX}"), raw))){
      raw <- gsub(pat, replacement="\\1\\4:\\2\\3\\4", raw)
    }

    write(raw, file=output_nexus_filename)
  }

  g$plot
}
