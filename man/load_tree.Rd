% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree.R
\name{load_tree}
\alias{load_tree}
\title{Load and pre-process tree from a filename}
\usage{
load_tree(filename, filetype = "nexus")
}
\arguments{
\item{filename}{path to the tree file}

\item{filetype}{string the tree file type (can be nexus, beast, newick or raxml)}
}
\value{
treedata object
}
\description{
This function will
  1) load the tree from a file
  2) find the midpoint
  3) label the internal nodes (n<node_id>)
  4) create a default data slot that contains the node/tip
     names and the node number
  5) cast the tree as a treedata object
}
