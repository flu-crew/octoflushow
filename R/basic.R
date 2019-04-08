#' Load the current swine surveillance data
#'
#' @export
load_current <- function(){
  infile <- system.file("app-data", "A0_Master_List.xlsx", package="wilbur")
  sheet <- "Data"
  my.data <- readxl::read_excel(infile, sheet = sheet, col_types = "text")

  # ===== Date type, only run once
  my.data$Date <- my.data$Date %>%
    as.numeric %>%
    zoo::as.Date(origin = "1899-12-30")

  # ===== Standardize Names
  fixH1names <- function(h1){
    replace(h1, h1=="pdm", "pandemic")
  }

  fixH3names<-function(h3){
    h3 %>%
      replace(., .=="human-like_2010" | .=="human-like_2010.1" | .=="Human-like_2010.1", "2010.1") %>%
      replace(., .=="human-like_2016" | .=="human-like_2010.2" | .=="Human-like_2010.2", "2010.2") %>%
      replace(., .=="Other-human", "other-human") %>%
      gsub("Cluster_", "", .) %>%
      gsub("IV", "IV-", .) %>%
      gsub("--", "-", .) %>%
      replace(., .=="IV-", "IV")
  }

  fixN1names<-function(n1){
    n1 %>%
      replace(., .=="pandemic", "Pandemic") %>%
      replace(., .=="classical", "Classical")
  }

  fixN2names<-function(n2){
    n2 %>% 
      replace(., .=="02" | .=="02A_1" | .=="02A_2" | .=="02B_1" | .=="02B_2" |.=="2002A" | .=="2002B", "2002") %>%
      replace(., .=="98" | .=="98A_1" | .=="98A_2" | .=="98B_1" | .=="98B_2" |.=="1998A" | .=="1998B", "1998") %>%
      replace(., .=="02" | .=="02A1" | .=="02A2" | .=="02B1" | .=="02B2" |.=="2002A" | .=="2002B", "2002") %>%
      replace(., .=="98" | .=="98A1" | .=="98A2" | .=="98B1" | .=="98B2" |.=="1998A" | .=="1998B", "1998")
  }

  fixIGnames<-function(ig){
    replace(ig, ig=="VTX98", "TX98")
  }

  my.data$H1 <- my.data$H1 %>% fixH1names(.)
  my.data$H3 <- my.data$H3 %>% fixH3names(.)
  my.data$N1 <- my.data$N1 %>% fixN1names(.)
  my.data$N2 <- my.data$N2 %>% fixN2names(.)

  my.data$PB2 <- my.data$PB2 %>% fixIGnames(.)
  my.data$PB1 <- my.data$PB1 %>% fixIGnames(.)
  my.data$PA <- my.data$PA %>% fixIGnames(.)
  my.data$NP <- my.data$NP %>% fixIGnames(.)
  my.data$M <- my.data$M %>% fixIGnames(.)
  my.data$NS <- my.data$NS %>% fixIGnames(.)
  my.data
}

#' Make a basic barplot of the data
#'
#' @param d data.frame swine surveillance data (i.e., output or \code{load_current})
#' @param segment The influenze segment that will be plotted [H1,H3,N1,N3,PB2,PB1,PA,NP,M,NS]
#' @param byMonth Should be plot be by month (if FALSE, then it will be by day)
#' @export
#' @return ggplot object
plot_basic <- function(d, segment="H1", byMonth=TRUE){
  # ===== Basic Counting and plotting
  # ==== Colors and Order of names
  # Stacked bar chart for phylocluster by month

  # NOTE: currently the subtype palette is not used
  subtype_palette <- c("#d7191c","#fdae61","#ffffbf","#abd9e9","#2C7FB8","#e0f3f8","#2c7bb6","#2c7bb6","#2c7bb6","#2c7bb6","#2c7bb6")
  internalgene_palette <- c("TRIG"="#C2A5CF","PDM"="#762A83","unknown"="#800000","TX98"="#C7EAE5")
  segment_palette <- switch(
    segment,
    H1 = c("alpha"="#543005", "beta"="#8c510a", "gamma"="#80cdc1",
           "gamma2"="#35978f", "delta1"="#bf812d", "delta1a"="#dfc27d",
           "delta1b"="#f6e8c3", "delta2"="#c7eae5", "pandemic"="#01665e",
           "gamma2-beta-like"="#60b0f4", "No_data"="#EDF8B1"),
    H3 = c("2010.1"="#999999", "2010.2"="#996633", "other-human"="#F90005",
           "I"="#542788", "II"="#d73027", "III"="#2166AC", "IV"="#E69F00",
           "IV-A"="#56B4E9", "IV-B"="#009E73", "IV-C"="#F0E442",
           "IV-D"="#0072B2", "IV-E"="#D55E00", "IV-F"="#CC79A7",
           "No_data"="#EDF8B1"),
    N1 = c("Classical"="#FEB24C", "Pandemic"="#F03B20",
           "Human_seasonal"="#BD2D2B", "MN99"="#91BFDB", "No_data"="#EDF881"),
    N2 = c("1998"="#7FCDBB", "98"="#7FCDBB", "98A" ="#78c8b4",
           "98A1"="#1b7837", "98A2"="#9970ab", "98B"="#82d2be",
           "98B1"="#5aae61", "98B2"="#762a83", "2002"="#2C7FB8",
           "02"="#2C7FB8", "02A" ="#2878b4", "02A1"="#d6604d",
           "02A2"="#b2182b", "02B"="#3282be", "02B1"="#d1e5f0",
           "02B2"="#fddbc7", "Human-like"="#1F38FF", "Human_N2"="#969696",
           "Human-to-swine_2016"="#1F38FF", "Human-like_2010.2"="#996633",
           "2016"="#996633", "TX98"="#C7EAE5", "No_data"="#EDF8B1"),
    PB2 = internalgene_palette,
    PB1 = internalgene_palette,
    PA = internalgene_palette,
    NP = internalgene_palette,
    M = internalgene_palette,
    NS = internalgene_palette
  )

  # Die if the segment is not correctly named
  stopifnot(segment %in% c("H1", "H3", "N1", "N2", "PB2", "PB1", "PA", "NP", "M", "NS"))
  # The segment must be a column name in the data
  stopifnot(segment %in% names(d))
  # The input data must have a Date column
  stopifnot("Date" %in% names(d))

  if(byMonth){
    lubridate::day(d$Date) <- 1
  }

  # Get user-provided segment
  m <- d[, c("Date", segment)]
  names(m)[2] <- "Segment"
  m <- subset(m, !is.na(Segment))
  # e.g., ("unknown,TRIG" -> "unknown")
  m$Segment <- gsub(",.*", "", m$Segment)

  # --- segment-specific prep -----------------------
  if(segment == "H1"){
    m$Segment <- factor(m$Segment)
  }
  else if(segment == "H3"){
    H3_order <- c("2010.1","2010.2","other-human","I","II","III","IV","IV-A","IV-B","IV-C","IV-D","IV-E","IV-F","No_data")
    m$Segment <- factor(m$Segment, ordered=TRUE, levels=H3_order)
  }
  else if(segment == "N1"){
    N1_order <- c("Classical","Pandemic","Human_seasonal","MN99","No_data")
    m$Segment <- factor(m$Segment, ordered=TRUE, levels=N1_order)
  }
  # -------------------------------------------------

  summary.data <- m %>%
    dplyr::group_by(Date, Segment) %>%
    dplyr::summarise(n=dplyr::n(),
                     tot=nrow(.),
                     per=round(dplyr::n() / nrow(.)*100, digits = 2))  # Calculate summary statistics

  summary.data$Date <- as.POSIXct(summary.data$Date)

  title_suffix <- if(byMonth){ "month" } else { "day" } 

  unscaled <- ggplot2::ggplot(summary.data, ggplot2::aes(x=Date, y=n, fill=Segment)) +
    ggplot2::geom_bar(stat="identity", position="stack") +
    # ggplot2::theme_bw() +
    ggplot2::ggtitle(paste(segment, "phylogenetic-clades by", title_suffix)) +
    ggplot2::scale_fill_manual(values=segment_palette) +
    # ggplot2::scale_x_datetime(labels = scales::date_format("%m-%Y"), breaks = scales::date_breaks("months")) +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle=90, size=10, vjust=0.5),
      legend.title    = element_blank(),
      legend.position = "none", # no legend since it will be the same as the scaled fig
      strip.text      = ggplot2::element_text(size=12),
    ) +
    labs(x="", y="Number of Swine Isolates") # also fix the x and y axis labels...

  scaled <- ggplot(summary.data, aes(x=Date, y=n, fill=Segment)) +
    geom_bar(stat="identity", position="fill") +    # duplicate, and change "stack" to "fill"
    # ggplot2::theme_bw() +
    ggplot2::ggtitle(paste(segment, "phylogenetic-clades by", title_suffix)) +
    # ggplot2::scale_x_datetime(labels = scales::date_format("%m-%Y"), breaks = scales::date_breaks("months")) +
    scale_fill_manual(values=segment_palette)+
    theme(
      axis.text.x     = element_text(angle=90, size=10,vjust=0.5),
      legend.title    = element_blank(),
      legend.position = "bottom",
      strip.text      = element_text(size=12)
    ) +
    labs(x="", y="Swine Isolates by %") # also fix the x and y axis labels...

  legend <- cowplot::get_legend(scaled)
  scaled <- scaled + theme(legend.position = "none")

  cowplot::plot_grid(unscaled, scaled, legend, rel_heights=c(1,1,0.3), ncol=1, labels=NULL)
}
