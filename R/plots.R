# ==== Global Variables
config <- yaml::read_yaml(system.file("config.yaml", package="octoflushow"))

#' Return the segment palette for the plots
#' @param seg H1, H3, N1, N2, PB2, PB1, PA, NP, M, NS
#' @return Return a named vector of hex colors
#' @export
get_palette <- function(seg="H1"){
  return(unlist(config$colors[[seg]]))
}

order_data_factors <- function(d, config){
  if("H1" %in% names(d)) {d$H1 <- factor(d$H1, ordered=TRUE, levels=names(config$colors$H1)) %>% droplevels(.) }
  if("H3" %in% names(d)) {d$H3 <- factor(d$H3, ordered=TRUE, levels=names(config$colors$H3)) %>% droplevels(.) }
  if("N1" %in% names(d)) {d$N1 <- factor(d$N1, ordered=TRUE, levels=names(config$colors$N1)) %>% droplevels(.) }
  d
}

#' Make a basic barplot of the data
#'
#' @param d data.frame swine surveillance data (i.e., output or \code{load_current})
#' @param segment The influenze segment that will be plotted [H1,H3,N1,N3,PB2,PB1,PA,NP,M,NS]
#' @param floorDateBy An argument that is passed to
#' \code{lubridate::floor_date} specifying how to group dates (e.g., "month",
#' "week", "3 months", "year")
#' @param dateFormat A formatting string for the x-axis date labels (e.g. "%Y" or "%m-%Y")
#' @param dateBreaks Date to break by (e.g., "years", or "months")
#' @export
#' @return ggplot object
plot_basic <- function(d, segment="H1", floorDateBy="month", dateFormat="%Y", dateBreaks="years", xlim=NULL){
  # ===== Basic Counting and plotting
  # ==== Colors and Order of names
  # Stacked bar chart for phylocluster by month

  # Die if the segment is not correctly named
  stopifnot(segment %in% c("H1", "H3", "N1", "N2", "PB2", "PB1", "PA", "NP", "M", "NS"))
  # The segment must be a column name in the data
  stopifnot(segment %in% names(d))
  # The input data must have a Date column
  stopifnot("Date" %in% names(d))

  segment_palette <- config$color[[segment]]

  d <- order_data_factors(d, config)

  if(floorDateBy=="quarter"){
    floorDateBy="3 months"
  }

  # cd = clean data
  cd <- d %>%
    dplyr::select(c("Date",segment)) %>% {
      names(.) = c("Date","Segment")
      .
    } %>% subset(., !is.na(Segment)) %>%
    dplyr::filter(Segment != "mixed") %>% 
    dplyr::filter(! is.na(Segment)) %>%
    dplyr::mutate(
      Date = lubridate::as_datetime(lubridate::floor_date(Date, floorDateBy))
    )

  if (is.null(xlim)){
    xlim <- lubridate::as_datetime(c(min(d$Date), max(d$Date)))
  }

  summary_data <- cd %>% # m
    dplyr::group_by(Date, Segment) %>%
    dplyr::summarise(n=dplyr::n(),
                     tot=nrow(.),
                     per=round(dplyr::n() / nrow(.)*100, digits = 2)) %>% # Calculate summary statistics
    dplyr::ungroup(.) %>% dplyr::mutate(
      Date=lubridate::as_datetime(Date) %>% as.POSIXct(., format="%Y-%m-%d")
    )

  for (clade in unique(summary_data$Segment)){
    if(! (clade %in% names(segment_palette))){
      stop(glue::glue("No color found for {clade} in {segment} color config. You need to either update the config.yaml file or the basic.R::clean_data function.")) 
    }
  }

  plotit <- function(position, ylab){
    ggplot2::ggplot(summary_data, ggplot2::aes(x=Date, y=n, fill=Segment)) +
    ggplot2::geom_bar(stat="identity", position=position) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(paste(segment, "phylogenetic-clades by", floorDateBy)) +
      ggplot2::scale_fill_manual(values=segment_palette) +
      ggplot2::scale_x_datetime(labels = scales::date_format(dateFormat), breaks = scales::date_breaks(dateBreaks)) +
      ggplot2::coord_cartesian(xlim=xlim) +
      ggplot2::theme(
        axis.text.x     = ggplot2::element_text(angle=90, size=10, vjust=0.5),
        legend.title    = ggplot2::element_blank(),
        legend.position = "bottom",
        strip.text      = ggplot2::element_text(size=12),
      ) +
      ggplot2::labs(x="", y=ylab)
  }

  unscaled <- plotit("stack", ylab="Number of Swine Isolates")
  scaled <- plotit("fill", ylab="Swine Isolates by %")

  legend <- cowplot::get_legend(scaled)
  unscaled <- unscaled + ggplot2::theme(legend.position = "none")
  scaled   <- scaled   + ggplot2::theme(legend.position = "none")

  cowplot::plot_grid(unscaled, scaled, legend, rel_heights=c(1,1,0.3), ncol=1, labels=NULL)
}

# ===== State plots
# state stuff starts here...

#' Make sure all states are there, rename states to regions for plotting maps
#'
#' @param state_str State two letter codes as strings
#' @return Returns the state names as full names
#' @examples
#' abbr2state(c("AK", "AL", "IA"))
#' @export
abbr2state <- function(state_str){
  state_str = as.character(state_str) %>% {
    . = dplyr::case_when(.=="AK"~"alaska", .=="AL"~"alabama", .=="AR"~"arkansas",
                         .=="AZ"~"arizona", .=="CA"~"california", .=="CO"~"colorado",
                         .=="CT"~"connecticut", .=="DC"~"district of columbia", .=="DE"~"delaware",
                         .=="FL"~"florida", .=="GA"~"georgia", .=="HI"~"hawaii",
                         .=="IA"~"iowa", .=="ID"~"idaho", .=="IL"~"illinois",
                         .=="IN"~"indiana", .=="KS"~"kansas", .=="KY"~"kentucky",
                         .=="LA"~"louisiana", .=="MA"~"massachusetts", .=="MD"~"maryland",
                         .=="ME"~"maine", .=="MI"~"michigan", .=="MN"~"minnesota",
                         .=="MO"~"missouri", .=="MS"~"mississippi", .=="MT"~"montana",
                         .=="NC"~"north carolina", .=="ND"~"north dakota", .=="NE"~"nebraska",
                         .=="NH"~"new hampshire", .=="NJ"~"new jersey", .=="NM"~"new mexico",
                         .=="NV"~"nevada", .=="NY"~"new york", .=="OH"~"ohio",
                         .=="OK"~"oklahoma", .=="OR"~"oregon", .=="PA"~"pennsylvania",
                         .=="RI"~"rhode island", .=="SC"~"south carolina", .=="SD"~"south dakota",
                         .=="TN"~"tennessee", .=="TX"~"texas", .=="UT"~"utah",
                         .=="VA"~"virginia", .=="VT"~"vermont", .=="WA"~"washington",
                         .=="WI"~"wisconsin", .=="WV"~"west virginia", .=="WY"~"wyoming")
  } %>% factor(.,
               levels=c("alaska","alabama","arkansas","arizona","california","colorado","connecticut",
                        "district of columbia","delaware","florida","georgia","hawaii","iowa","idaho",
                        "illinois","indiana","kansas","kentucky","louisiana","massachusetts","maryland",
                        "maine","michigan","minnesota","missouri","mississippi","montana","north carolina",
                        "north dakota","nebraska","new hampshire","new jersey","new mexico","nevada",
                        "new york","ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina",
                        "south dakota","tennessee","texas","utah","virginia","vermont","washington","wisconsin",
                        "west virginia","wyoming"))
  return(state_str)
}

# get states long and lat values
states <- ggplot2::map_data("state")

#' Make state map
#'
#' @param df the input surveyllance data
#' @param segment the column to facet by
#' @export
facetMaps <- function(df, segment){
#  config <- yaml::read_yaml(system.file("config.yaml", package="octoflushow"))
#  df <- order_data_factors(clean_data(df), config) %>% droplevels(.)
  df <- order_data_factors(df, config) #%>% droplevels(.)

  # add info
  cdata <- df %>% subset(.,State!="NoState" ) %>%
    subset(. ,!is.na(.[[segment]])) %>%
#    dplyr::mutate(region=abbr2state(State)) %>%
    reshape2::dcast(. ,region~.[[segment]], fun.aggregate = length, value.var=segment, drop=FALSE) %>%
    reshape2::melt(id="region")

  data_geo <- cdata %>%
    merge(states,., by="region",all.x=T) %>%
    dplyr::arrange(order)

  # Labels
  snames <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
  snames <- merge(snames, cdata, by="region")

  # do not label zeros
  snames$value[snames$value=="0"]=""

  #plot, viridae color palette?
  ggplot2::ggplot() +
    ggplot2::geom_polygon(data=data_geo, ggplot2::aes(x=long,y=lat,group=group,fill=log(value)),color="lightgrey") +
    ggplot2::scale_fill_gradient2(low="white", mid = "#43a2ca",high="#0868ac", midpoint = 3, guide="colorbar") +
    ggplot2::geom_text(data=snames, ggplot2::aes(long, lat, label=value)) +
    ggplot2::theme_void() + ggplot2::theme(legend.position = "none",text=ggplot2::element_text(size=28)) +
    ggplot2::facet_wrap(~variable, drop=TRUE) +
    ggplot2::coord_fixed(1.3)
}

#' Make a HA NA Heatmap of the data
#'
#' @param d data.frame swine surveillance data (i.e., output or \code{load_current})
#' @export
#' @return ggplot object
plot_heatmap <- function(d){
  # ===== Basic Counting and plotting
  cdata <- d %>%
    dplyr::select(H1, H3, N1, N2) %>%     # only looking at HANA heatmap
    dplyr::mutate(
      HAtype=dplyr::case_when(!is.na(H1)~paste("H1",H1, sep="."),
                              !is.na(H3)~paste("H3",H3, sep=".")),
      NAtype=dplyr::case_when(!is.na(N1)~paste("N1",N1, sep="."),
                              !is.na(N2)~paste("N2",N2, sep="."))
    ) %>%
    dplyr::select(HAtype, NAtype) %>%
    subset(!is.na(HAtype) & !is.na(NAtype)) %>%
    dplyr::group_by(HAtype, NAtype) %>%
    dplyr::summarise(
      n=dplyr::n()
    ) %>%
    dplyr::ungroup(.) %>%
    reshape2::dcast(., HAtype ~ NAtype, fill=0, value.var = "n") %>%
    reshape2::melt(.) %>%
    dplyr::mutate(
      NAtype=variable,
      n=value,
      variable=NULL,
      value=NULL
    ) %>% {
      .$tot = sum(.$n)
      .
    } %>%
    dplyr::mutate(
      percent= (n/tot * 100) %>% round(., digits=1),
      tot=NULL
    )

  mid.value <- (min(cdata$percent) + max(cdata$percent)) / 2
  tot <- sum(cdata$n)

  (p <- cdata %>% ggplot2::ggplot(., ggplot2::aes(y=HAtype, x=NAtype, fill=percent)) +
      ggplot2::geom_tile(color="black")+
      ggplot2::geom_text(ggplot2::aes(label=percent))+
      ggplot2::scale_fill_gradient2(low = "white", mid = "#43a2ca", midpoint=mid.value,
                                    high = "#0868ac", space = "Lab", guide = "colorbar")+
      ggplot2::theme_minimal()+
      ggplot2::labs(x="NA type", y="HA type", title=paste("Percentage of HA and NA combinations (n=",tot,")", sep=""))+
      ggplot2::theme(legend.position = "bottom")
  )
  return(p)
}

#' Make a Gene Constellation Heatmap of the data
#'
#' @param d data.frame swine surveillance data (i.e., output or \code{load_current})
#' @export
#' @return ggplot object
plot_constellation <- function(d){
  cdata <- d %>%
    subset(!grepl("-", Constellation)) %>%
    subset(!grepl(",", Subtype)) %>%
    subset(Subtype!="mixed") %>%
    subset(Subtype!="H4N6") %>%
    subset(!is.na(Constellation))

  # Get counts
  # ===== Get the counts
  (tots <- nrow(cdata))

  hhdata <- prepGConstData(cdata)

  xlabel <- "HA and NA Phylogenetic Clade Pairs"
  ylabel <- "Gene constellations"
  title <- paste("Gene Constellations (n=", tots, ")", sep = "")

  p<-hhdata %>% ggplot2::ggplot(.,
                                ggplot2::aes(x = labels, y = Constellation,
                                             fill = log(nn))) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_gradient2(
      low = "white", mid = "#43a2ca",
      high = "#0868ac", na.value = "white",
      midpoint = mean(log(hhdata$nn))
    ) +
    ggplot2::geom_text(ggplot2::aes(label = n)) +
    ggplot2::labs(
      title = title,
      x = xlabel,
      y = ylabel
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.margin = grid::unit(rep(0, 4), "lines"),
      axis.text.y = ggplot2::element_text(family = "mono")
    ) +
    ggplot2::facet_grid(. ~ Subtype, space = "free", scales = "free")
  # Add background lines
  for (yint in c(-1:length(unique(hhdata$Constellation))+1)) { # number of unique constellations
    p <- p + ggplot2::geom_hline(yintercept = yint + 0.5, size = 0.25, color = "gray")
  }
  for (xint in c(-1:length(unique(hhdata$labels))+1)) { # number of unique ha/na pairings
    p <- p + ggplot2::geom_vline(xintercept = xint + 0.5, size = 0.25, color = "gray")
  }
  return(p)

}

# ===== prepConstellationData
prepGConstData <- function(d){

  cdata <- d %>%
    dplyr::mutate(
      N1 = as.character(N1),
      N2 = as.character(N2),
      H1 = as.character(H1),
      H3 = as.character(H3)
    ) %>%
    dplyr::mutate(                      # change name for heatmap
      N1 = gsub("Pandemic", "pdm", N1),
      N1 = gsub("Classical", "classical", N1),
      H3 = gsub("Cluster_", "", H3),
      H3 = gsub("Human-like", "hu-like", H3)
    ) %>%
    dplyr::mutate(                      # create a HAtype and NAtype column
      H = factor(dplyr::coalesce(H1, H3)),
      N = factor(dplyr::coalesce(N1, N2))
    )

  # Get counts
  # ===== Get the counts
  (tots <- nrow(cdata))

  hdata <- cdata %>%
    dplyr::group_by(Subtype, H, N, Constellation) %>%
    dplyr::summarise(
      n = (dplyr::n()/tots*100) %>% round(., digits=1),
      nn = n
    ) %>%
    dplyr::select(Constellation, Subtype, H, N, n, nn) %>%
    dplyr::ungroup(.)

  totdata <- cdata %>%
    dplyr::group_by(Constellation) %>%
    dplyr::summarise(
      Subtype="total",
      H="total",
      N="total",
      n = (dplyr::n()/tots*100) %>% round(., digits=1),
      nn = n/3,
      nn = dplyr::case_when(nn<0.5 ~ 0.5,
                            1==1 ~ nn),
      labels="total"
    ) %>%
    dplyr::select(Constellation, Subtype, H, N, n, nn) %>%
    dplyr::ungroup(.)

  subtype_order <- c("total", "H1N1", "H1N2", "H3N1", "H3N2", "mixed")

  hhdata <- rbind(hdata, totdata) %>%
    dplyr::mutate(
      labels=dplyr::case_when(H=="total" ~ "total",
                              1==1 ~ paste(H, N, sep=".")),
      Subtype=factor(Subtype, subtype_order)
    )

  return(hhdata)
}

#' Make a count plot from a column
#'
#' @param df data.frame swine surveillance data (i.e., output or \code{load_current})
#' @param x_string The influenze segment that will be plotted [H1,H3,N1,N3,PB2,PB1,PA,NP,M,NS]
#' @param titletext The title of the plot
#' @export
#' @return ggplot object
countplot <- function(df, x_string, titletext) {
  titletext <- paste(titletext, " (n=", nrow(df), ")", sep = "")
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = x_string)) + ggplot2::geom_bar(ggplot2::aes(fill = I("royalblue"))) +
    ggplot2::labs(title = titletext) +
    ggplot2::theme_bw() +
    ggplot2::geom_text(stat = "count", ggplot2::aes(label = ..count..), vjust = -0.25) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1)) +
    ggplot2::scale_y_continuous(expand=ggplot2::expand_scale(mult=c(0,0.3)))
  return(p)
}

#' Bar charts for H1, H3, N1, N2, depends on countplot function
#'
#' @param df data.frame swine surveillance data (i.e., output or \code{load_current})
#' @param timespan_str The title of the plot
#' @export
#' @return ggplot object
quadcountplot <- function(df, timespan_str) {
  aa <- df %>%
    subset(!is.na(H1)) %>% # Subset to H1 Data
    countplot(., "H1", paste(timespan_str, "H1")) # Plot barchart (df, column name, title string)
  bb <- df %>%
    subset(!is.na(H3)) %>%
    countplot(., "H3", paste(timespan_str, "H3"))
  cc <- df %>%
    subset(!is.na(N1)) %>%
    countplot(., "N1", paste(timespan_str, "N1"))
  dd <- df %>%
    subset(!is.na(N2)) %>%
    countplot(., "N2", paste(timespan_str, "N2"))
  p <- cowplot::plot_grid(aa, bb, cc, dd, nrow = 2)
  return(p)
}

#' Plot barchart
#' @param df is the dataframe of clades and counts by date, an count_bytime output
#' @param palette is the named array of hex colors by clade
#' @param title is the title of the plot
#' @param value contains the counts
#' @param variable contains the clade names
#' @param bartype is either "stack" or "fill"
#' @param fed is either T or F if should use federal quarters
#' @export
barchart_bytime <- function(df, value="n", variable="H1", palette=octoflushow::get_palette(variable),
                            title="",
                            bartype="stack",
                            limits=NULL,
                            tunit="month",
                            fed=FALSE) {

  # local format of federal quarter
  format_Q <- function(ddf){
    octoflushow::date2quarter(ddf, fed=fed)
  }

  df[[variable]]=df[[variable]] %>% as.character %>% factor(levels=names(palette))

  p <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = "Date", y = value, fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = bartype) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(y="Number of Swine Isolates", x="", title=title) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 10, vjust = 0.5),
                   legend.title = ggplot2::element_blank(), legend.position = "bottom")

  if(tunit=="month"){
    p <- p + ggplot2::scale_x_date(
      labels = scales::date_format("%b-%y"),
      date_breaks = "1 month",
      expand=c(0,0),
      limits=NULL
    )
  }

  if(tunit=="quarter"){
    p <- p +
      ggplot2::scale_x_date(
        labels = format_Q,
        breaks = unique(df$Date),
        expand=c(0,0),
        limits=limits
      )
  }

  if(bartype=="fill"){
    p <- p + ggplot2::scale_y_continuous(labels = scales::percent)+
      ggplot2::labs(y="Swine Isolates by %", x="", title=title)
  }
  return(p)
}
