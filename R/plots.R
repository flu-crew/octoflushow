# ==== Global Variables
config <- yaml::read_yaml(system.file("config.yaml", package="wilbur"))

order_data_factors <- function(d, config){
  d$H1 <- factor(d$H1, ordered=TRUE, levels=names(config$colors$H1))
  d$H3 <- factor(d$H3, ordered=TRUE, levels=names(config$colors$H3))
  d$N1 <- factor(d$N1, ordered=TRUE, levels=names(config$colors$N1))
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

  d <- order_data_factors(clean_data(d), config) # JC: for some reason we're cleaning again?
#  d <- order_data_factors(d, config)  # JC: okay I understand why, but we should only clean the segment needed
  
  if(floorDateBy=="quarter"){
    floorDateBy="3 months"
  } 
  
  # cd = clean data
  cd <- d %>%
    dplyr::select(c("Date",segment)) %>% {
      names(.) = c("Date","Segment")
      .
    } %>% subset(., !is.na(Segment)) %>% 
    dplyr::mutate(
      Date = lubridate::as_datetime(lubridate::floor_date(Date, floorDateBy))
    ) 

#  d$Date <- lubridate::as_datetime(lubridate::floor_date(d$Date, floorDateBy))

  if (is.null(xlim)){
    xlim <- lubridate::as_datetime(c(min(d$Date), max(d$Date)))
  }

  # Get user-provided segment
  
  #m <- d[, c("Date", segment)]
  #names(m)[2] <- "Segment"
  #m <- subset(m, !is.na(Segment))

  summary.data <- cd %>% # m
    dplyr::group_by(Date, Segment) %>%
    dplyr::summarise(n=dplyr::n(),
                     tot=nrow(.),
                     per=round(dplyr::n() / nrow(.)*100, digits = 2)) %>% # Calculate summary statistics
    dplyr::ungroup(.) %>% dplyr::mutate(
      Date=lubridate::as_datetime(Date) %>% as.POSIXct(., format="%Y-%m-%d")
    )

  #summary.data$Date <- as.POSIXct(summary.data$Date)

  plotit <- function(position, ylab){
    ggplot2::ggplot(summary.data, ggplot2::aes(x=Date, y=n, fill=Segment)) +
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

#' Round dates to floor month values
#' 
#' @param date A date
#' @return The date where days set to 1.
#' @examples
#' roundByMonth(as.Date(c("2019-10-27", "2017-01-12")))
roundByMonth <- function(date){
  lubridate::day(date) = 1
  date
}

#' Round dates to floor year values
#' 
#' @param date A date
#' @return The date where days set to 1, and month set to January.
#' @examples
#' roundByYear(as.Date(c("2019-10-27", "2017-01-12")))
roundByYear <- function(date){
  lubridate::day(date) = 1
  lubridate::month(date) = 1
  date
}

#' Round dates to floor year values
#' 
#' @param date A date
#' @return The date where days set to 1, and month set to January/April/July/October.
#' @examples
#' roundByYear(as.Date(c("2019-10-27", "2017-01-12")))
roundByYear <- function(date){
  m = lubridate::month(date)
  m/3

  lubridate::day(date) = 1
  lubridate::month(date) = 1
  date
}

# ===== State plots
# state stuff starts here...

# Make sure all states are there, rename states to regions for plotting maps
prepStateNames <- function(state_str){
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

#' Make state map
#'
#' @param df the input surveyllance data
#' @param segment the column to facet by
#' @export
facetMaps <- function(df, segment){
  config <- yaml::read_yaml(system.file("config.yaml", package="wilbur"))
  df <- order_data_factors(clean_data(df), config) %>% droplevels(.)

  # get states long and lat values
  states <- ggplot2::map_data("state")

  # add info
  cdata <- df %>% subset(.,State!="NoState" ) %>%
    subset(. ,!is.na(.[[segment]])) %>%
    dplyr::mutate(region=prepStateNames(State)) %>%
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
