# # TODO: finish this function for importing new files
# #' Load a new swine surveillance report
# #'
# #' @param filename The name of an excel file
# #' @param sheet The sheet in the excel file that containsq
# #' @export
# load_new_file <- function(filename sheet=1){
#   my.data <- readxl::read_excel(infile, sheet = sheet, col_types = "text")
#
# }

#' Load the current swine surveillance data
#'
#' @export
load_current <- function(){
  infile <- system.file("app-data", "A0_Master_List.xlsx", package="wilbur")
  sheet <- "Data"
  my.data <- readxl::read_excel(infile, sheet = sheet, col_types = "text")

  # ===== Remove the single weird H4 strain
  my.data <- subset(my.data, (is.na(H3) || H3 != "H4"))

  # ===== Date type, only run once
  my.data$Date <- my.data$Date %>%
    as.numeric %>%
    zoo::as.Date(origin = "1899-12-30")

  # ===== Standardize Names
  fixH1names <- function(h1){
      h1 %>% replace(., .=="pdm", "pandemic") %>%
      replace(., .=="gamma.1" | .=="gamma.2" | .=="gamma.3" , "gamma")
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
      replace(., .=="02A1" | .=="02A2" | .=="02B1" | .=="02B2" |.=="02A" | .=="02B", "2002") %>%
      replace(., .=="98A1" | .=="98A2" | .=="98B1" | .=="98B2" |.=="98A" | .=="98B", "1998") %>%
      replace(., .=="2002B,2002" | .=="2002,2002B", "2002")
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


  my.data$State <- droplevels(factor(my.data$State))
  my.data$Subtype <- droplevels(factor(my.data$Subtype))
  my.data$US_Clade <- droplevels(factor(my.data$US_Clade))
  my.data$GL_Clade <- droplevels(factor(my.data$GL_Clade))

  my.data$H1 <- droplevels(factor(my.data$H1))
  my.data$H3 <- droplevels(factor(my.data$H3))
  my.data$N1 <- droplevels(factor(my.data$N1))
  my.data$N2 <- droplevels(factor(my.data$N2))
  my.data$PB2 <- droplevels(factor(my.data$PB2))
  my.data$PB1 <- droplevels(factor(my.data$PB1))
  my.data$PA <- droplevels(factor(my.data$PA))
  my.data$NP <- droplevels(factor(my.data$NP))
  my.data$M <- droplevels(factor(my.data$M))
  my.data$NS <- droplevels(factor(my.data$NS))

  my.data
}

clean_data <- function(d, remove_mixed=TRUE){
  if(remove_mixed){
    # e.g., ("unknown,TRIG" -> "unknown")
    d$H1  <- gsub(",.*", "", d$H1)
    d$H3  <- gsub(",.*", "", d$H3)
    d$N1  <- gsub(",.*", "", d$N1)
    d$N2  <- gsub(",.*", "", d$N2)
    d$PB2 <- gsub(",.*", "", d$PB2)
    d$PB1 <- gsub(",.*", "", d$PB1)
    d$PA  <- gsub(",.*", "", d$PA)
    d$NP  <- gsub(",.*", "", d$NP)
    d$M   <- gsub(",.*", "", d$M)
    d$NS  <- gsub(",.*", "", d$NS)
  }
  d
}

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

  config <- yaml::read_yaml(system.file("config.yaml", package="wilbur"))
  segment_palette <- config$color[[segment]]

  d <- order_data_factors(clean_data(d), config)

  d$Date <- lubridate::as_datetime(lubridate::floor_date(d$Date, floorDateBy))

  if (is.null(xlim)){
    xlim <- lubridate::as_datetime(c(min(d$Date), max(d$Date)))
  }

  # Get user-provided segment
  m <- d[, c("Date", segment)]
  names(m)[2] <- "Segment"
  m <- subset(m, !is.na(Segment))

  summary.data <- m %>%
    dplyr::group_by(Date, Segment) %>%
    dplyr::summarise(n=dplyr::n(),
                     tot=nrow(.),
                     per=round(dplyr::n() / nrow(.)*100, digits = 2))  # Calculate summary statistics

  summary.data$Date <- as.POSIXct(summary.data$Date)

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
