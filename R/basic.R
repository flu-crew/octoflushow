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


  my.data$State <- factor(my.data$State)
  my.data$Subtype <- factor(my.data$Subtype)
  my.data$US_Clade <- factor(my.data$US_Clade)
  my.data$GL_Clade <- factor(my.data$GL_Clade)
  my.data$check <- factor(my.data$check)

  my.data$H1 <- factor(my.data$H1)
  my.data$H3 <- factor(my.data$H3)
  my.data$N1 <- factor(my.data$N1)
  my.data$N2 <- factor(my.data$N2)
  my.data$PB2 <- factor(my.data$PB2)
  my.data$PB1 <- factor(my.data$PB1)
  my.data$PA <- factor(my.data$PA)
  my.data$NP <- factor(my.data$NP)
  my.data$M <- factor(my.data$M)
  my.data$NS <- factor(my.data$NS)

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

order_data_factors <- function(d){
  H3_order <- c("2010.1","2010.2","other-human","I","II","III","IV","IV-A","IV-B","IV-C","IV-D","IV-E","IV-F","No_data")
  N1_order <- c("Classical","Pandemic","Human_seasonal","MN99","No_data")
  d$H1 <- factor(d$H1)
  d$H3 <- factor(d$H3, ordered=TRUE, levels=H3_order)
  d$N1 <- factor(d$N1, ordered=TRUE, levels=N1_order)
  d
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

  d <- order_data_factors(clean_data(d))

  if(byMonth){
    lubridate::day(d$Date) <- 1
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


# ===== State plots
# state stuff starts here...

# Make sure all states are there, rename states to regions for plotting maps
prepStateNames <- function(state_str){
  state_str = as.character(state_str) %>% {
    . = case_when(.=="AK"~"alaska", .=="AL"~"alabama", .=="AR"~"arkansas",
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
  df <- order_data_factors(clean_data(df))

  # get states long and lat values
  states <- ggplot2::map_data("state")

  # add info
  cdata <- df %>% subset(.,State!="NoState" ) %>%
    subset(. ,!is.na(.[[segment]])) %>%
    dplyr::mutate(region=prepStateNames(State)) %>%
    dcast(. ,region~.[[segment]], fun.aggregate = length, value.var=segment, drop=FALSE) %>%
    melt(id="region") 

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
    ggplot2::facet_wrap(~variable) +
    ggplot2::coord_fixed(1.3)
}
