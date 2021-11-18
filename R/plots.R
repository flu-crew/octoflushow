# ==== Global Variables
config <- yaml::read_yaml(system.file("config.yaml", package="octoflushow"))

hana_theme = ggplot2::theme(
    text = ggplot2::element_text(size=10),
    axis.text.x = ggplot2::element_text(size=10),
    axis.text.y = ggplot2::element_text(size=10),
    legend.text = ggplot2::element_text(size=10),
    strip.text = ggplot2::element_text(size=10)
  )

#' Return the segment palette for the plots
#' @param seg H1, H3, N1, N2, PB2, PB1, PA, NP, M, NS
#' @return Return a named vector of hex colors
#' @export
get_palette <- function(seg="H1"){
  return(unlist(config$colors[[seg]]))
}

order_data_factors <- function(d, config){
  if("H1" %in% names(d)) {d$H1 <- factor(d$H1, ordered=TRUE, levels=names(config$colors$H1)) %>% droplevels }
  if("H3" %in% names(d)) {d$H3 <- factor(d$H3, ordered=TRUE, levels=names(config$colors$H3)) %>% droplevels }
  if("N1" %in% names(d)) {d$N1 <- factor(d$N1, ordered=TRUE, levels=names(config$colors$N1)) %>% droplevels }
  d
}

#' Stacked bar chart for phylocluster by month, quarter or year
#'
#' @param d data.frame swine surveillance data (i.e., output or \code{load_current})
#' @param segment The influenze segment that will be plotted [H1,H3,N1,N3,PB2,PB1,PA,NP,M,NS]
#' @param floorDateBy An argument that is passed to
#' \code{lubridate::floor_date} specifying how to group dates (e.g., "month",
#' "week", "3 months", "year")
#' @return ggplot object
#' @export
plot_basic <- function(d, segment="H1", floorDateBy="month", xfontsize=10, yfontsize=10, limits=NULL,
                      title1 = glue::glue("{segment} phylogenetic-clade count by {floorDateBy}"),
                      title2 = glue::glue("{segment} phylogenetic-clade % by {floorDateBy}")
                      ){
  d <- d[!is.na(d[[segment]]) & d[[segment]] != "mixed", c("Date", segment)]

  # Show empty plot if not data is selected
  if(nrow(d) == 0){
    q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
    return(q)
  }

  d <- order_data_factors(d, config) %>%
       droplevels %>%
       octoflushow::countByTimeUnit(col_names=segment, Date="Date", tunit=floorDateBy)

  unscaled <- octoflushow::barchart_bytime(d, variable=segment, tunit=floorDateBy, limits=limits, title=title1, xfontsize=xfontsize, yfontsize=yfontsize)
  scaled <- octoflushow::barchart_bytime(d, variable=segment, tunit=floorDateBy, limits=limits, title=title2, bartype = "fill", xfontsize=xfontsize, yfontsize=yfontsize)
  octoflushow::shared_legend_plot(unscaled, scaled)
}

#' Count number of entries by time chunk
#' @param df is dataframe containing a column of Date and clade to count by.
#' @param col_names by default is "H1" but can change it to "N2","Subtype","region", etc.
#' @param Date by default is "Date" but can change if column name is different.
#' @param tunit is "month", "quarter" or "year".
#' @param minDate is default NULL, but can expand the date range lubridate::as_date("2019-01-01")
#' @param maxDate is default NULL, but can expand the date range
#' @return a datatable with columns Date, Clade, n (counts)
#' @export
countByTimeUnit <- function(df, col_names="H1", Date="Date", tunit="month", 
                            minDate=NULL, maxDate=NULL){

  # Get range of dates in order to fill in missing date values
  if(is.null(minDate)){ 
    minDate <- df[[Date]] %>% min(.) 
  }
  if(is.null(maxDate)){
    maxDate <- df[[Date]] %>% max(.)
  }
  minDate <- lubridate::floor_date(minDate, unit=tunit)
  maxDate <- lubridate::floor_date(maxDate, unit=tunit)

  # Count isolates by clade/subtype/state (col_names)
  cdf <- df %>%
    dplyr::select(c(Date, col_names)) %>%
    dplyr::mutate(
      Date = zoo::as.Date(lubridate::as_datetime(lubridate::floor_date(zoo::as.Date(Date), unit=tunit)))
    ) %>%
    dplyr::group_by(.dots=c(Date, col_names)) %>%
    dplyr::summarise(
      n=dplyr::n()
    )%>%
    dplyr::ungroup() %>%
    tidyr::complete(Date=seq.Date(minDate, maxDate, by=tunit)) # fill in missing dates
  
  # For the missing dates, fill in a placeholder clade label and n=0
  for(col_i in col_names){
    placeholder = cdf[[col_i]][!is.na(cdf[[col_i]])] %>% unique(.) %>% {.[1]}
    cdf[[col_i]][is.na(cdf[[col_i]])]=placeholder
  }

  cdf$n[is.na(cdf$n)] <- 0

  return(cdf)
}


# ===== State plots
# state stuff starts here...

abbr2state = c(
  "AL"="alabama",
  "AR"="arkansas",
  "CA"="california",
  "CO"="colorado",
  "DC"="district of columbia",
  "DE"="delaware",
  "GA"="georgia",
  "HI"="hawaii",
  "ID"="idaho",
  "IL"="illinois",
  "KS"="kansas",
  "KY"="kentucky",
  "MA"="massachusetts",
  "MD"="maryland",
  "MI"="michigan",
  "MN"="minnesota",
  "MS"="mississippi",
  "MT"="montana",
  "ND"="north dakota",
  "NE"="nebraska",
  "NJ"="new jersey",
  "NM"="new mexico",
  "NY"="new york",
  "OH"="ohio",
  "OR"="oregon",
  "PA"="pennsylvania",
  "SC"="south carolina",
  "SD"="south dakota",
  "TX"="texas",
  "UT"="utah",
  "VT"="vermont",
  "WA"="washington",
  "WV"="west virginia",
  "WY"="wyoming",
  "AK"="alaska",
  "AZ"="arizona",
  "CT"="connecticut",
  "FL"="florida",
  "IA"="iowa",
  "IN"="indiana",
  "LA"="louisiana",
  "ME"="maine",
  "MO"="missouri",
  "NC"="north carolina",
  "NH"="new hampshire",
  "NV"="nevada",
  "OK"="oklahoma",
  "RI"="rhode island",
  "TN"="tennessee",
  "VA"="virginia",
  "WI"="wisconsin"
)

# get states long and lat values
states <- ggplot2::map_data("state")

#' Make state map
#'
#' @param df the input surveyllance data
#' @param segment the column to facet by
#' @export
facetMaps <- function(df, segment, counts=FALSE, normalization="clade-max"){
  df <- df[!is.na(df[[segment]]) & df$State %in% names(abbr2state), ]

  # If the input dataset is empty (for example when only H3N2 are selected and
  # the user tries to plot H1 data), return an empty figure.
  if (nrow(df) == 0){
    q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
    return(q)
  }

  title <- glue::glue("{segment}: {nrow(df)} strains collected from {min(df$Date)} to {max(df$Date)} with colors normalized via {normalization}")

  data(state)

  if(normalization == "clade-max"){
    normalize <- function(d){
      dplyr::group_by(d, clade) %>%
        dplyr::mutate(r = n / max(n, na.rm=TRUE)) %>%
        dplyr::ungroup()
    }
  } else if (normalization == "state-percentage") {
    normalize <- function(d) {
      dplyr::group_by(d, region) %>%
        dplyr::mutate(r = n / sum(n, na.rm=TRUE)) %>%
        dplyr::ungroup()
    }
  } else {
    normalize <- function(d) {
      dplyr::mutate(d, r = n)
    }
  }

  cdata <- df %>%
    dplyr::select_(clade=segment, region="State") %>%
    dplyr::mutate(region = unname(abbr2state[as.character(region)])) %>%
    order_data_factors(config) %>% droplevels %>%
    dplyr::group_by(region, clade) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(region=factor(region, levels=unname(abbr2state))) %>%
    tidyr::complete(clade, region, fill=list(n=NA)) %>%
    dplyr::group_by(clade) %>% dplyr::mutate(
      label = glue::glue("{clade} (n={sum(n, na.rm=TRUE)})")
    ) %>% dplyr::ungroup() %>%
    normalize

  data_geo <- merge(states, cdata, all=TRUE, by="region") %>%
    dplyr::mutate(r = ifelse(is.na(r), NA, r)) %>%
    dplyr::arrange(order)

  # Labels
  snames <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y) %>%
    merge(cdata, all=TRUE, by="region") %>%
    dplyr::mutate(n = ifelse(n == 0, "", n)) %>%
    dplyr::mutate(n = ifelse(is.na(n), "", n))

  g <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data=data_geo, ggplot2::aes(x=long,y=lat,group=group,fill=r), size=0.2, color="black") +
    viridis::scale_fill_viridis(name="r", label = scales::comma)

  if (counts) {
    g <- g + ggplot2::geom_text(data=snames, ggplot2::aes(long, lat, label=n), size=2)
  }

  g + ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.title=ggplot2::element_text(size=12, hjust=0.5, margin=ggplot2::margin(0,0,10,0)),
      text=ggplot2::element_text(size=12)
    ) +
    ggplot2::facet_wrap(~label, drop=TRUE) +
    ggplot2::ggtitle(title) +
    ggplot2::coord_fixed(1.3)
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

  # Show empty plot if not data is selected
  if(nrow(hhdata) == 0){
    q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
    return(q)
  }

  xlabel <- "HA and NA Phylogenetic Clade Pairs"
  ylabel <- "Gene constellations"
  title <- paste("Gene Constellations (n=", tots, ")", sep = "")

  p <- ggplot2::ggplot(hhdata, ggplot2::aes(x=labels, y=Constellation, fill=log(nn))) +
    ggplot2::geom_tile(color="black") +
    ggplot2::scale_fill_gradient2(
      low="white", mid="#43a2ca",
      high="#0868ac", na.value="white",
      midpoint = mean(log(hhdata$nn))
    ) +
    ggplot2::geom_text(ggplot2::aes(label=n)) +
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
                            fed=FALSE,
                            xfontsize=10,
                            yfontsize=10,
                            ytitle="Number of Swine Isolates") {

  # local format of federal quarter
  format_Q <- function(ddf){
    octoflushow::date2quarter(ddf, fed=fed)
  }

  df[[variable]] <- df[[variable]] %>% as.character %>% factor(levels=names(palette)) %>% droplevels

  # Show empty plot if not data is selected
  if(nrow(df) == 0){
    q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
    return(q)
  }

  palette = palette[levels(df[[variable]])]

  p <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = "Date", y = value, fill = variable)) +
    ggplot2::geom_bar(stat = "identity", position = bartype) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(y=ytitle, x="", title=title) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 10, vjust = 0.5),
                   axis.text.y = ggplot2::element_text(size = 10),
                   legend.title = ggplot2::element_blank(), legend.position = "bottom")

  if(tunit=="month"){
    p <- p + ggplot2::scale_x_date(
      labels = scales::date_format("%b-%y"),
      date_breaks = "1 month",
      expand=c(0,0),
      limits=limits
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

  if(tunit=="year"){
    p <- p +
      ggplot2::scale_x_date(
        labels = scales::date_format("%Y"),
        date_breaks = "year",
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



#' Make two plots on top of eachother with a shared legend beneath 
#'
#' This layout works well for wide barcharts. Both plots are assumed to have
#' the same legend, but this is not checked.
#'
#' @param p1 Top plot (the shared legend is taken from this plot)
#' @param p2 Bottom plot
#' @return a ggplot2 object
#' @export
shared_legend_plot <- function(p1, p2){
  legend <- cowplot::get_legend(p1)
  p1 <- p1 + ggplot2::theme(legend.position = "none")
  p2 <- p2 + ggplot2::theme(legend.position = "none")
  cowplot::plot_grid(p1, legend, p2, rel_heights=c(1,0.3,1), ncol=1, labels=NULL)
}

#' Prepare data for hana
#'
#' @export
make_hhdata <- function(x, quarters){
  subset(x, Collection_Q %in% quarters) %>%
  subset(Subtype != "mixed") %>%            # drop mixed isolates
  subset(!is.na(Subtype)) %>%               # drop anything missing a subtype
  subset(!(is.na(H1) & is.na(H3))) %>%      # must have an H1 or H3 classified
  subset(!(is.na(N1) & is.na(N2))) %>%      # must have an N1 or N2 classified
  subset(grepl("^[A-Z][A-Z]$", State)) %>%  # drop isolates without state information
  subset(!is.na(Constellation)) %>%         # drop missing constellation isolates
  subset(!grepl("-", Constellation))        # drop missing constellation isolates
}


### heatmaps

#' Prepare data for heatmaps
#'
#' @export
make_heatmap_data_by_interval <- function(d, quarters){
  if(!is.null(quarters)){
    # limit heatmap data to the last 4 quarters
    d <- subset(d, Collection_Q %in% quarters)
  }

  removeMixed <- function(d){
    if ("Subtype" %in% names(d)){
      d <- subset(d, Subtype != "mixed")
    }
    if ("State" %in% names(d)){
      d <- subset(d, State != "NoState")
    }
    d
  }

  # Preparing Heatmap data
  removeMixed(d) %>% # Not mixed subtype or no state
    dplyr::mutate(
      H_Type = dplyr::case_when(!is.na(H1) ~ paste("H1", H1, sep = "."), # H Type
                                !is.na(H3) ~ paste("H3", H3, sep = ".")),
      N_Type = dplyr::case_when(!is.na(N1) ~ paste("N1", N1, sep = "."), # N Type
                                !is.na(N2) ~ paste("N2", N2, sep = "."))
    ) %>%
    # Drop if H or N are NA
    dplyr::filter(!is.na(H_Type)) %>%
    dplyr::filter(!is.na(N_Type))
}

#' Make a HANA heatmap
#'
#' The input is the full dataset which is then reduced to HA/NA counts before building.
#'
#' @export
heatmap_HANA <- function(df, dates=NULL, text=TRUE, totals=FALSE, font_size=3) {

  # Show empty plot if not data is selected
  if(nrow(df) == 0){
    q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
    return(q)
  }

  df <- octoflushow::make_heatmap_data_by_interval(df, NULL) %>%
    hana_counts %>%
    dplyr::select(H_Type, variable, percent)

  if(totals){
    x_totals <- dplyr::group_by(df, H_Type) %>%
      dplyr::summarize(variable = "HA.Total", percent = sum(percent, na.rm=TRUE)) %>% dplyr::ungroup()
    y_totals <- dplyr::group_by(df, variable) %>%
      dplyr::summarize(H_Type = "NA.Total", percent = sum(percent, na.rm=TRUE)) %>% dplyr::ungroup()
    df <- rbind(df, x_totals, y_totals)
    df$variable <- droplevels(df$variable)
    df$H_Type <- droplevels(df$H_Type)
    df$H_Type <- factor(df$H_Type, levels=rev(levels(df$H_Type)))
    nX = nlevels(df$variable)
    nY = nlevels(df$H_Type)
  }

  title = "Percentage of HA and NA combinations"
  if(!is.null(dates)){
    title = paste(title, "-", octoflushow::dates_to_str(dates))
  }

  mid.value <- (min(df$percent) + max(df$percent)) / 2
  df$label <- ifelse(df$percent == 0, "", round(df$percent, 1)) 

  p <- ggplot2::ggplot(df, ggplot2::aes(x = variable, y = H_Type)) +
    ggplot2::geom_tile(ggplot2::aes(fill = percent), colour = "black") +
    ggplot2::scale_fill_gradient2(low = "white", mid = "#43a2ca", high = "#0868ac",
                                  space = "Lab", midpoint = mid.value, guide = "colorbar") +
    ggplot2::theme_bw() +
    ggplot2::labs(y="HA Clade", x="NA Clade", title=title) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 8, hjust = 1, vjust = 0.5)) +
    hana_theme

  if(totals){
    p <- p +
      ggplot2::geom_segment(ggplot2::aes(x = nX - 0.5, xend = nX - 0.5, y = 0.5, yend = nY + 0.5), color="black", size=1) +
      ggplot2::geom_segment(ggplot2::aes(x = 0.5, xend = nX + 0.5, y = 1.5, yend = 1.5), color="black", size=1)
  }

  if(text==TRUE){
    p <- p + ggplot2::geom_text(ggplot2::aes(label = label), size = font_size)
  }
  return(p)
}


#' Make a HANA diff heatmap
#'
#' The input is the full dataset which is then reduced to HA/NA counts before building.
#'
#' @export
heatmap_hana_diff <- function(d, cquarters, pquarters, font_size=3, totals=FALSE){

  # Show empty plot if not data is selected
  if(nrow(d) == 0){
    q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
    return(q)
  }

  cdata <- octoflushow::hana_counts(octoflushow::make_heatmap_data_by_interval(d, cquarters)) %>% dplyr::select(H=H_Type, N=variable, cpercent=percent)
  pdata <- octoflushow::hana_counts(octoflushow::make_heatmap_data_by_interval(d, pquarters)) %>% dplyr::select(H=H_Type, N=variable, ppercent=percent)

  dif_title <- paste0("HA/NA percentage differences (Current ", labelFromQuarters(cquarters), " - Prior ", labelFromQuarters(pquarters), ")")

  cpdata <- merge(cdata, pdata, all=FALSE)
  cpdata$diff <- (cpdata$cpercent - cpdata$ppercent) %>% ifelse(. == 0, NA, .)
  cpdata <- dplyr::select(cpdata, H, N, diff)

  ha_totals <- dplyr::group_by(cpdata, H) %>% dplyr::summarize(N = "NA.Total", diff = sum(diff, na.rm=TRUE))
  na_totals <- dplyr::group_by(cpdata, N) %>% dplyr::summarize(H = "HA.Total", diff = sum(diff, na.rm=TRUE))
  cpdata <- rbind(cpdata, ha_totals, na_totals)
  cpdata$H <- droplevels(cpdata$H)

  nX = length(unique(cpdata$N))
  nY = length(unique(cpdata$H))
  cpdata$H <- factor(cpdata$H, levels=rev(levels(cpdata$H)))

  p <- ggplot2::ggplot(cpdata, ggplot2::aes(x = N, y = H, fill = diff)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = round(diff, 1)), size = font_size) +
    ggplot2::scale_fill_gradient2(
      low = "#b2182b", mid = "#91bfdb", # mid = "#E0f3F8",
      high = "#0868ac", na.value = "white",
      midpoint = 0
    ) +
    ggplot2::geom_segment(ggplot2::aes(x = nX - 0.5, xend = nX - 0.5, y = 0.5, yend = nY + 0.5), color="black", size=1) +
    ggplot2::geom_segment(ggplot2::aes(x = 0.5, xend = nX + 0.5, y = 1.5, yend = 1.5), color="black", size=1) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = dif_title, x = "NA Clade", y = "HA Clade") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 8, hjust = 1, vjust = 0.5)) +
    hana_theme

  if(totals){
    p <- p +
      ggplot2::geom_segment(ggplot2::aes(x = nX - 0.5, xend = nX - 0.5, y = 0.5, yend = nY + 0.5), color="black", size=1) +
      ggplot2::geom_segment(ggplot2::aes(x = 0.5, xend = nX + 0.5, y = 1.5, yend = 1.5), color="black", size=1)
  }

  return(p)
}


unifyHANA <- function(d, global=FALSE){
  if(global){
    d$H = d$GL_Clade
  } else {
    d$H = ifelse(is.na(d$H1), paste("H3", d$H3, sep="."), paste("H1", d$H1, sep="."))
  }
  d$N = ifelse(is.na(d$N1), paste("N2", d$N2, sep="."), paste("N1", d$N1, sep="."))
  d
}

#' Barchart comparing the most common clades over time
#'
#' @export
hana_barplots <- function(d, floorDateBy="month", global=FALSE, ...){
  d <- d %>%
    unifyHANA(global) %>%
    dplyr::filter(!is.na(H) & !is.na(N)) %>%
    dplyr::mutate(Group = paste(H, N, sep="/"))

  common_barplot(d, floorDateBy=floorDateBy,
    title1 = glue::glue("HA/NA phylogenetic-clade count by {floorDateBy}"),
    title2 = glue::glue("HA/NA phylogenetic-clade % by {floorDateBy}"), ...)
}

#' Barchart comparing the most common HA/NA/Constellation triplet
#'
#' @export
triple_barplots <- function(d, floorDateBy="month", global=FALSE, ...){
  d <- d %>%
    unifyHANA(global) %>%
    dplyr::filter(!is.na(H) & !is.na(N) & grepl("[PTVH]{6}", Constellation, perl=TRUE)) %>%
    dplyr::mutate(Group = paste(H, N, Constellation, sep="/"))

  common_barplot(d, floorDateBy=floorDateBy,
    title1 = glue::glue("HA/NA/Constellation count by {floorDateBy}"),
    title2 = glue::glue("HA/NA/Constellation % by {floorDateBy}"), ...)
}

#' Barchart comparing the most common HA/NA/Constellation triplet
#'
#' @export
common_barplot <- function(d, floorDateBy="month", xfontsize=10, yfontsize=10, limits=NULL,
                      title1 = glue::glue("count by {floorDateBy}"),
                      title2 = glue::glue("% by {floorDateBy}")){

  # Show empty plot if not data is selected
  if(nrow(d) == 0){
    q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
    return(q)
  }

  d <- octoflushow::countByTimeUnit(d, col_names="Group", Date="Date", tunit=floorDateBy)

  mostCommon <- d %>%
    dplyr::group_by(Group) %>%
    dplyr::summarize(n = sum(n)) %>%
    dplyr::arrange(-n) %>% head(12) %$% Group

  d <- d %>%
    dplyr::ungroup() %>%
    dplyr::mutate(d, Group = ifelse(Group %in% mostCommon, Group, "Other")) %>%
    dplyr::group_by(Group, Date) %>%
    dplyr::summarize(n = sum(n)) %>%
    dplyr::arrange(Date, -n)

  palette <- c(RColorBrewer::brewer.pal(length(mostCommon), "Paired"), "#666666")
  names(palette) <- c(mostCommon, "Other")

  d$Group <- factor(d$Group, levels=c(mostCommon, "Other"))

  unscaled <- octoflushow::barchart_bytime(d, palette=palette, variable="Group", tunit=floorDateBy, limits=limits, title=title1, xfontsize=xfontsize, yfontsize=yfontsize)
  scaled <- octoflushow::barchart_bytime(d, palette=palette, variable="Group", tunit=floorDateBy, limits=limits, title=title2, bartype = "fill", xfontsize=xfontsize, yfontsize=yfontsize)
  octoflushow::shared_legend_plot(unscaled, scaled)
}

#' Get HANA counts
#'
#' @param df Data from @make_heatmap_data_by_interval@ function
#' @export
hana_counts <- function(df){
  # # Order the labels
  # ha_order <- c(
  #   "H1.alpha",
  #   "H1.beta",
  #   "H1.gamma",
  #   "H1.gamma2",
  #   "H1.gamma2-beta-like",
  #   "H1.pandemic",
  #   "H1.delta1",
  #   "H1.delta1a",
  #   "H1.delta1b",
  #   "H1.delta2",
  #   "H3.I",
  #   "H3.II",
  #   "H3.III",
  #   "H3.IV",
  #   "H3.IV-A",
  #   "H3.IV-B",
  #   "H3.IV-C",
  #   "H3.IV-D",
  #   "H3.IV-E",
  #   "H3.IV-F",
  #   "H3.IV-G",
  #   "H3.IV-H",
  #   "H3.IV-I",
  #   "H3.IV-J",
  #   "H3.IV-K",
  #   "H3.2010.1",
  #   "H3.2010.2",
  #   "H3.other-human"
  # )
  #
  # na_order <- c(
  #   "N1.Classical",
  #   "N1.Pandemic",
  #   "N1.MN99",
  #   "N2.1998",
  #   "N2.2002",
  #   "N2.2016",
  #   "N2.TX98",
  #   "N2.Human-like"
  # )

  df %>%
    # Barcode, H1.clade, N1.clade (or H3, and N2)
    dplyr::select(Barcode, H_Type, N_Type) %>%
    # Count number of HA and NA pairs
    reshape2::dcast(H_Type ~ N_Type, fun.aggregate = length, value.var = "Barcode") %>%
    # Prep for ggplot format
    reshape2::melt(id = "H_Type") %>%
    # get 2 digit percentage counts
    dplyr::mutate(percent = round(value * 100 / sum(value), digits = 1)) %>%
    dplyr::arrange(desc(percent)) %>%
    dplyr::mutate(
      # H_Type = factor(H_Type, levels = ha_order),
      # variable = factor(variable, levels = na_order)
      H_Type = factor(H_Type),
      variable = factor(variable)
      )
}
