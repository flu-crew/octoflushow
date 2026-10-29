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
  if("N2" %in% names(d)) {d$N2 <- factor(d$N2, ordered=TRUE, levels=names(config$colors$N2)) %>% droplevels }
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
plot_basic <- function(d, segment="H1", plotType="bar", floorDateBy="month", xfontsize=10, yfontsize=10, limits=NULL,
                      title1 = glue::glue("{segment} phylogenetic-clade count by {floorDateBy}"),
                      title2 = glue::glue("{segment} phylogenetic-clade % by {floorDateBy}")
                      ){
  d <- d[!is.na(d[[segment]]) & d[[segment]] != "mixed", c("Date", segment)]

  # Show empty plot if not data is selected
  if(nrow(d) == 0){
    if(plotType == "data") { return(d) }
    else {
      q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
      return(q)
    }
  }

  d <- order_data_factors(d, config) %>%
       droplevels %>%
       octoflushow::countByTimeUnit(col_names=segment, Date="Date", tunit=floorDateBy)

  if(plotType=="stream") {
    title2 <- title1
  }

  unscaled <- octoflushow::barchart_bytime(d, variable=segment, chartype=plotType, tunit=floorDateBy, limits=limits, title=title1, xfontsize=xfontsize, yfontsize=yfontsize)
  scaled <- octoflushow::barchart_bytime(d, variable=segment, chartype=plotType, bartype="fill", tunit=floorDateBy, limits=limits, title=title2, xfontsize=xfontsize, yfontsize=yfontsize)
  if (plotType == "data") { return(unscaled) } # scaled/unscaled are identical dataframes
  else { octoflushow::shared_legend_plot(unscaled, scaled) }
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
    dplyr::group_by(across(c(Date, col_names))) %>%
    dplyr::summarise(
      n=dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    tidyr::complete(Date=seq.Date(minDate, maxDate, by=tunit)) # fill in missing dates
  
  # For the missing dates, fill in a placeholder clade label and n=0
  # introduces null value every quarter for x-axis (time) labels
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

# Compute approximate center coordinates for each state
#state_centers <- states %>%
#  dplyr::group_by(region) %>%  # 'region' is the lowercase state name
#  dplyr::summarise(
#    center_long=mean(range(long)),  # midpoint of longitude range
#    center_lat =mean(range(lat))    # midpoint of latitude range
#  )

state_centers <- data.frame(
  region=c("alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", "district of columbia", "florida", "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire", "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming"),
  long=c(-86.8286, -152.2783, -111.6603, -92.4425, -119.4697, -105.5478, -72.7272, -75.5050, -77.0147, -82.4497, -83.4425, -156.3736, -114.6131, -89.1964, -86.2817, -93.4961, -98.3772, -85.3022, -91.9967, -69.2428, -76.7908, -71.8083, -85.4103, -94.3053, -89.6678, -92.4581, -109.6333, -99.7950, -116.6311, -71.5811, -74.6728, -106.1125, -75.5267, -79.3878, -100.4658, -82.7936, -97.4942, -120.5583, -77.7997, -71.5561, -80.8964, -100.2264, -86.3506, -99.3311, -111.6703, -72.6658, -78.8536, -120.4472, -80.6228, -89.9942, -107.5511),
  lat=c(32.7794, 64.0686, 34.2744, 34.8939, 37.1842, 38.9972, 41.6219, 38.9897, 38.9100, 28.6306, 32.6414, 20.2928, 44.3508, 40.0417, 39.8942, 42.0750, 38.4939, 37.5347, 31.0689, 45.3694, 39.0550, 42.2597, 44.3467, 46.2808, 32.7364, 38.3567, 47.0528, 41.5378, 39.3289, 43.6806, 40.1908, 34.4072, 42.9539, 35.5558, 47.4500, 40.2861, 35.5889, 43.9336, 40.8781, 41.6761, 33.9169, 44.4442, 35.8581, 31.4758, 39.3056, 44.0686, 37.5214, 47.3825, 38.6408, 44.6242, 42.9958)
)

#' Make state pie map
#'
#' @param df the input surveyllance data
#' @param segment the column to select
#' @export
pieMap <- function(df, segment, counts=FALSE, palette=octoflushow::get_palette(segment), data_out=FALSE){
  df <- df[!is.na(df[[segment]]) & df$State %in% names(abbr2state), ]
  #df[[segment]] <- df[[segment]] %>% as.character %>% factor(levels=names(palette)) %>% droplevels

  # If the input dataset is empty (for example when only H3N2 are selected and
  # the user tries to plot H1 data), return an empty figure.
  if(nrow(df) == 0){
    if(data_out) { return(df) }
    else {
      q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
      return(q)
    }
  }

  title <- glue::glue("{segment}: {nrow(df)} strains collected from {min(df$Date)} to {max(df$Date)}")

  cdata <- df %>%
    dplyr::select(clade=segment, region="State") %>%
    dplyr::mutate(region=unname(abbr2state[as.character(region)])) %>%
#    order_data_factors(config) %>% droplevels %>%
    dplyr::group_by(region, clade) %>% dplyr::count() %>% dplyr::ungroup() %>%
    dplyr::mutate(region=factor(region, levels=unname(abbr2state))) %>%
    tidyr::complete(clade, region, fill=list(n=NA))

  data_geo <- merge(state_centers, cdata, all=TRUE, by="region") %>% 
    tidyr::drop_na(c(clade, n)) %>%
    #dplyr::mutate(n=ifelse(is.na(n), 0, n)) %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(total=sum(n, na.rm=TRUE)) %>%
    dplyr::mutate(size=floor(log10(total))+1) %>%
    dplyr::mutate(percent=n / total * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(clade=ifelse(percent < 5, "Other", as.character(clade))) %>%
    # merge Other
    dplyr::group_by(region, long, lat, clade, total, size) %>%
    dplyr::summarise(across(c(n, percent), sum))

  clade_li <- factor(data_geo$clade, ordered=TRUE, levels=names(config$colors[[segment]])) %>% droplevels %>% levels()
  even_size <- seq(from=0.9, to=1.8, length.out=max(data_geo$size))
  
  xyz <- mapplots::make.xyz(x=data_geo$long, y=data_geo$lat, z=data_geo$n, group=data_geo$clade)
  xyz$df <- as.data.frame(xyz$z)

  # xyz$z has a strange structure, re-build dataframe
  fix_df <- data.frame()
  for (row_n in seq_along(xyz$x)) {
    fix_df[row_n, ] <- NA
    fix_df[row_n, "long"] <- xyz$x[row_n]
    fix_df[row_n, "lat"] <- xyz$y[row_n]
    for (clade in colnames(xyz$df)) {
      fix_df[row_n, clade] <- as.vector(xyz$df[[clade]])[row_n]
    }
  }
  fix_df <- merge(fix_df, data_geo[c("long", "lat", "region", "total", "size")], all=TRUE, by=c("long", "lat")) %>%
    dplyr::mutate(size=even_size[size]) %>%
    dplyr::distinct()

  if (data_out) {  return(na.omit(fix_df)) }

  g <- ggplot2::ggplot(data=states, ggplot2::aes(x=long, y=lat, group=group)) +
      ggplot2::geom_polygon(fill="light grey", color="white") +
      ggplot2::coord_fixed(1) + 
      ggplot2::theme_void() +
      ggplot2::theme(
        text=ggplot2::element_text(size=12),
        plot.title=ggplot2::element_text(size=12, hjust=0.5, margin=ggplot2::margin(0,0,10,0)),
        legend.title=ggplot2::element_blank(), 
        legend.position="bottom"
      ) +
      ggplot2::ggtitle(title) +
      ggplot2::scale_fill_manual(values=palette)
    # demo state centers
    #geom_point(data=state_centers, ggplot2::aes(long, lat, group=region), color="red") +

  if (counts) {
    g <- g + ggplot2::geom_text(data=fix_df, ggplot2::aes(x=long, y=lat, group=region, label=total), hjust=0.5, vjust=0.5, size=2.5)
  }
  g <- g + scatterpie::geom_scatterpie(
      data=fix_df,
      mapping=ggplot2::aes(x=long, y=lat, r=size),
      cols=clade_li, #colnames(xyz$df),
      color=NA, #"grey",
      donut_radius=0.4,
      sorted_by_radius=TRUE,
      label_radius=NULL
  )
}


#' Make state map
#'
#' @param df the input surveyllance data
#' @param segment the column to facet by
#' @export
facetMaps <- function(df, segment, counts=FALSE, normalization="clade-max", data_out=FALSE){
  df <- df[!is.na(df[[segment]]) & df$State %in% names(abbr2state), ]

  # If the input dataset is empty (for example when only H3N2 are selected and
  # the user tries to plot H1 data), return an empty figure.
  if(nrow(df) == 0){
    if(data_out) { return(df) }
    else {
      q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
      return(q)
    }
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
    dplyr::select(clade=segment, region="State") %>%
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
  if (data_out) {  return(na.omit(snames)) }

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
plot_constellation <- function(d, data_out=FALSE){
  results <- d %>%
    subset(!grepl("-", Constellation)) %>%
    subset(Constellation!="mixed") %>%
    subset(!is.na(Constellation)) %>%
    subset(!grepl(",", Subtype)) %>%
    subset(Subtype!="mixed") %>%
    subset(Subtype!="H4N6") %>%
    prepGConstData()

  # Show empty plot if not data is selected
  if(nrow(d) == 0){
    if(data_out) { return(d) }
    else {
      q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
      return(q)
    }
  }

  if (data_out) { return(results$hhdata) } # data output

  # remove duplicate const/rows (causes bold text)
  results$hhdata <- results$hhdata %>%
    dplyr::select(-c(const, count)) %>%
    dplyr::distinct()

  xlabel <- "HA and NA Phylogenetic Clade Pairs"
  ylabel <- "Gene constellations"
  title <- paste("Gene Constellations (n=", results$tots, ")", sep = "")

  p <- ggplot2::ggplot(results$hhdata, ggplot2::aes(x=labels, y=Constellation, fill=log(nn))) +
    ggplot2::geom_tile(color="black") +
    ggplot2::scale_fill_gradient2(
      low = "white", mid = "#43a2ca", high = "#0281db",
      na.value = "white", midpoint = log(mean(results$hhdata$nn))
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
  for (yint in c(0:length(unique(results$hhdata$Constellation)))) { # number of unique constellations
    p <- p + ggplot2::geom_hline(yintercept = yint + 0.5, linewidth = 0.25, color = "gray")
  }
  xlines_df <- results$hhdata %>%
    dplyr::group_by(Subtype, labels) %>%
    dplyr::summarise() %>%
    # add empty row to each group
    dplyr::group_modify(~ dplyr::add_row(.x, .before = 0)) %>%
    dplyr::mutate(xint = dplyr::row_number(Subtype))
  p <- p + ggplot2::geom_vline(data = xlines_df, ggplot2::aes(xintercept = xint - 0.5), linewidth = 0.25, color = "gray")
  return(p) # figure output
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
    ) %>%
    # remove any entries missing HA/NA
    dplyr::filter(!( is.na(H1) & is.na(H3) )) %>%
    dplyr::filter(!( is.na(N1) & is.na(N2) ))

  # Get counts
  # ===== Get the counts
  (tots <- nrow(cdata))
  n_min <- floor(tots/1000/2)

  hdata <- cdata %>%
    dplyr::group_by(Subtype, H, N, Constellation) %>%
    dplyr::reframe(
      count=dplyr::n(),
    ) %>%
    dplyr::mutate(const=Constellation) %>% # save original const value
    dplyr::mutate(
      Constellation=dplyr::case_when(count>n_min ~ Constellation,
                                     count<=n_min ~ " Other")) %>%
#    aggregate(count ~ Subtype + H + N + Constellation, FUN = sum) %>%
    dplyr::group_by(Subtype, H, N, Constellation) %>% 
    dplyr::reframe(
      group_count=sum(count),
      const, count
    ) %>%
    dplyr::mutate(
      n = (group_count/tots*100) %>% round(., digits=1),
      nn = group_count/3
    ) %>%
    dplyr::select(Constellation, const, Subtype, H, N, n, nn, count, group_count) %>%
    dplyr::ungroup(.)

  totdata <- hdata %>%
#    aggregate(group_count ~ Constellation, FUN = sum) %>%
    dplyr::select(-c(const, count)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(Constellation) %>% 
    dplyr::summarise(
      group_count=sum(group_count),
    ) %>%
    dplyr::mutate(
      n = (group_count/tots*100) %>% round(., digits=1),
      nn = group_count/3,
      #nn = dplyr::case_when(nn<0.5 ~ 0.5, 1==1 ~ nn),
      Subtype="total",
      H="total",
      N="total",
      const="total",
      count="total"
    ) %>%
    dplyr::select(Constellation, const, Subtype, H, N, n, nn, count, group_count) %>%
    dplyr::ungroup(.)

  subtype_order <- c("total", "H1N1", "H1N2", "H3N1", "H3N2", "mixed")

  hhdata <- rbind(hdata, totdata) %>%
    # remove rows with n=0
    dplyr::filter(n > 0) %>%
    dplyr::mutate(
      labels=dplyr::case_when(H=="total" ~ "total",
                              1==1 ~ paste(H, N, sep="/")),
      Subtype=factor(Subtype, subtype_order)
    )

  return(list(hhdata=hhdata, tots=tots))
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
                            chartype="bar",
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
  data <- df %>% dplyr::group_by(Date) %>% dplyr::filter(n > 0) %>% dplyr::mutate(proportion = n / sum(n))

  # Show empty plot if not data is selected
  if(nrow(df) == 0){
    if(plotType == "data") { return(d) }
    else {
      q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
      return(q)
    }
  }

  palette = palette[levels(df[[variable]])]

  p <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = "Date", y = value, fill = variable)) +
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
      expand=c(0.001,0.001),
      limits=limits
    )
  }

  if(tunit=="quarter"){
    p <- p +
      ggplot2::scale_x_date(
        labels = format_Q,
        breaks = unique(df$Date),
        expand=c(0.001,0.001),
        limits=limits
      )
  }

  if(tunit=="year"){
    p <- p +
      ggplot2::scale_x_date(
        labels = scales::date_format("%Y"),
        date_breaks = "year",
        expand=c(0.001,0.001),
        limits=limits
      )
  }

  if(chartype=="bar") {
    p <- p + ggplot2::geom_bar(stat = "identity", position = bartype)
    if(bartype=="fill"){
      p <- p + ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::labs(y="Swine Isolates by %", x="", title=title)
    }
  }
  else {
    if(bartype=="stack"){
      p <- p + geom_stream(type="ridge")
    }
    else if(bartype=="fill"){
      p <- p + geom_stream(type="mirror") +
        ggplot2::scale_y_continuous(
          breaks = pretty( c(max(data$n/data$proportion)/2, -max(data$n/data$proportion)/2) ),
          labels = abs(pretty( c(max(data$n/data$proportion)/2, -max(data$n/data$proportion)/2) ))
        )
    }
  }

  if (chartype == "data") { 
    return(data)
  }
  else { return(p) }
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
  cowplot::plot_grid(p1, legend, p2, rel_heights=c(1,0.4,1), ncol=1, labels=NULL)
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
      H_Type = dplyr::case_when(!is.na(H1) ~ sub("^(H1.|)", "H1.", H1), # H Type
                                !is.na(H3) ~ sub("^(H3.|)", "H3.", H3)),
      N_Type = dplyr::case_when(!is.na(N1) ~ sub("^(N1.|)", "N1.", N1), # N Type
                                !is.na(N2) ~ sub("^(N2.|)", "N2.", N2))
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
heatmap_HANA <- function(df, dates=NULL, text=TRUE, totals=FALSE, font_size=3, data_out=FALSE) {

  # Show empty plot if not data is selected
  if(nrow(df) == 0){
    if(data_out) { return(df) }
    else {
      q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
      return(q)
    }
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
    ggplot2::scale_fill_gradient2(low = "white", mid = "#43a2ca", high = "#0281db",
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
  if (data_out) { return(df) }
  else { return(p) }
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
  d$H = ifelse(is.na(d$H1), sub("^(H3.|)", "H3.", d$H3), sub("^(H1.|)", "H1.", d$H1))
  d$N = ifelse(is.na(d$N1), sub("^(N2.|)", "N2.", d$N2), sub("^(N1.|)", "N1.", d$N1))
  d
}

#' Barchart comparing the most common clades over time
#'
#' @export
hana_barplots <- function(d, plotType="bar", floorDateBy="month", global=FALSE, ...){
  d <- d %>%
    unifyHANA(global) %>%
    dplyr::filter(!is.na(H) & !is.na(N)) %>%
    dplyr::mutate(Group = paste(H, N, sep="/"))

  common_barplot(d, plotType=plotType, floorDateBy=floorDateBy,
    title1 = glue::glue("HA/NA phylogenetic-clade count by {floorDateBy}"),
    title2 = glue::glue("HA/NA phylogenetic-clade % by {floorDateBy}"), ...)
}

#' Barchart comparing the most common HA/NA/Constellation triplet
#'
#' @export
triple_barplots <- function(d, plotType="bar", floorDateBy="month", global=FALSE, ...){
  d <- d %>%
    unifyHANA(global) %>%
    dplyr::filter(!is.na(H) & !is.na(N) & grepl("[A-Z]{6}", Constellation, perl=TRUE)) %>%
    dplyr::mutate(Group = paste(H, N, Constellation, sep="/"))

  common_barplot(d, plotType=plotType, floorDateBy=floorDateBy,
    title1 = glue::glue("HA/NA/Constellation count by {floorDateBy}"),
    title2 = glue::glue("HA/NA/Constellation % by {floorDateBy}"), ...)
}

#' Barchart comparing the most common HA/NA/Constellation triplet
#'
#' @export
common_barplot <- function(d, plotType=plotType, floorDateBy="month", xfontsize=10, yfontsize=10, limits=NULL,
                      title1 = glue::glue("count by {floorDateBy}"),
                      title2 = glue::glue("% by {floorDateBy}")){

  # Show empty plot if not data is selected
  if(nrow(d) == 0){
    if(plotType == "data") { return(d) }
    else {
      q <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::ggtitle("No data selected")
      return(q)
    }
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

  if(plotType == "stream") { 
    title2 <- title1 
  }

  unscaled <- octoflushow::barchart_bytime(d, palette=palette, variable="Group", chartype=plotType, tunit=floorDateBy, limits=limits, title=title1, xfontsize=xfontsize, yfontsize=yfontsize)
  scaled <- octoflushow::barchart_bytime(d, palette=palette, variable="Group", chartype=plotType, bartype="fill", tunit=floorDateBy, limits=limits, title=title2, xfontsize=xfontsize, yfontsize=yfontsize)
  if (plotType == "data") { return(unscaled) } # scaled/unscaled are identical dataframes
  else { octoflushow::shared_legend_plot(unscaled, scaled) }
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
