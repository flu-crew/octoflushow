#' Load a new swine surveillance report
#'
#' @param filename The name of the input TAB-delimited file (the output of
#' `octofludb make masterlist`)
#' @export
load_file <- function(filename) {
  my.data <- clean_data(readr::read_tsv(filename))
  return(my.data)
}

#' Load the current swine surveillance data
#'
#' @export
load_current <- function() {
  infile <- system.file("app-data", "A0_Master_List.tab", package = "octoflushow")
  my.data <- load_file(infile)
  return(my.data)
}

#' @param The input dataset
#' @return Data suitable for basic_plot
#' @export
toggle_clade_definition <- function(d, global_clade="False") {
  sub_regex = "\\|[^|]*$"
  if(global_clade) {
    sub_regex = "^[^|]*\\|"
  }
  column_list <- list("H1", "H3", "N1", "N2", "PB2", "PB1", "PA", "NP", "M", "NS")
  for(column in column_list) {
    d[[column]] <- sub(sub_regex, "", d[[column]], perl=TRUE)
    if(is.factor(d[[column]])) {
      d[[column]] <- droplevels(d[[column]]) 
    }
  }
  d
}

#' @param The input dataset
#' @return Data suitable for basic_plot
#' @export
collapse_h1 <- function(d) {
  # alpha
  d$H1 <- sub("^alpha.*", "alpha", d$H1, perl=TRUE)
  d$H1 <- sub("^1A.1.*", "1A.1", d$H1, perl=TRUE)
  # gamma (exclude gamma2)
  d$H1 <- sub("^gamma[^2].*", "gamma", d$H1, perl=TRUE)
  d$H1 <- sub("^1A.3.3.3.*", "1A.3.3.3", d$H1, perl=TRUE)
  if(is.factor(d$H1)) {
    d$H1 <- droplevels(d$H1) 
  }
  d
}

#' @param The input dataset
#' @return Data suitable for basic_plot
#' @export
collapse_h3 <- function(d) {
  # collapse cluster IV
  d$H3 <- sub("^IV-[A-Z].*", "IV", d$H3, perl=TRUE)
  d$H3 <- sub("^1990.4.*", "1990.4", d$H3, perl=TRUE)
  if(is.factor(d$H3)) {
    d$H3 <- droplevels(d$H3) 
  }
  d
}

#' @param The input dataset
#' @return Data suitable for basic_plot
#' @export
collapse_n1 <- function(d) {
  # avian (EU)
  d$N1 <- sub("^N1-avian.*", "N1-avian", d$N1, perl=TRUE)
  d$N1 <- sub("^N1.E.*", "N1.E", d$N1, perl=TRUE)
  # classical
  d$N1 <- sub("^[Cc]lassical", "Classical", d$N1, perl=TRUE)
  d$N1 <- sub("^N1.C.*", "N1.C", d$N1, perl=TRUE)
  if(is.factor(d$N1)) {
    d$N1 <- droplevels(d$N1) 
  }
  d
}

#' @param The input dataset
#' @return Data suitable for basic_plot
#' @export
collapse_n2 <- function(d) {
  # collapse 1998[AB] and 2002[AB]
  d$N2 <- sub("^(1998|98).*", "1998", d$N2, perl=TRUE)
  d$N2 <- sub("^(2002|02).*", "2002", d$N2, perl=TRUE)
  if(is.factor(d$N2)) {
    d$N2 <- droplevels(d$N2) 
  }
  d
}

#' Clean the dataset
#'
#' @param df Datatable containing swine surveillance information
#' @return Return a data table with standardized clade names and filtered datasets
#' @export
clean_data <- function(d, remove_mixed = TRUE) {
  my.data <- d

  # ===== Remove the single weird H4 strain
  # my.data <- subset(my.data, (is.na(H3) || H3 != "H4"))

  # remove duplicate dates
  my.data$Date <- sub("([^,+]*).*", "\\1", my.data$Date)

  # convert date strings to date objects
  my.data$Date <- zoo::as.Date(my.data$Date, "%Y-%m-%d")

  # remove rows with missing dates
  my.data <- my.data[!(is.na((my.data$Date))), ]

  my.data <- my.data %>%
    # remove any entries with multiple HA or NA entries
    dplyr::filter(!grepl(",", H1)) %>%
    dplyr::filter(!grepl(",", H3)) %>%
    dplyr::filter(!grepl(",", N1)) %>%
    dplyr::filter(!grepl(",", N2)) %>%
    # remove any entries with multiple subtypes
    dplyr::filter(!grepl(",", Subtype)) %>%
    # remove any entries with multiple dates
    dplyr::filter(!grepl(",", Date)) %>%
    # remove any entries with multiple states
    dplyr::filter(!grepl(",", State)) %>%
    # remove any entries with both H1 and H3 entries
    dplyr::filter(!(!is.na(H1) & !is.na(H3))) %>%
    dplyr::filter(!(!is.na(N1) & !is.na(N2))) %>%
    # remove anything that is missing State info
    dplyr::filter(!is.na(State))

  # add federal collection quarter
  my.data$Collection_Q <- octoflushow::date2quarter(my.data$Date, fed=TRUE)

  # ======================================== Add Regions
  region1 <- c("ME", "VT", "NH", "MA", "CT", "RI", "NY",
               "PA", "NJ", "DE", "MD", "DC", "VA", "WV", "NC",
               "SC", "TN", "AL", "GA", "FL")
  region2 <- c("MN", "IA", "WI", "IL", "IN", "KY", "OH", "MI")
  region3 <- c("MO", "AR", "MS", "LA", "OK", "TX")
  region4 <- c("ID", "MT", "WY", "ND", "SD", "NE", "KS")
  region5 <- c("WA", "OR", "CA", "NV", "UT", "AZ", "CO", "NM")

  my.data <- my.data %>%
    dplyr::mutate(region = dplyr::case_when(
      State %in% region1 ~ "Region1",
      State %in% region2 ~ "Region2",
      State %in% region3 ~ "Region3",
      State %in% region4 ~ "Region4",
      State %in% region5 ~ "Region5"
    ))

  my.data <- my.data %>% dplyr::mutate(
    H1 = fixH1names(H1),
    H3 = fixH3names(H3),
    N1 = fixN1names(N1),
    N2 = fixN2names(N2),
    PB2 = fixIGnames(PB2),
    PB1 = fixIGnames(PB1),
    PA = fixIGnames(PA),
    NP = fixIGnames(NP),
    M = fixIGnames(M),
    NS = fixIGnames(NS)
  )

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

  my.data$WGS <- grepl("^[HVTP]{6}$", my.data$Constellation, perl=TRUE)

  my.data$Constellation <- as.factor(my.data$Constellation)

  my.data
}

#' Prepare a text description of a date range
#'
#' For example, "Jun 2019 to Jun 2020"
#'
#' @param dates a vector of dates
#' @export
dates_to_str <- function(dates){
  min_date = min(dates) 
  max_date = max(dates)
  paste(lubridate::month(min_date, label=T, abbr=T), lubridate::year(min_date), "to", lubridate::month(max_date, label=T, abbr=T), lubridate::year(max_date))
}

#' Convert from date to fiscal quarter
#'
#' @param datey date
#' @return quarter (Q1, Q2, Q3, Q4)
#' @export
date_to_quarter <- function(datey){
  year_str = sub("20", "", year(datey %m+% months(3)))
  quarter_str = (((month(datey) + 2) %% 12)) %/% 3 + 1
  paste0(year_str, "Q", quarter_str)
}

#' Increment fiscal quarter
#'
#' @param quarter The fiscal quarter (e.g., 20Q1)
#' @return the next quarter (e.g., 20Q1 -> 20Q2 and 20Q4 -> 21Q1)
#' @export
increment_quarter <- function(quarter){
  y = as.integer(sub("Q.*", "", quarter))
  q = as.integer(sub(".*Q", "", quarter))
  if(all(q == 4)){
    y <- y + 1
    q <- 1
  } else {
    q <- q + 1
  }
  paste0(y, "Q", q)
}

#' Make a label stating the quarterly range 
#'
#' @param quarters a list of quarters 
#' @return A string describing the quarterly range (e.g., "[20Q1 to 21Q4]")
#' @export
labelFromQuarters <- function(quarters){
  quarters <- sort(quarters)
  if(length(quarters) == 1){
    quarters
  } else {
    paste0("[", quarters[1], " to ", rev(quarters)[1], "]") 
  }
}

#' Make cannonical constellation ordering
#'
#' @param d A dataframe with a Constellation column
#' @return The dataframe with the ordered Constellation column
#' @export
orderConstellations <- function(d){
  # FIXME: this should just take the constellation vector as an argument rather
  # than operating on the entire dataframe and hard-coded column name
  clvl <- levels(d$Constellation)
  d$Constellation <- factor(d$Constellation, levels=c(
    sort(clvl[!grepl("V", clvl)]),
    sort(clvl[ grepl("V", clvl)])
  ))
  d
}


#' Return regular or federal quarters
#' @param dates Vector of date values
#' @param fed True for returning federal quarters (19Q1 -> 19Q2)
#' @return Return a vector of quarters from the dates YYQ1 - YYQ4
#' @export

date2quarter <- function(dates, fed = FALSE){
  temp <- data.frame(dates = dates, 
                     Year=lubridate::year(dates), 
                     Quarter = lubridate::quarter(dates))
  if(fed){
    temp$Quarter = temp$Quarter+1
    temp$Year[temp$Quarter>4] = temp$Year[temp$Quarter>4] + 1
    temp$Quarter[temp$Quarter>4] = 1
  }
  temp$qtr = paste(temp$Year, temp$Quarter, sep="Q") %>% substr(., 3, 6)
  return(temp$qtr)
}

# ==== Private functions
# ===== Standardize clade names
fixH1names <- function(h1) {
  h1 %>%
    sub(".*,.*", "mixed", .) %>%
    sub("_", " ", .) %>%
    sub("pdm-vaccine|pdm", "pandemic", .)
}

fixH3names <- function(h3) {
  h3 %>%
    sub(".*,.*", "mixed", .) %>%
    sub("_", " ", .) %>%
    sub("^C IV", "IV-", .) %>%
    sub("Other-Human.*", "other-human", .)
}

fixN1names <- function(n1) {
  n1 %>%
    sub(".*,.*", "mixed", .) %>%
    sub("_", " ", .) %>%
    sub(".andemic|pdm", "Pandemic", .) %>%
    sub("classicalSwine|classical", "Classical", .) %>%
    sub("LAIV-Classical|MN99", "LAIV", .)
}

fixN2names <- function(n2) {
  patTX98 <- "TX1998|TX98|LAIV|LAIV-98"
  n2 %>%
    sub(".*,.*", "mixed", .) %>%
    sub("_", " ", .) %>%
    sub(glue::glue("^({patTX98})$"), "LAIV-98", .)
}

fixIGnames <- function(ig) {
  ig %>%
    sub(".*,.*", "mixed", .) %>%
    sub("_", " ", .) %>%
    sub(".*[Hh]uman.*", "Human-seasonal", .) %>%
    sub(".*(pdm|[Pp]andemic).*", "PDM", .) %>%
    sub(".*avian.*", "avian", .)
}
