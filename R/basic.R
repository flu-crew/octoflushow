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

#' Load a new swine surveillance report
#'
#' @param filename The name of an excel file
#' @param sheet The sheet in the excel file that contains the data.
#' @param region_to_state logical Replace US regions with states (required for octoflushow Shiny app but not for quarterly reports)
#' @export
load_file <- function(filename, sheet = 1, region_to_state=TRUE) {
  my.data <- clean_data(readr::read_tsv(filename))
  if (region_to_state) {
    my.data$region = abbr2state(state_str = my.data$State) 
  }
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

#' Clean the dataset
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
  my.data$Date <- zoo::as.Date(my.data$Date)

  # remove rows with missing dates
  my.data <- my.data[!(is.na((my.data$Date))), ]

  # add federal collection quarter
  my.data$Collection_Q <- octoflushow::date2quarter(my.data$Date, fed=TRUE)

  # set anything other than a state postal code to "NoState"
  # this will include ambiguous states like "TX,WY"
  my.data$State <- ifelse(grepl("^..$", my.data$State), my.data$State, "NoState")

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

  my.data$Constellation <- as.factor(my.data$Constellation)

  my.data
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
    temp$Year[temp$Quarter>4] = temp$Year[temp$Quarter>4] +1
    temp$Quarter[temp$Quarter>4] = 1
  }
  temp$qtr = paste(temp$Year, temp$Quarter, sep="Q") %>% substr(., 3,6)
  return(temp$qtr)
}

#' Count number of entries by time chunk
#' @param df is dataframe containing a column of Date and clade to count by.
#' @param col_name by default is "H1" but can change it to "N2","Subtype","region", etc.
#' @param Date by default is "Date" but can change if column name is different.
#' @param tunit is "month", "quarter" or "year".
#' @param minDate is default NULL, but can expand the date range lubridate::as_date("2019-01-01")
#' @param maxDate is default NULL, but can expand the date range
#' @return a datatable with columns Date, Clade, n (counts)
#' @export
countByTimeUnit <- function(df, col_name="H1", Date="Date", tunit="month", 
                            minDate=NULL, maxDate=NULL){
  # Get range of dates in order to fill in missing date values
  if(is.null(minDate)){ 
    minDate <-df[[Date]] %>% min(.) 
  }
  if(is.null(maxDate)){
    maxDate <- df[[Date]] %>% max(.)
  }
  minDate <- lubridate::floor_date(minDate, unit=tunit)
  maxDate <- lubridate::floor_date(maxDate, unit=tunit)

  # Count isolates by clade/subtype/state (col_name)
  cdf <- df %>%
    dplyr::select(., c(Date, col_name)) %>%
    dplyr::mutate(
      Date = zoo::as.Date(lubridate::as_datetime(lubridate::floor_date(zoo::as.Date(Date), unit=tunit)))
    ) %>%
    dplyr::group_by(.dots=c(Date, col_name)) %>%
    dplyr::summarise(
      n=dplyr::n()
    )%>%
    dplyr::ungroup(.) %>%
    tidyr::complete(Date=seq.Date(minDate, maxDate, by=tunit)) # fill in missing dates
  
  # For the missing dates, fill in a placeholder clade label and n=0
  for(col_i in col_name){
    placeholder = cdf[[col_i]][!is.na(cdf[[col_i]])] %>% unique(.) %>% {.[1]}
    cdf[[col_i]][is.na(cdf[[col_i]])]=placeholder
  }
  cdf$n[is.na(cdf$n)] <- 0
  return(cdf)
}

# ==== Private functions
# ===== Standardize clade names
fixH1names <- function(h1) {
  h1 %>%
    replace(., . == "gamma.1" | . == "gamma.2" | . == "gamma.3", "gamma") %>%
    sub(".*,.*", "mixed", .) %>%
    replace(., . == "LAIV.gamma2-beta-like", "gamma2-beta-like") %>%
    sub("pdm-vaccine|pdm", "pandemic", .)
}

fixH3names <- function(h3) {
  h3 %>%
    sub("Cluster_", "", .) %>%
    sub(".*,.*", "mixed", .) %>%
    sub("^LAIV.C-I$", "I", .) %>%
    sub("^C.IVB[12]?$", "IV-B", .) %>%
    sub("^C.IV(.)$", "IV-\\1", .) %>%
    sub("^C.(I|II|III|IV)$", "\\1", .) %>%
    sub("2010-human.like", "2010.1", .) %>%
    sub("2016-human.like", "2010.2", .) %>%
    sub("humanVaccine", "other-human", .) %>%
    replace(. == "Other-human", "other-human")
}

fixN1names <- function(n1) {
  n1 %>%
    sub(".*,.*", "mixed", .) %>%
    sub(".andemic|pdm", "Pandemic", .) %>%
    sub("classicalSwine|classical", "Classical", .) %>%
    sub("LAIV-Classical", "MN99", .)
}

fixN2names <- function(n2) {
  pat2002 <- "2002|02|02A_[12]|02B_[12]|2002[AB]|02[AB][12]?"
  pat1998 <- "1998|98|98A_[12]|98B_[12]|1998[AB]|98[AB][12]?"
  patTX98 <- "TX1998|TX98|LAIV|LAIV-98"
  n2 %>%
    sub(glue::glue("^({pat2002}),({pat2002})$"), "2002", .) %>%
    sub(glue::glue("^({pat1998}),({pat1998})$"), "1998", .) %>%
    sub(glue::glue("^({patTX98}),({patTX98})$"), "TX98", .) %>%
    sub(glue::glue("^({patTX98})$"), "TX98", .) %>%
    sub("humanSeasonal", "Human-like", .) %>%
    sub(glue::glue("^({pat2002})$"), "2002", .) %>%
    sub(glue::glue("^({pat1998})$"), "1998", .) %>%
    sub(".*,.*", "mixed", .)
}

fixIGnames <- function(ig) {
  ig %>%
    sub(".*,.*", "mixed", .) %>%
    replace(., . == "humanSeasonal", "Human-seasonal") %>%
    replace(., . == "LAIV", "TX98") %>%
    replace(., . == "VTX98", "TX98") %>%
    replace(., . == "pdm", "PDM")
}
