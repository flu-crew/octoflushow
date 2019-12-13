#' Load a new swine surveillance report
#'
#' @param filename The name of an excel file
#' @param sheet The sheet in the excel file that contains the data.
#' @export
load_file <- function(filename, sheet=1){
  my.data <- readxl::read_excel(filename, sheet = sheet, col_types = "text") %>% 
    clean_data(.)
  
  # ===== Convert Excell dates to R dates, Date only run once
  my.data$Date <- my.data$Date %>%
    as.numeric %>%
    zoo::as.Date(origin = "1899-12-30")
  
  return(my.data)
}

#' Load the current swine surveillance data
#'
#' @export
load_current <- function(){
  infile <- system.file("app-data", "A0_Master_List.xlsx", package="wilbur")
  sheet <- "Data"
  my.data <- load_file(infile, sheet = sheet)

  return(my.data)
}

#' Clean the dataset 
#' @param df Datatable containing swine surveillance information
#' @return Return a data table with standardized clade names and filtered datasets
#' @export
clean_data <- function(d, remove_mixed=TRUE){

  my.data = d

  

  # ===== Remove the single weird H4 strain
  # my.data <- subset(my.data, (is.na(H3) || H3 != "H4"))

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

#  my.data$State <- droplevels(factor(my.data$State))
#  my.data$Subtype <- droplevels(factor(my.data$Subtype))
#  my.data$US_Clade <- droplevels(factor(my.data$US_Clade))
#  my.data$GL_Clade <- droplevels(factor(my.data$GL_Clade))
# 
#   my.data$H1 <- droplevels(factor(my.data$H1))
#   my.data$H3 <- droplevels(factor(my.data$H3))
#   my.data$N1 <- droplevels(factor(my.data$N1))
#   my.data$N2 <- droplevels(factor(my.data$N2))
#   my.data$PB2 <- droplevels(factor(my.data$PB2))
#   my.data$PB1 <- droplevels(factor(my.data$PB1))
#   my.data$PA <- droplevels(factor(my.data$PA))
#   my.data$NP <- droplevels(factor(my.data$NP))
#   my.data$M <- droplevels(factor(my.data$M))
#   my.data$NS <- droplevels(factor(my.data$NS))

  d = my.data
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

# ==== Private functions
# ===== Standardize clade names
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