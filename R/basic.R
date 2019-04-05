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
#' @export
#' @return ggplot object
plot_basic <- function(d){
  # ===== Basic Counting and plotting
  # ==== Colors and Order of names
  # Stacked bar chart for phylocluster by month
  H1_palette <- c("alpha" = "#543005", "beta" = "#8c510a", "gamma" = "#80cdc1", "gamma2" = "#35978f",
                  "delta1"="#bf812d","delta1a"="#dfc27d","delta1b"="#f6e8c3","delta2"="#c7eae5",
                  "pandemic"="#01665e","gamma2-beta-like"="#60b0f4","No_data"="#EDF8B1")
  H3_order <- c("2010.1","2010.2","other-human","I","II","III","IV","IV-A","IV-B","IV-C","IV-D","IV-E","IV-F","No_data")
  H3_palette <- c("2010.1"="#999999","2010.2"="#996633","other-human"="#F90005","I"="#542788","II"="#d73027","III"="#2166AC",
                  "IV"="#E69F00","IV-A"="#56B4E9","IV-B"="#009E73","IV-C"="#F0E442","IV-D"="#0072B2","IV-E"="#D55E00","IV-F"="#CC79A7",
                  "No_data"="#EDF8B1")
  N1_palette <- c("Classical"="#FEB24C","Pandemic"="#F03B20","Human_seasonal"="#BD2D2B","MN99"="#91BFDB","No_data"="#EDF881")
  N1_order <- c("Classical","Pandemic","Human_seasonal","MN99","No_data")
  N2_palette <- c("1998"="#7FCDBB","98"="#7FCDBB", "98A" ="#78c8b4", "98A1"="#1b7837", "98A2"="#9970ab", "98B"="#82d2be", "98B1"="#5aae61","98B2"="#762a83",
                  "2002"="#2C7FB8","02"="#2C7FB8", "02A" ="#2878b4", "02A1"="#d6604d", "02A2"="#b2182b", "02B"="#3282be", "02B1"="#d1e5f0","02B2"="#fddbc7",
                  "Human-like"="#1F38FF", "Human_N2"="#969696", "Human-to-swine_2016"="#1F38FF","Human-like_2010.2"="#996633","2016"="#996633", 
                  "TX98"="#C7EAE5", "No_data"="#EDF8B1")

  internalgene_palette <- c("TRIG"="#C2A5CF","PDM"="#762A83","unknown"="#800000","TX98"="#C7EAE5")
  subtype_palette <- c("#d7191c","#fdae61","#ffffbf","#abd9e9","#2C7FB8","#e0f3f8","#2c7bb6","#2c7bb6","#2c7bb6","#2c7bb6","#2c7bb6")

  d$ByMonth=d$Date
  lubridate::day(d$ByMonth)=1

  # Prepping ggplot input data (Current Version)
  summary.data = d %>%
    subset(!is.na(H1)) %>%{
      .$H1 = gsub(",.*","",.$H1)
      .
    } %>%
    dplyr::group_by(., ByMonth, H1) %>% # Group by Month and H1 clade (extend this to any segment H3, N1, N2, etc)
    dplyr::summarise(n=dplyr::n(),
                     tot=nrow(.),
                     per = round(dplyr::n() / nrow(.)*100, digits = 2))  # Calculate summary statistics

  # example barchart
  ggplot2::ggplot(summary.data, ggplot2::aes(x=ByMonth, y=n, fill=H1)) +
    ggplot2::geom_bar(stat="identity",position = "stack") +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = H1_palette) +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle=90, size=10, vjust=0.5),
      legend.title    = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.text.x    = ggplot2::element_text(size=14)
    )
}
