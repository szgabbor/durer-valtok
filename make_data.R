library(dplyr)
library(data.table)
library(purrr)
library(tidyr)
library(stringr)
library(readr)

source("data_processing.R")


make_data <- function(name_of_data) {
  path_of_data <- paste("data/", name_of_data, "_be.tsv", sep ="")
  data <- fread(path_of_data, sep="\t", encoding="UTF-8", header = TRUE, dec = ".")

  nms <- c("question", "point", "image", "answer", "text", "hint1", "hint2", "tag")
  setnames(data, nms)

  data_first_half <- data %>% 
    slice(rep(1:as.integer((n()+1)/2)))

  data_second_half <- data %>% 
    slice(rep(as.integer((n()+1)/2)+1:n()))

  data_first <- process_data(data_first_half, name_of_data)
  data_second <- process_data(data_second_half, name_of_data)

  path_of_first <- paste("results/", name_of_data, "_first.tsv", sep ="")
  path_of_second <- paste("results/", name_of_data, "_second.tsv", sep ="")

  write_tsv(data_first, path_of_first, na = "", append = FALSE, quote_escape = "double")
  write_tsv(data_second, path_of_second, na = "", append = FALSE, quote_escape = "double")
}

years <- c("1_D_B", "1_D_C", "1_D_D", "2_D_B", "2_D_C", "2_D_D", "3_D_B", "3_D_C", "3_D_D", "4_D_B", "4_D_C", "4_D_D",
           "5_D_A", "5_D_B", "5_D_C", "5_D_D")

map(years, make_data)

