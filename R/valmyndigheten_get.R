library(tidyverse)
library(httr)
library(readxl)

urls <- c("https://data.val.se/val/val2010/statistik/slutligt_valresultat_kommuner_R.xls",
          "https://data.val.se/val/val2014/statistik/2014_riksdagsval_per_kommun.xls",
          "https://data.val.se/val/val2018/statistik/2018_R_per_kommun.xlsx",
          "https://data.val.se/val/val2010/statistik/slutligt_valresultat_kommuner_L.xls",
          "https://data.val.se/val/val2014/statistik/2014_landstingsval_per_kommun.xls",
          "https://data.val.se/val/val2018/statistik/2018_L_per_kommun.xlsx",
          "https://data.val.se/val/val2010/statistik/slutligt_valresultat_kommuner_K_antal.xls",
          "https://data.val.se/val/val2010/statistik/slutligt_valresultat_kommuner_K_procent.xls",
          "https://data.val.se/val/val2014/statistik/2014_kommunval_per_kommun.xlsx",
          "https://data.val.se/val/val2018/statistik/2018_K_per_kommun.xlsx")

#' Get data from Valmyndigheten
#'
#' @title valmyndigheten_get
#' @param years A numeric vector of years (2010, 2014 and/or 2018)
#' @param election_type A character vector of election types ("riksdag", "county" and/or "municipality")
#' @return A tibble of elections matching the specificed \code{year} and \code{election_type}
#' @examples
#' valmyndigheten_get(c(2010), c("riksdag"))
#' valmyndigheten_get(c(2010, 2014, 2018), c("riksdag", "county", "municipal"))
#' @export
#'
valmyndigheten_get <- function(years, election_types) {
  if(!all(years %in% c(2010, 2014, 2018))) stop("Incorrect year! Data is only available for 2010, 2014 and 2018.")
  if(!all(election_types %in% c("riksdag", "county", "municipal"))) stop("Incorrect election type! Available election types are riksdag, county and municipal.")

  df <- tibble(`Year` = numeric(),
               `Election type` = character(),
               `County code` = numeric(),
               `Municipality code` = numeric(),
               `County` = character(),
               `Municipality` = character(),
               `M no` = numeric(),
               `M pct` = numeric(),
               `C no` = numeric(),
               `C pct` = numeric(),
               `L no` = numeric(),
               `L pct` = numeric(),
               `KD no` = numeric(),
               `KD pct` = numeric(),
               `S no` = numeric(),
               `S pct` = numeric(),
               `V no` = numeric(),
               `V pct` = numeric(),
               `MP no` = numeric(),
               `MP pct` = numeric(),
               `SD no` = numeric(),
               `SD pct` = numeric(),
               `FI no` = numeric(),
               `FI pct` = numeric(),
               `Other no` = numeric(),
               `Other pct` = numeric(),
               `Blank no` = numeric(),
               `Blank pct` = numeric(),
               `Invalid no` = numeric(),
               `Invalid pct` = numeric(),
               `Valid votes` = numeric(),
               `Votes cast` = numeric(),
               `Registered voters` = numeric(),
               `Turnout` = numeric())

  #######################################
  #### Riksdag (Sweden's parliament) ####
  #######################################

  if("riksdag" %in% election_types) {
    ## Riksdag, 2010 ##
    if(2010 %in% years) {
      df <-  bind_rows(df,
                       read_xls(GET(urls[1],
                                    write_disk(tempfile(fileext = strsplit(basename(urls[1]), split="\\.")[[1]][2])))$content) %>%
                         mutate(`Other no` = rowSums(.[,c(23,25,27)], na.rm = TRUE))  %>%
                         mutate(`Other pct` = rowSums(.[,c(24,26,28)], na.rm = TRUE)) %>%
                         select(-c(23:28)) %>%
                         rename_with(., ~ gsub(" tal", " no", .x, fixed = TRUE)) %>%
                         rename_with(., ~ gsub(" proc", " pct", .x, fixed = TRUE)) %>%
                         rename(`County code` = LAN, `Municipality code` = KOM, County = LÄN, Municipality = KOMMUN,
                                `L no` = `FP no`, `L pct` = `FP pct`,
                                `Blank no` = `BL no`, `Blank pct` = `BL pct`, `Valid votes` = `Rost Giltiga`,
                                `Votes cast` = Rostande, `Registered voters` = Rostb, `Turnout` = VDT,
                                `Invalid no` = `OG no`, `Invalid pct` = `OG pct`) %>%
                         add_column(`Year` = 2010, .before = "County code") %>%
                         add_column(`Election type` = "Riksdag", .before = "County code"))
    }

    ## Riksdag, 2014 ##
    if(2014 %in% years) {
      df <- bind_rows(df,
                      read_xls(GET(urls[2],
                                   write_disk(tempfile(fileext = strsplit(basename(urls[2]), split="\\.")[[1]][2])))$content, skip = 2) %>%
                        rename_with(., ~ gsub(" tal", " no", .x, fixed = TRUE)) %>%
                        rename_with(., ~ gsub(" proc", " pct", .x, fixed = TRUE)) %>%
                        rename(`County code` = LAN, `Municipality code` = KOM, County = LÄN, Municipality = KOMMUN,
                               `L no` = `FP no`, `L pct` = `FP pct`, `Other no` = `OVR no`, `Other pct` = `OVR pct`,
                               `Blank no` = `BL no`, `Blank pct` = `BL pct`, `Valid votes` = `Rost Giltiga`,
                               `Votes cast` = Rostande, `Registered voters` = Rostb, `Turnout` = VDT,
                               `Invalid no` = `OG no`, `Invalid pct` = `OG pct`) %>%
                        add_column(`Year` = 2014, .before = "County code") %>%
                        add_column(`Election type` = "Riksdag", .before = "County code"))
    }

    ## Riksdag, 2018 ##
    if(2018 %in% years) {
      df <- bind_rows(df,
                      left_join(read_xlsx(GET(urls[3],
                                              write_disk(tempfile(fileext = strsplit(basename(urls[3]), split="\\.")[[1]][2])))$content, sheet = "R antal") %>%
                                  mutate(`Invalid no` = rowSums(.[,c(39,41)], na.rm = TRUE))  %>%
                                  mutate(`Other no` = rowSums(.[,c(14:38)], na.rm = TRUE)) %>%
                                  select(-c(14:39,41)) %>%
                                  rename_with(.fn = ~ paste0(.x, " no"), .cols = c(5:14)) %>%
                                  add_column(`Year` = 2018, .before = "LÄNSKOD") %>%
                                  add_column(`Election type` = "Riksdag", .before = "LÄNSKOD"),
                                read_xlsx(GET(urls[3],
                                              write_disk(tempfile(fileext = strsplit(basename(urls[3]), split="\\.")[[1]][2])))$content, sheet = "R procent") %>%
                                  mutate(`Invalid pct` = rowSums(.[,c(39,41)], na.rm = TRUE))  %>%
                                  mutate(`Other pct` = rowSums(.[,c(14:38)], na.rm = TRUE)) %>%
                                  select(-c(14:39,41)) %>%
                                  rename_with(.fn = ~ paste0(.x, " pct"), .cols = c(5:14))  %>%
                                  add_column(`Year` = 2018, .before = "LÄNSKOD") %>%
                                  add_column(`Election type` = "Riksdag", .before = "LÄNSKOD")) %>%
                        rename_with(., ~ gsub(" tal", " no", .x, fixed = TRUE)) %>%
                        rename_with(., ~ gsub(" proc", " pct", .x, fixed = TRUE)) %>%
                        rename(`County code` = LÄNSKOD, `Municipality code` = KOMMUNKOD, County = LÄNSNAMN,
                               Municipality = KOMMUNNAMN, `Valid votes` = `RÖSTER GILTIGA`, `Votes cast` = RÖSTANDE,
                               `Registered voters` = RÖSTBERÄTTIGADE, `Turnout` = VALDELTAGANDE,
                               `Blank no` = `BLANK no`, `Blank pct` = `BLANK pct`))
    }
  }

  ########################
  #### County Council ####
  ########################
  if("county" %in% election_types) {
    ## County Council, 2010 ##
    if(2010 %in% years) {
      df <- bind_rows(df,
                      read_xls(GET(urls[4],
                                   write_disk(tempfile(fileext = strsplit(basename(urls[4]), split="\\.")[[1]][2])))$content) %>%
                        mutate(`Other no` = rowSums(.[,c(seq(21, 40, 2), seq(43, 92, 2))], na.rm = TRUE)) %>%
                        mutate(`Other pct` = rowSums(.[,c(seq(22, 40, 2), seq(44, 92, 2))], na.rm = TRUE)) %>%
                        select(-c(21:40, 43:92)) %>%
                        rename_with(., ~ gsub(" tal", " no", .x, fixed = TRUE)) %>%
                        rename_with(., ~ gsub(" proc", " pct", .x, fixed = TRUE)) %>%
                        rename(`County code` = `LÄN...1`, `Municipality code` = KOM, County = `LÄN...3`, Municipality = KOMMUN,
                               `L no` = `FP no`, `L pct` = `FP pct`, `Blank no` = `BLANK no`, `Blank pct` = `BLANK pct`,
                               `Valid votes` = `Rost Giltiga`, `Votes cast` = Rostande, `Registered voters` = Rostb,
                               `Turnout` = VDT, `Invalid no` = `OG no`, `Invalid pct` = `OG pct`)  %>%
                        add_column(`Year` = 2010, .before = "County code") %>%
                        add_column(`Election type` = "County council", .before = "County code"))
    }


    ## County Council, 2014 ##
    if(2014 %in% years) {
      df <- bind_rows(df,
                      read_xls(GET(urls[5],
                                   write_disk(tempfile(fileext = strsplit(basename(urls[5]), split="\\.")[[1]][2])))$content, skip = 2) %>%
                        mutate(`Other no` = rowSums(.[,seq(23, 112, 2)], na.rm = TRUE)) %>%
                        mutate(`Other pct` = rowSums(.[,seq(24, 112, 2)], na.rm = TRUE)) %>%
                        select(-c(23:112)) %>%
                        rename_with(., ~ gsub(" tal", " no", .x, fixed = TRUE)) %>%
                        rename_with(., ~ gsub(" proc", " pct", .x, fixed = TRUE)) %>%
                        rename(`County code` = `LÄN...1`, `Municipality code` = KOM, County = `LÄN...3`, Municipality = KOMMUN,
                               `L no` = `FP no`, `L pct` = `FP pct`, `Blank no` = `BLANK no`, `Blank pct` = `BLANK pct`,
                               `Valid votes` = `Rost Giltiga`, `Votes cast` = Rostande, `Registered voters` = Rostb,
                               `Turnout` = VDT, `Invalid no` = `OG no`, `Invalid pct` = `OG pct`) %>%
                        add_column(`Year` = 2014, .before = "County code") %>%
                        add_column(`Election type` = "County council", .before = "County code"))
    }

    ## County Council, 2018 ##
    if(2018 %in% years) {
      df <- bind_rows(df,
                      left_join(read_xlsx(GET(urls[6],
                                              write_disk(tempfile(fileext = strsplit(basename(urls[6]), split="\\.")[[1]][2])))$content,  sheet = "L antal") %>%
                                  mutate(`Invalid no` = rowSums(.[,c(53,55)], na.rm = TRUE))  %>%
                                  mutate(`Other no` = rowSums(.[,c(14:52)], na.rm = TRUE)) %>%
                                  select(-c(14:52,53,55)) %>%
                                  rename_with(.fn = ~ paste0(.x, " no"), .cols = c(5:14)),
                                read_xlsx(GET(urls[6],
                                              write_disk(tempfile(fileext = strsplit(basename(urls[6]), split="\\.")[[1]][2])))$content,  sheet = "L procent") %>%
                                  mutate(`Invalid pct` = rowSums(.[,c(53,55)], na.rm = TRUE))  %>%
                                  mutate(`Other pct` = rowSums(.[,c(14:52)], na.rm = TRUE)) %>%
                                  select(-c(14:52,53,55)) %>%
                                  rename_with(.fn = ~ paste0(.x, " pct"), .cols = c(5:14))) %>%
                        rename_with(., ~ gsub(" tal", " no", .x, fixed = TRUE)) %>%
                        rename_with(., ~ gsub(" proc", " pct", .x, fixed = TRUE)) %>%
                        rename(`County code` = LÄNSKOD, `Municipality code` = KOMMUNKOD, County = LÄNSNAMN,
                               Municipality = KOMMUNNAMN, `Valid votes` = `RÖSTER GILTIGA`, `Votes cast` = RÖSTANDE,
                               `Registered voters` = RÖSTBERÄTTIGADE, `Turnout` = VALDELTAGANDE,
                               `Blank no` = `BLANK no`, `Blank pct` = `BLANK pct`) %>%
                        add_column(`Year` = 2018, .before = "County code") %>%
                        add_column(`Election type` = "County council", .before = "County code"))
    }
  }

  ###########################
  #### Municipal Council ####
  ###########################
  if("municipal" %in% election_types) {
    ## Municipal Council, 2010 ##
    if(2010 %in% years) {
      df <- bind_rows(df,
                      left_join(read_xls(GET(urls[7],
                                             write_disk(tempfile(fileext = strsplit(basename(urls[7]), split="\\.")[[1]][2])))$content) %>%
                                  mutate(`Other no` = rowSums(.[,c(13:96, 98:222)], na.rm = TRUE)) %>%
                                  select(-c(13:96, 98:222)) %>%
                                  rename(`County code` = `LÄN...1`, `County` = `LÄN...3`, `Municipality code` = `KOM...2`,
                                         `Municipality` = `KOMMUN`, `Blank` = `BLANK`, `Valid votes` = `Rost Giltiga`,
                                         `Votes cast` = Rostande, `Registered voters` = Rostb,
                                         `Turnout` = VDT, `Invalid` = `OG`, `Invalid` = `OG`,
                                         L = FP) %>%
                                  rename_with(.fn = ~ paste0(.x, " no"), .cols = c(5:15))  %>%
                                  add_column(`Year` = 2010, .before = "County code") %>%
                                  add_column(`Election type` = "Municipal council", .before = "County code"),
                                read_xls(GET(urls[8],
                                             write_disk(tempfile(fileext = strsplit(basename(urls[8]), split="\\.")[[1]][2])))$content) %>%
                                  mutate(`Other pct` = rowSums(.[,c(13:96, 98:222)], na.rm = TRUE)) %>%
                                  select(-c(13:96, 98:222)) %>%
                                  rename(`County code` = `LÄN...1`, `County` = `LÄN...3`, `Municipality code` = `KOM...2`,
                                         `Municipality` = `KOMMUN`, `Blank` = `BLANK`, `Valid votes` = `Rost Giltiga`,
                                         `Votes cast` = Rostande, `Registered voters` = Rostb,
                                         `Turnout` = VDT, `Invalid` = `OG`, `Invalid` = `OG`,
                                         L = FP) %>%
                                  rename_with(.fn = ~ paste0(.x, " pct"), .cols = c(5:15)) %>%
                                  add_column(`Year` = 2010, .before = "County code") %>%
                                  add_column(`Election type` = "Municipal council", .before = "County code")))
    }

    ## Municipal Council, 2014 ##
    if(2014 %in% years) {
      df <- bind_rows(df,
                      read_xlsx(GET(urls[9],
                                    write_disk(tempfile(fileext = strsplit(basename(urls[9]), split="\\.")[[1]][2])))$content, skip = 2) %>%
                        mutate(`Other no` = rowSums(.[,seq(23, 518, 2)], na.rm = TRUE)) %>%
                        mutate(`Other pct` = rowSums(.[,seq(24, 518, 2)], na.rm = TRUE)) %>%
                        select(-c(23:518)) %>%
                        rename_with(., ~ gsub(" tal", " no", .x, fixed = TRUE)) %>%
                        rename_with(., ~ gsub(" proc", " pct", .x, fixed = TRUE)) %>%
                        rename(`County code` = `LÄN...1`, `Municipality code` = KOM, County = `LÄN...3`, Municipality = KOMMUN,
                               `L no` = `FP no`, `L pct` = `FP pct`, `Blank no` = `BLANK no`, `Blank pct` = `BLANK pct`,
                               `Valid votes` = `Rost Giltiga`, `Votes cast` = Rostande, `Registered voters` = Rostb,
                               `Turnout` = VDT, `Invalid no` = `OG no`, `Invalid pct` = `OG pct`) %>%
                        add_column(`Year` = 2014, .before = "County code") %>%
                        add_column(`Election type` = "Municipal council", .before = "County code"))
    }

    ## Municipal Council, 2018 ##
    if(2018 %in% years) {
      df <- bind_rows(df,
                      left_join(read_xlsx(GET(urls[10],
                                              write_disk(tempfile(fileext = strsplit(basename(urls[10]), split="\\.")[[1]][2])))$content, sheet = "K antal") %>%
                                  mutate(`Invalid no` = rowSums(.[,c(225,227)], na.rm = TRUE))  %>%
                                  mutate(`Other no` = rowSums(.[,c(14:224)], na.rm = TRUE)) %>%
                                  select(-c(14:224,225,227)) %>%
                                  rename_with(.fn = ~ paste0(.x, " no"), .cols = c(5:14)),
                                read_xlsx(GET(urls[10],
                                              write_disk(tempfile(fileext = strsplit(basename(urls[10]), split="\\.")[[1]][2])))$content,  sheet = "K procent") %>%
                                  mutate(`Invalid pct` = rowSums(.[,c(225,227)], na.rm = TRUE))  %>%
                                  mutate(`Other pct` = rowSums(.[,c(14:224)], na.rm = TRUE)) %>%
                                  select(-c(14:224,225,227)) %>%
                                  rename_with(.fn = ~ paste0(.x, " pct"), .cols = c(5:14))) %>%
                        rename_with(., ~ gsub(" tal", " no", .x, fixed = TRUE)) %>%
                        rename_with(., ~ gsub(" proc", " pct", .x, fixed = TRUE)) %>%
                        rename(`County code` = LÄNSKOD, `Municipality code` = KOMMUNKOD, County = LÄNSNAMN,
                               Municipality = KOMMUNNAMN, `Valid votes` = `RÖSTER GILTIGA`, `Votes cast` = RÖSTANDE,
                               `Registered voters` = RÖSTBERÄTTIGADE, `Turnout` = VALDELTAGANDE,
                               `Blank no` = `BLANK no`, `Blank pct` = `BLANK pct`) %>%
                        add_column(`Year` = 2018, .before = "County code") %>%
                        add_column(`Election type` = "Municipal council", .before = "County code"))
    }
  }
  return(df)
}
