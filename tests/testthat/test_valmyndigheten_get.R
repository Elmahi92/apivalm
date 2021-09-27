context("valmyndigheten_get")

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

df <- valmyndigheten_get(c(2010, 2014, 2018), c("riksdag", "county", "municipal"))

test_that("Object valmyndigheten_get exists", {
  expect_true(exists("valmyndigheten_get"))
})

test_that("valmyndigheten_get is a function", {
  expect_is(valmyndigheten_get, class = "function")
})

test_that("valmyndigheten_get rejects errounous inputs", {
  expect_error(df <- valmyndigheten_get(c(2006, 2010), c("european", "riksdag")))
  expect_error(df <- valmyndigheten_get(c("european", "riksdag"), c(2006, 2010)))
  expect_error(df <- valmyndigheten_get(c(2006), c("riksdag")))
  expect_error(df <- valmyndigheten_get(c(2010), c("european")))
})

test_that("Data requests succeded", {
  for(i in 1:length(urls)) {
    expect_true(GET(urls[i],
                    write_disk(tempfile(fileext = strsplit(basename(urls[2]), split="\\.")[[1]][2])))$status_code == 200)
  }
})

test_that("The returned object is a tbl_df", {
  expect_is(valmyndigheten_get(c(2010), c("riksdag")), "tbl_df")
})

test_that("The returned tbl_df contains correct number of rows", {
  expect_true(nrow(df) == 2607)
})

test_that("The returned tbl_df contains the correct number of columns", {
  expect_true(ncol(df) == 34)
})

test_that("Columns in tbl_df are named correctly", {
  expect_true(all(colnames(df) == c("Year", "Election type", "County code", "Municipality code", "County", 
              "Municipality", "M no", "M pct", "C no", "C pct", "L no", "L pct", "KD no", "KD pct", 
              "S no", "S pct", "V no", "V pct", "MP no", "MP pct", "SD no", "SD pct", "FI no", 
              "FI pct", "Other no", "Other pct", "Blank no", "Blank pct", "Invalid no", 
              "Invalid pct", "Valid votes", "Votes cast", "Registered voters", "Turnout")))
})

test_that("Numeric variables only contains numeric values", {
  expect_true(all(apply(df[,which(grepl(" pct", colnames(df)) | grepl(" no", colnames(df)))], 2, function(x) is.numeric(x))))
})

test_that("Percentages between 0 and 100", {
  expect_true(all(df[,which(grepl(" pct", colnames(df)))] >= 0 & df[,which(grepl(" pct", colnames(df)))] <= 100, na.rm = TRUE))
})