library(shiny)
library(tidyverse)
library(apivalm)

df <- valmyndigheten_get(c(2010, 2014, 2018), c("riksdag", "county", "municipal"))

