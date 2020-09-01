############################################################
###                        ABOUT                         ###
############################################################
# This script should resolve issue posted at:
# https://community.rstudio.com/t/function-problem/78158
# by user: https://community.rstudio.com/u/muhammadinfo 

############################################################
###                  REQUIRED PACKAGES                   ###
############################################################

# install.packages("rio")
# install.packages("SASxport")
library(rio)
library(survey)
library(tidyverse)
library(srvyr)
library(SASxport)

############################################################
###                     Get Data                         ###
############################################################

if (!exists("FL18_pe")){
  download.file("https://www.cdc.gov/brfss/annual_data/2018/files/LLCP2018XPT.zip",
                destfile = "LLCP2018XPT.zip",
                quiet = TRUE)
  unzip("LLCP2018XPT.zip", overwrite = TRUE)
  llcp2018 <- read.xport("LLCP2018.XPT", name.chars = "_")
}

############################################################
###                     PREPARE DATA                     ###
############################################################

Fl18 <- filter(llcp2018, X_STATE == 12)
# export(Fl18, "FL18.csv")
keep_var <- c("X_DENVST3", "DIABETE3", "LASTDEN4",
              "SEX1", "X_LLCPWT2", "X_STSTR", "X_PSU")
FL18_pe <- Fl18[, keep_var]
# added this to remove the "labelled" class from the data.frame
FL18_pe <- as.data.frame(lapply(FL18_pe, unclass))

FL18_pe$SEX1 <- as.factor(recode(FL18_pe$SEX1,
                                 "1" = "M",
                                 "2" = "F",
                                 .missing = "0",
                                 .default = "0"))

FL18_pe$DIABETE3 <- as.factor(recode(FL18_pe$DIABETE3,
                                     "1" = "1",
                                     .missing = "0",
                                     .default = "0"))

FL18_pe$LASTDEN4 <- as.factor(recode(FL18_pe$LASTDEN4,
                                     "1" = "1",
                                     "2" = "2",
                                     .missing = "0",
                                     .default = "0"))

FL18_pe$X_DENVST3 <- as.factor(recode(FL18_pe$X_DENVST3,
                                      "1" = "1",
                                      .missing = "0",
                                      .default = "0"))
FL18_pe <- as.data.frame(FL18_pe)
# export(FL18_pe, "FL18_pe.csv")

############################################################
###                     MAKE DESIGNS                     ###
############################################################

FL18_pe_dsgn <- svydesign(id = ~1,
                          strata = ~X_STSTR,
                          weights = ~X_LLCPWT2,
                          data = FL18_pe)

svymean(~ factor(X_DENVST3), FL18_pe_dsgn, na.rm = TRUE)

FL18_pe_design <- as_survey_design(.data = FL18_pe,
                                   ids = "X_PSU",
                                   strata = "X_STSTR",
                                   weights = "X_LLCPWT2")

FL18_pe_design <- FL18_pe %>% as_survey_design(ids = "X_PSU",
                                               strata = "X_STSTR",
                                               weights = "X_LLCPWT2")

############################################################
###                     PROCESS DATA                     ###
############################################################

FL18_pe_design %>%
  group_by(SEX1, LASTDEN4) %>%
  summarize(proportion = survey_mean(vartype = c("se", "ci")))

FL18_pe_design %>%
  group_by(SEX1, LASTDEN4) %>%
  summarize(proportion = survey_mean(vartype = c("se", "ci"))) %>%
  filter(LASTDEN4 == 1)

FL18_pe_design %>%
  group_by(SEX1, LASTDEN4) %>%
  summarize(proportion = survey_mean(vartype = c("se", "ci"))) %>%
  filter(LASTDEN4 == 1)
FL18_pe %>%
  count(vars = LASTDEN4, by = SEX1) %>%
  filter(vars == 2)

############################################################
###                      FUNCTIONS                       ###
############################################################

my_fun <- function(y) {
  FL18_pe_design %>%
    group_by(SEX1, !!y) %>%
    summarize(proportion = survey_mean(vartype = c("se", "ci"))) %>%
    filter(!!y == 1) %>%
    cbind(FL18_pe %>%
            count(vars = !!Y, by = SEX1) %>%
            filter(vars == 1))
}

pe <- function(y) {
  y <- enquo(y)
  FL18_pe_design %>%
    group_by(SEX1, !!y) %>%
    summarize(proportion = survey_mean(vartype = c("se", "ci"))) %>%
    filter(!!y == 1) %>%
    cbind(FL18_pe %>%
            count(vars = !!y, by = SEX1) %>%
            filter(vars == 1))
}

############################################################
###                        RESULT                        ###
############################################################
pe(LASTDEN4)
