library(dplyr)
install.packages("data.table", type = "binary")
# there is no package called ‘data.table’ 오류 해결
library(data.table)

# Benchmark #1 - Reading in data

system.time({read.csv("/Users/sallyride/data/pisa2015.csv")})
system.time({fread("/Users/sallyride/data/pisa2015.csv", na.strings = "")})

system.time({read.csv("/Users/sallyride/data/pisa2015.csv")})

# Benchmark @ - Calculating a conditional mean

# Calculate proportion that strongly agreed to an item
# @param x likert-type item as a numeric vector
getSA <- function(x, ...) mean(x == "Strongly agree", ...)

# read in data using fread()
pisa <- fread("/Users/sallyride/data/pisa2015.csv", na.strings = "")
class(pisa)
print(object.size(pisa), unit = "GB")
fwrite(pisa, file = "/Users/sallyride/data/pisa2015.csv")

pisa.tib <- tibble::as_tibble(pisa)
pisa.df <- as.data.frame(pisa)

region6 <- subset(pisa, CNT %in% c("United States", "Canada", "Mexico",
                                   "B-S-J-G (China)", "Japan", "Korea",
                                   "Germany", "Italy", "France", "Brazil",
                                   "Colombia", "Uruguay", "Australia",
                                   "New Zealand", "Jordan", "Israel", "Lebanon"))
fwrite(region6, file = "/Users/sallyride/data/region6.csv")

random6 <- subset(pisa, CNT %in% c("Mexico", "Uruguay", "Japan",
                                   "Germany", "New Zealand", "Lebanon"))
fwrite(random6, file = "/Users/sallyride/data/random6.csv")

# Using the i in data.table

pisa[CNTRYID == "Mexico" & ST063Q01NA == "Checked"]
pisa[10:25]

pisa[CNTRYID == "Mexico" & ST063Q01NA == "Checked"][17:20]

pisa[order(CNTRYID, decreasing = TRUE)][, head(CNTRYID)]

# Using the j in data.table

pisa[, CNTRYID]

pisa[, list(CNTRYID)]

pisa[, .(CNTRYID)]

pisa[, .(CNTRYID, SCIEEFF)]

pisa[CNTRYID %in% c("Mexico", "Japan"), table(ST063Q01NA)]

pisa[CNTRYID %in% c("Mexico", "Japan"), table(ST118Q04NA)]

pisa[CNTRYID %in% c("Mexico", "Japan"), 
     .(tense = factor(ST118Q04NA, 
                      levels = c("Strongly disagree", 
                                 "Disagree", "Agree", 
                                 "Strongly agree")))][, table(tense)]

pisa[, .(tense.as.char = ST118Q04NA, 
         tense.as.fac = factor(ST118Q04NA, 
                               levels = c("Strongly Disagree", 
                                          "Disagree", "Agree", 
                                          "Strongly Agree")))][,
                                                               .(character = object.size(tense.as.char), 
                                                                 factor = object.size(tense.as.fac))]

pisa[CNTRYID %in% c("Mexico", "Japan"), 
     .(xbar = mean(SCIEEFF, na.rm = T), 
       sigma = sd(SCIEEFF, na.rm = T), 
       minimum = min(SCIEEFF, na.rm = T), 
       med = median(SCIEEFF, na.rm = T), 
       maximum = max(SCIEEFF, na.rm = T))]

pisa[CNTRYID %in% c("Mexico", "Japan"), 
     .(plot(y = SCIEEFF, x = JOYSCIE, 
            col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3)), 
       xbar.joyscie = mean(JOYSCIE, na.rm = T))]

table(pisa$ST078Q01NA)

pisa[, 
     "eat.dinner" := sapply(ST078Q01NA, 
                            function(x) {
                              if (is.na(x)) NA 
                              else if (x == "No") 0L 
                              else if (x == "Yes") 1L
                            })][, table(eat.dinner)]

# Convert a dichtomous item (yes/no) to numeric scoring
# @param x a character vector containing "Yes" and "No" responses.

bin.to.num <- function(x){
  if (is.na(x)) NA 
  else if (x == "Yes") 1L 
  else if (x == "No") 0L
}

# create some variables as well as recoding gender to give it a more intuitive variable name
pisa[, ':='
     (female = ifelse(ST004D01T == "Female", 1, 0), sex = ST004D01T, 
       
       # At my house we have...
       desk = sapply(ST011Q01TA, bin.to.num), 
       own.room = sapply(ST011Q02TA, bin.to.num), 
       quite.study = sapply(ST011Q03TA, bin.to.num), 
       computer = sapply(ST011Q04TA, bin.to.num), 
       software = sapply(ST011Q05TA, bin.to.num), 
       internet = sapply(ST011Q06TA, bin.to.num), 
       lit = sapply(ST011Q07TA, bin.to.num), 
       poetry = sapply(ST011Q08TA, bin.to.num), 
       art = sapply(ST011Q09TA, bin.to.num), 
       book.sch = sapply(ST011Q10TA, bin.to.num), 
       dict = sapply(ST011Q12TA, bin.to.num), 
       art.book = sapply(ST011Q16NA, bin.to.num)
       )]

pisa[, ':=' 
     (math = rowMeans(pisa[, c(paste0("PV", 1:10, "MATH"))], na.rm = TRUE), 
       reading = rowMeans(pisa[, c(paste0("PV", 1:10, "READ"))], na.rm = TRUE), 
       science = rowMeans(pisa[, c(paste0("PV", 1:10, "SCIE"))], na.rm = TRUE))]

# Summarizing using the by in data.table

# the proportion of students in each country that have their own room at home

pisa[, 
     .(mean(own.room, na.rm = TRUE)), 
     by = .(CNTRYID)][1:6,]

pisa[, 
     .(own.room = mean(own.room, na.rm = TRUE)), 
     by = .(country = CNTRYID)
     ][order(own.room, decreasing = TRUE)
       ][1:6]

# compare just the Canada and Iceland on the proportion of students that have books of poetry at home (poetry) or and their mean on the enjoyment of science by student’s biological sex

pisa[CNTRYID %in% c("Canada", "Iceland"), 
     .(poetry = mean(poetry, na.rm = TRUE), 
       enjoy = mean(JOYSCIE, na.rm = TRUE)), 
     by = .(country = CNTRYID, sex = sex)]

# Let’s examine books of poetry at home by countries and sort it in descending order.

pisa[, 
     .(poetry = mean(poetry, na.rm = TRUE)), 
     by = .(country = CNTRYID)
     ][order(poetry, decreasing = TRUE)
       ][1:6]

# predict a student’s score on science self-efficacy scale given their score on the enjoyment of science scale and their sex for just the G7 countries (Canada, France, Germany, Italy, Japan, the United Kingdom, and the United States)

get.params <- function(cntry){
  mod <- lm(SCIEEFF ~ JOYSCIE + sex, cntry) 
  est.params <- list(int = coef(mod)[[1]], 
                     enjoy.slope = coef(mod)[[2]], 
                     sex.slope = coef(mod)[[3]])
  return(est.params)
}

g7.params <- pisa[CNTRYID %in% c("Canada", "France", "Germany", "Italy",
                                 "Japan", "United Kingdom", "United States"), 
                  get.params(.SD), 
                  by = .(CNTRYID)]

g7.params

# Reshaping data

# creating a student ID and then subsetting this ID and the at-home variables

pisa$id <- 1:nrow(pisa)
athome <- subset(pisa, select = c(id, desk:art.book))

# To transform the data to long format we melt the data
athome.l <- melt(athome, id.vars = "id", 
                 measure.vars = c("desk", "own.room", "quiet.study", 
                                  "lit", "poetry", "art", "book.sch", 
                                  "tech.book", "dict", "art.book"))

athome.l

athome.guess <- melt(athome)
athome.guess

# go back to wide format
athome.w <- dcast(athome.l, id ~ variable)

# The sparklyr package

library("sparklyr")
library("dplyr")

sc <- spark_connect(master = "local")

# copy the pisa data set to the Spark cluster.

pisa_sub <- subset(pisa, CNTRYID %in% c("Canada", "France", "Germany",
                                        "Italy", "Japan", "United Kingdom",
                                        "United States"), 
                   select = c("DISCLISCI", "TEACHSUP", "IBTEACH", "TDTEACH",
                              "ENVAWARE", "JOYSCIE", "INTBRSCI", "INSTSCIE",
                              "SCIEEFF", "EPIST", "SCIEACT", "BSMJ", "MISCED",
                              "FISCED", "OUTHOURS", "SMINS", "TMINS",
                              "BELONG", "ANXTEST", "MOTIVAT", "COOPERATE",
                              "PERFEED", "unfairteacher", "HEDRES", "HOMEPOS",
                              "ICTRES", "WEALTH", "ESCS", "math", "reading",
                              "CNTRYID", "sex"))

# use the selected variables in the labs and a description of these variables 
pisa_tbl <- copy_to(sc, pisa_sub, overwrite = TRUE)

# you can use the %>% to chain together commands or to pass data to functions
# With sparklyr, we can use the filter function instead of subset
# see the female students’ scores on these scales for Germany
pisa_tbl %>% 
  filter(CNTRYID == "Germany" & sex == "Female")

# If we wanted to calculate the average disciplinary climate in science classes (DISCLISCI) by country and by sex and have it reorder by country than sex, we can do the following:
pisa_tbl %>% 
  group_by(CNTRYID, sex) %>% 
  summarize(ave_disclip = mean(DISCLISCI, na.rm = TRUE)) %>% 
  arrange(CNTRYID, sex)

# We can also create new variables using the mutate function.
pisa_tbl %>% 
  mutate(totl_home = HEDRES + HOMEPOS) %>% 
  group_by(CNTRYID) %>% 
  summarize(xbar = mean(totl_home, na.rm = TRUE))

# calculate conditional means
# This is the proportion of students in each country that
# strongly agree that
# "I want top grades in most or all of my courses."
benchmark(
  "baseR" = {
    X <- aggregate(ST119Q01NA ~ CNTRYID, data = pisa, getSA, na.rm = TRUE)
  }, 
  "data.table" = {
    X <- pisa[, 
              getSA(ST119Q01NA, na.rm = TRUE), 
              by = CNTRYID]
  }, 
  "tidyverse" = {
    X <- pisa %>% 
      group_by(CNTRYID) %>% 
      summarize(getSA(ST119Q01NA, na.rm = TRUE))
  },
  replications = 1000)