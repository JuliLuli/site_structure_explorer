library('xml2')
library('tools')
library('data.table')
library('stringi')
library('writexl')
library('glue')
library('RODBC')
library('readxl')
library('rvest')

# write url
my_link <- 'https://zakupki.gov.ru/epz/contract/search/results.html?morphology=on&search-filter=%D0%94%D0%B0%D1%82%D0%B5+%D1%80%D0%B0%D0%B7%D0%BC%D0%B5%D1%89%D0%B5%D0%BD%D0%B8%D1%8F&fz44=on&contractStageList_0=on&contractStageList_1=on&contractStageList_2=on&contractStageList_3=on&contractStageList=0%2C1%2C2%2C3&contractCurrencyID=-1&budgetLevelsIdNameHidden=%7B%7D&sortBy=UPDATE_DATE&pageNumber=1&sortDirection=false&recordsPerPage=_50&showLotsInfoHidden=false'
my_data <- read_html(my_link)

# create first string future data table 
# current node
curdata <- 'my_data'
flag <- 0

# future table structure
restable <- data.table(link = curdata, 
                       flag = flag, 
                       data = NA_character_)

# string the cursor points to
curline <- 1
# flag that we aren't finished yet
notEOtable_flag <- TRUE

# flag: 0 - didn't check
# flag: 1 - node has children 
# flag: 2 - node has no children

# the function fills the final table with children of the current row
step_up <- function(ix) {
  # x <- my_data
  # take a step along the object structure tree to by the cursor
  cur_data <- xml_children(eval(parse(text=restable[ix,link])))
  if (length(cur_data) > 0) # если у данного узла есть дети
  {
    # children' list in text form insert to the table
    cur_children <- paste0('xml_children(',restable[ix,link],')[',1:length(cur_data),']')
    restable <- rbind(restable, data.table(link = cur_children, flag = 0, data = NA_character_ ))
    restable[ix,'flag'] <- 1 
  } else { # if node has no children
    restable[ix,'data'] <- xml_text(eval(parse(text=restable[ix,link])))
    restable[ix,'link'] <- paste0('xml_text(',restable[ix,link],')')
    # restable[ix,'link'] <- restable[ix,link]
    restable[ix,'flag'] <- 2
  }
  restable
}

while (notEOtable_flag)
{
  restable <- step_up(curline)
  if (curline == restable[,.N])  notEOtable_flag <-  FALSE
  # if (curline == 20)  notEOtable_flag <-  FALSE
  # transfer the cursor
  curline <- curline + 1 
}

restable[grep('№',restable[,data]),link]






