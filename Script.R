library(readxl)
library(rstudioapi)
library(magrittr)
library(openxlsx)
library(dplyr)
library(stringr)

options(tibble.print_max = 500, tibble.print_min = 300)

(function() {
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path))
})()


data <- read_excel("Исходные данные.xlsx")

init <- function(data) {
  data$`Номер состояния` <- 0
  data$`Символ перехода` <- 
    str_extract(data$Содержимое, "\\..") %>% 
    str_replace(".", "")
  data$Переход <- dense_rank(data$`Символ перехода`)
  data
}

data %<>% init()

del_point <- function(string) {
  str_replace(string, "\\.", "")
}

get_all_transition_symbol <- function(res) {
  str_extract(res$Содержимое, "\\..") %>% del_point()
}

getSetPointNonterminal <- function(rules) {
  uni <- 
    rules %>% 
    str_extract(., "\\.[:alpha:]") %>% 
    del_point() %>% 
    unique() %>% 
    .[!is.na(.)]
  uni
}

getTable <- function(new_rules) {
  n <- length(new_rules)
  tibble(
    `Номер состояния` = rep(NA, n), 
    `Содержимое` = new_rules, 
    `Символ перехода` = rep(NA, n),
    `Переход`= rep(NA, n)
  )
}

get_rules <- function(sum_data, i, native_data) {
  
  native_data_copy <- native_data
  uni <- getSetPointNonterminal(native_data$Содержимое)
  
  # Получение правил
  rules <- filter(sum_data, Переход == i)
  rules <- filter(rules, rules$`Номер состояния` == rules$`Номер состояния`[1])
  
  new_rules <- 
    str_replace(rules$Содержимое, "\\..", str_c(rules$`Символ перехода`, '\\.'))
  
  nonTermRules <- getSetPointNonterminal(new_rules)
  
  while (str_detect(nonTermRules, "[:upper:]") %>% any(na.rm = TRUE)) {
    append_rules <- native_data_copy$Содержимое[str_extract(native_data_copy$Содержимое, '.') %in% nonTermRules]
    native_data_copy %<>% filter(!(`Содержимое` %in% append_rules))
    new_rules %<>% c(append_rules)
    nonTermRules <- getSetPointNonterminal(append_rules)
  }
  
  res <- getTable(new_rules)
  res$`Номер состояния` <- i
    
  return(res)
}

pr_rules_with_sv <- function(local_res, native_rules) {
  rules <- local_res$Содержимое
  native_rules %<>% del_point()
  rules %<>% del_point()

  for (i in 1:nrow(local_res)) {
    local_res$`Символ перехода`[i] <- 
      local_res$Содержимое[i] %>% 
      str_sub(1, 1) %>% 
      str_c("СЛЕД ", .)
    print(rules[i])
    print(native_rules)
    rule_num <- which(rules[i] == native_rules)
    local_res$Переход[i] <- -rule_num
  }
  local_res
}

### Получние таблицы
get_table <- function(data) {
  native_data <- sum_data <- data
  new_state_index <- max(data$Переход) + 1 
  i = 1;
  
  while (TRUE) {
    
    res <- get_rules(sum_data, i, native_data)
    res$`Символ перехода` <- get_all_transition_symbol(res)

    ##Получение правил с символом перехода NA
    rules_with_svert <- filter(res, is.na(res$`Символ перехода`))
    rules_without_svert <- filter(res, !is.na(res$`Символ перехода`))
    
    plus_data <- filter(native_data, FALSE)
    if (nrow(rules_with_svert) > 0) {
      procRWS <- pr_rules_with_sv(rules_with_svert, native_data$Содержимое)
      plus_data %<>% bind_rows(procRWS)
    }
    sum_data %<>% bind_rows(plus_data)
    
    all_symbol_per <- unique(rules_without_svert$`Символ перехода`)

    res <- rules_without_svert
    for (sym in all_symbol_per) {
      rools_with_one_transition_symbol <- filter(res, `Символ перехода` == sym)
      search_rules <- rools_with_one_transition_symbol$Содержимое
      searched_rules <- filter(sum_data, `Содержимое` %in% search_rules)
      all_trans_number <- sum_data$Переход[sum_data$Содержимое %in% rools_with_one_transition_symbol] %>% unique()
      all_rules_with_trans_number <- filter(sum_data, `Переход` %in% all_trans_number)
      delta <- dplyr::setdiff(all_rules_with_trans_number, searched_rules)
      
      if (nrow(delta)) {
        delta_numbers <- unique(delta$`Переход`)
        searched_rules %<>% filter(!(`Переход` %in% delta_numbers))
      }
      
      if (nrow(searched_rules)) {
        searched_rules %<>% 
          select(., -"Номер состояния") %>% 
          unique() %>%
          cbind(`Номер состояния` = 1:nrow(.), .)
      }
      
      n_searched_rules <- length(search_rules)
      trans <- 
        count(searched_rules, `Переход`) %>% 
        filter(n == n_searched_rules) %>% 
        pull(`Переход`)
      
      if (length(trans)) {
        rools_with_one_transition_symbol$Переход <- trans[1]
      } else {
        rools_with_one_transition_symbol$Переход <- new_state_index
        new_state_index <- new_state_index + 1
      }
      
      sum_data %<>% bind_rows(rools_with_one_transition_symbol)
      plus_data %<>% bind_rows(rools_with_one_transition_symbol)
    }
  
    i = i + 1
    if (i == new_state_index) {
      break
    }
  }
  sum_data
}

graph <- get_table(data)

getDirectTable <-function(graph, nextSet) {
  
  initDirectTable <- function(graph) {
    columns <- graph$`Символ перехода` %>% unique() %>% .[!str_detect(., "СЛЕД")]
    columns <- c(sort(columns), "0")
    
    max <- graph$`Номер состояния`[nrow(graph)] + 1
    directTable <- data.frame(rep("", max + 1), stringsAsFactors = F)
    for (i in 2:length(columns))
      directTable %<>% cbind(data.frame(rep("", max + 1), stringsAsFactors = F))
    colnames(directTable) <- columns
    rownames(directTable) <- 0:max
    return(directTable)
  }
  
  dirTable <- initDirectTable(graph)
  for (i in 1:nrow(graph)) {
    s <- graph$`Номер состояния`[i]
    p <- graph$Переход[i]
    sp <- graph$`Символ перехода`[i]
    #отрицательные переходы
    if (p < 0) {
      sp <- str_sub(sp, str_length(sp))
      nextEl <- nextSet$Следующие[nextSet$Нетерминал == sp] %>% str_split(' ')
      nextEl <- nextEl[[1]]
      for (j in 1:length(nextEl)) {
        dirTable[s+1, nextEl[j]] = str_c("C", -p)
      }
    }
    #большие буквы
    else if (sp %>% str_detect("[:upper:]")) {
      dirTable[s+1, sp] = str_c("V", p)
    }
    #остальное
    else {
      dirTable[s+1, sp] = str_c("P", p)
    }
  }
  return(dirTable[sort(colnames(dirTable))])
}

dirTable <- getDirectTable(res, read_excel(path = "Множество следующиx.xlsx"))

