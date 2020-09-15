
fun_check_prob_selectMeth <- function(variables) {
  
  
  
}

fun_replace_selectMeth <- function(input_list, selectMeth = ) {
  
  test_1 <- rapply(test$su1, function(x) ifelse(test$su1$selectMeth == "NPAH","SRSWR",x), how = "replace")
  
}


correct_data <- function(input_list = H1_upper) {
  
  
  for (i in c("VS", "FT", "FO", "SA")) {
    
    eval(parse(text = paste0("input_list[[i]]$", i, "selectMeth <- 'SRSWR'")))
    
    eval(parse(text = paste0("input_list[[i]]$", i, "sampled", "[is.na(input_list[[i]]$", i, "sampled)] <- 100")))
    
    eval(parse(text = paste0("input_list[[i]]$", i, "sampled", "[is.na(input_list[[i]]$", i, "sampled)] <- 100")))
    
  }
  
  return(input_list)
  
}


fun_add_probs <- function(input_list = H1_upper, type_of_probs = "inclusion") {
  
  for (i in c("VS", "FT", "FO", "SA")) {
    
    for (j in 1:nrow(input_list[[i]])) {
      
      dat <- input_list[[i]][j,]
      
      if (dat$prob > 0) {
        
        "don't do anything"
        
      } else if (dat$prob < 0 | is.na(dat$prob)) {
        
        dat$prob <- generate_probs(x = dat, type = type_of_probs)
        
        dat_with_prob <- rbind(dat_with_prob, dat)
        
      }
    }
    
    dat_with_prob_out <- as.matrix(dat_with_prob$prob)
    rownames(data_with_prob_out) <- c(as.character(data_with_prob$id))
    
    if (type_of_probs == "inclusion") {
      eval(parse(text = paste0("input_list$su", i, "$comb_inc_probs", " = dat_with_prob_out")))
   
    } else if (type_of_probs == "selection") {
      
      eval(parse(text = paste0("input_list$su", i, "$comb_select_probs", " = dat_with_prob_out")))
      
    }
    
    
    
    
  }
  
  return(input_list)
    
  }
  




if (input_list[[1]]$designTable$prob > 0 | is.na(input_list[[1]]$designTable$prob)) {
  
  inc_prob <- input_list[[1]]$designTable$prob
}