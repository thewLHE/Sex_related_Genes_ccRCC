library(openxlsx)
library(tidyverse)
library(readxl)
library(dplyr)
library(stats)
library(knitr)
library(kableExtra)

info <- read.csv("koreamerged0706_ccRCC_1panelv2.csv")
gdata <- info[, 16:ncol(info)]
cdata <- info[, 1:15]
# Create a combined data frame to store the results
combined_results <- data.frame()

for (f_cli in colnames(cdata)[3]) {
  print(f_cli)
  
  # Create an empty data frame for each f_cli
  f_cli_results <- data.frame(g_cli = character(),
                              cross_table = character(),
                              p_value = numeric(),
                              ci_lower = numeric(),
                              ci_upper = numeric(),
                              odds_ratio = numeric(),
                              stringsAsFactors = FALSE)
  
  # Iterate over columns in gdata
  for (g_cli in colnames(gdata)) {
    print(g_cli)
    c <- f_cli
    g <- g_cli
    filter_data <- info[, c(c, g)]
    
    # Create cross table
    cross_table <- table(filter_data)
    print(cross_table)
    
    # Perform Fisher's test if the cross table is valid
    if (nrow(cross_table) >= 2 && ncol(cross_table) >= 2) {
      result <- tryCatch(
        fisher.test(cross_table, simulate.p.value = False),
        error = function(e) {
          return(NULL)  # Return NULL if Fisher's test fails
        }
      )
      print(result)
    } else {
      result <- NULL  # Set result as NULL if cross table is not valid
    }
    
    # Extract p-value, confidence interval, and odds ratio from the result
    if (!is.null(result)) {
      p_value <- sprintf("%.100f", result$p.value)
      #p_value <- result$p.value
      ci_lower <- result$conf.int[1]
      ci_upper <- result$conf.int[2]
      odds_ratio <- result$estimate
    } else {
      p_value <- NA
      ci_lower <- NA
      ci_upper <- NA
      odds_ratio <- NA
    }
    
    # Append results to f_cli_results
    f_cli_results <- rbind(f_cli_results,
                           data.frame(g_cli = g_cli,
                                      cross_table = paste0(collapse = "_", as.character(cross_table)),
                                      p_value = p_value,
                                      ci_lower = ci_lower,
                                      ci_upper = ci_upper,
                                      odds_ratio = odds_ratio,
                                      stringsAsFactors = FALSE))
  }
  
  # Append f_cli_results to combined_results
  combined_results <- rbind(combined_results, f_cli_results)
}

# Save combined results to a CSV file
combined_results_filename <- "Fisher_results_Korean0719.csv"
write.csv(combined_results, file = combined_results_filename, row.names = FALSE, fileEncoding = "UTF-8")

# Print the combined_results data frame
combined_results
