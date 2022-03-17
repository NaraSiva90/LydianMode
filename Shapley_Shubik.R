# Code to examine election results through Shapley-Shubik power
# Author: Narayanan Sivasailam
# Date: March 2022.



# Load required packages --------------------------------------------------

source("THEME_WAB.R") # Loads Tidyverse, knitr, kableExtra, formattable, showtext libraries.
library(combinat)
library(openxlsx)



# Key functions ----------------------------------------------------

shapley_shubik <- function(parl, threshold) {
    
    ## Generate names integer-vector for ease of operations.
    parl %>% pull(Seats) -> seats
    names(seats) <- parl %>% pull(Faction)
    
    ## Generate permutations and calculate cumulative sums for each.
    lapply(combinat::permn(seats), cumsum) -> cum_seats

    
    ## For each permutation, find faction pushing the 'bill' to required majority.
    sapply(cum_seats, 
           function(k) names(k)[min(which(k >= sum(seats) * threshold))]) %>% 
        table() %>% 
        as.data.frame() -> SS_kingmakers
    names(SS_kingmakers) <- c("Faction", "SS_power")
    
    ## Append summary stat to ensure that all factions are represented. 
    right_join(SS_kingmakers, parl, by = "Faction") -> SS_output
    SS_output %>% pull(SS_power)/length(cum_seats) -> SS_index
    names(SS_index) <- SS_output$Faction
    
    SS_index[names(seats)]
}



# Preliminary Test Cases --------------------------------------------------

parl1 <- data.frame(Faction = , Seats = c(45, 35, 20))

sapply(0.1 * c(5:9), shapley_shubik, parl = parl1) %>% t %>% as.data.frame() %>% 
    mutate(Pass = 0.1 * c(5:9)) %>% 
    publish_wab()



# India Assembly Elections 2022 -------------------------------------------

sheetnames <- openxlsx::getSheetNames("India_Elections_2022_data.xlsx")
lapply(sheetnames, 
       function(x) openxlsx::read.xlsx("India_Elections_2022_data.xlsx", sheet = x)) -> election_results
names(election_results) <- sheetnames

lapply(election_results, function(STATE) {
    Pass_mark <- c(50:99)/100
    
    sapply(Pass_mark, function(x) shapley_shubik(STATE, x)) %>% 
        t() -> SS_state
    
    SS_state <- as.data.frame(SS_state)
    SS_state <- SS_state %>% mutate(Pass = Pass_mark)
    # rownames(SS_state) <- paste0(100 * Pass_mark, "%")
    
    SS_state
}) -> SS_results

names(SS_results) <- sheetnames



# Outputs -----------------------------------------------------------------

# openxlsx::write.xlsx(SS_results, "India_Elections_SSResults.xlsx")

lapply(1:5, function(STATE) {
    # browser()
    SS_results[[STATE]] %>% gather("Faction", "SS_index", -Pass) %>% 
        ggplot(., aes(x = Pass, y = SS_index, colour = Faction)) + geom_line(size = 2, alpha = 0.5) + 
        labs(title = paste0("Shapley Shubik Index for ", sheetnames[STATE]), 
             subtitle = "Share of permutations where Faction is Pivotal for Legislation passage",
             x = "Minimum vote-share required to Pass legislation", 
             y = "Shapley-Shubik Power")
})