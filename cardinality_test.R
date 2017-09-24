# This function investigates potential cardinality between two vectors.
# Based on the observed combinations of A and B the function will return one of:
# "One:One", "One:Many", "Many:One", "Many:Many"
# In general, joins should occur before calling cardinality_test().
# This function is meant for exploratory analysis, and there is no certain
# method to derive what the true entity relationship is.

cardinality_test <- function(vector_a, vector_b){
    # Tabulate observed combinations
    card_tb <- table(vector_a, vector_b)
    
    # What is the greatest number of observed "A" choices for all "B" choices?
    card_a <- max(apply(card_tb, 2, function(x) sum(x!=0)))
    
    # What is the greatest number of observed "B" choices for all "A" choices?
    card_b <- max(apply(card_tb, 1, function(x) sum(x!=0)))
    
    # Convert to language
    card_a <- ifelse(card_a == 1, 'One', 'Many')
    card_b <- ifelse(card_b == 1, 'One', 'Many')
    
    # Returned value
    paste0(card_a,':',card_b)
}
