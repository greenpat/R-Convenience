# This function investigates potential cardinality between two vectors.
# Based on the observed combinations of A and B the function will return one of:
# "One:One", "One:Many", "Many:One", "Many:Many"
# In general, joins should occur before calling cardinality_test().
# This function is meant for exploratory analysis, and there is no certain
# method to derive what the true entity relationship is.

cardinality_test <- function(vector_a, vector_b){
    
    # Table of observed combinations
    df <- unique(data.frame(
        'a' = vector_a,
        'b' = vector_b
    ))
    
    # Most observations per single category on each side
    a_to_b <- max(aggregate(a ~ b, df, length)$a, na.rm=T)
    b_to_a <- max(aggregate(b ~ a, df, length)$b, na.rm=T)

    
    # Convert to text
    a_to_b <- ifelse(a_to_b > 1, 'Many', 'One')
    b_to_a <- ifelse(b_to_a > 1, 'Many', 'One')
    
    # Return
    paste0(a_to_b,':',b_to_a)
}
