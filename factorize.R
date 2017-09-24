# This function takes a vector x and returns a factor representation of the same vector.
# The key advantage of factorize is that you can assign levels for infrequent categories,
# as well as empty and NA values. This makes it much easier to perform
# multidimensional/thematic analysis on your largest population subsets.
factorize <- function(
    x,  # vector to be transformed
    min_freq = .01,  # all levels < this % of records will be bucketed
    min_n = 1,  # all levels < this # of records will be bucketed
    NA_level = '(missing)',  # level created for NA values
    blank_level = '(blank)',  # level created for "" values
    infrequent_level = 'Other',  # level created for bucketing rare values
    infrequent_can_include_blank_and_NA = F,  # default NA and blank are not bucketed
    order = T,  # default to ordered
    reverse_order = F  # default to increasing order
) {
    x <- as.factor(x)
    if (class(x) == 'factor'){
        # suspect this is faster than reassigning new factor object
        levels(x) <- c(levels(x), NA_level, infrequent_level, blank_level)
    }
    
    # Swap out the NA and blank categories
    x[is.na(x)] <- NA_level
    x[x == ''] <- blank_level
    
    # Going to use this table to reorder
    f_tb <- table(x, useNA = 'always')
    
    # Which levels will be bucketed?
    infreq_set <- c(
        names(f_tb[f_tb < min_n]),
        names(f_tb[(f_tb/sum(f_tb)) < min_freq])
    )
    
    # If NA and/or blank were infrequent levels above, this prevents bucketing
    if(!infrequent_can_include_blank_and_NA){
        infreq_set <- infreq_set[!infreq_set %in% c(NA_level, blank_level)]
    }
    
    # Relabel all the infrequent choices
    x[x %in% infreq_set] <- infrequent_level

    # Return the reordered factor
    reorder(droplevels(x), rep(1-(2*reverse_order),length(x)), FUN = sum, order = order)
}
