factorize <- function(
    x,
    min_n = 1,
    min_freq = .00,
    NA_level = '(missing)',
    blank_level = '(blank)',
    infrequent_level = 'Other',
    infrequent_can_include_blank_and_NA = F,
    order = T,
    reverse_order = F
) {
    x <- as.factor(x)
    if (class(x) == 'factor'){
        levels(x) <- c(levels(x), NA_level, infrequent_level, blank_level)
    }
    
    x[is.na(x)] <- NA_level
    x[x == ''] <- blank_level
    
    f_tb <- table(x, useNA = 'always')
    
    infreq_set <- c(
        names(f_tb[f_tb < min_n]),
        names(f_tb[(f_tb/sum(f_tb)) < min_freq])
    )
    
    if(!greedy_infrequent){
        infreq_set <- infreq_set[!infreq_set %in% c(NA_level, blank_level)]
    }
    
    x[x %in% infreq_set] <- infrequent_level

    reorder(droplevels(x), rep(1-(2*reverse_order),length(x)), FUN = sum, order = order)
}
