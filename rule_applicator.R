# This ruleset applicator takes a dataframe with categorical data as well as a
# "ruleset" dataframe and returns the first matching rule along with some
# additional label. It supports negative rules as well as concatenated values.

# As a mock example, based on what types of ingredients are included in a salad,
# a ruleset determines if it matches a specialty salad

# Mock data ---------------------------------------------------------------

n = 500
df <- data.frame(
    'Carbohydrate' = c('Lentil','Rice','Crouton','Pita')[rgeom(n,.6)%%3+1],
    'Greens' = c('Lettuce','Arugula','Spinach','Kale')[sample(seq_len(4), n, T)],
    'Protein' = c('Tofu','Beans','Shrimp','Chicken')[sample(seq_len(4), n, T)],
    'Dressing' = c('Ranch','Russian','Bleu','Balsamic')[sample(seq_len(4), n, T)]
)

rules <- data.frame(stringsAsFactors = F,
    'Carbohydrate' = character(),
    'Greens' = character(),
    'Protein' = character(),
    'Dressing' = character(),
    'Recipe' = character()
)

rules[1,] <- c('!Crouton','Arugula','Beans','Balsamic','Healthy Greens')
rules[2,] <- c('Crouton','Lettuce|Arugula','Chicken','Ranch','Cobb')
rules[3,] <- c('Pita','Kale','!Shrimp|Chicken','Bleu','Veggie Special')
rules[4,] <- c('Rice','Spinach','Shrimp','Russian','Seafood')

record <- df[1,]
delim = '|'
negation = '!'

rule_applicator(rules = rules, delim = '|', negation = '!'){
    
    # Derive which columns are shared with input (and therefore drive decisions)
    attr(rules, 'rulecols') <- intersect(colnames(rules), colnames(record))
    
    # Generate keys to eventually report back which rule was triggered
    rownames(rules) <- do.call(function(...) paste(..., sep='.:.'), rules[,attr(rules, 'rulecols')])
    
    # Create negation columns 
    for (n in attr(rules, 'rulecols')){
        rules[[paste0(n,'_N')]] <- grepl(negation,rules[[n]])
        rules[[n]] <- sub(negation,'',rules[[n]])
    }
    
    parsedf <- function(rules = rules, col_to_parse, delim = '|'){
        s = strsplit(rules[[col_to_parse]], split=paste0('\\',delim)
        return()
    }
    
    lapply(attr(rules, 'rulecols'), FUN = function(n) rules[[n]]==record[[n]])
    
}
