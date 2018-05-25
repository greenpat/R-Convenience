# This ruleset applicator takes a dataframe with categorical data as well as a
# "ruleset" dataframe and returns the first matching rule along with some
# additional label. It supports negative rules as well as delimited values.

# As a mock example, based on what types of ingredients are included in a salad,
# a ruleset determines if it matches a specialty salad type

rule_applicator <- function(rules = rules, delim = '|', negation = '!', wildcard = 'any',
                            returned_col = tail(colnames(rules),1)) {
    
    # Basic approach:
    # Break "OR", negation, and wildcard logic into 3 matrices for easy evaluation
    
    # Which columns are used to determine rule match?
    attr(rules, 'rulecols') <- setdiff(colnames(rules), returned_col)
    
    # Row splitter utility - to transform rules into rules_parse table
    # Equivalent to tidyr::unnest()
    parsedf <- function(df, col_to_parse, delim){
        s = strsplit(df[[col_to_parse]], split=paste0('\\',delim))
        s[lengths(s)==0] <- ''
        df = df[rep(rownames(df), sapply(s, length)),]
        df[[col_to_parse]] = unlist(s)
        df
    }
    
    # This table will check "OR" logic for concatenated rules
    rules_delim <- rules
    for (n in attr(rules, 'rulecols')){
        rules_delim[[n]] <- sub(negation, '', rules_delim[[n]])
        rules_delim <- parsedf(rules_delim, n, delim)
    }
    
    # Used to collapse rule evaluations during ave() call
    key_dict <- setNames(!grepl('\\.',rownames(rules_delim)), gsub('\\.\\d+$','',rownames(rules_delim)))
    
    # This table will check negation on rules
    rules_neg <- data.frame(
        apply(rules[attr(rules, 'rulecols')], 2, function(x) grepl(negation,x))
    )
    
    # This table will check wildcard match in rules (matches everything)
    rules_wild <- data.frame(
        apply(rules[attr(rules, 'rulecols')], 2, function(x) x==wildcard)
    )
    
    # Heavily nested function demands explanation:
    # First we apply "OR" to the rules_delim against the record. lapply(...ave(...
    # Given A|B|C we want a single "TRUE" per attribute, per (original) rule
    # Then we map xor() between that result and rules_neg (mapply(xor...
    # Then we overwrite those with any wildcard matches (mapply(`|`...
    # And finally we check all columns have been satisfied with Reduce(`&`...
    function(record){
        which(
            Reduce(`&`,
                   mapply(`|`,
                          rules_wild,
                          mapply(xor,
                                 lapply(attr(rules, 'rulecols'), FUN = function(n) {
                                     ave(rules_delim[[n]]==record[[n]], names(key_dict),
                                         FUN = function(x) Reduce(`|`,x))[key_dict]
                                 }),
                                 rules_neg,
                                 SIMPLIFY = F
                          ),
                          SIMPLIFY = F
                   )
            )
        )[1]  # only first result
    }
}

# Mock data ---------------------------------------------------------------

n = 500
df <- data.frame(stringsAsFactors = F,
                 'Carbohydrate' = c('Lentil','Rice','Crouton','Pita')[rgeom(n,.6)%%3+1],
                 'Greens' = c('Lettuce','Arugula','Spinach','Kale')[sample(seq_len(4), n, T)],
                 'Protein' = c('Tofu','Beans','Shrimp','Chicken')[sample(seq_len(4), n, T)],
                 'Dressing' = c('Ranch','Russian','Bleu','Balsamic')[sample(seq_len(4), n, T)]
)

df$Recipe <- NA
aggregate(Dressing ~ Carbohydrate + Greens + Protein + Dressing, df, length)

rules <- data.frame(stringsAsFactors = F,
                    'Carbohydrate' = character(),
                    'Greens' = character(),
                    'Protein' = character(),
                    'Dressing' = character(),
                    'Recipe' = character()
)

rules[1,] <- c('!Crouton','Arugula','Beans','Balsamic','Healthy Greens')
rules[2,] <- c('Crouton','Lettuce|Arugula|Kale','Chicken','Ranch|Bleu','Cobb')
rules[3,] <- c('Pita','Kale','!Shrimp|Chicken','Bleu','Veggie Special')
rules[4,] <- c('Rice','Spinach','Shrimp','Russian','Seafood')
rules[5,] <- c('Lentil', 'Spinach','!Shrimp|Tofu|Chicken','any','Seasonal')
rules[6,] <- c('any', 'any', 'Tofu', 'any', 'Tofu Salad')
rules[7,] <- c('!Crouton|Pita', 'any', '!Beans', 'Balsamic', 'Low Carb')

# Leave F unless debugging
if (F){
    delim = '|'
    negation = '!'
    wildcard = 'any'
}

# Usage -------------------------------------------------------------------

# Closure for this particular setup
salad_rules <- rule_applicator(rules, '|', '!', 'any', 'Recipe')

# Quick loop application
df$Recipe <- 0
for (i in 1:nrow(df)){
    df$Recipe[i] <- salad_rules(df[i,])
}

# Bind on the recipe names
df$Recipe_Name <- rules$Recipe[df$Recipe]

# Summary of results
table(df$Recipe_Name, useNA = 'always')