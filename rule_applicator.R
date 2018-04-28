# TODO:
# This approach needs refactoring... given a rule "!A|B|C" need to break apart
# into OR(A|B|C), flatten to single result, then apply negation across rules,
# then finally return the first fitting rule.

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
record[] <- c('Pita','Kale','Tofu','Bleu')
delim = '|'
negation = '!'
returned_col = 'Recipe'

rule_applicator(rules = rules, delim = '|', negation = '!',
                returned_col = tail(colnames(rules),1)){
    
    # Derive which columns are shared with input (and therefore drive decisions)
    attr(rules, 'rulecols') <- setdiff(colnames(rules), returned_col)
    
    # Row splitter utility - to transform rules into rules_parse table
    # Equivalent to tidyr::unnest()
    parsedf <- function(df, col_to_parse, delim){
        s = strsplit(df[[col_to_parse]], split=paste0('\\',delim))
        df = df[rep(rownames(df), sapply(s, length)),]
        df[[col_to_parse]] = unlist(s)
        df
    }
    
    # This table will check negation on rules
    rules_neg <- data.frame(
        apply(rules, 2, function(x) grepl(negation,x))
    )
    
    # This table will check "OR" logic for concatenated rules
    rules_delim <- rules
    for (n in attr(rules, 'rulecols')){
        rules_delim[[n]] <- sub(negation, '', rules_delim[[n]])
        rules_delim <- parsedf(rules_delim)
    }
    
    # Used to collapse rule evaluations
    key_dict <- setNames(!grepl('\\.',rownames(rules_parse)), gsub('\\.\\d+$','',rownames(rules_parse)))
    
    eval_attrib <- function(n){
        xor(
            ave(rules_parse[[n]]==record[[n]], names(key_dict), FUN = function(x) Reduce(`|`,x))[key_dict],
            rules[[paste0(n,'_N')]]
        )
    }
    
    eval_record <- function(record){}
    
    # Nested logic deserves special commentary
    # at lowest level, ave checks "OR" against parsed out rows per rule
    # that is wrapped in lapply to cover all columns
    # you can then just take the top record for each rule (per ave behavior)
    # top record indexed by key_dict
    lapply(attr(rules, 'rulecols'), FUN = function(n) {
        ave(rules_parse[[n]]==record[[n]], names(key_dict), FUN = function(x) Reduce(`|`,x))[key_dict]
    })
    
    # Per record, this will return which rules were matched (OR logic)
    lapply(attr(rules, 'rulecols'), FUN = function(n) {
        rules[[n]]==record[[n]]
    })
    record_eval_or
    
}
