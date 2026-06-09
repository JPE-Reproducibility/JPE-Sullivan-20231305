process.cuisines <- function(cuisines){
    cuisines <- tolower(cuisines)
    cuisine.subs <- c('american (new)' = 'american',
                      'new american' = 'american',
                      'sandwich' = 'sandwiches',
                      'coffee and tea' = 'coffee',
                      'coffee & tea' = 'coffee',
                      'breakfast & brunch' = 'breakfast',
                      'breakfast and brunch' = 'breakfast',
                      'burger' = 'burgers',
                      'hamburgers' = 'burgers',
                      'salad'  = 'salads',
                      'new mexican' = 'mexican',
                      'lunch specials' = 'lunch',
                      'ice cream + frozen yogurt' = 'ice cream',
                      'ice cream & frozen yogurt' = 'ice cream',
                      'sushi bars' = 'sushi',
                      'american (traditional)' = 'american',
                      'vegetarian friendly' = 'vegetarian',
                      'kids friendly' = 'family friendly',
                      'asian food' = 'asian',
                      'barbecue' = 'bbq',
                      'barbeque' = 'bbq',
                      'donut' = 'donuts',
                      'pizzeria' = 'pizza',
                      'traditional american' = 'american',
                      'juice bar' = 'juices',
                      'smoothies and juices' = 'juices',
                      'juice and smoothies' = 'juices',
                      'burrito' = 'burritos',
                      'taco' = 'tacos',
                      'middle east' = 'middle eastern',
                      'wine & spirits' = 'wine')
    for (k in 1:length(cuisine.subs)){
        idx <- which(cuisines == names(cuisine.subs)[k])
        cuisines[idx] <- cuisine.subs[k]
    }
    return(cuisines)
}
