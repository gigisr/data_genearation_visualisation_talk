odds_tbl <- data.frame(
    out_value = c( "Aviation", "Motor", "Public Liability" ),
    odds = c( 0.6, 0.2, 0.2 ),
    cumulative_odds = c( 0.6, 0.8, 1 )
)
odds_tbl %>% write_csv( 'assets/odds_tbl.csv' )

runif( 1000 ) %>% 
    cut( 
        c( 0, odds_tbl %>% select( cumulative_odds ) %>% .[[1]] ),
        labels = odds_tbl %>% select( out_value ) %>% .[[1]]
    ) %>% 
    data.frame( values = . ) %>% 
    group_by( values ) %>% 
    summarise(
        count = n()
    ) %>% 
    mutate(
        pct = round( count * 100 / sum( count ), 3 )
    )

odds_level_2_tbl <- read_csv( 'assets/odds_level_2_tbl.csv' )

temp <- function ( n, filter_val ) {
    
    temp_tbl <- odds_level_2_tbl %>% 
        filter( filter_1 == filter_val[1] )
    
    out_vals <- runif( n ) %>% 
        cut(
            c( 0, temp_tbl %>% select( cumulative_odds ) %>% .[[1]] ),
            labels = temp_tbl %>% select( out_value ) %>% .[[1]]
        )
    
    return( out_vals )
    
}

runif( 3000 ) %>% 
    cut( 
        c( 0, odds_tbl %>% select( cumulative_odds ) %>% .[[1]] ),
        labels = odds_tbl %>% select( out_value ) %>% .[[1]]
    ) %>% 
    data_frame( value = . ) %>% 
    group_by( value ) %>% 
    mutate(
        value_level_2 = temp( n(), value )
    ) %>% 
    group_by( value, value_level_2 ) %>% 
    summarise(
        count = n()
    ) %>% 
    mutate(
        pct = round( count * 100 / sum( count ), 1 )
    ) %>% 
    ungroup
