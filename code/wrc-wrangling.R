## ---- get_splits_wide --------
get_splits_wide = function(splits){
  driver_splits = get_driver_splits(splits)
  
  split_cols =  get_split_cols(splits)
  splits_cols = c('entryId', 'splitPointId', 'elapsedDurationS')
  
  splits_wide = driver_splits %>% 
    group_by(entryId) %>%
    select(all_of(splits_cols)) %>%
    tidyr::spread(key = splitPointId,
                  value = elapsedDurationS) %>%
    select(all_of(c('entryId', split_cols))) %>%
    # If we don't cast, it's a
    # non-rankable rowwise df
    as.data.frame()
  
  splits_wide
}

## ---- get_split_duration --------
get_split_duration = function(df, split_cols,
                              retId=TRUE, idcol='entryId') {
  
  # [-1] drops the first column, [-ncol()] drops the last
  df_ = df[,split_cols][-1] - df[,split_cols][-ncol(df[,split_cols])]
  
  # The split time to the first split is simply the first split time
  df_[split_cols[1]] = df[split_cols[1]]
  
  if (retId) {
    # Add in the entryId column
    df_[[idcol]] = df[[idcol]]
    
    # Return the dataframe in a sensible column order
    df_ %>% select(all_of(c(idcol, split_cols)))
  } else {
    df_
  }
  
}

## ---- map_stage_codes --------
map_stage_codes = function(df, stage_list) {
  # Get stage codes lookup id->code
  stages_lookup_code = get_stages_lookup(stages, 'stageId', 'code')
  
  #https://stackoverflow.com/a/34299333/454773
  plyr::rename(df, replace = stages_lookup_code,
               warn_missing = FALSE)
}

## ---- map_driver_names --------
map_driver_names = function(df, cars){
  df %>%
    merge(cars[,c('entryId','code')],
          by='entryId')  %>%
    # Limit columns and set column order
    select(-'entryId') %>%
    # Move last column to first
    select('code', everything())
}

## ---- relabel_times_df --------
relabel_times_df = function(df, stage_list, cars) {
  df %>%  
    map_stage_codes(stage_list) %>%
    map_driver_names(cars)
}

## ---- get_split_locations --------
get_split_locations = function(splits){
  splits_locations = splits$splitPoints
  splits_locations %>% arrange(number)
}