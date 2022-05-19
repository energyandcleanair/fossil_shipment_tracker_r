# flow_mat = matrix(c(100, 100,0,0,-100,-5,90,5,0,-90,10,100,0,-5,-100,-105),
#        nrow=4, ncol=4)




# Approach 1: net flows ---------------------------------------------------
process_old <- function(consolidated_date){

  net_total <- consolidated_date %>%
    group_by(country=to_country) %>%
    summarise(import=sum(value, na.rm=T)) %>%
    left_join(
      consolidated_date %>%
        group_by(country=from_country) %>%
        summarise(export=sum(value, na.rm=T))
    ) %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
    mutate(net_import=import-export) %>%
    arrange(desc(net_import))


  U = net_total$country[net_total$net_import>=0]
  P = net_total$country[net_total$net_import<0]


  net <- consolidated_date %>%
    group_by(country=to_country, partner=from_country) %>%
    summarise(import=sum(value, na.rm=T)) %>%
    left_join(
      consolidated_date %>%
        group_by(country=from_country, partner=to_country) %>%
        summarise(export=sum(value, na.rm=T)),
      by=c("country", "partner")
    ) %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
    mutate(net_import=import-export,
           net_export=export-import) %>%
    arrange(country, partner)


  # Spp: share of what is exported from producer P that is actually from other P
  (Spp <- net %>%
      select(country, partner, net_export) %>%
      mutate(net_export=pmax(0, net_export)/1e9) %>%
      spread(partner, net_export) %>%
      filter(country %in% P) %>%
      ungroup() %>%
      select_at(c("country", sort(P))))




  Ipa <- net %>%
    select(country, partner, net_import) %>%
    spread(partner, net_import) %>%
    filter(country %in% U) %>%
    ungroup() %>%
    select_at(c("country", P))

  Ipa_mat = Ipa[match(U, Ipa$country), -1] %>% as.matrix()

  Cab <- net %>%
    filter(country %in% U,
           partner %in% U) %>%
    mutate(net_import_pos_only=pmax(net_import, 0)) %>%
    select(country, partner, net_import_pos_only) %>%
    spread(partner, net_import_pos_only) %>%
    ungroup() %>%
    select_at(c("country", U))


  Cab_mat = Cab[match(U, Cab$country), -1] %>% as.matrix()


  W <- net_total %>%
    select(country, net_import) %>%
    filter(country %in% U)

  W_mat = W[match(U, W$country), -1]  %>% as.matrix()

  Spa <- bind_cols(country=Ipa$country, as.data.frame(t(scale(t(Ipa[,P]), center = FALSE,
                                                              scale = colSums(t(Ipa[,P]), na.rm=T))))) %>%
    tibble() %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
    select_at(c("country", P))


  Spa_mat = Spa[match(U, Spa$country),-1] %>% as.matrix()

  # From Russia to all countries
  bind_cols(tibble(country=U),as.data.frame(Ipa_mat) / 1e9)
  bind_cols(tibble(country=U),as.data.frame(Cab_mat / 1e9))
  bind_cols(tibble(country=U),as.data.frame(Spa_mat))
  bind_cols(tibble(country=U),as.data.frame(Cab_mat %*% Spa_mat / 1e9))

  # Side 1
  side_1 = bind_cols(tibble(country=U),as.data.frame(Cab_mat %*% Spa_mat / 1e9 + Ipa_mat / 1e9))


  # Side 2
  # Cba_mat = -t(Cab_mat)
  Cb_mat = colSums(Cab_mat) %>%
    as.matrix()

  Yb = sweep(Spa_mat, MARGIN=1, Xb, `*`)

  side_2 = bind_cols(tibble(country=U), as.data.frame(Yb / 1e9))


  result <- bind_rows(
    gather(side_1, key = "from_country", value = "value", -country) %>%
      mutate(method="Inflow") %>%
      rename(to_country=country),
    gather(side_2, key = "from_country", value = "value", -country) %>%
      mutate(method="Outflow") %>%
      rename(to_country=country))

  return(result)
}


# Approach2: net flows + iterative ----------------------------------------------------
# Ensure all countries are both in from_country and to_country
# so as to have a square matrix
consolidate <- function(d){
  all_countries = unique(c(d$from_country, d$to_country))
  d %>%
    full_join(tidyr::crossing(from_country=all_countries,
                              to_country=all_countries),
              by=c("to_country", "from_country")
    ) %>%
    mutate(value=replace_na(value, 0))
}

# "Netize" so that there is no A <-> B
netize <- function(d){
  d %>%
    ungroup() %>%
    left_join(
      d %>%
        ungroup() %>%
        select(from_country=to_country,
               to_country=from_country,
               value_opposite=value) %>%
        filter(from_country!=to_country), #We keep production
      by=c("to_country", "from_country")
    ) %>%
    mutate(value_opposite=tidyr::replace_na(value_opposite, 0)) %>%
    mutate(value=pmax(0, value-value_opposite)) %>%
    # mutate(value=value-value_opposite)) %>%
    select(-c(value_opposite))
}

to_flow_mat <- function(d){

  d <- d %>%
    group_by(country=to_country, partner=from_country) %>%
    summarise(import=sum(value, na.rm=T)) %>%
    left_join(
      d %>%
        group_by(country=from_country, partner=to_country) %>%
        summarise(export=sum(value, na.rm=T)),
      by=c("country", "partner")
    ) %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
    mutate(net_import=import-export,
           net_export=export-import) %>%
    arrange(country, partner) %>%
    # filter(country %in% c, partner %in% c) %>%
    ungroup() %>%
    # mutate(across(net_import, net_export, export, import), as.numeric)) %>%
    select(country, partner, net_import) %>%
    spread(partner, net_import)

  flow_mat <- as.matrix(d %>% select(-c(country))) %>% `dimnames<-`(list(d$country,d$country))

  # Add production consumption on diagonal
  diag(flow_mat) <- -rowSums(flow_mat)
  all(rowSums(flow_mat) < 1e-4) #should be equal to 0
  return(flow_mat)
}

distribute <- function(pos){

  prod = diag(pos)

  i_ukraine <- which(dimnames(pos)[[1]]=="Ukraine")
  i_russia <- which(dimnames(pos)[[1]]=="Russia")

  n = length(prod)
  for(i in seq(n)){
    # sapply(seq(n), function(i){
    new_posi = diag(diag(pos))[i,]
    for(j in seq(n)){
      # sapply(seq(n), function(j){
      if(i==j){
        # Do nothing
        # diag(diag(pos))[i,]
      }else{

        pos_j = pos[j,]
        # Ukraine special case: Ukraine cannot reimport gas from Russia
        if((length(i_ukraine)>0) && (i==i_ukraine)){
          pos_j[i_russia] <- 0
        }

        add <- if(sum(pos_j)>0){pos[i,j] * pos_j/sum(pos_j)}else{0}


        # print(add)
        new_posi = new_posi + add
        # What was added needs to be removed, sort of
        # by passing the original
        pos[j,] = pos[j,] - add
        pos[j,j] = prod[j]
      }
    }

    # if(i==i_ukraine){
    #   print(pos[i,])
    #   print(new_posi)
    # }

    pos[i,] = new_posi
    # print(pos)
  }

  return(pos)
}


# cap <- function(pos){
#   prod = diag(pos)
#
#   sapply(seq(n), function(i){
#     new_posi = pos[i,]
#
#     to_scaledown = which(new_posi > prod * 1.1) # There are numerical rounding
#     i <- 0
#     nmax <- 1000
#     while(length(to_scaledown)>0 & (i<nmax)){
#       i <- i + 1
#       toscale_idx = to_scaledown[1]
#       diff = new_posi[toscale_idx] - prod[toscale_idx]
#       total_others = sum(new_posi) - new_posi[toscale_idx]
#       new_posi = sapply(seq(n), function(j){
#         if(j==toscale_idx){
#           prod[toscale_idx]
#         }else{
#           new_posi[j] + diff * new_posi[j]/total_others
#         }
#       })
#       to_scaledown = which(new_posi > prod)
#     }
#     if(i>nmax){
#       print("Failed to converge")
#     }
#     return(new_posi)
#   }) %>%
#     t() %>%
#     `dimnames<-`(dimnames(pos))
# }

process_iterative <- function(flows){

  options(dplyr.summarise.inform = FALSE)

  if(("date" %in% names(flows)) && (length(unique(flows$date))>1)){
    stop("This function should only work on one date at a time.
       Please aggregate if need be.")
  }

  # flows_test <- tibble(
  #   from_country=c("1","2","4","2"),
  #   to_country=c("2","3","2","5"),
  #   value=c(100,15,50,285)
  # )
  #
  # flow_mat  <- flows_test %>%
  #   consolidate() %>%
  #   netize() %>%
  #   to_flow_mat()

  # Ukraine just a transmitter of Russian Gas
  # Not necessarily valid in 2020
  # scaling <- flows %>% filter(from_country=="Russia", to_country=="Ukraine") %>% pull(value) %>% sum() /
  #   flows %>% filter(from_country=="Ukraine") %>% pull(value) %>% sum()
  #
  # print(abs(scaling-1))


  sanity_check <- function(flows, flow_mat, pos){

    # We use flow_mat to have consumption
    pos_without_prod <- pos %>% `diag<-`(0)

    # Country consumption
    # consumption <- flows %>% group_by(country=from_country) %>% summarise(export=sum(value, T)) %>%
    #   left_join(flows %>% group_by(country=to_country) %>% summarise(import=sum(value, T))) %>%
    #   mutate(consumption=tidyr::replace_na(import, 0) - tidyr::replace_na(export, 0))
    #
    # consumption %>% select(country, consumption) %>%
    #   left_join(
    #     rowSums(pos_without_prod) %>% as.data.frame() %>% `names<-`('value') %>% mutate(country=rownames(.)) %>% tibble()
    #   )


    nearly_equal <- function(mat1, mat2, abs_threshold=1){
      all(abs(mat1-mat2) < abs_threshold)
    }

    ok <- TRUE
    ok <- ok &  nearly_equal(colSums(pos_without_prod), rowSums(pos_without_prod) + diag(flow_mat))
    ok <- ok &  all(dimnames(pos)[[1]]==dimnames(pos)[[2]])
    if(ok){
      # print("OK")
    }else{
      print("ERROR")
    }
    return(ok)
  }


  flows <- flows %>%
    mutate(
      # value = ifelse(from_country=="Ukraine", value*scaling, value),
      from_country=recode(from_country, "Ukraine"="Russia")) %>%
    mutate(value=ifelse((from_country=='Russia') & (to_country=='Ukraine'), 0, value))


  flow_mat <- flows %>%
    consolidate() %>%
    netize() %>%
    to_flow_mat()

  pos = pmax(flow_mat, 0)
  sanity_check(flows, flow_mat, pos)

  n <- dim(pos)[1] # Matrix must be square
  if(dim(pos)[1]!=dim(pos)[2]){stop("Matrix not square")}

  new_pos = pos

  for(i in seq(n)){
    old_pos = new_pos
    new_pos = distribute(new_pos)
    if(dim(new_pos)[1]!=dim(new_pos)[2]){stop("Matrix not square")}
    # if(all(old_pos == new_pos)){
    #   print(sprintf("leaving at iteration %d",i))
    # }
  }




  sanity_check(flows, flow_mat, pos)
  sanity_check(flows, flow_mat, new_pos)

  # Shouldn't be useful anymore
  # new_pos <- cap(new_pos)
  # sanity_check(flow_mat, new_pos)
  diag(new_pos) <- 0

  all_countries = dimnames(new_pos)[[1]]

  bind_cols(country=sort(all_countries), as.data.frame(new_pos)) %>%
    gather(key = "from_country", value = "value", -country) %>%
    rename(to_country=country)
}
