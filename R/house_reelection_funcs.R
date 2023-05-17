
prep_votes_rc <- function(votes_file) {
  votes <- read_csv(votes_file) |> 
    # Subset to congresses in house member data set
    filter(congress %in% 93:117) |> 
    mutate(yea = case_when(cast_code %in% 1:3 ~ 1,
                           cast_code %in% 4:6 ~ 0,
                           .default = NA),
           icpsr = as.character(icpsr)) |> 
    filter(!is.na(yea)) |> 
    dplyr::select(congress, rollnumber, icpsr, yea)
  
  # List element for each congress
  votes_list <- split(votes, votes$congress)
  
  # Legislator - vote matrix
  votes_list_wide <- map(votes_list, 
                         pivot_wider,
                         id_cols = icpsr,
                         values_from = yea,
                         names_from = rollnumber)
  
  # pscl roll call object
  votes_list_rc <- pmap(tibble(data = map(votes_list_wide, dplyr::select, -icpsr),
                               legis.names = map(votes_list_wide, ~.x$icpsr)),
                        pscl::rollcall)
  
  return(votes_list_rc)
}
# a<-prep_votes(here::here("data-raw", "Hall_votes.csv"))
# c <- a[[1]]
# d <- a[[1]] |> dplyr::select(-icpsr)
# 
# f <- pscl::rollcall(d, legis.names = c$icpsr)
# 
# 
# 
# a[[1]] |> 
#   pscl::ideal(
#         dropList = list(lop = NA),
#         normalize = TRUE)

