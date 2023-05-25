
prep_votes_rc <- function(votes_file, legis_file, congress_list) {
  legis_party <- readxl::read_xls(legis_file) |> 
    janitor::clean_names() |> 
    mutate(republican = ifelse(x100_dem_200_rep_other == 200, 1, 0),
           icpsr = as.character(icpsr_number_according_to_poole_and_rosenthal)) |> 
    dplyr::select(icpsr, republican) |> 
    distinct() |> 
    # sorry party switchers--you are deleted
    group_by(icpsr) |> 
    filter(n() == 1)
  
  votes <- read_csv(votes_file) |> 
    # Subset to congresses in house member data set
    filter(congress %in% congress_list) |> 
    mutate(yea = case_when(cast_code %in% 1:3 ~ 1,
                           cast_code %in% 4:6 ~ 0,
                           .default = NA),
           icpsr = as.character(icpsr),
           rollnumber = as.character(rollnumber)) |> 
    filter(!is.na(yea)) |> 
    group_by(rollnumber) |> 
    mutate(pct_yes = mean(yea == 1)) |> 
    # get rid of unanimous votes
    filter(pct_yes > .2 & pct_yes < .8) |> 
    group_by(congress) |> 
    filter(rollnumber %in% sample(unique(rollnumber), 50)) |>
    ungroup() |> 
    dplyr::select(congress, rollnumber, icpsr, yea) |> 
    left_join(legis_party, by = "icpsr")
  
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
  
  return(lst(votes_list_rc, votes_list))
}


process_ideal_points <- function(ideal_obj, congress) {
  ideal_summaries <- as_tibble(ideal_obj$x) |> 
    pivot_longer(cols = everything(),
                 names_to = "icpsr",
                 values_to = "ideal_point") |> 
    group_by(icpsr) |> 
    mutate(mu = mean(ideal_point),
           sigma = sd(ideal_point)) |> 
    nest_by() |> 
    mutate(skew_mod = list(selm(ideal_point ~ 1, data = data)),
           skew_dp = list(extractSECdistr(skew_mod)),
           xi = slot(skew_dp, "dp")[["xi"]],
           omega = slot(skew_dp, "dp")[["omega"]],
           alpha = slot(skew_dp, "dp")[["alpha"]]) |> 
    unnest(data) |> 
    dplyr::select(icpsr, mu, sigma, xi, omega, alpha) |> 
    distinct() |> 
    mutate(icpsr = str_remove(icpsr, ".D1"),
           # Legis ideal needs to merge with following year election data
           congress := congress + 1)
  return(ideal_summaries)
}

prep_candidates <- function(candidate_file) {
  cands <- read_csv(candidate_file) |> 
    filter(party %in% c("DEMOCRAT", "REPUBLICAN")) |> 
    mutate(party = ifelse(party == "DEMOCRAT", "D", "R"),
           vote_pct = (candidatevotes / totalvotes) * 100,
           district = as.numeric(gsub("^0+", "", district)),
           district = ifelse(is.na(district), 1, district),
           congress = ceiling(year / 2) - 893) |> 
    select(state = state_po, party, vote_pct, district, congress, year)
  
  return(cands)
}
# prep_candidates(here::here("data-raw", "1976-2020-house.csv")) 

prep_legis <- function(legis_file) {
  legis <- readxl::read_xls(legis_file) |> 
    janitor::clean_names() |> 
    mutate(icpsr = as.character(icpsr_number_according_to_poole_and_rosenthal),
           party = case_when(x100_dem_200_rep_other == 100 ~ "D",
                             x100_dem_200_rep_other == 200 ~ "R",
                             TRUE ~ NA_character_)) |> 
    dplyr::select(icpsr, party,
                  congress = congress_number,
                  female = x1_female,
                  state = two_letter_state_code,
                  district = congressional_district_number)
  
  return(legis)
}

prep_district_presidential_votes <- function(district_file, congress) {
  if (congress == 113) {
    # 113 file has broken column names
    districts <- read_csv(district_file, col_names = F) |> 
      rename(STA = X2, ED = X3, G12P_DP = X23, G12P_RP = X24, G08P_DP = X33, G08P_RP = X34)
  } else {
    districts <- read_csv(district_file)
  }
  
  districts <- districts |> 
    mutate(congress := congress,
           # Fixing at-large districts for merging
           district = ifelse(ED == 99, 1, ED)) |> 
    rowwise() |> 
    mutate(d_vote_pct = mean(c_across(ends_with("P_DP")), na.rm = TRUE),
           r_vote_pct = mean(c_across(ends_with("P_RP")), na.rm = TRUE)) |> 
    select(congress, district, state = STA, d_vote_pct, r_vote_pct)
  
  return(districts)
}


