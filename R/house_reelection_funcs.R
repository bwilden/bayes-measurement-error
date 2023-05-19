
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


process_ideal_points <- function(ideal_obj, congress) {
  ideal_summaries <- as_tibble(ideal_obj$x) |> 
    pivot_longer(cols = everything(),
                 names_to = "icpsr",
                 values_to = "ideal_point") |> 
    nest_by(icpsr) |> 
    mutate(normal_mod = list(selm(ideal_point ~ 1, data = data, fixed.param = list(alpha = 0))),
           normal_dp = list(extractSECdistr(normal_mod)),
           # Normal dps
           mu = slot(normal_dp, "dp")[["xi"]],
           sigma = slot(normal_dp, "dp")[["omega"]],
           skew_mod = list(selm(ideal_point ~ 1, data = data)),
           skew_dp = list(extractSECdistr(skew_mod)),
           # Skew normal dps
           xi = slot(skew_dp, "dp")[["xi"]],
           omega = slot(skew_dp, "dp")[["omega"]],
           alpha = slot(skew_dp, "dp")[["alpha"]]) |> 
    unnest(data) |> 
    dplyr::select(icpsr, mu, sigma, xi, omega, alpha) |> 
    distinct() |> 
    mutate(icpsr = str_remove(icpsr, ".D1"),
           congress := congress)
  return(ideal_summaries)
}

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
                  vote_pct = percent_vote_received_to_enter_this_congress,
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
