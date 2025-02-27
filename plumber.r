#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(BCRA)

#* @filter secure
function(req, res) {
  # Retrieve the Authorization header
  auth_header <- req$HTTP_AUTHORIZATION
  cat("Authorization header:", auth_header, "\n")  # Logging for debugging
  
  # Expected token value (could be stored in an env variable)
  expected_token <- "Inception123"
  
  # Check if the header exists and is formatted as "Bearer <token>"
  if (is.null(auth_header) || !grepl("^Bearer ", auth_header)) {
    res$status <- 401
    return(list(error = "Unauthorized: Missing Bearer token"))
  }
  
  # Extract the token from the header
  token <- sub("^Bearer ", "", auth_header)
  
  if (token != expected_token) {
    res$status <- 401
    return(list(error = "Unauthorized: Invalid token"))
  }
  
  plumber::forward()
}

#* @apiTitle Breast Cancer Risk Calculator API
#* @apiDescription Plumber example description.


#* Calculate Breast Cancer Risk
#* @param currentAge:number The current age of the patient.
#* @param years:number The number of years over which to calculate the risk (e.g., 10).
#* @param ageAtMenarche:number Age at first menstruation.
#* @param ageAtFirstLiveBirth:number The age at first live birth (use NA if nulliparous).
#* @param numberOfBiopsies:number The number of prior benign breast biopsies.
#* @param atypicalHyperplasia:number 1 if atypical hyperplasia is present, 0 if not.
#* @param numberOfFirstDegreeRelatives:number The number of first-degree relatives with breast cancer.
#* @get /calculateRisk
function(currentAge, years, ageAtMenarche, ageAtFirstLiveBirth, numberOfBiopsies, atypicalHyperplasia, numberOfFirstDegreeRelatives) {
  
  # Convert parameters from strings to appropriate types
  currentAge <- as.numeric(currentAge)
  years <- as.numeric(years)
  ageAtMenarche <- as.numeric(ageAtMenarche)
  ageAtFirstLiveBirth <- as.numeric(ageAtFirstLiveBirth)
  numberOfBiopsies <- as.numeric(numberOfBiopsies)
  atypicalHyperplasia <- as.logical(as.numeric(atypicalHyperplasia))
  numberOfFirstDegreeRelatives <- as.numeric(numberOfFirstDegreeRelatives)
  
  # Call the risk calculation function from the BCRA package

  # Bundle the parameters into a data frame
  calculator_df <- data.frame(
    ID = sample(1:1000000, 1),  # Random integer between 1 and 1,000,000
    T1 = currentAge,            # Initial age
    T2 = currentAge + years,    # Projection age (T1 < T2)
    N_Biop = ifelse(numberOfBiopsies == 99, 0, numberOfBiopsies),  # Recode 99 as 0
    HypPlas = atypicalHyperplasia,  # 0 = no, 1 = yes
    AgeMen = ageAtMenarche,         # Age at menarche
    Age1st = ifelse(is.na(ageAtFirstLiveBirth), 98, ageAtFirstLiveBirth),  # 98 indicates nulliparity
    N_Rels = numberOfFirstDegreeRelatives,  # Number of first-degree relatives with BrCa
    Race = 1,  # Default to 1 (White) unless provided otherwise
    stringsAsFactors = FALSE
  )
  
  
  
   risk <- absolute.risk(calculator_df, Raw_Ind = 1, Avg_White = 0
   )
  # Return the risk value in JSON format
  list(risk = risk)
}



#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}



