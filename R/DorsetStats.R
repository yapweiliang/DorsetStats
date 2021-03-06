# DorsetStats.R
#' Dorset Statistics
#'
#' get Dorset GP Surgery statistics from hscic and NHSPD.
#' also exports a number of constants of CCG codes
#'
#'
#' @section Sources:
#' \describe{
#'  \item{epraccur (previously)}{\url{http://systems.hscic.gov.uk/data/ods/datadownloads/gppractice/index_html}}
#'  \item{epraccur}{\url{https://digital.nhs.uk/organisation-data-service/data-downloads/gp-data}}
#'  \item{gp_syoa (previously)}{\url{http://www.hscic.gov.uk/article/2021/Website-Search?q=gp+list+size&go=Go&area=both}}
#'  \item{gp_syoa}{\url{http://www.content.digital.nhs.uk/catalogue/PUB23978}})
#'  \item{localities}{from CCG Pivot Table}
#' }
#'
#' @section History/Discussion
#'
#' previous generic getGPDataset now hidden.
#'
#' in 2017 there are 94 GP surgeries in Dorset.  However DCH does not appear to have updated themselves
#' such that there are still references to non-existant or merged surgeries (e.g. J81026) therefore reverting
#' back to the 2015 dataset for now
#'
#' NHS Digital (previously HSCIC) appear to have changed SYOA data from wide to long format, and separated male + female.
#' Code has been rewritten to accomodate this.  Appears to work, but needs further in depth testing.
#'
#' @section TODO:
#' TODO see if significant benefits using data.table?
#'
#' TODO now that we use NHSPD instead of codepoint, could optimise use of PCD7/PCD8
#'
#' TODO eventually remove codepoint option just leave NHSPD
#'
#' TODO NHS digital appears to have changed SYOA data from wide to long format.  Need to test, document, and tidy.
#'
#' @section DONE:
#'
#' DONE new col names for the more recent SYOA, etc
#'
#' @docType package
#' @name DorsetStats
NULL

# default source data files -----------------------------------------------

.use2015 <- TRUE # use March 2015 data (CSR2015 analysis)?

if(.use2015) {
  # March 2015 data, keep for CSR2015 analysis
  .default_GP_SYOA_DataFile    <- "h:/DATASETS/HSCIC/gpsyoa/keep/GP_SYOA_20150331.csv"
  .default_localities_DataFile <- "h:/DATASETS/Dorset Statistics Derived/CCG 2014/Dorset_GP_Names_and_Localities_from_CCG_PivotTable.csv"
  .default_epraccur_DataFile   <- "h:/DATASETS/HSCIC/epraccur/keep/epraccur_20150318.csv"
} else {
  # newer data
  .default_GP_SYOA_male_DataFile   <- "h:/DATASETS/HSCIC/gpsyoa/gp-reg-pat-prac-sing-age-male.csv"
  .default_GP_SYOA_female_DataFile <- "h:/DATASETS/HSCIC/gpsyoa/gp-reg-pat-prac-sing-age-female.csv"
  .default_localities_DataFile     <- "h:/DATASETS/Dorset Statistics Derived/CCG 2014/Dorset_GP_Names_and_Localities_from_CCG_PivotTable.csv"
  .default_epraccur_DataFile       <- "h:/DATASETS/HSCIC/epraccur/epraccur_20170522.csv"
}

# GP_SYOA header definitions ----------------------------------------------

.GP_SYOA_headers_classes_old <- c("character", # PRACTICE_CODE
                                  "character",  # POSTCODE
                                  rep("factor", 3), # PARENT_ORGANISATION_CODE, NHSE_AREA_TEAM, NHSE_REGION
                                  rep("integer", (200 - 5))) # SYOA

.GP_SYOA_headers_classes <- c("NULL", # EXTRACT_DATE (skip)
                              rep("factor", 3), # CCG_CODE, ONS_CCG_CODE, ORG_CODE (PRACTICE_CODE)
                              "character",  # POSTCODE
                              "factor", # SEX
                              "character", # AGE
                              "integer") # NUMBER_OF_PATIENTS)

# 20150930 onwards has different fields
# 2017 - not sure when changed to NHS digital - now separate male and female files.  Note the AGE field contains text as well "95+", and "ALL"

# localities header definition --------------------------------------------

.localities_headers_classes <- c("factor","character","character")

# epraccur header definitions ---------------------------------------------

.epraccur_headers_full <- c( "Organisation Code",
                            "Name",
                            "National Grouping",
                            "High Level Health Geography",
                            "Address Line 1",
                            "Address Line 2",
                            "Address Line 3",
                            "Address Line 4",
                            "Address Line 5",
                            "Postcode",
                            "Open Date",
                            "Close Date",
                            "Status Code",
                            "Organisation Sub-Type Code",
                            "Commissioner",
                            "Join Provider/Purchaser Date",
                            "Left Provider/Purchaser Date",
                            "Contact Telephone Number",
                            "Null",
                            "Null",
                            "Null",
                            "Amended Record Indicator",
                            "Null",
                            "Provider/Purchaser",
                            "Null",
                            "Prescribing Setting",
                            "Null" )

.epraccur_headers_short <- c( "PRACTICE_CODE", # "Organisation Code" re-spell headers to match GP_SYOA headers where applicable
                             "NAME",
                             "NHSE_REGION",    # "National Grouping"
                             "NHSE_AREA_TEAM", # "High Level Health Grouping"
                             "Addr1",
                             "Addr2",
                             "Addr3",
                             "Addr4",
                             "Addr5",
                             "POSTCODE",       # "Postcode"
                             "OpenDate",
                             "CloseDate",
                             "StatusCode",
                             "OrganisationSubTypeCode",
                             "Commissioner", # "Commissioner" # or should this be CCG_Code?
                             "JoinProviderPurchaserDate",
                             "LeftProviderPurchaserDate",
                             "ContactTelephoneNumber",
                             "Null",
                             "Null",
                             "Null",
                             "AmendedRecordIndicator",
                             "Null",
                             "ProviderPurchaser", # which is PARENT_ORGANISATION_CODE # should this be CCG_Code?
                             "Null",
                             "PrescribingSetting",
                             "Null" )

.epraccur_headers_classes <- c( "character",  # "PRACTICE_CODE",
                               "character",  # "NAME",
                               "factor",  # "NHSE_REGION",
                               "factor",  # "NHSE_AREA_TEAM",
                               "NULL",  # "Addr1",
                               "NULL",  # "Addr2",
                               "NULL",  # "Addr3",
                               "NULL",  # "Addr4",
                               "NULL",  # "Addr5",
                               "character",  # "POSTCODE",
                               "character",  # "OpenDate",
                               "character",  # "CloseDate",
                               "factor",  # "StatusCode",
                               "factor",  # "OrganisationSubTypeCode",
                               "factor",  # "Commissioner",
                               "character",  # "JoinProviderPurchaserDate",
                               "character",  # "LeftProviderPurchaserDate",
                               "NULL",  # "ContactTelephoneNumber",
                               "NULL",  # "Null",
                               "NULL",  # "Null",
                               "NULL",  # "Null",
                               "NULL",  # "AmendedRecordIndicator",
                               "NULL",  # "Null",
                               "factor",  # "ProviderPurchaser",
                               "NULL",  # "Null",
                               "factor",  # "PrescribingSetting",
                               "NULL" )

# exported constants ------------------------------------------------------
#' @exportPattern CCGcode[s]?.\w+

CCGcode.Dorset <- '11J'
CCGcode.BathAndNorthEastSomerset <- '11E'
CCGcode.NorthSomerset <- '11T'
CCGcode.Somerset <- '11X'
CCGcode.Bristol <- '11H'
CCGcode.Wiltshire <- '99N'
CCGcode.SouthGloucestershire <- '12A'
CCGcode.Swindon <- '12D'
CCGcode.NorthEastWestDevon <- '99P'
CCGcode.SouthDevonAndTorbay <- '99Q'
CCGcode.WestHampshire <- '11A'
CCGcode.Southampton <- '10X'
CCGcode.Portsmouth <- '10R'
CCGcode.FarehamAndGosport <- '10K'
CCGcode.Gloucestershire <- '11M'
CCGcode.NorthHampshire <- '10J'

CCGcodes.DorsetsSurroundings <- c(
  CCGcode.BathAndNorthEastSomerset,
  CCGcode.NorthSomerset,
  CCGcode.Somerset,
  CCGcode.Bristol,
  CCGcode.Wiltshire,
  CCGcode.SouthGloucestershire,
  CCGcode.Swindon,
  CCGcode.NorthEastWestDevon,
  CCGcode.SouthDevonAndTorbay,
  CCGcode.WestHampshire,
  CCGcode.Southampton,
  CCGcode.Portsmouth,
  CCGcode.FarehamAndGosport
)

CCGcodes.DorsetAndSurroundings <- c(
  CCGcode.Dorset,
  CCGcodes.DorsetsSurroundings
)

# function: .getGPDataset --------------------------------------------


#' get GP Dataset (current)
#'
#' Links up data from \emph{epraccur} (list of GP surgeries) and
#' \emph{gp_syoa} (registered patients by single year of age) and
#' optionally (for Dorset) specify which \emph{locality} they are in.
#' Then, links up with \pkg{NHSPD} \emph{(NHS Postcode Directory)} to obtain LSOA and other data
#'
#' @param GP_SYOA_male_DataFile full path to the SYOA \file{gp-reg-pat-prac-sing-age-male.csv}
#' @param GP_SYOA_female_DataFile download from NHS digital
#' @param CCG_List character vector of CCG codes, e.g. \code{c("11J")}
#' @param CodePoint OBSOLETE, REMOVED. specify the Code Point dataset, or load it from a saved \file{codepoint.rds} file
#' @param NHSPD specify the NHSPD dataset (was CodePoint) or use NHSPD::getNHSPD
#' @param epraccur_DataFile
#'
#' @return return this
#' #@export
#'
#' @examples DorsetGPDataset <- getDorsetGPDataset()
.getGPDataset <-
  function(GP_SYOA_male_DataFile = .default_GP_SYOA_male_DataFile,
           GP_SYOA_female_DataFile = .default_GP_SYOA_female_DataFile,
           CCG_List = CCGcodes.DorsetsSurroundings,
           #CodePoint = NHSPD::getNHSPD(),
           NHSPD = NHSPD::getNHSPD(),
           epraccur_DataFile = .default_epraccur_DataFile) {


    # # temp
    # GP_SYOA_male_DataFile = .default_GP_SYOA_male_DataFile
    # GP_SYOA_female_DataFile = .default_GP_SYOA_female_DataFile
    # CCG_List = CCGcodes.DorsetsSurroundings
    # #CodePoint = NHSPD::getNHSPD()
    # NHSPD = NHSPD::getNHSPD()
    # epraccur_DataFile = .default_epraccur_DataFile

    # load epraccur data
    message(sprintf("Loading epraccur file: %s", epraccur_DataFile))
    epraccur <- read.csv(epraccur_DataFile, header = FALSE, col.names = .epraccur_headers_short, colClasses = .epraccur_headers_classes)
    # reduce epraccur data
    epraccur <- subset(epraccur, Commissioner %in% CCG_List)

    # load gpsyoa data (expect the NHS Digital (new) headers)
    message(sprintf("Loading male GP_SYOA file: %s", GP_SYOA_male_DataFile))
    GP_SYOA_male <- read.csv(GP_SYOA_male_DataFile, colClasses = .GP_SYOA_headers_classes)
    message(sprintf("Loading female GP_SYOA file: %s", GP_SYOA_female_DataFile))
    GP_SYOA_female <- read.csv(GP_SYOA_female_DataFile, colClasses = .GP_SYOA_headers_classes)
    # convert GP_SYOA to wide format (which was how it previously was)
    ## first reduce the size
    GP_SYOA_male <- subset(GP_SYOA_male, CCG_CODE %in% CCG_List)
    GP_SYOA_female <- subset(GP_SYOA_female, CCG_CODE %in% CCG_List)
    ## now make wide to long
    GP_SYOA_male <- spread(GP_SYOA_male, AGE, NUMBER_OF_PATIENTS, sep = "_")
    GP_SYOA_female <- spread(GP_SYOA_female, AGE, NUMBER_OF_PATIENTS, sep = "_")
    ## now merge
    GP_SYOA <- merge(GP_SYOA_male, GP_SYOA_female,
                     by = c("CCG_CODE", "ONS_CCG_CODE", "ORG_CODE", "POSTCODE"),
                     suffixes = c("_M", "_F"))
    ## now rename ORG_CODE to PRACTICE_CODE
    names(GP_SYOA)[which(names(GP_SYOA) == "ORG_CODE")] <- "PRACTICE_CODE"

    # merge SYOA data with epraccur data
    GPDataset <- merge(epraccur, GP_SYOA, all = TRUE, by = "PRACTICE_CODE", suffixes = c("", ".GP_SYOA"))

    # to subset again or not?
    GPDataset <- subset(GPDataset, CCG_CODE %in% CCG_List) # if referencing SYOA list
    #GPDataset <- subset(GPDataset, Commisioner %in% CCG_List) # if referencing epraccur instead (full list, includes closed surgeries)

    # check data for missing stuff and check for consistency
    .missing.SYOA.info <- length(which(is.na(GP_SYOA$AGE_ALL_M))) + length(which(is.na(GP_SYOA$AGE_ALL_F)))
    if( .missing.SYOA.info > 0 ) {
      warning(
        sprintf("There are %d row(s) with undefined SYOA information.  The GP_SYOA_DataFile might not contain information on every surgery.",
                .missing.SYOA.info))
    }

    .missing.epraccur.info <- length(which(is.na(GPDataset$StatusCode)))
    if( .missing.epraccur.info > 0 ) {
      warning(
        sprintf("There are %d row(s) with undefined epraccur information.  This should not happen!",
                .missing.epraccur.info)
      )
    }

    # old SYOA fields were MALE_0_1, for males aged 0 - 1
    # new SYOA fields are AGE_0_M, for males aged 0 - 1

    # work out preferred age bands, and re-define postcode
    GPDataset <- within(GPDataset,{

      # 0 - 4
      nM_00_04 <- AGE_0_M + AGE_1_M + AGE_2_M + AGE_3_M + AGE_4_M
      nF_00_04 <- AGE_0_F + AGE_1_F + AGE_2_F + AGE_3_F + AGE_4_F
      n_00_04 <- nM_00_04 + nF_00_04
      # 5 - 16
      nM_05_16 <- AGE_5_M + AGE_6_M + AGE_7_M + AGE_8_M + AGE_9_M + AGE_10_M + AGE_11_M + AGE_12_M + AGE_13_M + AGE_14_M + AGE_15_M + AGE_16_M
      nF_05_16 <- AGE_5_F + AGE_6_F + AGE_7_F + AGE_8_F + AGE_9_F + AGE_10_F + AGE_11_F + AGE_12_F + AGE_13_F + AGE_14_F + AGE_15_F + AGE_16_F
      n_05_16 <- nM_05_16 + nF_05_16
      # 0 - 15
      nM_00_15 <- nM_00_04 + nM_05_16 - AGE_16_M
      nF_00_15 <- nF_00_04 + nF_05_16 - AGE_16_F
      n_00_15 <- nM_00_15 + nF_00_15
      # 0 - 16
      nM_00_16 <- nM_00_04 + nM_05_16
      nF_00_16 <- nF_00_04 + nF_05_16
      n_00_16 <- nM_00_16 + nF_00_16
      # 0 - 17
      nM_00_17 <- nM_00_16 + AGE_17_M
      nF_00_17 <- nF_00_16 + AGE_17_F
      n_00_17 <- nM_00_17 + nF_00_17
      # 0 - 18
      nM_00_18 <- nM_00_16 + AGE_17_M + AGE_18_M
      nF_00_18 <- nF_00_16 + AGE_17_F + AGE_18_F
      n_00_18 <- nM_00_18 + nF_00_18
      # 9 - 10 (Baird)
      nM_09_10 <- AGE_9_M + AGE_10_M
      nF_09_10 <- AGE_9_F + AGE_10_F
      n_09_10 <- nM_09_10 + nF_09_10
      # 5 - 7
      nM_05_07 <- AGE_5_M + AGE_6_M + AGE_7_M
      nF_05_07 <- AGE_5_F + AGE_6_F + AGE_7_F
      n_05_07 <- nM_05_07 + nF_05_07
      # 16 - 17
      nM_16_17 <- AGE_16_M + AGE_17_M
      nF_16_17 <- AGE_16_F + AGE_17_F
      n_16_17 <- nM_16_17 + nF_16_17
      # WHO females of child bearing age
      nF_15_49_WHO <-
        AGE_15_F + AGE_16_F + AGE_17_F + AGE_18_F + AGE_19_F + AGE_20_F + AGE_21_F + AGE_22_F + AGE_23_F +
        AGE_24_F + AGE_25_F + AGE_26_F + AGE_27_F + AGE_28_F + AGE_29_F + AGE_30_F + AGE_31_F + AGE_32_F +
        AGE_33_F + AGE_34_F + AGE_35_F + AGE_36_F + AGE_37_F + AGE_38_F + AGE_39_F + AGE_40_F + AGE_41_F +
        AGE_42_F + AGE_43_F + AGE_44_F + AGE_45_F + AGE_46_F + AGE_47_F + AGE_48_F + AGE_49_F
      # proxy for birth rate
      n_00_01 <- AGE_0_M + AGE_0_F

      POSTCODE<-paste0(str_sub(POSTCODE,1,4),str_sub(POSTCODE,-3,-1)) # make it the same format as CodePoint database
      # TODO - now that we use NHSPD which has PCD8 (and PCD7 added by me), maybe can stick with PCD8?
    })

    # trim off unwanted column names (remove AGE_ )
    GPDataset <- GPDataset[ , -grep("AGE_",names(GPDataset)) ]

    # exception handling
    #   before adding Code Point coordinates, tweak for missing coordinate in CPO
    #     BH229HF is not on Code Point Open
    #     however it is adjacent to BH229HB, so use same coordinates

    if (is.na(match("BH229HF", NHSPD$Postcode))) {
      z <- which(GPDataset$POSTCODE == "BH229HF")
      if (length(z) > 0) {
        GPDataset[z, ]$POSTCODE <- "BH229HB"
        warning( "No coordinates for BH229HF, so substituting BH229HB instead")
      }
    }

    message("Loading/Merging Code Point data")

    # add Code Point coordinates

    GPDataset <- merge(GPDataset,NHSPD,by.x="POSTCODE",by.y="Postcode",all.x=TRUE)

    .missing.NHSPD.coordinates <- length(which(is.na(GPDataset$Eastings)))
    if( .missing.NHSPD.coordinates > 0 ) {
      warning( sprintf("There are %d row(s) with missing NHSPD/codepoint coordinates.",.missing.NHSPD.coordinates))
    }

    # refactor everything
    cat <- sapply(GPDataset, is.factor)
    GPDataset[cat] <- lapply(GPDataset[cat], factor)

    return(GPDataset)

  }



# function: .old.getGPDataset --------------------------------------------------

#' get GP Dataset (old)
#'
#' Links up data from \emph{epraccur} (list of GP surgeries) and
#' \emph{gp_syoa} (registered patients by single year of age) and
#' optionally (for Dorset) specify which \emph{locality} they are in.
#' Then, links up with \pkg{NHSPD} \emph{(NHS Postcode Directory)} to obtain LSOA and other data
#'
#' @param GP_SYOA_DataFile full path to the \file{GP_SYOA.csv} file, download from \url{http://www.hscic.gov.uk}
#' @param CCG_List character vector of CCG codes, e.g. \code{c("11J")}
#' @param epraccur_DataFile full path to the \file{epraccur.csv} file, download and unzip from \url{http://systems.hscic.gov.uk/data/ods/datadownloads/gppractice/index_html}
#' @param CodePoint OBSOLETE, REMOVED. specify the Code Point dataset, or load it from a saved \file{codepoint.rds} file
#' @param NHSPD specify the NHSPD dataset (was CodePoint) or use NHSPD::getNHSPD
#' @param localities_DataFile full path to the file obtained from CCG, or scrape from \url{http://www.dorsetccg.nhs.uk/aboutus/localities.htm}
#' @param use_old_SYOA_headers \code{FALSE} Since 20150930, three of the column names have changed, and become six columns
#'
#' @return return this
#' #@export
#'
#' @examples DorsetGPDataset <- getDorsetGPDataset()
.old.getGPDataset <-
  function(GP_SYOA_DataFile = .default_GP_SYOA_DataFile,
           CCG_List = CCGcodes.DorsetsSurroundings,
           CodePoint = NHSPD::getNHSPD(),
           NHSPD = NHSPD::getNHSPD(),
           epraccur_DataFile = .default_epraccur_DataFile
           ) {

  # load csv data

  message(sprintf("Loading GP_SYOA file: %s", GP_SYOA_DataFile))
  GP_SYOA <- read.csv(GP_SYOA_DataFile, colClasses = .GP_SYOA_headers_classes_old)

  message(sprintf("Loading epraccur file: %s", epraccur_DataFile))
  epraccur <- read.csv(epraccur_DataFile, header = FALSE, col.names = .epraccur_headers_short, colClasses = .epraccur_headers_classes)

  # process data

  GPDataset <- merge(epraccur, GP_SYOA, all = TRUE, by = "PRACTICE_CODE", suffixes = c("", ".GP_SYOA"))
  GPDataset <- subset(GPDataset, PARENT_ORGANISATION_CODE %in% CCG_List) # for pre-201508 SYOA data

  # check data for missing stuff and check for consistency
  .missing.SYOA.info <- length(which(is.na(GPDataset$Total_All)))
  if( .missing.SYOA.info > 0 ) {
    warning(
      sprintf("There are %d row(s) with undefined SYOA information.  The GP_SYOA_DataFile might not contain information on every surgery.",
              .missing.SYOA.info))
  }

  .missing.epraccur.info <- length(which(is.na(GPDataset$StatusCode)))
  if( .missing.epraccur.info > 0 ) {
    warning(
      sprintf("There are %d row(s) with undefined epraccur information.  This should not happen!",
              .missing.epraccur.info)
    )
  }

  # SYOA colum name anomalies
  # MALE_4, MALE_5, MALE_6 are the odd ones out
  # this seems to be corrected in the 20150930 gp syoa dataset
  # but seems to be uncorrected/reverted in the 20160101 dataset
  colnames(GPDataset)[which(names(GPDataset) == "MALE_4")] <- "MALE_4_5"
  colnames(GPDataset)[which(names(GPDataset) == "MALE_5")] <- "MALE_5_6"
  colnames(GPDataset)[which(names(GPDataset) == "MALE_6")] <- "MALE_6_7"

  # work out preferred age bands, and re-define postcode

  GPDataset <- within(GPDataset,{


    # 0 - 4
    nM_00_04 <-   MALE_0_1 +   MALE_1_2 +   MALE_2_3 +   MALE_3_4 +   MALE_4_5
    nF_00_04 <- FEMALE_0_1 + FEMALE_1_2 + FEMALE_2_3 + FEMALE_3_4 + FEMALE_4_5
    n_00_04 <- nM_00_04 + nF_00_04
    # 5 - 16
    nM_05_16 <-   MALE_5_6 +   MALE_6_7 +   MALE_7_8 +   MALE_8_9 +   MALE_9_10 +   MALE_10_11 +   MALE_11_12 +   MALE_12_13 +   MALE_13_14 +   MALE_14_15 +   MALE_15_16 +   MALE_16_17
    nF_05_16 <- FEMALE_5_6 + FEMALE_6_7 + FEMALE_7_8 + FEMALE_8_9 + FEMALE_9_10 + FEMALE_10_11 + FEMALE_11_12 + FEMALE_12_13 + FEMALE_13_14 + FEMALE_14_15 + FEMALE_15_16 + FEMALE_16_17
    n_05_16 <- nM_05_16 + nF_05_16
    # 0 - 15
    nM_00_15 <- nM_00_04 + nM_05_16 -   MALE_16_17
    nF_00_15 <- nF_00_04 + nF_05_16 - FEMALE_16_17
    n_00_15 <- nM_00_15 + nF_00_15
    # 0 - 16
    nM_00_16 <- nM_00_04 + nM_05_16
    nF_00_16 <- nF_00_04 + nF_05_16
    n_00_16 <- nM_00_16 + nF_00_16
    # 0 - 17
    nM_00_17 <- nM_00_16 +   MALE_17_18
    nF_00_17 <- nF_00_16 + FEMALE_17_18
    n_00_17 <- nM_00_17 + nF_00_17
    # 0 - 18
    nM_00_18 <- nM_00_16 +   MALE_17_18 +   MALE_18_19
    nF_00_18 <- nF_00_16 + FEMALE_17_18 + FEMALE_18_19
    n_00_18 <- nM_00_18 + nF_00_18
    # 9 - 10 (Baird)
    nM_09_10 <-   MALE_9_10 +   MALE_10_11
    nF_09_10 <- FEMALE_9_10 + FEMALE_10_11
    n_09_10 <- nM_09_10 + nF_09_10
    # 5 - 7
    nM_05_07 <-   MALE_5_6 +   MALE_6_7 +   MALE_7_8
    nF_05_07 <- FEMALE_5_6 + FEMALE_6_7 + FEMALE_7_8
    n_05_07 <- nM_05_07 + nF_05_07
    # 16 - 17
    nM_16_17 <-   MALE_16_17 +   MALE_17_18
    nF_16_17 <- FEMALE_16_17 + FEMALE_17_18
    n_16_17 <- nM_16_17 + nF_16_17

    nF_15_49_WHO <-
      FEMALE_15_16 + FEMALE_16_17 + FEMALE_17_18 + FEMALE_18_19 + FEMALE_19_20 + FEMALE_20_21 + FEMALE_21_22 + FEMALE_22_23 + FEMALE_23_24 +
      FEMALE_24_25 + FEMALE_25_26 + FEMALE_26_27 + FEMALE_27_28 + FEMALE_28_29 + FEMALE_29_30 + FEMALE_30_31 + FEMALE_31_32 + FEMALE_32_33 +
      FEMALE_33_34 + FEMALE_34_35 + FEMALE_35_36 + FEMALE_36_37 + FEMALE_37_38 + FEMALE_38_39 + FEMALE_39_40 + FEMALE_40_41 + FEMALE_41_42 +
      FEMALE_42_43 + FEMALE_43_44 + FEMALE_44_45 + FEMALE_45_46 + FEMALE_46_47 + FEMALE_47_48 + FEMALE_48_49 + FEMALE_49_50

    n_00_01 <- MALE_0_1 + FEMALE_0_1

    POSTCODE<-paste0(str_sub(POSTCODE,1,4),str_sub(POSTCODE,-3,-1)) # make it the same format as CodePoint database
    # TODO - now that we use NHSPD which has PCD8 (and PCD7 added by me), maybe can stick with PCD8?
  })

  # trim off unwanted column names (remove MALE_ and FEMALE_ )

  GPDataset <- GPDataset[ , -grep("MALE",names(GPDataset)) ]
  #GPDataset <- GPDataset[ , -grep("_95.",names(GPDataset)) ] # no longer needed

  # exception handling
  #   before adding Code Point coordinates, tweak for missing coordinate in CPO
  #     BH229HF is not on Code Point Open
  #     however it is adjacent to BH229HB, so use same coordinates

  if (is.na(match("BH229HF", NHSPD$Postcode))) {
    z <- which(GPDataset$POSTCODE == "BH229HF")
    if (length(z) > 0) {
      GPDataset[z, ]$POSTCODE <- "BH229HB"
      warning( "No coordinates for BH229HF, so substituting BH229HB instead")
    }
  }

  message("Loading/Merging Code Point data")

  # add Code Point coordinates

  GPDataset <- merge(GPDataset,NHSPD,by.x="POSTCODE",by.y="Postcode",all.x=TRUE)

  .missing.NHSPD.coordinates <- length(which(is.na(GPDataset$Eastings)))
  if( .missing.NHSPD.coordinates > 0 ) {
    warning( sprintf("There are %d row(s) with missing NHSPD/codepoint coordinates.",.missing.NHSPD.coordinates))
  }

  # refactor everything
  cat <- sapply(GPDataset, is.factor)
  GPDataset[cat] <- lapply(GPDataset[cat], factor)

  return(GPDataset)
}




# function: getDorsetGPDataset --------------------------------------------

#' @describeIn getGPDataset wrapper function to obtain Dorset GP information and add localities
#' @export
getDorsetGPDataset <- function( localities_DataFile = .default_localities_DataFile,
                                use2015 = .use2015) {

  message(sprintf("Loading NHS Dorset localities file: %s", localities_DataFile))
  DorsetLocalities <- read.csv(localities_DataFile, colClasses = .localities_headers_classes)

  if(use2015) {
    DorsetGPDataset <- .old.getGPDataset(
      CCG_List = CCGcode.Dorset,
      #use_old_SYOA_headers = use_old_SYOA_headers
    )
  } else {
    DorsetGPDataset <- .getGPDataset(
      CCG_List = CCGcode.Dorset
    )
  }

  message("Merging Dorset CCG localities")

  DorsetGPDataset <- merge(DorsetLocalities, DorsetGPDataset, by.x = "GP_Practice_Code", by.y = "PRACTICE_CODE", all = TRUE)

  # check that the localities data file contains all the surgeries
  .missing.locality.info <- length(which(is.na(DorsetGPDataset$Locality)))
  if( .missing.locality.info > 0 ) {
    warning(
      sprintf(paste("There are %d row(s) with undefined Dorset locality.  ",
                    "This probably means the specified localities_Datafile is not up to date ",
                    "(i.e. there are new GP surgery codes).", sep = ""),
              .missing.locality.info))
  }

  # check that we have all that the localities data file says we should
  .missing.GP.info <- length(which(is.na(DorsetGPDataset$NAME)))
  if( .missing.GP.info > 0 ) {
    warning(
      sprintf(paste("There are %d row(s) with undefined GP surgery.  ",
                    "This probably means the GP_SYOA_Datafile is missing some information.  ",
                    "Also check the epraccur_Datafile.",
                    "\nAlternatively the specificed localities_Datafile is not up to date ",
                    "(i.e. one or more GP surgeries have closed - therefore please consider trimming the unneeded rows).", sep = ""),
              .missing.GP.info ))
  }

  # refactor everything
  cat <- sapply(DorsetGPDataset, is.factor)
  DorsetGPDataset[cat] <- lapply(DorsetGPDataset[cat], factor)

  return(DorsetGPDataset)
}
