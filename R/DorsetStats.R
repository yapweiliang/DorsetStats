# DorsetStats.R

# sources -----------------------------------------------------------------

# epraccur  http://systems.hscic.gov.uk/data/ods/datadownloads/gppractice/index_html
# gp_syoa   http://www.hscic.gov.uk/article/2021/Website-Search?q=gp+list+size&go=Go&area=both
# localities from CCG Pivot Table

# default source data files -----------------------------------------------

.default_GP_SYOA_DataFile    <- "h:/DATASETS/HSCIC/GP_SYOA_20150331.csv"
.default_localities_DataFile <- "h:/DATASETS/Dorset Statistics Derived/CCG 2014/Dorset_GP_Names_and_Localities_from_CCG_PivotTable.csv"
.default_epraccur_DataFile   <- "h:/DATASETS/HSCIC/epraccur_20150318.csv"

.default_CodePoint_RDSFile   <- "H:/DATASETS/OS/CodePoint/CodePoint 2015.4.0 compiled.rds"

# GP_SYOA header definitions ----------------------------------------------

.GP_SYOA_headers_classes <- c("character", # PRACTICE_CODE
                              "character",  # POSTCODE
                              rep("factor", 3), # 20150930 has different fields, put 6; prior to that put 3
                              rep("integer", (200 - 5))) # SYOA

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

# constants

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

# function: getGPDataset --------------------------------------------------

getGPDataset <-
  function(GP_SYOA_DataFile = .default_GP_SYOA_DataFile,
           CCG_List = CCGcodes.DorsetsSurroundings,
           epraccur_DataFile = .default_epraccur_DataFile,
           CodePoint = readRDS(.default_CodePoint_RDSFile)) {

  # load csv data

  message(sprintf("Loading GP_SYOA file: %s", GP_SYOA_DataFile))
  GP_SYOA <- read.csv(GP_SYOA_DataFile, colClasses = .GP_SYOA_headers_classes)

  message(sprintf("Loading epraccur file: %s", epraccur_DataFile))
  epraccur <- read.csv(epraccur_DataFile, header = FALSE, col.names = .epraccur_headers_short, colClasses = .epraccur_headers_classes)

  # process data

  GPDataset <- merge(epraccur, GP_SYOA, all = TRUE)
  GPDataset <- subset(GPDataset,PARENT_ORGANISATION_CODE %in% CCG_List) # for pre-201508 SYOA data
  #GPDataset <- subset(GPDataset,CCG_CODE %in% CCG_List) # for for post-201508 SYOA data
  #GPDataset <- subset(GPDataset,ProviderPurchaser %in% CCG_List) # if referencing epraccur instead

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


  # work out preferred age bands, and re-define postcode

  GPDataset<- within(GPDataset,{
    # MALE_4, MALE_5, MALE_6 are the odd ones out, this seems to be corrected in the 20150930 gp syoa dataset

    # 0 - 4
    nM_00_04 <-   MALE_0_1 +   MALE_1_2 +   MALE_2_3 +   MALE_3_4 +   MALE_4  ###
    nF_00_04 <- FEMALE_0_1 + FEMALE_1_2 + FEMALE_2_3 + FEMALE_3_4 + FEMALE_4_5
    n_00_04 <- nM_00_04 + nF_00_04
    # 5 - 16
    nM_05_16 <-   MALE_5   +   MALE_6   +   MALE_7_8 +   MALE_8_9 +   MALE_9_10 +   MALE_10_11 +   MALE_11_12 +   MALE_12_13 +   MALE_13_14 +   MALE_14_15 +   MALE_15_16 +   MALE_16_17  ###
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
    nM_05_07 <-   MALE_5   +   MALE_6   +   MALE_7_8  ###
    nF_05_07 <- FEMALE_5_6 + FEMALE_6_7 + FEMALE_7_8
    n_05_07 <- nM_05_07 + nF_05_07
    # 16 - 17
    nM_16_17 <-   MALE_16_17 +   MALE_17_18
    nF_16_17 <- FEMALE_16_17 + FEMALE_17_18
    n_16_17 <- nM_16_17 + nF_16_17

    POSTCODE<-paste0(str_sub(POSTCODE,1,4),str_sub(POSTCODE,-3,-1)) # make it the same format as CodePoint database
  })

  # trim off unwanted column names (remove MALE_ and FEMALE_ )

  GPDataset <- GPDataset[ , -grep("MALE",names(GPDataset)) ]
  #GPDataset <- GPDataset[ , -grep("_95.",names(GPDataset)) ] # no longer needed

  # exception handling
  #   before adding Code Point coordinates, tweak for missing coordinate in CPO
  #     BH229HF is not on Code Point Open
  #     however it is adjacent to BH229HB, so use same coordinates

  if (is.na(match("BH229HF", CodePoint$Postcode))) {
    z <- which(GPDataset$POSTCODE == "BH229HF")
    if (length(z) > 0) {
      GPDataset[z, ]$POSTCODE <- "BH229HB"
      warning( "No coordinates for BH229HF, so substituting BH229HB instead")
    }
  }

  message("Loading/Merging Code Point data")

  # add Code Point coordinates

  GPDataset <- merge(GPDataset,CodePoint,by.x="POSTCODE",by.y="Postcode",all.x=TRUE)

  .missing.codepoint.coordinates <- length(which(is.na(GPDataset$Eastings)))
  if( .missing.codepoint.coordinates > 0 ) {
    warning( sprintf("There are %d row(s) with missing Code Point coordinates.",.missing.codepoint.coordinates))
  }

  # refactor everything
  cat <- sapply(GPDataset, is.factor)
  GPDataset[cat] <- lapply(GPDataset[cat], factor)

  return(GPDataset)
}

# function: getDorsetGPDataset --------------------------------------------

getDorsetGPDataset <- function( GP_SYOA_DataFile = .default_GP_SYOA_DataFile,
                                localities_DataFile = .default_localities_DataFile,
                                epraccur_DataFile = .default_epraccur_DataFile,
                                CodePoint = readRDS(.default_CodePoint_RDSFile) ) {

  message(sprintf("Loading NHS Dorset localities file: %s", localities_DataFile))
  DorsetLocalities <- read.csv(localities_DataFile, colClasses = .localities_headers_classes)

  DorsetGPDataset <- getGPDataset(
    GP_SYOA_DataFile = GP_SYOA_DataFile,
    CCG_List = CCGcode.Dorset,
    epraccur_DataFile = epraccur_DataFile,
    CodePoint = CodePoint
  )

  message("Merging Dorset CCG localities")

  DorsetGPDataset <- merge(DorsetLocalities, DorsetGPDataset, by.x = "GP_Practice_Code", by.y = "PRACTICE_CODE", all = TRUE)

  # check that the localities data file contains all the surgeries
  .missing.locality.info <- length(which(is.na(DorsetGPDataset$Locality)))
  if( .missing.locality.info > 0 ) {
    warning(
      sprintf("There are %d row(s) with undefined Dorset locality.  This probably means the specified localities_Datafile is not up to date.",
              .missing.locality.info))
  }

  # check that we have all that the localities data file says we should
  .missing.GP.info <- length(which(is.na(DorsetGPDataset$NAME)))
  if( .missing.GP.info > 0 ) {
    warning(
      sprintf("There are %d row(s) with undefined GP surgery.  This probably means the GP_SYOA_Datafile is missing some information.  Also check the epraccur_Datafile.",
              .missing.GP.info ))
  }

  # refactor everything
  cat <- sapply(DorsetGPDataset, is.factor)
  DorsetGPDataset[cat] <- lapply(DorsetGPDataset[cat], factor)

  return(DorsetGPDataset)
}

