# DorsetStats.R

# GP_SYOA header definitions ----------------------------------------------

GP_SYOA_headers_classes <- c("character",
                             "character",
                             "factor",
                             "factor",
                             "factor",
                             rep("integer", (200 - 5) ) )

# epraccur header definitions ---------------------------------------------

epraccur_headers_full <- c( "Organisation Code",
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

epraccur_headers_short <- c( "PRACTICE_CODE", # "Organisation Code" re-spell headers to match GP_SYOA headers where applicable
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
                             "Commissioner", # "Commissioner"
                             "JoinProviderPurchaserDate",
                             "LeftProviderPurchaserDate",
                             "ContactTelephoneNumber",
                             "Null",
                             "Null",
                             "Null",
                             "AmendedRecordIndicator",
                             "Null",
                             "ProviderPurchaser", # which is PARENT_ORGANISATIONPCODE
                             "Null",
                             "PrescribingSetting",
                             "Null" )

epraccur_headers_classes <- c( "character",  # "PRACTICE_CODE",
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


# function: getDorsetGPDataset --------------------------------------------

getDorsetGPDataset <- function( GP_SYOA_DataFile = "h:/DATASETS/HSCIC/GP_SYOA_20150331.csv",
                                epraccur_DataFile = "h:/DATASETS/HSCIC/epraccur_20150318.csv",
                                CodePoint = readRDS("H:/DATASETS/OS/CodePoint/CodePoint 2015.1.0 compiled.rds") ) {

  # load csv data

  message(sprintf("Loading GP_SYOA file: %s", GP_SYOA_DataFile))
  GP_SYOA <- read.csv(GP_SYOA_DataFile, colClasses = GP_SYOA_headers_classes)
  message(sprintf("Loading epraccur file: %s", epraccur_DataFile))
  epraccur <- read.csv(epraccur_DataFile, header = FALSE, col.names = epraccur_headers_short, colClasses = epraccur_headers_classes)

  # process data

  GPDataset <- merge(epraccur, GP_SYOA )
  DorsetGPDataset <- subset(GPDataset,PARENT_ORGANISATION_CODE=="11J")  # 11J == NHS Dorset CCG

  # work out preferred age bands, and re-define postcode

  DorsetGPDataset<- within(DorsetGPDataset,{
    # MALE_4, MALE_5, MALE_6 are the odd ones out

    # 0 - 4
    nM_00_04 <-   MALE_0_1 +   MALE_1_2 +   MALE_2_3 +   MALE_3_4 +   MALE_4
    nF_00_04 <- FEMALE_0_1 + FEMALE_1_2 + FEMALE_2_3 + FEMALE_3_4 + FEMALE_4_5
    n_00_04 <- nM_00_04 + nF_00_04
    # 5 - 16
    nM_05_16 <-   MALE_5   +   MALE_6   +   MALE_7_8 +   MALE_8_9 +   MALE_9_10 +   MALE_10_11 +   MALE_11_12 +   MALE_12_13 +   MALE_13_14 +   MALE_14_15 +   MALE_15_16 +   MALE_16_17
    nF_05_16 <- FEMALE_5_6 + FEMALE_6_7 + FEMALE_7_8 + FEMALE_8_9 + FEMALE_9_10 + FEMALE_10_11 + FEMALE_11_12 + FEMALE_12_13 + FEMALE_13_14 + FEMALE_14_15 + FEMALE_15_16 + FEMALE_16_17
    n_05_16 <- nM_05_16 + nF_05_16
    # 0 - 16
    nM_00_16 <- nM_00_04 + nM_05_16
    nF_00_16 <- nF_00_04 + nF_05_16
    n_00_16 <- nM_00_16 + nF_00_16
    # 0 - 18
    nM_00_18 <- nM_00_16 +   MALE_17_18 +   MALE_18_19
    nF_00_18 <- nF_00_16 + FEMALE_17_18 + FEMALE_18_19
    n_00_18 <- nM_00_18 + nF_00_18
    # 9 - 10 (Baird)
    nM_09_10 <-   MALE_9_10 +   MALE_10_11
    nF_09_10 <- FEMALE_9_10 + FEMALE_10_11
    n_09_10 <- nM_09_10 + nF_09_10
    # 5 - 7
    nM_05_07 <-   MALE_5   +   MALE_6   +   MALE_7_8
    nF_05_07 <- FEMALE_5_6 + FEMALE_6_7 + FEMALE_7_8
    n_05_07 <- nM_05_07 + nF_05_07

    POSTCODE<-paste0(str_sub(POSTCODE,1,4),str_sub(POSTCODE,-3,-1)) # make it the same format as CodePoint database
  })

  # trim off unwanted column names (remove MALE_ and FEMALE_ )

  DorsetGPDataset <- DorsetGPDataset[ , -grep("MALE",names(DorsetGPDataset)) ]
  DorsetGPDataset <- DorsetGPDataset[ , -grep("_95.",names(DorsetGPDataset)) ]

  # add Code Point coordinates
  message("Loading/merging Code Point data")
  DorsetGPDataset <- merge(DorsetGPDataset,CodePoint,by.x="POSTCODE",by.y="Postcode",all.x=TRUE) # add CodePoint coordinates

  .missing.codepoint.coordinates <- length(which(is.na(DorsetGPDataset$Eastings)))
  if( .missing.codepoint.coordinates > 0 ) {
    warning( sprintf("There are %d row(s) with missing Code Point coordinates.",.missing.codepoint.coordinates))
  }

  # refactor everything
  cat <- sapply(DorsetGPDataset, is.factor)
  DorsetGPDataset[cat] <- lapply(DorsetGPDataset[cat], factor)

  return(DorsetGPDataset)
}
