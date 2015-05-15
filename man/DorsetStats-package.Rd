\name{DorsetStats-package}
\alias{DorsetStats-package}
\alias{DorsetStats}
\alias{getDorsetGPDataset}
\docType{package}
\title{
Stuff to get Dorset Statistics in a manner that I want
}
\description{
Dorset GP Surgery statistics from hscic and codepoint
}
\details{
get source data from \url{http://www.hscic.gov.uk}

needs codepoint data
}
\author{
Author: Wei Liang Yap

Maintainer: Wei Liang Yap <yapweiliang@yahoo.co.uk>
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }
\seealso{
}
\usage{
getDorsetGPDataset( GP_SYOA_DataFile = "h:/DATASETS/HSCIC/GP_SYOA_20150331.csv",
                    localities_DataFile = "h:/DATASETS/Dorset Statistics Derived/CCG 2014/Dorset_GP_Names_and_Localities_from_CCG_PivotTable.csv",
                    epraccur_DataFile = "h:/DATASETS/HSCIC/epraccur_20150318.csv",
                    CodePoint = readRDS("H:/DATASETS/OS/CodePoint/CodePoint 2015.1.0 compiled.rds" ) )
}
\arguments{
  \item{GP_SYOA_DataFile}{
full path to the \file{GP_SYOA.csv} file, download from \url{http://www.hscic.gov.uk}
  }
  \item{localities_DataFile}{
full path to the file obtained from CCG, or scrape from \url{http://www.dorsetccg.nhs.uk/aboutus/localities.htm}
  }
  \item{epraccur_DataFile}{
full path to the \file{epraccur.csv} file, download and unzip from \url{http://systems.hscic.gov.uk/data/ods/datadownloads/gppractice/index_html}
  }
  \item{CodePoint}{
specify the Code Point dataset, or load it from a saved \file{codepoint.rds} file
  }
}
\examples{
DorsetGPDataset <- getDorsetGPDataset()
}
