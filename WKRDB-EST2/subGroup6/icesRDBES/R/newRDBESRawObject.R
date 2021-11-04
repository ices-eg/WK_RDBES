#' Constructor for RDBESRawObject class
#'
#' @param DE Data table of RDBES DE data or null
#' @param SD Data table of RDBES DE data or null
#' @param VS Data table of RDBES DE data or null
#' @param FT Data table of RDBES DE data or null
#' @param FO Data table of RDBES DE data or null
#' @param TE Data table of RDBES DE data or null
#' @param LO Data table of RDBES DE data or null
#' @param OS Data table of RDBES DE data or null
#' @param LE Data table of RDBES DE data or null
#' @param SS Data table of RDBES DE data or null
#' @param SA Data table of RDBES DE data or null
#' @param FM Data table of RDBES DE data or null
#' @param BV Data table of RDBES DE data or null
#' @param VD Data table of RDBES DE data or null
#' @param SL Data table of RDBES DE data or null
#' @param CL Data table of RDBES DE data or null
#' @param CE Data table of RDBES DE data or null
#'
#' @return
#' @export
#'
newRDBESRawObject <- function(DE = NULL,
                              SD = NULL,
                              VS = NULL,
                              FT = NULL,
                              FO = NULL,
                              TE = NULL,
                              LO = NULL,
                              OS = NULL,
                              LE = NULL,
                              SS = NULL,
                              SA = NULL,
                              FM = NULL,
                              BV = NULL,
                              VD = NULL,
                              SL = NULL,
                              CL = NULL,
                              CE = NULL){


  # Check any arguments are either NULL or data tables
  stopifnot(is.null(DE) | ("data.table" %in% class(DE)))
  stopifnot(is.null(SD) | ("data.table" %in% class(SD)))
  stopifnot(is.null(VS) | ("data.table" %in% class(VS)))
  stopifnot(is.null(FT) | ("data.table" %in% class(FT)))
  stopifnot(is.null(FO) | ("data.table" %in% class(FO)))
  stopifnot(is.null(TE) | ("data.table" %in% class(TE)))
  stopifnot(is.null(LO) | ("data.table" %in% class(LO)))
  stopifnot(is.null(OS) | ("data.table" %in% class(OS)))
  stopifnot(is.null(LE) | ("data.table" %in% class(LE)))
  stopifnot(is.null(SS) | ("data.table" %in% class(SS)))
  stopifnot(is.null(SA) | ("data.table" %in% class(SA)))
  stopifnot(is.null(FM) | ("data.table" %in% class(FM)))
  stopifnot(is.null(BV) | ("data.table" %in% class(BV)))
  stopifnot(is.null(VD) | ("data.table" %in% class(VD)))
  stopifnot(is.null(SL) | ("data.table" %in% class(SL)))
  stopifnot(is.null(CL) | ("data.table" %in% class(CL)))
  stopifnot(is.null(CE) | ("data.table" %in% class(CE)))


  # Create the named list
  x <- list(DE = DE,
            SD = SD,
            VS = VS,
            FT = FT,
            FO = FO,
            TE = TE,
            LO = LO,
            OS = OS,
            LE = LE,
            SS = SS,
            SA = SA,
            FM = FM,
            BV = BV,
            VD = VD,
            SL = SL,
            CL = CL,
            CE = CE)

  # Set the class of the object
  class(x) <- c("RDBESRawObject","list")

  x

}
