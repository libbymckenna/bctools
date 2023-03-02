# This script contains general tools
# No idea what else to add

########################################################################################################################*

#' Sets a general file location so different users don't have to change their R Scripts. Note that this works with ProjectWise Drive
#' and OneDrive when you have connected projects to your local system.
#'
#' @param Drive Drive type, choose between "OneDrive", "ProjectWise", and "PersonalOneDrive"
#' @param ProjectFolder Specify the rest of the file path in quotes
#'
#' @examples bc_drive("Reuse Pilot/Data", "Personal One Drive")
#' bc_drive("000000 - Project Name/001 Task Name/01 Subfolder", "projectwise")
#'
#' @export
#'
bc_drive <- function(ProjectFolder, Drive = "OneDrive") {
  UserID <- Sys.info()["user"]

  if (grepl("^one ?drive ?$", Drive, ignore.case = TRUE)) {
    DrivePath <- "/OneDrive - Brown and Caldwell/"
  } else if (grepl("^project ?wise ?$", Drive, ignore.case = TRUE)) {
    DrivePath <- "/ProjectWise/Brown & Caldwell/"
  } else if (grepl("(my)?(personal)?(documents)? ?one ?drive ?(personal)?(documents)?", Drive, ignore.case = TRUE)) {
    DrivePath <- "/OneDrive - Brown and Caldwell/Documents/"
  } else {
    warning("Specified drive does not match current options")
  }

  setwd(paste0("C:/Users/", UserID, DrivePath, ProjectFolder))

}


