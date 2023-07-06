# This script contains general tools
# No idea what else to add

########################################################################################################################*

#' Sets a general file location so different users don't have to change their R Scripts. Note that this works with ProjectWise Drive
#' and OneDrive when you have connected projects to your local system.
#'
#' @param drive Drive type. Defaults to the shared Code Review folder. Otherwise, choose between the following:
#'   * "CodeReview" (the default): The internally shared OneDrive folder that should typically house data files for R Scripts.
#'   This folder should be used whenever possible to make it easier for others to run and review your code.
#'   File path - "C:/Users/yourname/Brown and Caldwell/Sierra Johnson - GitHub Code Review Files/" or
#'   "C:/Users/yourname/OneDrive - Brown and Caldwell/GitHub Code Review Files/" (path depends on whether you used a shortcut or sync to add this folder to your OneDrive)
#'   * "OneDrive" : Other folders in the file path "C:/Users/yourname/Brown and Caldwell" or "C:/Users/yourname/OneDrive - Brown and Caldwell" if you haven't synced any shared folders.
#'   This method is recommended if you have project files only shared with certain people. (It also works with folders you've shared with others from "Shared Internally" folder).
#'   * "ProjectWise" : ProjectWise Drive files. Note that these are often slower than OneDrive files and are not recommended.
#'   File path:  C:/Users/yourname/ProjectWise/Brown & Caldwell
#'   * "MyOneDrive" : Files in your personal OneDrive documents.
#'   File path: C:/Users/yourname/OneDrive - Brown and Caldwell/Documents/
#' @param projectfolder Specify the rest of the file path in quotes
#'
#' @examples bc_drive("Reuse Pilot/Data", "Personal One Drive")
#' bc_drive("000000 - Project Name/001 Task Name/01 Subfolder", "projectwise")
#'
#' @export
#'
bc_drive <- function(projectfolder, drive = "CodeReview") {
  UserID <- Sys.info()["user"]

  if (grepl("code ?review ?", drive, ignore.case = TRUE)) {
    DrivePath <- "/Brown and Caldwell/Sierra Johnson - GitHub Code Review Files/"
    DrivePath2 <- "/OneDrive - Brown and Caldwell/GitHub Code Review Files/"
    DrivePath3 <- "/OneDrive - Brown and Caldwell/Shared Internally/GitHub Code Review Files/"
  } else if (grepl("^one ?drive ?$", drive, ignore.case = TRUE)) {
    DrivePath <- "Brown and Caldwell/"
    DrivePath2 <- "/OneDrive - Brown and Caldwell/"
    DrivePath3 <- "/OneDrive - Brown and Caldwell/Shared Internally/"
  } else if (grepl("^project ?wise ?$", drive, ignore.case = TRUE)) {
    DrivePath <- "/ProjectWise/Brown & Caldwell/"
    DrivePath2 <- DrivePath
    DrivePath3 <- DrivePath
  } else if (grepl("(my)?(personal)?(documents)? ?one ?drive ?(personal)?(documents)?", drive, ignore.case = TRUE)) {
    DrivePath <- "/OneDrive - Brown and Caldwell/Documents/"
    DrivePath2 <- DrivePath
    DrivePath3 <- DrivePath
  } else {
    stop("Specified drive does not match current options. Use 'CodeReview', 'OneDrive', 'ProjectWise' or 'MyOneDrive'")
  }

  dir1 <- paste0("C:/Users/", UserID, DrivePath)
  dir2 <- paste0("C:/Users/", UserID, DrivePath2)
  dir3 <- paste0("C:/Users/", UserID, DrivePath3)

  if(dir.exists(dir1)) {
    dir <- dir1
  } else if(dir.exists(dir2)) {
    dir <- dir2
  } else if(dir.exists(dir3)) {
    dir <- dir3
  } else {
    stop("No folders found in expected file PW or OneDrive paths. Contact Sierra or Libby to update this function with your file path.")
  }

  dir0 <- paste0(dir, projectfolder)
  dir1 <- paste0(dir1, projectfolder)
  dir2 <- paste0(dir2, projectfolder)
  dir3 <- paste0(dir3, projectfolder)

  if(dir.exists(dir0)) {
    setwd(dir0)
  } else if(dir.exists(dir1)) {
    setwd(dir1)
  } else if(dir.exists(dir2)) {
    setwd(dir2)
  } else if(dir.exists(dir3)) {
    setwd(dir3)
  } else {
    setwd(dir)
    warning("Project Folder not found. Working drive was set to main PW or OneDrive folder specified.")
  }

}


