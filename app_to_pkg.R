# the below function provides a handy way to bundle shiny app in an R package.
# Provide name of the package in `package_name`, directory to create the new package in `pkg_dir`, and location of shiny app in `shiny_app_dir`
# Other fields are optional and can be populated to make the package more useful for the user
shiny_app_to_R_package = function(package_name,
                                  pkg_dir,
                                  shiny_app_dir,
                                  force_pkg_dir_delete = FALSE,
                                  shiny_app_copy_pvt_files = FALSE,
                                  rscript_location = paste0(R.home(), "/bin/Rscript.exe"),
                                  pkg_author = "Who wrote it?",
                                  pkg_email = "none@none.com",
                                  pkg_headline = "What does it do?",
                                  pkg_description = "Describe your package?",
                                  pkg_license = "What license is it under?",
                                  pkg_version = "1.0.0"
) {
  
  # create package dir if it doesn't exists!
  if(!dir.exists(pkg_dir)) {
    dir.create(pkg_dir)
  } else {
    if(force_pkg_dir_delete == TRUE) {
      # clear all directory contents
      unlink(pkg_dir, recursive = T)
      dir.create(pkg_dir)
    } else {
      stop(paste0("Directory ", pkg_dir,
                  " already exists! Set force_pkg_dir_delete to TRUE to proceed!"))
    }
  }
  
  # create package directory and required files
  dir.create(paste0(pkg_dir, "/", package_name))
  
  # create .Rbuildignore file
  text = paste0('^.*\\.Rproj$',
                '\n',
                '^\\.Rproj\\.user$')
  writeLines(text = text,
             con = paste0(pkg_dir, "/", package_name, "/", ".Rbuildignore"))
  
  
  # create DESCRIPTION file
  text = paste0('Package: ', package_name, '\n',
                'Type: Package', '\n',
                'Title: ', pkg_headline, '\n',
                'Version: ', pkg_version, '\n',
                'Author: ', pkg_author, '\n',
                'Maintainer: The package maintainer <', pkg_email, '>', '\n',
                'Description: ', pkg_description, '\n',
                'License: ', pkg_license, '\n',
                'Encoding: UTF-8', '\n',
                'LazyData: true'
  )
  writeLines(text = text,
             con = paste0(pkg_dir, "/", package_name, "/", "DESCRIPTION"))
  
  
  # create NAMESPACE file
  text = paste0('exportPattern("^[[:alpha:]]+")')
  writeLines(text = text,
             con = paste0(pkg_dir, "/", package_name, "/", "NAMESPACE"))
  
  # create other required directories
  dir.create(paste0(pkg_dir, "/", package_name, "/", "R"))
  dir.create(paste0(pkg_dir, "/", package_name, "/", "man"))
  dir.create(paste0(pkg_dir, "/", package_name, "/", "inst"))
  
  
  # create main.R file in "R" folder
  text = paste0("#' @export run_app", '\n',
                '\n',
                'run_app = function(...) {', '\n',
                '  appDir <- system.file("shiny_app", package = "', package_name, '")', '\n',
                '  if (appDir == "") {', '\n',
                '    stop("Could not find the folder shiny_app! Try re-installing `', 
                package_name, '`.", call. = FALSE)', '\n',
                '  }', '\n',
                '  shiny::runApp(appDir, ...)', '\n',
                '}', '\n'
  )
  writeLines(text = text,
             con = paste0(pkg_dir, "/", package_name, "/", "R", "/", "main.R"))
  
  
  # create main.Rd file in "man" folder
  text = paste0('\\name{', 'run_app', '}', '\n',
                '\\alias{', 'run_app', '}', '\n',
                '\\title{', pkg_headline, '}', '\n',
                '\\usage{', '\n',
                'run_app()', '\n',
                '}', '\n',
                '\\description{', '\n',
                "Prints '", pkg_description, "'.", '\n',
                '}', '\n',
                '\\examples{', '\n',
                'run_app()', '\n',
                '}'
  )
  
  writeLines(text = text,
             con = paste0(pkg_dir, "/", package_name, "/", "man", "/", "main.Rd"))
  
  
  # copy shiny app to inst folder
  R.utils::copyDirectory(from = shiny_app_dir,
                         to = paste0(pkg_dir, "/", package_name, "/", "inst", "/", "shiny_app"),
                         private = shiny_app_copy_pvt_files,
                         recursive = TRUE,
                         copy.mode = TRUE, copy.date = TRUE)
  
  
  # build package
  zip_pkg = devtools::build(pkg = paste0(pkg_dir, "/", package_name),
                            path = pkg_dir,
                            binary = TRUE)
  
  
  # list all the packages in shiny app
  pkgs = packages_within_a_folder(dir = shiny_app_dir, include_RProfile = T)
  # create package installation file: package_init.R
  text = missing_package_R_code(pkgs, return_text = T)
  
  writeLines(text = text,
             con = paste0(pkg_dir, "/", "package_init.R"))
  

  # create run.R file
  text = paste0("if(length(list.files(path = .libPaths(), ", '\n',
                "                     ",
                "pattern = paste0('^', '", package_name, "', '$'))) == 0) {", '\n',
                '  install.packages("', zip_pkg, '", repos = NULL, type = "source")', '\n',
                '}', '\n',
                "source(\"package_init.R\")", "\n",
                'DSv2lite::run_app(launch.browser = TRUE)', "\n")
  
  writeLines(text = text,
             con = paste0(pkg_dir, "/", "run.R"))
  
  
  # create app.bat file
  text = paste0('"',
                rscript_location,
                '" "',
                paste0(pkg_dir, "/", "run.R"),
                '"')
  writeLines(text = str_replace_all(text, "/", "\\\\"),
             con = paste0(pkg_dir, "/", "app.bat"))
  
  
  
}

# usage
# shiny_app_to_R_package(package_name = "app-pkg",
#                        pkg_dir = "C:/Users/nikhi/Documents/app-pkg",
#                        shiny_app_dir = "C:/Users/nikhi/Documents/app",
#                        force_pkg_dir_delete = T)

