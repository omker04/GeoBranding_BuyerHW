packages_required <- c('shiny', 'shinydashboard', 'shinyjs', 'shinyBS', 'leaflet', 'sp', 'RColorBrewer',
                  'dplyr', 'data.table', 'magrittr', 'rhandsontable', 'readxl', 'DT', 'rjson', 'stringr')


#pack_lib_dir <- '/home/AzrRootAdminUser/geo_brand_pkgs/'
pack_lib_dir <- .libPaths()
packages_installed <- rownames(installed.packages(lib.loc = pack_lib_dir))
packages_to_install <- setdiff(packages_required, packages_installed)
lapply(packages_to_install,
       function(pkg){install.packages(pkg,
                                      dependencies = TRUE,
                                      repos = "http://cran.wal-mart.com",
                                      lib = pack_lib_dir)})
lapply(packages_required,
       function(pkg){library(pkg,
                             character.only = TRUE,
                             lib.loc = pack_lib_dir)}) # Server
rm(packages_required, packages_installed, packages_to_install)

clean_varname <- . %>%  
  str_replace_all(" ", "_")

clean_dfname <- . %>%   
  set_names(names(.) %>% clean_varname())

load('shiny_tool_startUp.RData', envir = environment())
#wd <- '/home/AzrRootAdminUser/geo_brand/tool_v2/'
wd <- 'session Images/'

library(tools)

shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
        for (i in seq_len(len)) {
            inputs[i] <- as.character(FUN(inputId = paste0(id, i), ...))
        }
    inputs
}

shinyValue <- function(id, len) {
    if(is.null(len)) return(NULL)
    else{
        unlist(lapply(seq_len(len), function(i) {
            value <- input[[paste0(id, i)]]
            if (is.null(value)) NA else value
        }))}
}