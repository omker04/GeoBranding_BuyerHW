shinyServer(
    function(input, output, session){
        global <- reactiveValues(DOMRdy = FALSE)
        options(shiny.maxRequestSize = 5 * 1024^3) 
        ##### Session Events ####
        observeEvent(input$continue,{
            shinyjs::show(id = 'box1')
            shinyjs::show(id = 'box0')
            shinyjs::hide(id = 'loadedSession')
            shinyjs::hide(id = 'loadedSession1')
            updateTabsetPanel(session, inputId = 'tabs', selected = 'tab1')
        })
        observeEvent(input$load,{
            shinyjs::show(id = 'box0')  
            output$loadedSession1 <- renderValueBox({
                valueBox(value = input$sessionID, 
                         subtitle = 'Loaded Session-ID', 
                         color = 'green', width = 12)
            })
            shinyjs::show(id = 'selBrand')
        })
        
        observe({
            if (nchar(input$sessionID) > 1){
                shinyjs::disable(id = 'continue')
                shinyjs::enable(id = 'load')
                allSessions <- list.files(wd)[list.files(wd) %like% '_all.rds'] %>% 
                    strsplit(., split = '_all.rds', fixed = TRUE) %>% 
                    unlist()
                if (allSessions %>% grepl(input$sessionID, ., fixed = TRUE) %>% sum() > 0){
                    output$loadedSession1 <- renderValueBox({
                        valueBox(value = input$sessionID, 
                                 subtitle = 'Your input seems to match with an existing Session-ID', 
                                 color = 'green', width = 12)
                    })
                } else {
                    output$loadedSession1 <- renderValueBox({
                        valueBox(value = input$sessionID, 
                                 subtitle = 'Your input DOES NOT seem to match with any existing Session-ID. Check your Input Please.', 
                                 color = 'red', width = 12)
                    })
                }
            } else {
                shinyjs::enable(id = 'continue')
                shinyjs::disable(id = 'load')
                output$loadedSession1 <- renderValueBox({
                    NULL
                })
            }
        })
        
        ###### File Input Read #####
        
        files <- reactiveValues(
            sheetNames = NULL,
            storeTable = NULL,
            dma_upc = NULL,
            clusterFile = NULL,
            vendorID_file = NULL
        )
        
        sheetname <- reactive({
            if(is.null(input$dma_upc)){
                return(NULL)
            } else {
                print(input$dma_upc$datapath)
                file.rename(input$dma_upc$datapath,
                            paste0(input$dma_upc$datapath, '.xlsx'))
                sheets <- readxl::excel_sheets(path = paste0(input$dma_upc$datapath, '.xlsx'))
                print(head(sheets))
                return(sheets)
            }
        })
        
        
        dma_upc <- reactive({
            if (is.null(input$dma_upc)){
                return(NULL)
            } else {
                withProgress(value = 0.1, message = 'Reading Uploaded File', detail = 'Identifying Worksheet to read', {
                    allSheets <- sheetname()
                    whichDMA <- which(toupper(allSheets) %like% 'DMA_UPC')
                    # file.rename(input$dma_upc$datapath,
                    #             paste0(input$dma_upc$datapath, '.xlsx'))
                    incProgress(amount = 0.3, detail = 'Reading Data')
                    dma_upc <- read_excel(path = paste0(input$dma_upc$datapath, '.xlsx'), 
                                          sheet = whichDMA) %>% 
                        clean_dfname()
                    incProgress(amount = 0.8)
                    return(dma_upc)
                })
            }
        })
        
        storetable <- reactive({
            if (is.null(input$dma_upc)){
                return(NULL)
            } else {
                withProgress(value = 0.1, message = 'Reading Uploaded File', detail = 'Identifying Worksheet to read', {
                    allSheets <- sheetname()
                    whichST <- which(toupper(allSheets) %like% 'STORETABLE')
                    # file.rename(input$dma_upc$datapath,
                    #             paste0(input$dma_upc$datapath, '.xlsx'))
                    incProgress(amount = 0.3, detail = 'Reading Data')
                    storeTable <- readxl::read_excel(path = paste0(input$dma_upc$datapath, '.xlsx'),
                                                  sheet = whichST) %>% 
                        clean_dfname() %>% 
                        set_colnames(., tolower(colnames(.)))
                    return(storeTable)
                })
            }
        })
        
        vendorID_file <- reactive({
            if (is.null(input$vendor_mtx)){
                return(1)
            } else {
                if (file_ext(input$vendor_mtx$name) == 'csv'){
                    vendor_mtx <- read.csv(input$vendor_mtx$datapath)
                } else {
                    file.rename(input$vendor_mtx$datapath,
                                paste0(input$vendor_mtx$datapath, '.xlsx'))
                    vendor_mtx <- readxl::read_excel(input$vendor_mtx$datapath)
                }
                return(vendor_mtx)
            }
        })
        
        observe(
            print(head(vendorID_file()))
        )
        
        observeEvent(input$startDataPrep,{
            files$storeTable <- storetable()
            if (sum(is.na(files$storeTable$ecs_cluster)) > 0.5 * nrow(files$storeTable)) {
               files$storeTable %<>%  
                    select(-ecs_cluster)
            }
            if (sum(is.na(files$storeTable$fixture)) > 0.5 * nrow(files$storeTable)) {
                files$storeTable %<>%  
                    select(-fixture)
            }
            if (sum(is.na(files$storeTable$footage)) > 0.5 * nrow(files$storeTable)) {
                files$storeTable %<>%  
                    select(-footage)
            }
            
            
            if ('ecs_cluster' %in% colnames(files$storeTable)) {
                files$clusterFile <- files$storeTable %>% 
                    select(store_number, ecs_cluster) %>% 
                    unique()
            } else {
                files$clusterFile <- NULL
            }
            
            # if (!is.null(dim(files$clusterFile))){
            #     if ('ecs_cluster' %in% colnames(files$storeTable)){
            #         files$storeTable %<>% 
            #             select(-ecs_cluster)
            #     }
            #     if (ncol(files$clusterFile) == 2){
            #         files$clusterFile %<>% 
            #             set_colnames(., c('store_number', 'ecs_cluster'))
            #         files$storeTable %<>% 
            #             inner_join(., files$clusterFile)
            #     } else {
            #         if (ncol(files$clusterFile) == 3){
            #             files$clusterFile %<>% 
            #                 set_colnames(., c('store_number', 'initial_cluster', 'reclassified_cluster')) %>% 
            #                 mutate('ecs_cluster' = if_else(is.na(initial_cluster), reclassified_cluster, initial_cluster)) %>% 
            #                 select(store_number, ecs_cluster) %>% 
            #                 mutate_each(funs(as.numeric), store_number)
            #             print(str(files$clusterFile))
            #             print(head(files$clusterFile))
            #             print(str(files$storeTable))
            #             files$storeTable %<>% 
            #                 inner_join(., files$clusterFile)
            #         } else {
            #             createAlert(session, anchorId = 'ecsAlert', alertId = 'ecs',
            #                     title = 'Demand Cluster File Format not Appropriate:',
            #                     content = 'The input File must have 2 columns, (Store, Cluster) or 3 columns (Store, Sales Cluster, Predicted Cluster). Current input file is being IGNORED !',
            #                     style = 'warning', dismiss = TRUE, append = TRUE)
            #         }
            #     }
            # }
            
            files$vendorID_file <- vendorID_file()
            if(!is.null(dim(files$vendorID_file))){
                if(ncol(files$vendorID_file) == 4) {
                    files$vendorID_file %<>% 
                        set_colnames(c('dma', 'concat', 'store_number', 'vendorID_geoCluster'))
                    files$storeTable %<>% 
                        inner_join(., files$vendorID_file %>% 
                                       select(store_number, vendorID_geoCluster))
                }  
                
                if(ncol(files$vendorID_file) > 4) {
                    files$vendorID_file %<>% 
                        mutate(concat = apply(., 1, function(x) paste(x[-1], collapse = '-'))) %>% 
                        mutate(vendorID_geoCluster = as.numeric(as.factor(concat))) %>% 
                        select(dma_id, vendorID_geoCluster)
                    
                    files$storeTable %<>% 
                        inner_join(., files$vendorID_file)
                }
            }
            
            files$dma_upc <- dma_upc()
        })
        
        ###### Data Prep ####
        initialStart <- reactiveValues(
            DFforLabel = NULL,
            dc_coord = NULL,
            allDCs = NULL,
            dc_label = NULL,
            dc_store = NULL,
            brandStatus = NULL,
            dma_upc_brand_info = NULL,
            dma_brand_performance1 = NULL,
            store_brand_comb = NULL,
            geoBrandCluster = NULL,
            session = NULL,
            load_check = NULL
        )
        
        ##### Load ####
        observeEvent(input$load,{
            if (!file.exists(paste0(wd,input$sessionID, '_startUp.rds'))){
                output$loadedSession1 <- renderValueBox({
                    valueBox(value = input$sessionID, 
                             subtitle = h3(strong('Session-ID does not exist !!')), 
                             color = 'red', width = 12)
                })
            } else {
                withProgress(message = 'Loading Session : ', detail = 'Reading Loaded Session Info', value = 0.3,{
                    rdsFile <- readRDS(file = paste0(wd, input$sessionID, '_startUp.rds'))
                    #rdsFile <- readRDS(file = paste0(input$sessionID, '_startUp.rds'))
                    filesLoaded <- rdsFile$files
                    initial <- rdsFile$initialStart
                    
                    incProgress(amount = 0.2, detail = 'Applying Loaded Sesssion Info')
                    files$storeTable <- filesLoaded$storeTable
                    files$dma_upc <- filesLoaded$dma_upc
                    files$clusterFile <- filesLoaded$clusterFile
                    
                    incProgress(amount = 0.2)
                    initialStart$DFforLabel <- initial$DFforLabel
                    initialStart$dc_coord <- initial$dc_coord
                    initialStart$allDCs <- initial$allDCs
                    initialStart$dc_label <- initial$dc_label
                    initialStart$dc_store <- initial$dc_store
                    initialStart$brandStatus <- initial$brandStatus
                    initialStart$dma_upc_brand_info <- initial$dma_upc_brand_info
                    initialStart$dma_brand_performance1 <- initial$dma_brand_performance1
                    initialStart$store_brand_comb <- initial$store_brand_comb
                    initialStart$geoBrandCluster <- initial$geoBrandCluster
                    initialStart$session <- initial$session
                    incProgress(amount = 0.2)
                })
                updateTabsetPanel(session, inputId = 'tabs', selected = 'tab1')
            }
        })
        
        ##### Create #####
        observeEvent(input$startDataPrep,{
            withProgress(message = 'Data Preparation in Progress', 
                         detail = 'Creating Required Input Data', value = 0, {
                             
                             ##### DC info gathering #####
                             initialStart$allDCs <- unique(c(files$storeTable$dry_grocery_dc,
                                                             files$storeTable$perishable_dc,
                                                             files$storeTable$regional_dc)) %>% na.omit()
                             
                             initialStart$dc_coord <- dc_coord %<>% 
                                 set_colnames(., c('longitude', 'latitude', 'dc_nbr')) %>% 
                                 filter(dc_nbr %in% initialStart$allDCs) %>% 
                                 mutate('regional' = if_else(dc_nbr %in% files$storeTable$regional_dc, 'TRUE', 'FALSE')) %>% 
                                 mutate('perishable' = if_else(dc_nbr %in% files$storeTable$perishable_dc, 'TRUE', 'FALSE')) %>% 
                                 mutate('dry_grocery' = if_else(dc_nbr %in% files$storeTable$dry_grocery_dc, 'TRUE', 'FALSE'))
                             
                             ##### DC Hover Data Creation #####
                             initialStart$dc_label <- initialStart$dc_coord %>% 
                                 select(dc_nbr, regional, perishable, dry_grocery) %>% 
                                 apply(., 1, function(x) {
                                     names(x) <- colnames(.)
                                     return(paste(names(x), x, sep = ' :: '))
                                 }) %>% 
                                 t() %>% 
                                 as.data.frame() %>% 
                                 mutate(dc_nbr = dc_coord$dc_nbr)
                             
                             initialStart$dc_store <- files$storeTable %>% 
                                 select(c(regional_dc, store_number)) %>% 
                                 unique() %>% 
                                 set_colnames(c('dc_nbr', 'store_number'))
                             initialStart$dc_store <- rbind(initialStart$dc_store, 
                                                            files$storeTable %>% 
                                                                select(c(perishable_dc, store_number)) %>% 
                                                                unique() %>% 
                                                                set_colnames(c('dc_nbr', 'store_number')))
                             initialStart$dc_store <- rbind(initialStart$dc_store, 
                                                            files$storeTable %>% 
                                                                select(c(dry_grocery_dc, store_number)) %>% 
                                                                unique() %>% 
                                                                set_colnames(c('dc_nbr', 'store_number')))
                             initialStart$dc_store %<>% 
                                 unique() %>% 
                                 arrange(dc_nbr) %>% 
                                 inner_join(., files$storeTable %>% 
                                                select(c(store_number, latitude, longitude))
                                 )
                             
                             ##### DMA_UPC data creation #####
                             incProgress(amount = 0.2, detail = 'Creating DMA Status')
                             ### Column Names Reformatting ####
                             colnames(files$dma_upc)[colnames(files$dma_upc) %>% 
                                                         tolower() %like% 
                                                         'future' %>% 
                                                         which()] <- 'FutureStateItem'
                             colnames(files$dma_upc)[colnames(files$dma_upc) %>% 
                                                         tolower() %like% 
                                                         'must' %>% 
                                                         which()] <- 'MustHaveBrand'
                             colnames(files$dma_upc)[colnames(files$dma_upc) %>% 
                                                         tolower() %like% 
                                                         'nice' %>% 
                                                         which()] <- 'NiceToHaveBrand'
                             
                             #print(colnames(files$dma_upc))
                             ### Adding Status ####
                             files$dma_upc %<>% 
                                 mutate('dma_status' = case_when(.$WM_Selling == 0 & .$FutureStateItem == 1 ~ 'Add',
                                                                 .$WM_Selling == 1 & .$FutureStateItem == 1 ~ 'Maintain',
                                                                 .$WM_Selling == 1 & .$FutureStateItem == 0 ~ 'Remove',
                                                                 .$WM_Selling == 0 & .$FutureStateItem == 0 ~ 'Under Consideration')
                                 )
                             ### Adding to_have's conditionally ####
                             if (sum(colnames(files$dma_upc) %>% tolower() %like% 'have') != 0){
                                 incProgress(amount = 0.2, detail = 'Joining with Store Information')
                                 files$dma_upc %<>% 
                                     mutate('to_have' = case_when(.$MustHaveBrand == 1 & .$NiceToHaveBrand == 1 ~ '',
                                                                  .$MustHaveBrand == 1 & .$NiceToHaveBrand == 0 ~ 'Must Have',
                                                                  .$MustHaveBrand == 0 & .$NiceToHaveBrand == 1 ~ 'Nice to Have',
                                                                  .$MustHaveBrand == 0 & .$NiceToHaveBrand == 0 ~ '')
                                     ) %>% 
                                     mutate_each(funs(as.numeric), dma_id, wm_dollars, tm_dollars, upc, brand_contribution_dma) %>% 
                                     mutate_each(funs(as.character), dma, category, segment, brand_family_desc) %>% 
                                     group_by(dma_id, dma, category, segment, brand_family_desc) %>% 
                                     summarise(wm_dollars = sum(wm_dollars, na.rm = TRUE),
                                               tm_dollars = sum(tm_dollars, na.rm = TRUE),
                                               upc = length(unique(upc)),
                                               brand_contribution_dma = mean(brand_contribution_dma, na.rm = TRUE),
                                               dma_status = mode(dma_status),
                                               to_have = mode(to_have)) %>% 
                                     ungroup()
                             } else {
                                 incProgress(amount = 0.2, detail = 'Joining with Store Information')
                                 files$dma_upc %<>% 
                                     mutate_each(funs(as.numeric), dma_id, wm_dollars, tm_dollars, upc, brand_contribution_dma) %>% 
                                     mutate_each(funs(as.character), dma, category, segment, brand_family_desc) %>% 
                                     group_by(dma_id, dma, category, segment, brand_family_desc) %>% 
                                     summarise(wm_dollars = sum(wm_dollars, na.rm = TRUE),
                                               tm_dollars = sum(tm_dollars, na.rm = TRUE),
                                               upc = length(unique(upc)),
                                               brand_contribution_dma = mean(brand_contribution_dma, na.rm = TRUE),
                                               dma_status = mode(dma_status)) %>% 
                                     ungroup() 
                             }
                             ### Adding Store Info conditionally ####
                             columns <- c('footage', 'ecs_cluster')[c('footage', 'ecs_cluster') %in% colnames(files$storeTable)]
                             files$dma_upc %<>% 
                                 inner_join(., files$storeTable %>% 
                                                select_(.dots = lapply(c('store_number', 'dma_id', 'latitude', 'longitude', columns), 
                                                                       as.symbol))
                                 ) %>% 
                                 as.data.frame()
                             
                             incProgress(amount = 0.2)
                             gsubDF <- files$dma_upc %>% 
                                 select(dma_id, dma) %>% 
                                 unique() %>% 
                                 mutate(dma_name = apply(., 1, function(y) 
                                     gsub(pattern = 'DMA', 
                                          replacement = y[1], 
                                          x = y[2])
                                 )
                                 )
                             
                             files$dma_upc %<>% 
                                 inner_join(., gsubDF) %>% 
                                 as.data.table()
                             
                             ##### 1st tab Show ####
                             incProgress(amount = 0.1, detail = 'Creating Brand Status Table')
                             #print(colnames(files$dma_upc))
                             initialStart$brandStatus <- files$dma_upc %>% 
                                 group_by(brand_family_desc, store_number) %>% 
                                 summarise(dma_status = mode(dma_status),
                                           wm_dollars = sum(wm_dollars),
                                           to_have = mode(to_have)) %>% 
                                 mutate(current_sell = if_else(dma_status %in% c('Maintain', 'Remove'), 1, 0),
                                        future_sell = if_else(dma_status %in% c('Maintain', 'Add'), 1, 0)
                                 )
                             
                             initialStart$dma_upc_brand_info <- initialStart$brandStatus %>% 
                                 group_by(brand_family_desc) %>% 
                                 summarise(revenue = round(sum(wm_dollars), 2),
                                           current_stores = sum(current_sell),
                                           recommended_stores = sum(future_sell)
                                 ) %>% 
                                 arrange(desc(revenue)) %>% 
                                 mutate(revenue_cumsum = cumsum(revenue)) %>% 
                                 ungroup()
                             
                             ##### Data for Heatmap ####
                             incProgress(amount = 0.1, detail = 'Creating Data for Heatmap')
                             initialStart$dma_brand_performance1 <- files$dma_upc %>% 
                                 select(dma_id, dma, brand_family_desc, category, segment, wm_dollars, tm_dollars) %>% 
                                 unique() %>% 
                                 group_by(dma_id, brand_family_desc, category, segment) %>%
                                 summarise(wm_dollars = sum(wm_dollars),
                                           tm_dollars = sum(tm_dollars)) %>%
                                 ungroup() %>%
                                 arrange(brand_family_desc) %>% 
                                 as.data.frame() %>% 
                                 right_join(., expand.grid('dma_id' = unique(files$dma_upc$dma_id), 
                                                           'brand_family_desc'=unique(files$dma_upc$brand_family_desc)))
                             
                             initialStart$store_brand_comb <- expand.grid('dma_id' = unique(files$dma_upc$dma_id),
                                                                          'brand_family_desc' = unique(files$dma_upc$brand_family_desc)
                             ) %>% 
                                 mutate_each(funs(as.character), brand_family_desc)
                             
                             ##### Creating GeoClusters ####
                             incProgress(amount = 0.1, detail = 'Creating GeoClusters')
                             initialStart$geoBrandCluster <- files$dma_upc %>% 
                                 select(dma_id, brand_family_desc, dma_status) %>% 
                                 group_by(dma_id, brand_family_desc) %>% 
                                 mutate(dma_status = mode(dma_status)) %>% 
                                 unique() %>% 
                                 as.data.frame() %>% 
                                 right_join(., initialStart$store_brand_comb) %>% 
                                 mutate(presence_code1 = case_when(.$dma_status == 'Maintain' ~ 1,
                                                                   .$dma_status == 'Add' ~ 1,
                                                                   .$dma_status == 'Remove' ~ 0,
                                                                   .$dma_status == 'Under Consideration' ~ 0)) %>% 
                                 mutate(presence_code = if_else(is.na(dma_status), 0, presence_code1)) %>% 
                                 select(dma_id, brand_family_desc, presence_code) %>% 
                                 as.data.frame() %>% 
                                 reshape(., timevar = 'brand_family_desc', idvar = 'dma_id', direction = 'wide') %>% 
                                 mutate(concat = apply(., 1, function(x) paste(x[-1], collapse = '-'))) %>% 
                                 mutate(geoCluster = as.numeric(as.factor(concat))) %>% 
                                 select(dma_id, concat, geoCluster, everything())
                             
                             files$storeTable %<>%  
                                 inner_join(., initialStart$geoBrandCluster %>% 
                                                select(dma_id, geoCluster) %>% 
                                                as.data.table(), by = 'dma_id')
                             files$storeTable$geocluster <- files$storeTable$geoCluster
                             files$storeTable$geoCluster <- NULL
                             
                             ##### Store Hover Data Creation #####
                             if ('ecs_cluster' %in% colnames(files$storeTable)){
                                 initialStart$DFforLabel <- files$storeTable %>% 
                                     select(store_number, store_type, city, state, dma_id, datadma,
                                            regional_dc, dry_grocery_dc, perishable_dc, geocluster, ecs_cluster) %>% 
                                     mutate(dma_name = apply(., 1, function(x) gsub('DMA', x[5], x[6]))) %>% 
                                     mutate(STORE_FEATURES = '') %>% 
                                     select(store_number, STORE_FEATURES, store_type, dma_name, regional_dc, dry_grocery_dc, perishable_dc, geocluster, ecs_cluster) %>% 
                                     mutate(DEMOGRAPHICS = '') %>% 
                                     left_join(., final_store_demo, by = c('store_number' = 'store_nbr'))
                             } else {
                                 initialStart$DFforLabel <- files$storeTable %>% 
                                     select(store_number, store_type, city, state, dma_id, datadma,
                                            regional_dc, dry_grocery_dc, perishable_dc, geocluster) %>% 
                                     mutate(dma_name = apply(., 1, function(x) gsub('DMA', x[5], x[6]))) %>% 
                                     mutate(STORE_FEATURES = '') %>% 
                                     select(store_number, STORE_FEATURES, store_type, dma_name, regional_dc, dry_grocery_dc, perishable_dc, geocluster) %>% 
                                     mutate(DEMOGRAPHICS = '') %>% 
                                     left_join(., final_store_demo, by = c('store_number' = 'store_nbr'))
                             }
                             colname <- which(colnames(initialStart$DFforLabel) %in% c('STORE_FEATURES', 'DEMOGRAPHICS'))
                             colnames(initialStart$DFforLabel)[colname] <- paste0(':: ', c('STORE_FEATURES', 'DEMOGRAPHICS'))
                             initialStart$DFforLabel %<>% 
                                 apply(., 1, function(x) {
                                     names(x) <- colnames(.)
                                     return(paste(names(x), x, sep = ' :: '))
                                 }) %>% 
                                 t() %>% 
                                 as.data.frame()
                             initialStart$DFforLabel$store_number <- files$storeTable$store_number
                             
                             ##### Getting SessionID ####
                             symbol <- c('-', '#', '@', '$', '%', '^', '&', '*', '+', '<', '>', '?')
                             initialStart$session <- paste0(sample(1:9999, 2, replace = TRUE), 
                                                            sample(symbol, 2, replace = TRUE),
                                                            sample(letters, 2, replace = TRUE), 
                                                            sample(LETTERS, 2, replace = TRUE), collapse = '-')
                             saveRDS(object = list('files' = reactiveValuesToList(files),
                                                   'initialStart' = reactiveValuesToList(initialStart)), 
                                     file = paste0(wd, initialStart$session, '_startUp.rds', collapse = ''))
                             #file = paste0(initialStart$session, '_startUp.rds', collapse = ''))
                             #print(paste(paste0(initialStart$session, '_startUp.rds', collapse = ''),  '   saved'))
                             incProgress(amount = 0.1)
                         })
        })
        
        
        observe({
            if (!is.null(initialStart$session)){
                shinyjs::show(id = 'box0')
                output$session <- renderValueBox({
                    valueBox(value = initialStart$session, 
                             subtitle = h3('This is your Session-ID. Please SAVE this for FUTURE USE.'), 
                             color = 'red', width = 12)
                })
            }
        })
        
        
        ###### Creating Alerts #####
        observe({
            if (!is.null(files$dma_upc) &
                is.null(files$clusterFile %>% dim())
            ){
                #print(is.null(files$dma_upc))
                #print(!is.null(files$clusterFile %>% dim()))
                #print(files$clusterFile %>% dim())
                createAlert(session, anchorId = 'ecsAlert', alertId = 'ecs', 
                            title = 'Demand Cluster (ECS Cluster) Information not Up-Dated in the Store Table Tab', 
                            content = 'The Demand Cluster filter will not be Usable', 
                            style = 'warning', dismiss = TRUE, append = TRUE)
            }
            if (!is.null(files$dma_upc) &
                !is.null(files$storeTable)
            ){
                if(!'footage' %in% colnames(files$storeTable) &
                   'fixture' %in% colnames(files$storeTable)) {
                    content = 'The Fixture filter will not be accessible'
                }
                if('footage' %in% colnames(files$storeTable) &
                   !'fixture' %in% colnames(files$storeTable)) {
                    content = 'The Footage filter will not be accessible'
                }
                if(!'footage' %in% colnames(files$storeTable) &
                   !'fixture' %in% colnames(files$storeTable)) {
                    content = 'The Fixture and Footage filters will not be accessible'
                }
                createAlert(session, anchorId = 'StoreTableAlert', alertId = 'fixture',
                            title = 'Store Table Tab is Not not Up-to-Date',
                            content = content,
                            style = 'warning', dismiss = TRUE, append = TRUE)
            }
            if (!is.null(files$dma_upc) &
                sum(colnames(files$dma_upc) %>% tolower() %like% 'have') == 0
            ){
                createAlert(session, anchorId = 'DMA_UPC_Alert', alertId = 'to_have',
                            title = 'Must Have / Nice to Have Information Not Available',
                            content = 'You may either proceed without the same, or upload an updated version of the DMA_UPC file',
                            style = 'warning', dismiss = TRUE, append = TRUE)
            }
            if (!is.null(files$dma_upc) &
                is.null(input$vendor_mtx)
            ){
                createAlert(session, anchorId = 'vendorID_Alert', alertId = 'vendor',
                            title = 'Vendor-ID based GeoCluster file Not Uploaded',
                            content = 'The Vendor-ID option to color the Stores on the Map will not be accessible',
                            style = 'warning', dismiss = TRUE, append = TRUE)
            }
        })
        
        
        
        ###### Input Data Load #####
        observe({
            if (!is.null(initialStart$session)){
                #print('secondTab')
                shinyjs::show(id = 'box2', anim = TRUE)
                shinyjs::disable(id = 'startDataPrep')
                shinyjs::show(id = 'secondTab')
            }
        })
        
        ### Create Reactive Values ####
        tab_check <- reactiveValues(check = FALSE)
        
        editData <- reactiveValues(
            ourInfo = NULL,
            tab = NULL
        )
        loaded <- reactiveValues(
            input = NULL
        )
        
        allData <- reactiveValues(
            storeTable = NULL,
            #storeTable_filtered = NULL,
            brandStatus = NULL,
            #ourInfo = NULL,
            dc_store = NULL,
            RtoHot = NULL,
            brand = '',
            layer_by = '',
            layer = '',
            noDF = NULL,
            color = NULL,
            colorDF = NULL,
            dma_geo = NULL,
            store_brand_comb = NULL,
            geo_change_ind = NULL,
            store_brand_ind = NULL,
            dma_upc_brand_info = NULL,
            dma_upc_brand_info_to_show = NULL,
            sel_brands = NULL)
        
        colors <-  reactiveValues(
            variable = NULL,
            state = NULL,
            colorState = NULL,
            color = NULL,
            output = NULL,
            radius = 3
        )
        
        click <- reactiveValues(
            n = 0,
            df = NULL,
            pointsSel = integer(0),
            storesSel = integer(0),
            DCmarker = NULL,
            storeMarker = NULL,
            marker = NULL,
            allornot = NULL,
            hoverPoint = NULL
        )
        
        markerInput <- reactiveValues(
            check1 = NULL, 
            check2 = NULL, 
            check3 = NULL, 
            check4 = NULL, 
            check5 = NULL, 
            check6 = NULL, 
            check7 = NULL
            )
        
        ### Important Observes ####
        observe({
            shinyjs::hide(id = 'pre_download')
            if (input$tabs == 'tab2'){
                shinyjs::show(id = 'clear')
                shinyjs::show(id = 'save_session')
                shinyjs::show(id = 'descTable')
            } else {
                shinyjs::hide(id = 'clear')
                shinyjs::hide(id = 'save_session')
                shinyjs::hide(id = 'descTable')
            }
        })
        
        observe({
            if('ecs_cluster' %in% colnames(files$storeTable) &
               'vendorID_geoCluster' %in% colnames(files$storeTable)){
                updateSelectInput(session, inputId = 'color_by', 
                                  choices = c('geocluster', 'ecs_cluster', 'vendorID_geoCluster', 'state'))
            }
            if('ecs_cluster' %in% colnames(files$storeTable) &
               !'vendorID_geoCluster' %in% colnames(files$storeTable)){
                updateSelectInput(session, inputId = 'color_by', 
                                  choices = c('geocluster', 'ecs_cluster', 'state'))
            }
            if('vendorID_geoCluster' %in% colnames(files$storeTable) &
               !'ecs_cluster' %in% colnames(files$storeTable)){
                updateSelectInput(session, inputId = 'color_by', 
                                  choices = c('geocluster', 'vendorID_geoCluster', 'state'))
            }
        })
        
        plotColors <- reactive({
            colors$variable <- which(colnames(allData$storeTable) == input$color_by)
            cols <- c(brewer.pal(10,"Paired"), brewer.pal(9,"Set1")[-(1:6)], brewer.pal(8,"Dark2")[-c(2,3,5)], 
                      brewer.pal(8,"Accent")[5:7], brewer.pal(8,"Set2"), brewer.pal(8,"Accent")[1:3], 
                      brewer.pal(10,"Spectral"), brewer.pal(10,"BrBG"), brewer.pal(10,"RdBu"))
            colors$color <- colorFactor(cols, levels = unique(allData$storeTable %>% 
                                                                  select(colors$variable) %>% 
                                                                  as.matrix() %>% 
                                                                  as.vector()))
            colors$output <- colors$color(allData$storeTable %>% select(colors$variable) %>% as.matrix() %>% as.vector())
            return(colors$output)
        })
        
        eventReactive(input$map_zoom, {
            if (input$map_zoom <= 5){
                colors$radius <- 3
            } else {
                colors$radius <- as.numeric(input$map_zoom)
            }
        })
        
        observe({
            print(input$tabs)
            if(input$tabs == 'tab1'){
                tab_check$check <- TRUE
            }
            print(tab_check$check)
        })
        
        
        ### Main Observe ####
        
        observe({
            ##### Load ####
            observeEvent(input$load,{
                if (is.null(initialStart$load_check)){
                    if (!file.exists(paste0(wd,input$sessionID, '_startUp.rds'))){
                        output$loadedSession1 <- renderValueBox({
                            valueBox(value = input$sessionID, 
                                     subtitle = h3(strong('Session-ID does not exist !!')), 
                                     color = 'red', width = 12)
                        })
                    } else {
                        withProgress(message = 'Loading Session :', detail = 'Reading previously saved session', value = 0.1, {
                            rdsFile <- readRDS(file = paste0(wd, input$sessionID, '_all.rds'))
                            #rdsFile <- readRDS(file = paste0(input$sessionID, '_all.rds'))
                            load_input <- rdsFile$input
                            load_allData <- rdsFile$allData
                            load_colors <- rdsFile$colors
                            load_click <- rdsFile$click
                            load_editData <- rdsFile$editData
                            
                            incProgress(amount = 0.2, detail = 'Applying inputs of previous session')
                            allData$storeTable <- load_allData$storeTable
                            allData$layer <- load_allData$layer
                            allData$brandStatus <- load_allData$brandStatus
                            allData$noDF <- load_allData$noDF
                            allData$brand <- load_allData$brand
                            allData$dc_store <- load_allData$dc_store
                            allData$geo_change_ind <- load_allData$geo_change_ind
                            allData$colorDF <- load_allData$colorDF
                            allData$dma_geo <- load_allData$dma_geo
                            allData$RtoHot <- load_allData$RtoHot
                            allData$color <- load_allData$color
                            allData$dma_upc_brand_info <- load_allData$dma_upc_brand_info
                            allData$dma_upc_brand_info_to_show <- load_allData$dma_upc_brand_info_to_show
                            allData$layer_by <- load_allData$layer_by
                            allData$store_brand_comb <- load_allData$store_brand_comb
                            allData$store_brand_ind <- load_allData$store_brand_ind
                            allData$sel_brands <- load_allData$sel_brands
                            
                            editData$ourInfo <- load_editData$ourInfo
                            
                            incProgress(amount = 0.2)
                            click$n <- load_click$n
                            click$df <- load_click$df
                            click$pointsSel <- load_click$pointsSel
                            click$storesSel <- load_click$storesSel
                            click$DCmarker <- load_click$DCmarker
                            click$storeMarker <- load_click$storeMarker
                            click$marker <- load_click$marker
                            click$allornot <- load_click$allornot
                            click$hoverPoint <- load_click$hoverPoint
                            
                            incProgress(amount = 0.1)
                            colors$variable <- load_colors$variable
                            colors$state <- load_colors$state
                            colors$colorState <- load_colors$colorState
                            colors$color <- load_colors$color
                            colors$output <- load_colors$output
                            colors$radius <- load_colors$radius
                            
                            incProgress(amount = 0.2)
                            output$selBrand <- renderValueBox({
                                valueBox(value = length(allData$sel_brands),
                                         subtitle = 'Number of Brands to focus on :', 
                                         color = 'orange', width = 12)
                            })
                            
                            loaded$input <- load_input
                            
                            # delay(200,{
                            initialStart$load_check <- 1
                            #print('open to save image')
                            # })
                        })
                        output$selBrand <- renderValueBox(
                            valueBox(value = sum(!is.na(allData$sel_brands)), 
                                     subtitle = 'Number of Brands to focus on :', 
                                     color = 'orange', width = 12)
                        )
                    }
                }
            })
            
            ##### Load Inputs #####
            
            observe({
                print(input$tabs)
                if(!is.null(loaded$input$tabs)){
                    updateTabsetPanel(session, inputId = 'tabs', selected = loaded$input$tabs)
                    loaded$input$tabs <- NULL
                }
                if (!is.null(loaded$input$state)){
                    updateSelectInput(session, inputId = 'state', selected = loaded$input$state)
                    loaded$input$state <- NULL
                }
                if (!is.null(loaded$input$dma_name)){
                    updateSelectInput(session, inputId = 'dma_name', selected = loaded$input$dma_name)
                    loaded$input$dma_name <- NULL
                }
                if (!is.null(loaded$input$regional)){
                    updateSelectInput(session, inputId = 'regional', selected = loaded$input$regional)
                    loaded$input$regional <- NULL
                }
                if (!is.null(loaded$input$perishable)){
                    updateSelectInput(session, inputId = 'perishable', selected = loaded$input$perishable)
                    loaded$input$perishable <- NULL
                }
                if (!is.null(loaded$input$dryGrocery)){
                    updateSelectInput(session, inputId = 'dryGrocey', selected = loaded$input$dryGrocery)
                    loaded$input$dryGrocery <- NULL
                }
                if (!is.null(loaded$input$store_type)){
                    updateCheckboxGroupInput(session, inputId = 'store_type', selected = loaded$input$store_type)
                    loaded$input$store_type <- NULL
                }
                if (!is.null(loaded$input$storeSize)){
                    updateSliderInput(session, inputId = 'storeSize', value = loaded$input$storeSize)
                    loaded$input$storeSize <- NULL
                }
                if (!is.null(loaded$input$remove_0_curr)){
                    updateCheckboxInput(session, inputId = 'remove_0_curr', value = loaded$input$remove_0_curr)
                    loaded$input$remove_0_curr <- NULL
                }
                if(!is.null(loaded$input$remove_0_reco)){
                    updateCheckboxInput(session, inputId = 'remove_0_reco', value = loaded$input$remove_0_reco)
                    loaded$input$remove_0_reco <- NULL
                }
                if(!is.null(loaded$input$removeNoDiff)){
                    updateCheckboxInput(session, inputId = 'removeNoDiff', value = loaded$input$removeNoDiff)
                    loaded$input$removeNoDiff <- NULL
                }
                if(!is.null(loaded$input$viewDC)){
                    updateCheckboxInput(session, inputId = 'viewDC', value = loaded$input$viewDC)
                    loaded$input$viewDC <- NULL
                }
                if(!is.null(loaded$input$HoT)){
                    updateCheckboxInput(session, inputId = 'viewHoT', value = loaded$input$viewHoT)
                    loaded$input$viewHoT <- NULL
                }
                
                output$howmany <- renderValueBox(valueBox(value = paste(nrow(allData$storeTable), 'Stores Selected'), subtitle = ''))
                output$nStores <- renderValueBox({
                    if(length(click$storesSel) > 0){
                        valueBox(value = length(click$storesSel), subtitle = 'Stores Selected')
                    } else {
                        valueBox(value = nrow(allData$storeTable), subtitle = 'Stores Selected')
                    }
                })
            })
            
            ##### Continue #####
            if (!is.null(initialStart$session)){
                #print(initialStart$load_check)
                if (is.null(allData$storeTable)){
                    allData$storeTable <- files$storeTable
                }
                if (is.null(allData$storeTable)){
                    allData$storeTable <- files$storeTable
                }
                if (is.null(allData$brandStatus)){
                    allData$brandStatus <- initialStart$brandStatus
                }
                if (is.null(editData$ourInfo)){
                    editData$ourInfo <- files$dma_upc %>%
                        as.data.table()
                    #print(colnames(editData$ourInfo))
                    initialStart$load_check <- 2
                }
                if (is.null(allData$dc_store)){
                    allData$dc_store <- initialStart$dc_store
                }
                if (is.null(allData$store_brand_comb)){
                    allData$store_brand_comb <- initialStart$store_brand_comb
                }
                if (is.null(allData$brand_ind)){
                    allData$store_brand_ind <- initialStart$geoBrandCluster
                }
                if (is.null(allData$dma_upc_brand_info)){
                    allData$dma_upc_brand_info <- initialStart$dma_upc_brand_info
                }
                if (is.null(allData$dma_upc_brand_info_to_show)){
                    allData$dma_upc_brand_info_to_show <- initialStart$dma_upc_brand_info
                }
            }
        })
        
        
        ###### Tab1 #####
            ### Form Brand Selection Table depends on checkbox #####
            observe({
                if(is.null(allData$sel_brands)){
                    output$brandTable <- renderDataTable({
                        table1 <- allData$dma_upc_brand_info_to_show %>% 
                            select(-revenue_cumsum)
                        return(table1)
                    }, filter = 'top', 
                    selection = list(mode = 'multiple', 
                                     selected = which(allData$dma_upc_brand_info_to_show$current_stores != 4510 |
                                                          allData$dma_upc_brand_info_to_show$recommended_stores != 4510)), 
                    options = list(pageLength = 25), rownames = FALSE
                    )
                } else {
                    print('In Else')
                    t1 <- allData$dma_upc_brand_info_to_show %>% 
                        filter(brand_family_desc %in% allData$sel_brands)
                    w1 <- 1:nrow(t1)
                    output$brandTable <- renderDataTable({
                        table1 <- allData$dma_upc_brand_info_to_show %>% 
                            filter(brand_family_desc %in% allData$sel_brands) %>% 
                            select(-revenue_cumsum) 
                        return(table1)
                    }, filter = 'top', 
                    selection = list(mode = 'multiple', 
                                     selected = w1), 
                    options = list(pageLength = 25), rownames = FALSE
                    )
                }
            })
            
            ### Once selection is done ####
            observeEvent(input$selDone,{
                allData$sel_brands <- allData$dma_upc_brand_info_to_show$brand_family_desc[input$brandTable_rows_selected]
                allData$sel_brands <- allData$sel_brands[!is.na(allData$sel_brands)]
                filtered_rows <- allData$dma_upc_brand_info_to_show$brand_family_desc[input$brandTable_rows_all]
                filtered_rows <- filtered_rows[!is.na(filtered_rows)]
                
                allData$sel_brands <- intersect(allData$sel_brands, filtered_rows)
                print(allData$sel_brands)
                
                shinyjs::show(id = 'selBrand')
                print(allData$sel_brands)
                output$selBrand <- renderValueBox(valueBox(value = sum(!is.na(allData$sel_brands)), 
                                                           subtitle = 'Number of Brands to focus on :', 
                                                           color = 'orange'))
                currentBrands <- unique(editData$ourInfo$brand_family_desc)
                newBrands <- allData$sel_brands
                toAdd <- setdiff(newBrands, currentBrands)
                newDF <- files$dma_upc %>%
                    filter(brand_family_desc %in% toAdd) %>%
                    mutate(color_code = case_when(.$dma_status == 'Maintain' ~ "#00B2EE",
                                                  .$dma_status == 'Add' ~ "#FFFF00",
                                                  .$dma_status == 'Remove' ~ "#FF3030",
                                                  .$dma_status == 'Under Consideration' ~ '#F78528')
                    )
                editData$ourInfo %<>%
                    filter(brand_family_desc %in% allData$sel_brands) %>%
                    rbind(., newDF)
                
                updateTabItems(session, inputId = 'tabs', selected = 'tab2')
            })
            
            ### CheckBox event ####
            observe({
                if(!is.null(allData$dma_upc_brand_info_to_show)){
                    if(input$remove_0_curr) {
                        if(input$remove_0_reco){
                            if(input$removeNoDiff) {
                                allData$dma_upc_brand_info_to_show %<>% 
                                    mutate(diff = current_stores - recommended_stores) %>% 
                                    filter(diff != 0) %>% 
                                    filter(current_stores > 0) %>% 
                                    filter(recommended_stores > 0) %>% 
                                    select(-diff)
                            } else {
                                print('curr reco')
                                allData$dma_upc_brand_info_to_show <- allData$dma_upc_brand_info %>% 
                                    filter(current_stores > 0) %>% 
                                    filter(recommended_stores > 0) 
                            }
                        } else {
                            if(input$removeNoDiff) {
                                allData$dma_upc_brand_info_to_show %<>% 
                                    mutate(diff = current_stores - recommended_stores) %>% 
                                    filter(diff != 0) %>% 
                                    filter(current_stores > 0) %>% 
                                    select(-diff)
                            } else {
                                print('curr')
                                allData$dma_upc_brand_info_to_show <- allData$dma_upc_brand_info %>% 
                                    filter(current_stores > 0) 
                            }
                        }
                        
                    } else {
                        if(input$remove_0_reco){
                            if(input$removeNoDiff) {
                                allData$dma_upc_brand_info_to_show %<>% 
                                    mutate(diff = current_stores - recommended_stores) %>% 
                                    filter(diff != 0) %>% 
                                    filter(recommended_stores > 0) %>% 
                                    select(-diff)
                            } else {
                                print('reco')
                                allData$dma_upc_brand_info_to_show <- allData$dma_upc_brand_info %>% 
                                    filter(recommended_stores > 0) 
                            }
                        } else {
                            if(input$removeNoDiff) {
                                allData$dma_upc_brand_info_to_show <- allData$dma_upc_brand_info %>%
                                    mutate(diff = current_stores - recommended_stores) %>% 
                                    filter(diff != 0) %>% 
                                    select(-diff)
                            } else {
                                allData$dma_upc_brand_info_to_show <- allData$dma_upc_brand_info
                            }
                        }
                    }
                    
                }
            })
            
            
            observe({
                if (allData$brand == ''){
                    updateSelectInput(session, inputId = 'brand', 
                                      choices = c('None', allData$sel_brands), 
                                      selected = 'None')
                } else {
                    if (input$tabs == 'tab2')
                        updateSelectInput(session, inputId = 'brand', 
                                          choices = c('None', allData$sel_brands), 
                                          selected = allData$brand)
                }
            })
            
            
            
        ###### Color Coded Map #####
        output$map <- renderLeaflet({
            myLeaflet <-
                leaflet() %>%
                addTiles(options = tileOptions(maxZoom = 8, minZoom = 4)) %>%
                #fitBounds(lng1 = -130.7812, lat1 = 50.79205, lng2 = -57.04102, lat2 = 21.45307) %>%
                #setMaxBounds(lng1 = -130.7812, lat1 = 50.79205, lng2 = -57.04102, lat2 = 21.45307) %>%
                #setView(-104.9903, 40.7392, zoom = 5) %>%
                #setView(-89.38477, 39.02772, zoom = 4.5) %>%
                addCircleMarkers(lng = allData$storeTable$longitude,
                                 lat = allData$storeTable$latitude,
                                 radius = colors$radius, color = plotColors(), opacity = 1, weight = 1,
                                 #fillColor = colors$state[colors$colorState$factor],
                                 fillColor = plotColors(),
                                 fillOpacity = 1,
                                 group = 'Store-Pointers', layerId = allData$storeTable$store_number ,
                                 label = paste0('Store Nbr :: ', allData$storeTable$store_number)
                )
            if (input$brand != 'None'){
                if ('to_have' %in% colnames(files$dma_upc)){
                    myLeaflet <- myLeaflet %>%
                        addLegend(position = 'bottomright', opacity = 1, title = 'Brand Status',
                                  colors = c("#006400", "#00FF00", "#FFFF00", "#00B2EE", "#FF3030", '#F78528', 'gray') %>% 
                                      rev(),
                                  labels = c('MustHave Brand in DMA', 'NiceToHave Brand in DMA', 
                                             'Add', 'Maintain', 'Remove', 'Under Consideration', 'Brand Not Avbl in Store') %>% 
                                      rev()
                        )
                } else{
                    myLeaflet <- myLeaflet %>%
                        addLegend(position = 'bottomright', opacity = 1, title = 'Brand Status',
                                  colors = c("#FFFF00", "#00B2EE", "#FF3030", '#F78528', 'gray'),
                                  labels = c('Add', 'Maintain', 'Remove', 'Under Consideration', 'Brand Not Avbl in Store'))
                }
            }
            return(myLeaflet)
        })
        
        ###### DC view #####
        #observeEvent(input$viewDC,{
        observe({
            if (input$viewDC){
                # icon.fa <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa',
                #                            iconColor = 'black', squareMarker = TRUE)
                iconPink <- makeIcon("red_pointer.png",
                                     #iconRetinaUrl = '/Users/omahala/Downloads/webex.png',
                                     iconWidth = 20, iconHeight = 35,iconAnchorX = 0, iconAnchorY = -40)
                
                leafletProxy('map', session) %>%
                    addMarkers(lng = initialStart$dc_coord$longitude,
                               lat = initialStart$dc_coord$latitude,
                               #radius = 5, color = 'black', fillColor = 'black',
                               icon = iconPink,
                               group = 'DC-Pointers', layerId = initialStart$dc_coord$dc_nbr)
            } else {
                leafletProxy('map', session) %>%
                    clearGroup('DC-Pointers') %>%
                    clearGroup('DC-Selected-Stores')
            }
        })
        
        ###### Shape in Map #####
        observeEvent(input$map_click,{
            #print(input$map_click)
            click$n <- click$n + 1
            if (click$n == 3){
                click$n <- 1
                # allData$storeTable <- storeTable
            }
            
            if (click$n %% 2 == 1){
                click$df <- input$map_click %>% data.frame()
                leafletProxy('map', session) %>%
                    addCircles(lat = click$df$lat, lng = click$df$lng, radius = 1, group = 'bluPoints')
            } else {
                df_temp <- input$map_click %>% data.frame()
                click$df <- rbind(click$df, df_temp)
                latXtrm <- c(min(click$df$lat), max(click$df$lat))
                lngXtrm <- c(min(click$df$lng), max(click$df$lng))
                click$pointsSel <- which(allData$storeTable$latitude > latXtrm[1] & allData$storeTable$latitude < latXtrm[2] &
                                             allData$storeTable$longitude > lngXtrm[1] & allData$storeTable$longitude < lngXtrm[2])
                click$storesSel <- allData$storeTable$store_number[click$pointsSel]
                allData$storeTable <- allData$storeTable[click$pointsSel,]
                #editData$ourInfo <- inner_join(allData$storeTable, allData$brandStatus, by = 'dma_id')
                #updateSelectInput(session, 'brand', choices = unique(editData$ourInfo$brand_family_desc), selected = integer(0))
                
                output$nStores <- renderValueBox({
                    if(length(click$storesSel) > 0){
                        valueBox(value = length(click$storesSel), subtitle = 'Stores Selected')
                    } else {
                        valueBox(value = nrow(allData$storeTable), subtitle = 'Stores Selected')
                    }
                })
                #print(click$df)
                leafletProxy('map', session) %>%
                    addPolylines(lat = rep(click$df$lat[1], 2), lng = click$df$lng, 
                                 opacity = 0.2, fillOpacity = 0.2, group = 'lines') %>%
                    addPolylines(lat = rep(click$df$lat[2], 2), lng = click$df$lng, 
                                 opacity = 0.2, fillOpacity = 0.2, group = 'lines') %>%
                    addPolylines(lat = click$df$lat, lng = rep(click$df$lng[1], 2), 
                                 opacity = 0.2, fillOpacity = 0.2, group = 'lines') %>%
                    addPolylines(lat = click$df$lat, lng = rep(click$df$lng[2], 2), 
                                 opacity = 0.2, fillOpacity = 0.2, group = 'lines')
            }
        })
        
        ###### Clear Selection #####
        observeEvent(input$clear,{
            updateCheckboxInput(session, 'viewHoT', value = FALSE)
            click$df <- NULL
            if (length(click$storesSel) == 0){
                updateSelectInput(session, 'layer', selected = 'None')
                updateSelectInput(session, inputId = 'brand', choices = c('None', allData$sel_brands), 
                                  selected = 'None')
            }
            click$storesSel <- integer(0)
            allData$storeTable <- files$storeTable
            shinyjs::show(id = 'color_by')
            updateSelectInput(session, inputId = 'color_by', selected = 'geocluster')
            
            leafletProxy('map', session) %>%
                clearGroup('lines') %>%
                #clearGroup('brandPoints') %>%
                clearGroup('Store-Brand-Not-Available') %>%
                clearGroup('Add') %>%
                clearGroup('Delete') %>%
                clearGroup('Maintain') %>%
                clearGroup('Under Consideration') %>%
                clearGroup('Store-Pointers') %>%
                clearGroup('DC-Selected-Stores') %>%
                addCircleMarkers(lng = allData$storeTable$longitude,
                                 lat = allData$storeTable$latitude,
                                 radius = colors$radius, color = plotColors(), opacity = 1, weight = 1,
                                 #fillColor = colors$state[colors$colorState$factor],
                                 fillColor = plotColors(),
                                 fillOpacity = 1,
                                 group = 'Store-Pointers', layerId = allData$storeTable$store_number,
                                 label = paste0('Store Nbr :: ', allData$storeTable$store_number)
                ) %>%
                removeLayersControl()
        })
        
        ###### Hover #####
        #observeEvent(input$map_marker_mouseover,{
        observe({
            click$hoverPoint <- input$map_marker_mouseover
            if (!is.null(click$hoverPoint)){
                if (click$hoverPoint$id %in% initialStart$allDCs){
                    output$descTable <- renderTable({
                        whichID <- which(initialStart$dc_label$dc_nbr == click$hoverPoint$id)
                        return(initialStart$dc_label[whichID,] %>% select(-dc_nbr) %>% t())
                    }, colnames = FALSE)
                } else {
                    output$descTable <- renderTable({
                        whichID <- which(allData$storeTable$store_number == click$hoverPoint$id)
                        tempDF <- inner_join(allData$storeTable %>% as.data.frame(), initialStart$DFforLabel)
                        ncol <- colnames(tempDF) %like% 'V' %>% sum()
                        tempDF1 <- tempDF[whichID, paste0('V', 1:ncol)] %>% t()
                        return(tempDF1)
                    }, colnames = FALSE)
                }
            }
        })
        
        ###### Filter Stores #####
        
        observeEvent(input$apply,{
            allData$storeTable <- files$storeTable
            
            if ('All States' %in% input$state){
                states <- unique(storeTable$state)
            } else {
                states <- input$state
            }
            if ('All DMAs' %in% input$dma_name){
                dmas <- unique(storeTable$dma_name)
            } else {
                dmas <- input$dma_name
            }
            if ('All DCs' %in% input$regional){
                regional <- unique(storeTable$regional_dc)
            } else {
                regional <- input$regional
            }
            if ('All DCs' %in% input$perishable){
                perishable <- unique(storeTable$perishable_dc)
            } else {
                perishable <- input$perishable
            }
            if ('All DCs' %in% input$dryGrocery){
                dry <- unique(storeTable$dry_grocery_dc)
            } else {
                dry <- input$dryGrocery
            }
            
            allData$storeTable <- allData$storeTable %>%
                filter(store_type %in% input$store_type) %>%
                filter(`store_size_(sq_ft)` %>% between(input$storeSize[1], input$storeSize[2])) %>%
                filter(state %in% states) %>%
                filter(dma_name %in% dmas) %>%
                filter(regional_dc %in% regional) %>%
                filter(perishable_dc %in% perishable) %>%
                filter(dry_grocery_dc %in% dry)
            
            click$storesSel <- allData$storeTable$store_number
            output$howmany <- renderValueBox(valueBox(value = paste(nrow(allData$storeTable), 'Stores Selected'), subtitle = ''))
            output$nStores <- renderValueBox({
                if(length(click$storesSel) > 0){
                    valueBox(value = length(click$storesSel), subtitle = 'Stores Selected')
                } else {
                    valueBox(value = nrow(allData$storeTable), subtitle = 'Stores Selected')
                }
            })
            
            #updateSelectInput(session, 'brand', choices = unique(editData$ourInfo$brand_family_desc), selected = integer(0))
        })
        
        ###### Color Brand Status #####
        observe({
            if (!is.null(initialStart$session) &
                !is.null(initialStart$load_check)){
                editData$ourInfo %<>%
                    mutate(color_code = case_when(.$dma_status == 'Maintain' ~ "#00B2EE",
                                                  .$dma_status == 'Add' ~ "#FFFF00",
                                                  .$dma_status == 'Remove' ~ "#FF3030",
                                                  .$dma_status == 'Under Consideration' ~ '#F78528'))
            }
        })
        
        ###### Brand Select ######
        observeEvent(input$brand,{
            allData$storeTable <- files$storeTable
            click$storesSel <- integer(0)
        })
        observe({
            if (input$brand != 'None'){
                shinyjs::show(id = 'viewHM')
                updateCheckboxInput(session, inputId = 'viewHoT', value = TRUE)
                if (allData$brand != input$brand){
                    allData$brand <- input$brand
                    # output$selBrand <- renderValueBox(valueBox(value = length(allData$sel_brands), 
                    #                                            subtitle = 'Number of Brands to focus on :', 
                    #                                            color = 'orange'))
                    shinyjs::hide(id = 'layer')
                    updateSelectInput(session, inputId = 'layer_by', selected = 'category')
                } else {
                    if (!is.null(loaded$input$layer_by)){
                        updateSelectInput(session, inputId = 'layer_by', selected = loaded$input$layer_by)
                        print('changed layer by')
                        loaded$input$layer_by <- NULL
                    }
                }
            } else {
                updateCheckboxInput(session, inputId = 'viewHoT', value = FALSE)
                shinyjs::hide(id = 'viewHM')
                allData$brand <- input$brand
            }
        })
            
        observeEvent(input$brand, {
            if(input$brand != 'None' & 
               input$tabs == 'tab2') {
                updateSelectInput(session, inputId = 'layer_by', selected = 'category')
            }
        })
        
        ###### Layering Options ####
        
        #observeEvent(input$layer_by,{
        observeEvent(input$layer_by, {
            print(input$layer_by)
            allData$layer_by <- input$layer_by
            print('observed a change')
            if (input$layer_by == 'category'){
                shinyjs::show(id = 'layer')
                cat <- editData$ourInfo %>% filter(brand_family_desc == input$brand)
                print(loaded$input$layer)
                if (!is.null(loaded$input$layer)){
                    if(loaded$input$layer != '') {
                        updateSelectInput(session, inputId = 'layer', choices = unique(cat$category), selected = loaded$input$layer)
                    } else {
                        updateSelectInput(session, inputId = 'layer', choices = unique(cat$category), selected = unique(cat$category)[1])
                    }
                    loaded$input$layer <- NULL
                } else {
                    updateSelectInput(session, inputId = 'layer', choices = unique(cat$category), selected = unique(cat$category)[1])
                }
            }
            if (input$layer_by == 'segment'){
                shinyjs::show(id = 'layer')
                seg <- editData$ourInfo %>% filter(brand_family_desc == input$brand)
                print(loaded$input$layer)
                if (!is.null(loaded$input$layer)){
                    if(loaded$input$layer != '') {
                        updateSelectInput(session, inputId = 'layer', choices = unique(seg$segment), selected = loaded$input$layer)
                    } else {
                        updateSelectInput(session, inputId = 'layer', choices = unique(seg$segment), selected = unique(seg$segment)[1])
                    }
                    loaded$input$layer <- NULL
                } else {
                    updateSelectInput(session, inputId = 'layer', choices = unique(seg$segment), selected = unique(seg$segment)[1])
                }
            }
            if (input$layer_by == 'None'){
                shinyjs::hide(id = 'layer')
            }
        })
        
        observeEvent(input$layer,{
            allData$layer <- input$layer
        })
        
        ###### Brand Select Observe #####
        observe({
            if (!is.null(initialStart$session) & input$brand != 'None'){
                shinyjs::show(id = 'layer_by')
                allData$layer_by <- input$layer_by
                allData$layer <- input$layer
                shinyjs::hide(id = 'color_by')
                
                ########## Heatmap Data Creation ####
                if (input$layer_by == 'None'){
                    tempDF <- initialStart$dma_brand_performance1 %>%
                        group_by(dma_id) %>% 
                        mutate(dma_wm_dollars = sum(wm_dollars, na.rm = TRUE),
                               dma_tm_dollars = sum(tm_dollars, na.rm = TRUE)
                        ) %>% 
                        ungroup() %>% 
                        filter(brand_family_desc == input$brand) %>%
                        select(-category, -segment) %>% 
                        group_by(dma_id, brand_family_desc) %>%
                        summarise(wm_dollars = sum(wm_dollars, na.rm = TRUE),
                                  tm_dollars = sum(tm_dollars, na.rm = TRUE),
                                  dma_wm_dollars = sum(dma_wm_dollars, na.rm = TRUE),
                                  dma_tm_dollars = sum(dma_tm_dollars, na.rm = TRUE)) %>%
                        ungroup() %>%
                        group_by(dma_id) %>% 
                        mutate(brand_contribution_dma_wm = round((100 * wm_dollars / dma_wm_dollars), 3),
                               brand_contribution_dma_tm = round((100 * tm_dollars / dma_tm_dollars), 3)
                               ) %>% 
                        unique()
                }
                if (input$layer_by == 'segment'){
                    tempDF <- initialStart$dma_brand_performance1 %>%
                        filter(is.na(segment) | segment == input$layer) %>%
                        group_by(dma_id) %>% 
                        mutate(dma_wm_dollars = sum(wm_dollars, na.rm = TRUE),
                               dma_tm_dollars = sum(tm_dollars, na.rm = TRUE)
                        ) %>%
                        ungroup() %>% 
                        filter(brand_family_desc == input$brand) %>%
                        select(-category) %>% 
                        right_join(., initialStart$dma_brand_performance1 %>%
                                       select(dma_id, brand_family_desc) %>%
                                       filter(brand_family_desc == input$brand)
                        ) %>%
                        group_by(dma_id, brand_family_desc, segment) %>%
                        summarise(wm_dollars = sum(wm_dollars, na.rm = TRUE),
                                  tm_dollars = sum(tm_dollars, na.rm = TRUE),
                                  dma_wm_dollars = sum(dma_wm_dollars, na.rm = TRUE),
                                  dma_tm_dollars = sum(dma_tm_dollars, na.rm = TRUE)) %>%
                        ungroup() %>%
                        group_by(dma_id) %>% 
                        mutate(brand_contribution_dma_wm = round((100 * wm_dollars / dma_wm_dollars), 3),
                               brand_contribution_dma_tm = round((100 * tm_dollars / dma_tm_dollars), 3)
                        ) %>% 
                        ungroup() %>% 
                        unique()
                }
                if (input$layer_by == 'category'){
                    tempDF <- initialStart$dma_brand_performance1 %>%
                        filter(is.na(category) | category == input$layer) %>%
                        group_by(dma_id) %>% 
                        mutate(dma_wm_dollars = sum(wm_dollars, na.rm = TRUE),
                               dma_tm_dollars = sum(tm_dollars, na.rm = TRUE)
                        ) %>%
                        ungroup() %>% 
                        filter(brand_family_desc == input$brand) %>%
                        select(-segment) %>% 
                        right_join(., initialStart$dma_brand_performance1 %>%
                                       select(dma_id, brand_family_desc) %>%
                                       filter(brand_family_desc == input$brand)
                        ) %>%
                        group_by(dma_id, brand_family_desc, category) %>%
                        summarise(wm_dollars = sum(wm_dollars, na.rm = TRUE),
                                  tm_dollars = sum(tm_dollars, na.rm = TRUE),
                                  dma_wm_dollars = sum(dma_wm_dollars, na.rm = TRUE),
                                  dma_tm_dollars = sum(dma_tm_dollars, na.rm = TRUE)) %>%
                        ungroup() %>%
                        group_by(dma_id) %>% 
                        mutate(brand_contribution_dma_wm = round((100 * wm_dollars / dma_wm_dollars), 3),
                               brand_contribution_dma_tm = round((100 * tm_dollars / dma_tm_dollars), 3)
                        ) %>% 
                        ungroup() %>% 
                        unique()
                }
                
                allData$colorDF <- tempDF %>%
                    inner_join(., dma_code_name, by = c('dma_id' = 'code'))
                
                allData$dma_geo <- dma_df[allData$colorDF$x] %>%
                    do.call('rbind', .)
                
                shinyjs::show(id = 'viewHM')
                
                
                ########## DT Data Prep ####
                #print(length(click$storesSel))
                if (length(click$storesSel) == 0){
                    if (input$layer_by == 'None'){
                        #print(colnames(editData$ourInfo))
                        #print(head(editData$ourInfo))
                        allData$RtoHot <- editData$ourInfo %>%
                            filter(brand_family_desc == input$brand) %>%
                            arrange(dma_id, segment, category, store_number) %>%
                            select(-c(latitude, longitude))
                    }
                    if (input$layer_by == 'category'){
                        allData$RtoHot <- editData$ourInfo %>%
                            filter(brand_family_desc == input$brand) %>%
                            filter(category == input$layer) %>%
                            arrange(dma_id, segment, store_number) %>%
                            select(-c(latitude, longitude))
                    }
                    if (input$layer_by == 'segment'){
                        allData$RtoHot <- editData$ourInfo %>%
                            filter(brand_family_desc == input$brand) %>%
                            filter(segment == input$layer) %>%
                            arrange(dma_id, category, store_number) %>%
                            select(-c(latitude, longitude))
                    }
                } else {
                    if (input$layer_by == 'None'){
                        allData$RtoHot <- editData$ourInfo %>%
                            filter(brand_family_desc == input$brand) %>%
                            filter(store_number %in% click$storesSel) %>%
                            arrange(dma_id, segment, category, store_number) %>%
                            select(-c(latitude, longitude))
                    }
                    if (input$layer_by == 'category'){
                        allData$RtoHot <- editData$ourInfo %>%
                            filter(brand_family_desc == input$brand) %>%
                            filter(category == input$layer) %>%
                            filter(store_number %in% click$storesSel) %>%
                            arrange(dma_id, segment, store_number) %>%
                            select(-c(latitude, longitude))
                    }
                    if (input$layer_by == 'segment'){
                        allData$RtoHot <- editData$ourInfo %>%
                            filter(brand_family_desc == input$brand) %>%
                            filter(segment == input$layer) %>%
                            filter(store_number %in% click$storesSel) %>%
                            arrange(dma_id, category, store_number) %>%
                            select(-c(latitude, longitude))
                    }
                    # allData$RtoHot %<>%
                    #   filter(store_number %in% click$storesSel)
                }
                
                ########## Map Status Data Prep 1  ######
                ##### To Have Info #####
                if ('to_have' %in% colnames(files$dma_upc)){
                    if (input$layer_by == 'None'){
                        brandInfo <- editData$ourInfo %>%
                            filter(brand_family_desc == input$brand) %>%
                            group_by(dma_id, dma, brand_family_desc, store_number, latitude, longitude) %>%
                            summarise(dma_status = mode(dma_status),
                                      to_have = mode(to_have),
                                      color_code = mode(color_code)) %>%
                            ungroup() %>%
                            unique()
                    }
                    if (input$layer_by == 'segment'){
                        brandInfo <- editData$ourInfo %>%
                            filter(brand_family_desc == input$brand) %>%
                            filter(segment == input$layer) %>%
                            group_by(dma_id, dma, brand_family_desc, segment, store_number, latitude, longitude) %>%
                            summarise(dma_status = mode(dma_status),
                                      to_have = mode(to_have),
                                      color_code = mode(color_code)) %>%
                            ungroup() %>%
                            unique()
                    }
                    if (input$layer_by == 'category'){
                        brandInfo <- editData$ourInfo %>%
                            filter(brand_family_desc == input$brand) %>%
                            filter(category == input$layer) %>%
                            group_by(dma_id, dma, brand_family_desc, category, store_number, latitude, longitude) %>%
                            summarise(dma_status = mode(dma_status),
                                      to_have = mode(to_have),
                                      color_code = mode(color_code)) %>%
                            ungroup() %>%
                            unique()
                    }
                } 
                ##### No To-Have Info #####
                else {
                    if (input$layer_by == 'None'){
                        brandInfo <- editData$ourInfo %>%
                            filter(brand_family_desc == input$brand) %>%
                            group_by(dma_id, dma, brand_family_desc, store_number, latitude, longitude) %>%
                            summarise(dma_status = mode(dma_status),
                                      #to_have = mode(to_have),
                                      color_code = mode(color_code)) %>%
                            ungroup() %>%
                            unique()
                    }
                    if (input$layer_by == 'segment'){
                        brandInfo <- editData$ourInfo %>%
                            filter(brand_family_desc == input$brand) %>%
                            filter(segment == input$layer) %>%
                            group_by(dma_id, dma, brand_family_desc, segment, store_number, latitude, longitude) %>%
                            summarise(dma_status = mode(dma_status),
                                      #to_have = mode(to_have),
                                      color_code = mode(color_code)) %>%
                            ungroup() %>%
                            unique()
                    }
                    if (input$layer_by == 'category'){
                        brandInfo <- editData$ourInfo %>%
                            filter(brand_family_desc == input$brand) %>%
                            filter(category == input$layer) %>%
                            group_by(dma_id, dma, brand_family_desc, category, store_number, latitude, longitude) %>%
                            summarise(dma_status = mode(dma_status),
                                      #to_have = mode(to_have),
                                      color_code = mode(color_code)) %>%
                            ungroup() %>%
                            unique()
                    }
                }
                
                brandInfo %<>% as.data.table()
                
                ########## Map Status Data Prep 2 ####
                if (length(click$storesSel) > 0){
                    brandInfo %<>%
                        filter(store_number %in% click$storesSel)
                }
                
                addDF <- brandInfo %>%
                    filter(dma_status == 'Add')
                deleteDF <- brandInfo %>%
                    filter(dma_status == 'Remove')
                keepDF <- brandInfo %>%
                    filter(dma_status == 'Maintain')
                discardDF <- brandInfo %>%
                    filter(dma_status == 'Under Consideration')
                
                allStores <- unique(storeTable$store_number)
                noDF <- storeTable %>%
                    select(c(store_number, latitude, longitude)) %>%
                    unique() %>%
                    anti_join(., addDF) %>%
                    anti_join(., deleteDF) %>%
                    anti_join(., keepDF) %>%
                    anti_join(., discardDF)
                
                allData$noDF <- noDF
                
                if ('to_have' %in% colnames(files$dma_upc)){
                    mustHaveDF <- brandInfo %>%
                        filter(to_have == 'Must Have')
                    nice2haveDF <- brandInfo %>%
                        filter(to_have == 'Nice to Have')
                }
                
                ########## Creating Map #####
                ##### To Have Info #####
                if ('to_have' %in% colnames(files$dma_upc)){
                    leafletProxy('map', session) %>%
                        clearGroup('brandPoints') %>%
                        clearGroup('Must Have') %>% 
                        clearGroup('Nice to Have') %>% 
                        clearGroup('Add') %>%
                        clearGroup('Remove') %>%
                        clearGroup('Maintain') %>%
                        clearGroup('Under Consideration') %>% 
                        clearGroup('Store-Brand-Not-Available') %>%
                        #clearGroup('DC-Pointers') %>%
                        addLayersControl(overlayGroups = c('Must Have', 'Nice to Have',
                                                           'Add', 'Remove', 'Maintain', 
                                                           'Under Consideration',
                                                           'Store-Brand-Not-Available'
                                                           ),
                                         position = 'bottomleft', options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
                        ) %>% 
                        addCircleMarkers(lat = noDF$latitude, lng = noDF$longitude,
                                         radius = 2, color = 'gray', opacity = 0.2, weight = 0.2, fillOpacity = 0.2,
                                         group = 'Store-Brand-Not-Available', layerId = allData$storeTable$store_number,
                                         label = paste0('Store Nbr :: ', allData$storeTable$store_number)
                        ) %>%
                        clearGroup('Store-Pointers') %>%
                        addCircleMarkers(lat = mustHaveDF$latitude, lng = mustHaveDF$longitude,
                                         radius = colors$radius + 1, color = '#006400', opacity = 1, weight = 1,
                                         fillColor = '#006400', fillOpacity = 1,
                                         group = 'Must Have', layerId = mustHaveDF$store_number,
                                         label = paste0('Store Nbr :: ', mustHaveDF$store_number)
                        ) %>%
                        addCircleMarkers(lat = nice2haveDF$latitude, lng = nice2haveDF$longitude,
                                         radius = colors$radius + 1, color = '#00FF00', opacity = 1, weight = 1,
                                         fillColor = '#00FF00', fillOpacity = 1,
                                         group = 'Nice to Have', layerId = nice2haveDF$store_number,
                                         label = paste0('Store Nbr :: ', nice2haveDF$store_number)
                        ) %>%
                        addCircleMarkers(lat = addDF$latitude, lng = addDF$longitude,
                                         radius = colors$radius - 1, color = addDF$color_code, opacity = 1, weight = 1,
                                         fillColor = addDF$color_code, fillOpacity = 1,
                                         group = 'Add', layerId = addDF$store_number,
                                         label = paste0('Store Nbr :: ', addDF$store_number)
                        ) %>%
                        addCircleMarkers(lat = deleteDF$latitude, lng = deleteDF$longitude,
                                         radius = colors$radius - 1, color = deleteDF$color_code, opacity = 1, weight = 1,
                                         fillColor = deleteDF$color_code, fillOpacity = 1,
                                         group = 'Remove', layerId = deleteDF$store_number,
                                         label = paste0('Store Nbr :: ', deleteDF$store_number)
                        ) %>%
                        addCircleMarkers(lat = keepDF$latitude, lng = keepDF$longitude,
                                         radius = colors$radius - 1, color = keepDF$color_code, opacity = 1, weight = 1,
                                         fillColor = keepDF$color_code, fillOpacity = 1,
                                         group = 'Maintain', layerId = keepDF$store_number,
                                         label = paste0('Store Nbr :: ', keepDF$store_number)
                        ) %>%
                        addCircleMarkers(lat = discardDF$latitude, lng = discardDF$longitude,
                                         radius = colors$radius - 1, color = discardDF$color_code, opacity = 0.5, weight = 1,
                                         fillColor = discardDF$color_code, fillOpacity = 0.3,
                                         group = 'Under Consideration', layerId = discardDF$store_number,
                                         label = paste0('Store Nbr :: ', discardDF$store_number)
                        # ) %>%
                        # addLayersControl(baseGroups = c(),
                        #                  overlayGroups = c('Add', 'Remove', 'Maintain', 'Under Consideration',
                        #                                    'Store-Brand-Not-Available', 
                        #                                    'Must Have', 'Nice to Have'),
                        #                  position = 'bottomleft', options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
                        ) %>% 
                        hideGroup('Store-Brand-Not-Available')
                }
                ##### No To-Have Info #####
                else {
                    leafletProxy('map', session) %>%
                        clearGroup('brandPoints') %>%
                        clearGroup('Add') %>%
                        clearGroup('Remove') %>%
                        clearGroup('Maintain') %>%
                        clearGroup('Under Consideration') %>% 
                        clearGroup('Store-Brand-Not-Available') %>%
                        #clearGroup('DC-Pointers') %>%
                        addLayersControl(overlayGroups = c('Add', 'Remove', 'Maintain', 'Under Consideration',
                                                           'Store-Brand-Not-Available'),
                                         position = 'bottomleft', options = layersControlOptions(collapsed = FALSE)) %>% 
                        addCircleMarkers(lat = noDF$latitude, lng = noDF$longitude,
                                         radius = 2, color = 'gray', opacity = 0.2, weight = 1, fillOpacity = 0.2,
                                         group = 'Store-Brand-Not-Available', layerId = allData$storeTable$store_number,
                                         label = paste0('Store Nbr :: ', allData$storeTable$store_number)
                        ) %>%
                        clearGroup('Store-Pointers') %>%
                        addCircleMarkers(lat = addDF$latitude, lng = addDF$longitude,
                                         radius = colors$radius, color = 'black', opacity = 0.5, weight = 1,
                                         fillColor = addDF$color_code, fillOpacity = 1,
                                         group = 'Add', layerId = addDF$store_number,
                                         label = paste0('Store Nbr :: ', addDF$store_number)
                        ) %>%
                        addCircleMarkers(lat = deleteDF$latitude, lng = deleteDF$longitude,
                                         radius = colors$radius, color = 'black', opacity = 0.5, weight = 1,
                                         fillColor = deleteDF$color_code, fillOpacity = 1,
                                         group = 'Remove', layerId = deleteDF$store_number,
                                         label = paste0('Store Nbr :: ', deleteDF$store_number)
                        ) %>%
                        addCircleMarkers(lat = keepDF$latitude, lng = keepDF$longitude,
                                         radius = colors$radius, color = 'black', opacity = 0.5, weight = 1,
                                         fillColor = keepDF$color_code, fillOpacity = 1,
                                         group = 'Maintain', layerId = keepDF$store_number,
                                         label = paste0('Store Nbr :: ', keepDF$store_number)
                        ) %>%
                        addCircleMarkers(lat = discardDF$latitude, lng = discardDF$longitude,
                                         radius = colors$radius - 1, color = discardDF$color_code, opacity = 0.5, weight = 1,
                                         fillColor = discardDF$color_code, fillOpacity = 0.3,
                                         group = 'Under Consideration', layerId = discardDF$store_number,
                                         label = paste0('Store Nbr :: ', discardDF$store_number)
                        # ) %>%
                        # addLayersControl(overlayGroups = c('Add', 'Remove', 'Maintain', 'Under Consideration',
                        #                                    'Store-Brand-Not-Available'),
                        #                  position = 'bottomleft', options = layersControlOptions(collapsed = FALSE)
                                         )
                }
            } else {
                shinyjs::hide(id = 'layer_by')
                shinyjs::show(id = 'color_by')
            }
        })
        
        
        ###### Save Layer Selections #####
        session$onFlushed(function() {
            global$DOMRdy <- TRUE
        })
        
        observeEvent(input$brand, {
            markerInput$check1 <- NULL
            markerInput$check2 <- NULL
            markerInput$check3 <- NULL
            markerInput$check4 <- NULL
            markerInput$check5 <- NULL
            markerInput$check6 <- NULL
            markerInput$check7 <- NULL
        })
        
        ### Layer Check reading ######
        observe({
            print(paste('dim', dim(click$df)))
            if(!is.null(click$df)) {
                if(nrow(click$df) == 1) {
                    session$sendCustomMessage(type = "findInput", message = "")
                    print(c('Initial', 
                            markerInput$check1,
                            markerInput$check2,
                            markerInput$check3,
                            markerInput$check4,
                            markerInput$check5,
                            markerInput$check6,
                            markerInput$check7))
                    if(tab_check$check) {
                        markerInput$check1 <- input$check5
                        markerInput$check2 <- input$check6
                        markerInput$check3 <- input$check7
                        markerInput$check4 <- input$check8
                        markerInput$check5 <- input$check9
                        markerInput$check6 <- input$check10
                        markerInput$check7 <- input$check11
                    } else {
                        markerInput$check1 <- input$check0
                        markerInput$check2 <- input$check1
                        markerInput$check3 <- input$check2
                        markerInput$check4 <- input$check3
                        markerInput$check5 <- input$check4
                        markerInput$check6 <- input$check5
                        markerInput$check7 <- input$check6
                    }
                    print(c('Final', 
                            markerInput$check1,
                            markerInput$check2,
                            markerInput$check3,
                            markerInput$check4,
                            markerInput$check5,
                            markerInput$check6,
                            markerInput$check7))
                }
            }
        })
        
        ### Layer Check Assigning ######
        observe({
            if(input$brand == allData$brand &
               input$tabs == 'tab2' &
               !is.null(click$df)) {
                if(nrow(click$df) == 2) {
                        if(!is.null(markerInput$check1)) {
                            if(!markerInput$check1) {
                                print(c('check1', length(markerInput$check1)))
                                leafletProxy('map', session) %>% 
                                    addLayersControl(overlayGroups = c('Must Have', 'Nice to Have',
                                                                       'Add', 'Remove', 'Maintain', 
                                                                       'Under Consideration',
                                                                       'Store-Brand-Not-Available'
                                    ),
                                    position = 'bottomleft', options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
                                    ) %>%
                                    hideGroup('Must Have')
                            }
                        }
                        if(!is.null(markerInput$check2)) {
                            if(!markerInput$check2) {
                                print(c('check2', length(markerInput$check2)))
                                leafletProxy('map', session) %>% 
                                    addLayersControl(overlayGroups = c('Must Have', 'Nice to Have',
                                                                       'Add', 'Remove', 'Maintain', 
                                                                       'Under Consideration',
                                                                       'Store-Brand-Not-Available'
                                    ),
                                    position = 'bottomleft', options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
                                    ) %>%
                                    hideGroup('Nice to Have')
                            }
                        }
                        if(!is.null(markerInput$check3)) {
                            if(!markerInput$check3) {
                                # print(c('check2', length(markerInput$check2)))
                                leafletProxy('map', session) %>% 
                                    addLayersControl(overlayGroups = c('Must Have', 'Nice to Have',
                                                                       'Add', 'Remove', 'Maintain', 
                                                                       'Under Consideration',
                                                                       'Store-Brand-Not-Available'
                                    ),
                                    position = 'bottomleft', options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
                                    ) %>%
                                    hideGroup('Add')
                            }
                        }
                        if(!is.null(markerInput$check4)) {
                            if(!markerInput$check4) {
                                # print(c('check2', length(markerInput$check2)))
                                leafletProxy('map', session) %>% 
                                    addLayersControl(overlayGroups = c('Must Have', 'Nice to Have',
                                                                       'Add', 'Remove', 'Maintain', 
                                                                       'Under Consideration',
                                                                       'Store-Brand-Not-Available'
                                    ),
                                    position = 'bottomleft', options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
                                    ) %>%
                                    hideGroup('Remove')
                            }
                        }
                        if(!is.null(markerInput$check5)) {
                            if(!markerInput$check5) {
                                # print(c('check2', length(markerInput$check2)))
                                leafletProxy('map', session) %>%
                                    addLayersControl(overlayGroups = c('Must Have', 'Nice to Have',
                                                                       'Add', 'Remove', 'Maintain', 
                                                                       'Under Consideration',
                                                                       'Store-Brand-Not-Available'
                                    ),
                                    position = 'bottomleft', options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
                                    ) %>%
                                    hideGroup('Maintain')
                            }
                        }
                        if(!is.null(markerInput$check6)) {
                            if(!markerInput$check6) {
                                # print(c('check2', length(markerInput$check2)))
                                leafletProxy('map', session) %>% 
                                    addLayersControl(overlayGroups = c('Must Have', 'Nice to Have',
                                                                       'Add', 'Remove', 'Maintain', 
                                                                       'Under Consideration',
                                                                       'Store-Brand-Not-Available'
                                    ),
                                    position = 'bottomleft', options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
                                    ) %>%
                                    hideGroup('Under Consideration')
                            }
                        }
                        if(!is.null(markerInput$check7)) {
                            if(!markerInput$check7) {
                                # print(c('check2', length(markerInput$check2)))
                                leafletProxy('map', session) %>% 
                                    addLayersControl(overlayGroups = c('Must Have', 'Nice to Have',
                                                                       'Add', 'Remove', 'Maintain', 
                                                                       'Under Consideration',
                                                                       'Store-Brand-Not-Available'
                                    ),
                                    position = 'bottomleft', options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
                                    ) %>%
                                    hideGroup('Store-Brand-Not-Available')
                            }
                        }
                }
            }
        })
        
        ###### Marker click ######
        observeEvent(input$map_marker_click,{
            clicked_marker <- input$map_marker_click
            click$marker <- clicked_marker
            ##### DC click #####
            if (clicked_marker$group == 'DC-Pointers'){
                ##### If First DC is clicked #####
                if (is.null(click$DCmarker)){
                    click$DCmarker <- clicked_marker$id
                    dc_df_sel <- allData$dc_store %>%
                        filter(dc_nbr == clicked_marker$id)
                    # dc_df <- allData$dc_store %>% 
                    #     anti_join(., dc_df_sel, by = c('store_number' = 'store_number')) %>% 
                    #     select(store_number, latitude, longitude) %>% 
                    #     unique()
                    # leafletProxy('map', session) %>%
                    #     clearGroup('DC-Selected-Stores') %>%
                    #     addCircles(lng = dc_df$longitude, lat = dc_df$latitude,
                    #                radius = colors$radius + 2, color = 'white', opacity = 0.9, 
                    #                fillColor = 'white', fillOpacity = 0.9,
                    #                group = 'DC-Selected-Stores')
                    click$storesSel <- unique(dc_df_sel$store_number)
                }
                ##### If subsequent DC is clicked #####
                else {
                    if (click$DCmarker == clicked_marker$id){
                        click$DCmarker <- NULL
                        leafletProxy('map', session) %>%
                            clearGroup('DC-Selected-Stores')
                    } else {
                        click$DCmarker <- clicked_marker$id
                        dc_df_sel <- allData$dc_store %>%
                            filter(dc_nbr == clicked_marker$id)
                        # dc_df <- allData$dc_store %>% 
                        #     anti_join(., dc_df_sel, by = c('store_number' = 'store_number')) %>% 
                        #     select(store_number, latitude, longitude) %>% 
                        #     unique()
                        # leafletProxy('map', session) %>%
                        #     clearGroup('DC-Selected-Stores') %>%
                        #     addCircles(lng = dc_df$longitude, lat = dc_df$latitude,
                        #                radius = colors$radius + 2, color = 'white', opacity = 0.9, 
                        #                fillColor = 'white', fillOpacity = 0.9,
                        #                group = 'DC-Selected-Stores')
                        click$storesSel <- unique(dc_df_sel$store_number)
                    }
                }
                ##### Enable Toggler for DC #####
                shinyjs::show(id = 'hiddenBox', anim = TRUE)
                shinyjs::show(id = 'allornot')
                click$marker$id
                updateSelectInput(session, inputId = 'allornot', 
                                  choices = c('Carry for All Stores in Selection',
                                              'Do Not Carry for All Stores in Selection')
                )
                shinyjs::show(id = 'save')
            }
            ##### Store Click #####
            ##### Change Group of MustHave/Nice2Have Stores ####
            if (click$marker$group %in% c('Must Have', 'Nice to Have')){
                if (input$layer_by == 'None'){
                    rowNbr <- which(
                        editData$ourInfo$store_number == click$marker$id &
                            editData$ourInfo$brand_family_desc == allData$brand
                    )
                }
                if (input$layer_by == 'category'){
                    rowNbr <- which(
                        editData$ourInfo$store_number == click$marker$id &
                            editData$ourInfo$brand_family_desc == allData$brand &
                            editData$ourInfo$category == input$layer
                    )
                }
                if (input$layer_by == 'segment'){
                    rowNbr <- which(
                        editData$ourInfo$store_number == click$marker$id &
                            editData$ourInfo$brand_family_desc == allData$brand &
                            editData$ourInfo$segment == input$layer
                    )
                }
                click$marker$group <- editData$ourInfo$dma_status[rowNbr]
            }
            ##### Case of Filtered Store ####
            if (length(click$storesSel) > 0){
                shinyjs::show(id = 'hiddenBox', anim = TRUE)
                shinyjs::show(id = 'allornot')
                shinyjs::show(id = 'save')
                updateActionButton(session, inputId = 'save', label = 'Save Selection')
                group <- click$marker$group
                if (group == 'Add'){
                    updateSelectInput(session, inputId = 'allornot',
                                      choices = c('Carry for All Stores in Selection',
                                                  'Do Not Carry for All Stores in Selection',
                                                  'Do Not Carry for Currently Clicked Store')
                    )
                }
                if (group == 'Maintain'){
                    updateSelectInput(session, inputId = 'allornot',
                                      choices = c('Carry for All Stores in Selection',
                                                  'Do Not Carry for All Stores in Selection',
                                                  'Do Not Carry for Currently Clicked Store')
                    )
                }
                if (group == 'Remove'){
                    updateSelectInput(session, inputId = 'allornot',
                                      choices = c('Carry for All Stores in Selection',
                                                  'Do Not Carry for All Stores in Selection',
                                                  'Carry for Currently Clicked Store')
                    )
                }
            }
            ##### Case of Single Store ####
            else {
                #print(click$marker$group)
                if (click$marker$group == 'Add'){
                    shinyjs::show(id = 'hiddenBox', anim = TRUE)
                    shinyjs::show(id = 'save')
                    updateActionButton(session, inputId = 'save', label = 'Change to Remove')
                }
                if (click$marker$group == 'Remove'){
                    shinyjs::show(id = 'hiddenBox', anim = TRUE)
                    shinyjs::show(id = 'save')
                    updateActionButton(session, inputId = 'save', label = 'Change to Maintain')
                }
                if (click$marker$group == 'Maintain'){
                    shinyjs::show(id = 'hiddenBox', anim = TRUE)
                    shinyjs::show(id = 'save')
                    updateActionButton(session, inputId = 'save', label = 'Change to Remove')
                }
                if (click$marker$group == 'Under Consideration'){
                    shinyjs::show(id = 'hiddenBox', anim = TRUE)
                    shinyjs::show(id = 'save')
                    updateActionButton(session, inputId = 'save', label = 'Change to Add')
                }
            }
        })
        
        ###### Toggle #####
        observeEvent(input$save, {
            shinyjs::hide(id = 'save')
            shinyjs::hide(id = 'allornot')
            shinyjs::hide(id = 'hiddenBox', anim = TRUE)
            shinyjs::hide(id = 'toggleDT')
            
            ##### Getting Row Nbr of clicked Store #####
            if (input$layer_by == 'None'){
                editedRownbr1 <- which(editData$ourInfo$brand_family_desc == allData$brand &
                                           editData$ourInfo$store_number %in% click$marker$id)
            }
            if (input$layer_by == 'category'){
                editedRownbr1 <- which(editData$ourInfo$brand_family_desc == allData$brand &
                                           editData$ourInfo$category == input$layer &
                                           editData$ourInfo$store_number %in% click$marker$id)
            }
            if (input$layer_by == 'segment'){
                editedRownbr1 <- which(editData$ourInfo$brand_family_desc == allData$brand &
                                           editData$ourInfo$segment == input$layer &
                                           editData$ourInfo$store_number %in% click$marker$id)
            }
            ##### Case of Filtered Store #####
            if (length(click$storesSel) > 0){
                statusColorCode <- editData$ourInfo %>% 
                    select(c(dma_status, color_code)) %>% 
                    unique()
                ##### Getting Row Nbr of All Filtered Stores #####
                if (input$layer_by == 'None'){
                    editedRownbr <- which(editData$ourInfo$brand_family_desc == allData$brand &
                                              editData$ourInfo$store_number %in% click$storesSel)
                }
                if (input$layer_by == 'category'){
                    editedRownbr <- which(editData$ourInfo$brand_family_desc == allData$brand &
                                              editData$ourInfo$category == input$layer &
                                              editData$ourInfo$store_number %in% click$storesSel)
                }
                if (input$layer_by == 'segment'){
                    editedRownbr <- which(editData$ourInfo$brand_family_desc == allData$brand &
                                              editData$ourInfo$segment == input$layer &
                                              editData$ourInfo$store_number %in% click$storesSel)
                }
                
                ##### Update Toggle Options ####
                if (input$allornot == 'Carry for All Stores in Selection'){
                    #print(editData$ourInfo[editedRownbr,])
                    editData$ourInfo[editedRownbr,] %<>% 
                        select(-color_code) %>% 
                        mutate(dma_status = case_when(
                            .$dma_status %in% c('Maintain', 'Remove') ~ 'Maintain',
                            .$dma_status == 'Add' ~ 'Add',
                            .$dma_status == 'Under Consideration' ~ 'Under Consideration')
                        ) %>% 
                        inner_join(., statusColorCode)
                    #print(editData$ourInfo[editedRownbr,])
                }
                if (input$allornot == 'Do Not Carry for All Stores in Selection'){
                    #print(editData$ourInfo[editedRownbr,])
                    editData$ourInfo[editedRownbr,] %<>% 
                        select(-color_code) %>% 
                        mutate(dma_status = case_when(
                            .$dma_status %in% c('Maintain', 'Remove', 'Add') ~ 'Remove',
                            .$dma_status == 'Under Consideration' ~ 'Under Consideration')
                        ) %>% 
                        inner_join(., statusColorCode)
                }
                if (input$allornot == 'Carry for Currently Clicked Store'){
                    editData$ourInfo$dma_status[editedRownbr1] <- 'Maintain'
                    editData$ourInfo$color_code[editedRownbr1] <- "#00B2EE"
                }
                if (input$allornot == 'Do Not Carry for Currently Clicked Store'){
                    editData$ourInfo$dma_status[editedRownbr1] <- 'Remove'
                    editData$ourInfo$color_code[editedRownbr1] <- "#FF3030"
                }
            }
            ##### Case of Single Store #####
            else {
                if (click$marker$group %in% c('Maintain', 'Add')){
                    editData$ourInfo$dma_status[editedRownbr1] <- 'Remove'
                    editData$ourInfo$color_code[editedRownbr1] <- "#FF3030"
                }
                if (click$marker$group == 'Remove'){
                    editData$ourInfo$dma_status[editedRownbr1] <- 'Maintain'
                    editData$ourInfo$color_code[editedRownbr1] <- "#00B2EE"
                }
                if (click$marker$group == 'Under Consideration'){
                    editData$ourInfo$dma_status[editedRownbr1] <- 'Add'
                    editData$ourInfo$color_code[editedRownbr1] <- "#FFFF00"
                }
            }
            leafletProxy('map', session) %>% 
                clearGroup('DC-Selected-Stores')
        })
        
        ###### Toggle to View DT ####
        
        observeEvent(input$viewHoT,{
            if (input$viewHoT){
                shinyjs::show(id = 'dt_table')
                shinyjs::show(id = 'toggleDT')
                shinyjs::show(id = 'save2')
                updateSelectInput(session, inputId = 'toggleDT', selected = integer(0))
            } else {
                shinyjs::hide(id = 'dt_table')
                shinyjs::hide(id = 'toggleDT')
                shinyjs::hide(id = 'save2')
            }
        })
        
        ###### Heatmap #####
        output$valueBrand <- renderValueBox({
            if (input$layer_by == 'category'){
                sub <- paste('Selected Category : ', input$layer)
            }
            if (input$layer_by == 'segment'){
                sub <- paste('Selected Segment : ', input$layer)
            }
            if (input$layer_by == 'None'){
                sub <- 'Selected Brand : '
            }
            valueBox(value = input$brand, subtitle = h4(sub), color = 'orange', width = 12)
        })
        output$heatmap <- renderLeaflet({
            #print(allData$colorDF)
            if(input$heatmap_choice == 'Walmart') {
                pal_map <- colorNumeric('YlOrRd', 
                                        domain = allData$colorDF$brand_contribution_dma_wm, na.color = '#C9C9C9')
                pal_legend <- colorNumeric('YlOrRd', 
                                           domain = allData$colorDF$brand_contribution_dma_wm[!is.na(allData$colorDF$brand_contribution_dma_wm)])
                my_heatmap <- leaflet() %>%
                    #addTiles(options = tileOptions(maxZoom = 8, minZoom = 1)) %>%
                    addTiles() %>%
                    addPolygons(lng = allData$dma_geo$longitude, lat = allData$dma_geo$latitude,
                                color = 'black', weight = 1,
                                fillColor = pal_map(allData$colorDF$brand_contribution_dma_wm),
                                fillOpacity = 1, opacity = 0.4,
                                label = paste(allData$colorDF$dma_id, allData$colorDF$name,
                                              ' - (',
                                              allData$colorDF$brand_contribution_dma_wm,
                                              '%)')
                    ) %>%
                    addLegend(position = 'bottomright', title = 'Brand Contribution',
                              pal = pal_legend,
                              values = c(min(allData$colorDF$brand_contribution_dma_wm, na.rm = TRUE),
                                         max(allData$colorDF$brand_contribution_dma_wm, na.rm = TRUE)),
                              #values = allData$colorDF$brand_contribution_dma,
                              opacity = 1,
                              labFormat = labelFormat(suffix = ' %'))
            }
            if(input$heatmap_choice == 'Total Market') {
                pal_map <- colorNumeric('YlOrRd', 
                                        domain = allData$colorDF$brand_contribution_dma_tm, na.color = '#C9C9C9')
                pal_legend <- colorNumeric('YlOrRd', 
                                           domain = allData$colorDF$brand_contribution_dma_tm[!is.na(allData$colorDF$brand_contribution_dma_tm)])
                my_heatmap <- leaflet() %>%
                    #addTiles(options = tileOptions(maxZoom = 8, minZoom = 1)) %>%
                    addTiles() %>%
                    addPolygons(lng = allData$dma_geo$longitude, lat = allData$dma_geo$latitude,
                                color = 'black', weight = 1,
                                fillColor = pal_map(allData$colorDF$brand_contribution_dma_tm),
                                fillOpacity = 1, opacity = 0.4,
                                label = paste(allData$colorDF$dma_id, allData$colorDF$name,
                                              ' - (',
                                              allData$colorDF$brand_contribution_dma_tm,
                                              '%)')
                    ) %>%
                    addLegend(position = 'bottomright', title = 'Brand Contribution',
                              pal = pal_legend,
                              values = c(min(allData$colorDF$brand_contribution_dma_tm, na.rm = TRUE),
                                         max(allData$colorDF$brand_contribution_dma_tm, na.rm = TRUE)),
                              #values = allData$colorDF$brand_contribution_dma,
                              opacity = 1,
                              labFormat = labelFormat(suffix = ' %'))
            }
            return(my_heatmap)
        })
        
        ###### Get GeoCluster #####
        observeEvent(input$getGeoCluster,{
            shinyjs::show(id = 'pre_download')
            updateSelectInput(session, inputId = 'brand', selected = 'None')
            shinyjs::hide(id = 'layer_by')
            shinyjs::hide(id = 'layer')
            shinyjs::hide(id = 'allornot')
            shinyjs::hide(id = 'hiddenBox', anim = TRUE)
            shinyjs::hide(id = 'save')
            withProgress(message = 'Re-Calculating GeoBrand Clusters', 
                         detail = 'Generating all Possible DMA-Brand combination', value = 0.3, {
                             geoBrandCluster <- editData$ourInfo %>%
                                 select(dma_id, brand_family_desc, dma_status) %>%
                                 group_by(dma_id, brand_family_desc) %>%
                                 summarise(dma_status = mode(dma_status)) %>%
                                 ungroup() %>%
                                 unique() %>%
                                 as.data.frame() %>%
                                 right_join(., initialStart$store_brand_comb) %>%
                                 mutate(presence_code1 = case_when(.$dma_status == 'Maintain' ~ 1,
                                                                   .$dma_status == 'Add' ~ 1,
                                                                   .$dma_status == 'Remove' ~ 0,
                                                                   .$dma_status == 'Under Consideration' ~ 0)) %>%
                                 mutate(presence_code = if_else(is.na(dma_status), 0, presence_code1)) %>%
                                 select(dma_id, brand_family_desc, presence_code) %>%
                                 as.data.frame() %>%
                                 reshape(., timevar = 'brand_family_desc', idvar = 'dma_id', direction = 'wide') %>%
                                 mutate(concat = apply(., 1, function(x) paste(x[-1], collapse = '-'))) %>%
                                 mutate(geoCluster = as.numeric(as.factor(concat))) %>%
                                 #select(store_number, geoCluster) %>%
                                 as.data.table()
                             allData$store_brand_ind <- geoBrandCluster
                             incProgress(amount = 0.3, detail = 'Re-writing Previous Values')
                             allData$storeTable %<>%
                                 inner_join(., geoBrandCluster %>%
                                                select(dma_id, geoCluster),
                                            by = 'dma_id')
                             incProgress(amount = 0.1)
                             allData$geo_change_ind <- unique(allData$storeTable$geoCluster) %>% length()
                             #print(allData$geo_change_ind)
                             allData$storeTable$geocluster <- allData$storeTable$geoCluster
                             
                             allData$storeTable$geoCluster <- NULL
                             
                             incProgress(amount = 0.3, detail = 'Re-creating Store Hover Info')
                             if ('ecs_cluster' %in% colnames(files$storeTable)){
                                 initialStart$DFforLabel <- files$storeTable %>% 
                                     select(store_number, store_type, city, state, dma_id, datadma,
                                            regional_dc, dry_grocery_dc, perishable_dc, geocluster, ecs_cluster) %>% 
                                     mutate(dma_name = apply(., 1, function(x) gsub('DMA', x[5], x[6]))) %>% 
                                     mutate(STORE_FEATURES = '') %>% 
                                     select(store_number, STORE_FEATURES, store_type, dma_name, regional_dc, dry_grocery_dc, perishable_dc, geocluster, ecs_cluster) %>% 
                                     mutate(DEMOGRAPHICS = '') %>% 
                                     left_join(., final_store_demo, by = c('store_number' = 'store_nbr'))
                             } else {
                                 initialStart$DFforLabel <- files$storeTable %>% 
                                     select(store_number, store_type, city, state, dma_id, datadma,
                                            regional_dc, dry_grocery_dc, perishable_dc, geocluster) %>% 
                                     mutate(dma_name = apply(., 1, function(x) gsub('DMA', x[5], x[6]))) %>% 
                                     mutate(STORE_FEATURES = '') %>% 
                                     select(store_number, STORE_FEATURES, store_type, dma_name, regional_dc, dry_grocery_dc, perishable_dc, geocluster) %>% 
                                     mutate(DEMOGRAPHICS = '') %>% 
                                     left_join(., final_store_demo, by = c('store_number' = 'store_nbr'))
                             }
                             colname <- which(colnames(initialStart$DFforLabel) %in% c('STORE_FEATURES', 'DEMOGRAPHICS'))
                             colnames(initialStart$DFforLabel)[colname] <- paste0(':: ', c('STORE_FEATURES', 'DEMOGRAPHICS'))
                             initialStart$DFforLabel %<>% 
                                 apply(., 1, function(x) {
                                     names(x) <- colnames(.)
                                     return(paste(names(x), x, sep = ' :: '))
                                 }) %>% 
                                 t() %>% 
                                 as.data.frame()
                             initialStart$DFforLabel$store_number <- files$storeTable$store_number
                             
                             incProgress(amount = 0.5)
                         })
        })
        
        output$nGeo <- renderValueBox({
            if (!is.null(allData$geo_change_ind)){
                valueBox(value = paste('Geo Clusters ::', allData$geo_change_ind), 
                         subtitle = '', width = 12)
            }
        })
        
        ###### Get DT #####
        observe({
            if(input$brand == 'None'){
                shinyjs::hide(id = 'viewHoT')
            } else {
                shinyjs::show(id = 'viewHoT')
            }
        })
        
        output$dt_table <- renderDataTable({
            columns1 <- c('ecs_cluster', 'footage')[c('ecs_cluster', 'footage') %in% colnames(files$storeTable)]
            columns2 <- c('to_have')[c('to_have') %in% colnames(files$dma_upc)]
            df <- allData$RtoHot %>%
                #select(-c(brand_family_desc, wm_dollars, brand_contribution_dma, color_code)) %>%
                select_(.dots = lapply(c('dma_name', 'store_number', columns1, 'category', 'segment', 'dma_status', columns2), 
                                       as.symbol)) %>%
                mutate_each(funs(factor), -store_number)
            df$store_number <- as.integer(df$store_number)
            if ('footage' %in% colnames(files$storeTable)){
                df$footage <- as.numeric(as.character(df$footage)) %>% as.integer()
            }
            datatable(df,
                      filter = 'top', extensions = 'Scroller', rownames = FALSE, selection = 'none',
                      options = list(
                          deferRender = TRUE,
                          scrollY = 300,
                          scroller = TRUE,
                          autoWidth = TRUE
                      )
            )
        })
        
        ###### DT toggle #####
        observeEvent(input$save2,{
            miniTable <- allData$RtoHot[input$dt_table_rows_all,]
            editedRownbr <- which(editData$ourInfo$dma_id %in% miniTable$dma_id &
                                      editData$ourInfo$brand_family_desc == allData$brand &
                                      editData$ourInfo$category %in% miniTable$category &
                                      editData$ourInfo$segment %in% miniTable$segment &
                                      editData$ourInfo$store_number %in% miniTable$store_number)
            statusColorCode <- editData$ourInfo %>% 
                select(c(dma_status, color_code)) %>% 
                unique()
            if (input$toggleDT == 'Carry for All Filtered Stores'){
                editData$ourInfo[editedRownbr,] %<>% 
                    mutate(dma_status = case_when(.$dma_status %in% c('Add', 'Under Consideration') ~ 'Add',
                                                  .$dma_status %in% c('Remove', 'Maintain') ~ 'Maintain'
                    )
                    ) %>% 
                    select(-color_code) %>% 
                    inner_join(., statusColorCode)
            }
            if (input$toggleDT == 'Do Not Carry for All Filtered Stores'){
                editData$ourInfo[editedRownbr,] %<>% 
                    mutate(dma_status = case_when(.$dma_status == 'Under Consideration' ~ 'Under Consideration',
                                                  .$dma_status %in% c('Add', 'Remove', 'Maintain') ~ 'Remove'
                    )
                    ) %>% 
                    select(-color_code) %>% 
                    inner_join(., statusColorCode)
            }
            updateSelectInput(session, inputId = 'toggleDT', selected = integer(0))
        })
        
        ###### Download Options ####
        observeEvent(input$pre_download,{
            shinyjs::show(id = 'downloadPanel')
        })
        
        what2download <- reactive({
            Store_Brand_Presence_File <- allData$store_brand_ind
            Store_GeoCluster_File <- allData$storeTable %>%
                select(store_number, geocluster)
            Detailed_Updated_File <- editData$ourInfo %>% 
                select(-brand_contribution_dma, -color_code)
            return(list(
                'Store-GeoCluster File' = Store_GeoCluster_File,
                'Store-Brand Presence File' = Store_Brand_Presence_File,
                'Detailed Updated File' = Detailed_Updated_File
            ))
        })
        
        output$download <- downloadHandler(
            filename = 'GeoBrandingRShinyOutputFiles.zip',
            content = function(folderName){
                tmpdir <- tempdir()
                setwd(tempdir())
                fileNames <- paste0(input$download_choice, '.csv')
                files2download <- what2download()[input$download_choice]
                for(i in 1:length(fileNames))
                    write.csv(x = files2download[[i]], file = fileNames[i], sep = ',', row.names = FALSE)
                zip(zipfile = folderName, files = fileNames)
            },
            contentType = "application/zip"
        )
        
        ###### Download Brand Store Status ####
        observeEvent(input$brand, {
            if(input$tabs == 'tab2'){
                if(input$brand == 'None'){
                    shinyjs::hide(id = 'downloadStore')
                } else {
                    shinyjs::show(id = 'downloadStore')
                }
            } else {
                shinyjs::hide(id = 'downloadStore')
            }
        })
        
        output$downloadStore <- downloadHandler(
            filename = function(){
                print(c(allData$brand, input$brand))
                paste0(input$brand, '_AllStores_Status.csv')
            }, 
            content = function(file) {
                file2write <- editData$ourInfo %>% 
                    filter(brand_family_desc == input$brand) %>% 
                    select(-brand_contribution_dma, -color_code) %>% 
                    arrange(brand_family_desc, category, segment)
                write.csv(file2write, file, row.names = FALSE)
            }
        )
        ###### Session-Info Save #####
        time <- reactiveValues(time = Sys.time())
        observe({
            reactiveValuesToList(editData)
            print('all Data changed')
            s <- Sys.time()
            if (!is.null(initialStart$load_check) & 
                s > time$time + 10){
                #print(s)  
                withProgress(message = 'Saving Current State of App', detail = 'Fetching Current State', value = 0.3, {
                    obj <- list('input' = reactiveValuesToList(input), 
                                'click' = reactiveValuesToList(click),
                                'allData' = reactiveValuesToList(allData),
                                'editData' = reactiveValuesToList(editData),
                                'colors' = reactiveValuesToList(colors))
                    incProgress(amount = 0.5, detail = 'Saving Current State')
                    saveRDS(obj, file = paste0(wd, initialStart$session, '_all.rds', collapse = ''))
                    #saveRDS(obj, file = paste0(initialStart$session, '_all.rds', collapse = ''))
                    time$time <- Sys.time()
                    #print(paste(paste0(initialStart$session, '_all.rds', collapse = ''),  '   saved'))
                    incProgress(amount = 0.4)
                })
            } 
        })
        
        observeEvent(input$save_session, {
            withProgress(message = 'Saving Current State of App', detail = 'Fetching Current State', value = 0.3, {
                obj <- list('input' = reactiveValuesToList(input), 
                            'click' = reactiveValuesToList(click),
                            'allData' = reactiveValuesToList(allData),
                            'editData' = reactiveValuesToList(editData),
                            'colors' = reactiveValuesToList(colors))
                incProgress(amount = 0.5, detail = 'Saving Current State')
                saveRDS(obj, file = paste0(wd, initialStart$session, '_all.rds', collapse = ''))
                incProgress(amount = 0.4)
            })
        })
        
        
        ###### Hide post onde download #####
        observe({
            print(c('geoInput', input$getGeoCluster))
            if(input$getGeoCluster > 0) { 
               if(input$brand != 'None') {
                    shinyjs::hide(id = 'downloadPanel')
                    shinyjs::hide(id = 'pre_download')
               } else {
                   shinyjs::show(id = 'downloadPanel')
                   shinyjs::show(id = 'pre_download')
               }
            }
        })
    }
)