getInputwithJS <- '
Shiny.addCustomMessageHandler("findInput",
function(message) {
var inputs = document.getElementsByTagName("input");
console.log(inputs);
Shiny.onInputChange("check0", inputs[11].checked);
Shiny.onInputChange("check1", inputs[12].checked);
Shiny.onInputChange("check2", inputs[13].checked);
Shiny.onInputChange("check3", inputs[14].checked);
Shiny.onInputChange("check4", inputs[15].checked);
Shiny.onInputChange("check5", inputs[16].checked);
Shiny.onInputChange("check6", inputs[17].checked);
Shiny.onInputChange("check7", inputs[18].checked);
Shiny.onInputChange("check8", inputs[19].checked);
Shiny.onInputChange("check9", inputs[20].checked);
Shiny.onInputChange("check10", inputs[21].checked);
Shiny.onInputChange("check11", inputs[22].checked);
}
);
'

jscode <- '
$(function() {
var $els = $("[data-proxy-click]");
$.each(
$els,
function(idx, el) {
var $el = $(el);
var $proxy = $("#" + $el.data("proxyClick"));
$el.keydown(function (e) {
if (e.keyCode == 13) {
$proxy.click();
}
});
}
);
});
'

header <- dashboardHeader(title = 'Geo-Branding Buyer Homework', titleWidth = '350px')
sidebar <- dashboardSidebar(width = '200px',{
    fluidPage(
        tags$style(type="text/css", 
                   ".shiny-output-error { visibility: hidden; }", 
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        useShinyjs(),
        shiny::absolutePanel(
            fluidPage(
                ##### Clear Button ####
                br(),
                shinyjs::hidden(
                    actionButton(inputId = 'save_session',
                                 label = strong('SAVE SESSION'), 
                                 icon = icon('floppy-o'))
                ),
                hr(),
                shinyjs::hidden(
                    actionButton(inputId = 'clear',
                                 label = strong('CLEAR Selection'), 
                                 icon = icon('refresh'))
                ),
                ##### Hover Description Table ####
                hr(),
                tableOutput('descTable'),
                ##### Download Panel #####
                hr(),
                shinyjs::hidden(downloadButton('downloadStore', label = 'Brand Status')),
                shinyjs::hidden(actionButton('pre_download', label = 'Download')),
                shinyjs::hidden(div(id = 'downloadPanel',
                                    fluidPage(
                                        checkboxGroupInput(inputId = 'download_choice', width = '150%',
                                                           label = 'Select the files, you wish to Download', 
                                                           choices = c('Store-GeoCluster File', 
                                                                       'Store-Brand Presence File', 
                                                                       'Detailed Updated File'), 
                                                           selected = c('Store-GeoCluster File', 
                                                                        'Store-Brand Presence File', 
                                                                        'Detailed Updated File')
                                        ),
                                        downloadButton(outputId = 'download', 
                                                       label = 'Download Files', width = '50%')
                                    )
                ))
            )
        )
    )
})
body <- dashboardBody({
    tags$style(type="text/css", 
               ".shiny-output-error { visibility: hidden; }", 
               ".shiny-output-error:before { visibility: hidden; }"
    )
    useShinyjs()
    tabsetPanel(id = 'tabs',
                ###### Tab 0 #####
                tabPanel(title = 'Session Option', value = 'tab0', 
                         fluidPage(
                             tags$head(tags$script(HTML(jscode))),
                             box(title = 'Load Previous Session', solidHeader = TRUE, status = 'warning', width = '100%',
                                 h3('In case you would need to use an already existing session, please enter the sessionID to load the same'),
                                 hr(),
                                 tagAppendAttributes(
                                     textInput(inputId = 'sessionID', label = 'Please Enter the sessionID that you would require to load :', value = ''),
                                     `data-proxy-click` = "load"
                                 ),
                                 #textInput(inputId = 'sessionID', label = 'Please Enter the sessionID that you would require to load :', value = ''),
                                 disabled(actionButton(inputId = 'load', label = h3(strong('Load Session')), width = '30%')),
                                 actionButton(inputId = 'continue', label = h3(strong('Continue to a NEW Session')), width = '30%'),
                                 hr(),
                                 valueBoxOutput('loadedSession1', width = 12)
                             )
                         )
                ),
                ###### Tab 1 #####
                tabPanel(title = 'Brand Selection', value = 'tab1', 
                         fluidPage(
                             ##### File Upload #####
                             shinyjs::hidden(div(id = 'box0', box(title = '', width = '100%', status = 'info',
                                                                  valueBoxOutput(outputId = 'session', width = 12)
                             )
                             )),
                             shinyjs::hidden(div(id = 'box1',
                                                 box(title = 'Input Data', 
                                                     status = 'danger', solidHeader = TRUE, collapsible = TRUE, width = '100%',
                                                     fluidPage(
                                                         column(width = 8, 
                                                                fileInput(inputId = 'dma_upc', 
                                                                          label = 'Please upload the DMA-UPC tab of the GeoClusterAssignment file:', 
                                                                          accept = c('.xlsx', '.xls'))
                                                         ),
                                                         # column(width = 4,
                                                         #        fileInput(inputId = 'store_table', 
                                                         #                  label = 'Please upload the StoreTable tab of the GeoClusterAssignment file (OPTIONAL):', 
                                                         #                  accept = c('.csv', '.xls', '.xlsx'))
                                                         # ),
                                                         # column(width = 4,
                                                         #        fileInput(inputId = 'ecs_cluster', 
                                                         #                  label = 'Please upload the Demand Cluster file (OPTIONAL):', 
                                                         #                  accept = c('.csv', '.xlsx', '.xls'))
                                                         # ),
                                                         column(width = 4,
                                                                fileInput(inputId = 'vendor_mtx', 
                                                                          label = 'Please upload the Vendor-Id based GeoCluster file (OPTIONAL):', 
                                                                          accept = c('.csv', '.xlsx', '.xls'))
                                                         ),
                                                         column(offset = 9, width = 3,
                                                                tags$head(
                                                                    tags$style(HTML('#startDataPrep{background-color:orange}'))
                                                                ),
                                                                actionButton(inputId = 'startDataPrep', 
                                                                             label = h4('Perform Data Preparation'), 
                                                                             width = '100%')
                                                         ),
                                                         fluidPage(
                                                             column(width = 12, 
                                                                    hr(),
                                                                    bsAlert(anchorId = 'ecsAlert'),
                                                                    bsAlert(anchorId = 'StoreTableAlert'),
                                                                    bsAlert(anchorId = 'DMA_UPC_Alert'),
                                                                    bsAlert(anchorId = 'vendorID_Alert')
                                                             )
                                                         )
                                                     )
                                                 )
                             )),
                             ##### Brand performance ####
                             shinyjs::hidden(div(id = 'box2',
                                                 box(title = 'Brand Performance', 
                                                     status = 'success', solidHeader = TRUE, collapsible = TRUE, width = '100%',
                                                     fluidPage(
                                                         column(width = 6, dataTableOutput('brandTable')),
                                                         hr(),
                                                         hr(),
                                                         column(offset = 1, width = 5,
                                                                helpText(h4('Please click on the rows corresponding to a brand_family_desc to consider its selection / de-selection for further use.',
                                                                            'Highlighted rows are selected by default.')),
                                                                hr(),
                                                                checkboxInput(inputId = 'remove_0_curr', 
                                                                              label = h4('Remove brands with 0 current stores'), value = FALSE),
                                                                checkboxInput(inputId = 'remove_0_reco', 
                                                                              label = h4('Remove brands with 0 recommended stores'), value = FALSE),
                                                                checkboxInput(inputId = 'removeNoDiff', 
                                                                              label = h4('Remove brands with NO Store Change'), value = FALSE),
                                                                hr(),
                                                                shinyjs::hidden(valueBoxOutput('selBrand', width = 12)),
                                                                hr(),
                                                                hr(),
                                                                tags$head(
                                                                    tags$style(HTML('#selDone{background-color:orange}'))
                                                                ),
                                                                actionButton('selDone', label = h4('Brand Selection Done. Proceed to Map.'))
                                                                # sliderInput(inputId = 'revenue_cutoff', 
                                                                #             label = 'Adjust the Category Revenue Contribution cutoff',
                                                                #             min = 0, max = 100, value = 60, step = 5)
                                                                # sliderInput(inputId = 'must_cutoff',
                                                                #             label = 'Adjust the Must-Have and Nice-to-Have cutoff',
                                                                #             min = 0, max = 100, value = 30, step = 2)
                                                         )
                                                     )
                                                 )
                             ))
                         )
                ),
                ###### Tab 2 #####
                tabPanel(title = 'Map View', value = 'tab2', 
                         tags$style(type = "text/css", "html, body {width:100%;height:100%}",
                                    ".leaflet .legend i{
                                  border-radius: 100%;
                                  width: 10px;
                                  height: 10px;
                                  margin-top: 4px;}"
                         ),
                         shinyjs::hidden(div(id = 'secondTab',
                                             fluidPage(
                                                 ##### MAP ####
                                                 box(title = '', status = 'info', solidHeader = TRUE, width = 10,
                                                     #column(width = 10,
                                                     leafletOutput(outputId = 'map',
                                                                   width = '100%',
                                                                   height = 750),
                                                     tags$head(tags$script(HTML(getInputwithJS)))
                                                 ),
                                                 ##### MAP Page Sidebar ####
                                                 box(title = '', status = 'warning', solidHeader = TRUE, width = 2,
                                                     #column(width = 2,
                                                     ##### View DC ####
                                                     checkboxInput(inputId = 'viewDC', 
                                                                   label = (strong('Plot DCs on Map?')), 
                                                                   value = FALSE),
                                                     ##### Filter Panel ####
                                                     actionButton(inputId = 'filter', 
                                                                  label = (strong('Click to Fiter Stores')), 
                                                                  width = '100%'),
                                                     bsModal(id = 'filterModal', 
                                                             title = 'Select your filtering options', trigger = 'filter', size = 'large',
                                                             fluidPage(
                                                                 column(width = 6,
                                                                        checkboxGroupInput(inputId = 'store_type', label = 'Select Store Type :',
                                                                                           choices = unique(storeTable$store_type),
                                                                                           selected = unique(storeTable$store_type)),
                                                                        sliderInput(inputId = 'storeSize', 
                                                                                    label = 'Filter Store Size (in sq_ft) :',
                                                                                    min = min(storeTable$`store_size_(sq_ft)`), 
                                                                                    max = max(storeTable$`store_size_(sq_ft)`),
                                                                                    value = c(min(storeTable$`store_size_(sq_ft)`), 
                                                                                              max(storeTable$`store_size_(sq_ft)`))
                                                                        ),
                                                                        selectInput(inputId = 'state', label = 'Select a state to filter :',
                                                                                    choices = c('All States', unique(storeTable$state)), 
                                                                                    multiple = TRUE, selected = 'All States'),
                                                                        selectInput(inputId = 'dma_name', label = 'Choose a DMA :',
                                                                                    choices = c('All DMAs', unique(storeTable$dma_name)), 
                                                                                    multiple = TRUE, selected = 'All DMAs')
                                                                 ),
                                                                 column(width = 6,
                                                                        selectInput(inputId = 'dryGrocery', label = 'Select dry-grocery DCs to filter :',
                                                                                    choices = c('All DCs', unique(storeTable$dry_grocery_dc)), 
                                                                                    multiple = TRUE, selected = 'All DCs'),
                                                                        selectInput(inputId = 'regional', label = 'Select regional DCs to filter :',
                                                                                    choices = c('All DCs', unique(storeTable$regional_dc)), 
                                                                                    multiple = TRUE, selected = 'All DCs'),
                                                                        selectInput(inputId = 'perishable', label = 'Select perishable DCs to filter :',
                                                                                    choices = c('All DCs', unique(storeTable$perishable_dc)), 
                                                                                    multiple = TRUE, selected = 'All DCs')
                                                                 ),
                                                                 actionButton('apply', label = strong(('Apply Filters'))),
                                                                 valueBoxOutput('howmany', width = 6)
                                                             )
                                                     ),
                                                     ##### nStores Selected ####
                                                     valueBoxOutput('nStores', width = 12),
                                                     hr(),
                                                     ##### Brand Select #####
                                                     selectInput(inputId = 'brand', label = (strong('Select a Brand to View decision')),
                                                                 choices = list(), selected = character(0), multiple = FALSE),
                                                     ##### Layer Options ####
                                                     shinyjs::hidden(selectInput(inputId = 'layer_by', label = 'Select a variable to layer brand family with :', 
                                                                                 choices = c('None', 'category', 'segment'), selected = 'None')),
                                                     shinyjs::hidden(selectInput(inputId = 'layer', label = 'Select a level to layer with', 
                                                                                 choices = list())),
                                                     ##### Heatmap Options ####
                                                     shinyjs::hidden(
                                                         actionButton('viewHM', label = 'View Heatmap')
                                                     ),
                                                     bsModal(id = 'HMmodal', title = 'DMA level Heatmap for Selected Brand', 
                                                             trigger = 'viewHM', size = 'large',
                                                             fluidPage(
                                                                 valueBoxOutput('valueBrand', width = 12),
                                                                 radioButtons(inputId = 'heatmap_choice', label = 'Show Heatmap for Dolar values corresponding to :', 
                                                                              choices = c('Walmart', 'Total Market'), selected = 'Walmart', inline = TRUE),
                                                                 leafletOutput('heatmap')
                                                             ))
                                                 ),
                                                 ##### Toggle Options ####
                                                 shinyjs::hidden(div(id = 'hiddenBox', 
                                                                     box(title = 'Toggle Status', status = 'info', solidHeader = TRUE, width = 2, background = 'yellow',
                                                                         shinyjs::hidden(
                                                                             selectInput(inputId = 'allornot', label = 'Please Select an Appropriate Option:',
                                                                                         choices = c('Carry for All Stores in Selection',
                                                                                                     'Do Not Carry for All Stores in Selection',
                                                                                                     'Carry for Currently Clicked Store',
                                                                                                     'Do Not Carry for Currently Clicked Store')
                                                                             )
                                                                         ),
                                                                         tags$head(
                                                                             tags$style(HTML('#save{background-color:lightblue}'))
                                                                         ),
                                                                         shinyjs::hidden(actionButton('save', label = 'Save Decision', width = '90%'))
                                                                     )
                                                 )),
                                                 ###### Below MAP #####
                                                 box(title = '', status = 'success', solidHeader = TRUE, width = 12,
                                                     fluidRow(
                                                         ##### MAP color and GeoCluster ####
                                                         column(width = 4,
                                                                selectInput(inputId = 'color_by', label = 'Select a variable to color stores by :',
                                                                            choices = c('state', 'geocluster'), 
                                                                            selected = 'geocluster', multiple = FALSE),
                                                                actionButton(inputId = 'getGeoCluster', label = strong('Obtain Updated Geo-Clusters'))
                                                         ),
                                                         column(width = 6, 
                                                                valueBoxOutput(outputId = 'nGeo', width = 12)
                                                         )
                                                     ),
                                                     ##### Detail Table View ####
                                                     checkboxInput('viewHoT', label = h3('View Detailed Table'), value = FALSE),
                                                     column(width = 12, dataTableOutput('dt_table')),
                                                     ##### Table Toggler ####
                                                     fluidRow(
                                                         column(offset = 7, width = 3, 
                                                                selectInput('toggleDT', 
                                                                            label = 'Please Select an Appropriate Option', 
                                                                            choices = c('Carry for All Filtered Stores', 
                                                                                        'Do Not Carry for All Filtered Stores'), 
                                                                            selected = character(0), width = '100%')
                                                         ),
                                                         column(width = 2,
                                                                tags$head(
                                                                    tags$style(HTML('#save2{background-color:lightblue}'))
                                                                ),
                                                                actionButton('save2', label = 'Save Decision', width = '100%')
                                                         )
                                                     ),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br()
                                                 )
                                             )
                         ))
                )
    )
})
dashboardPage(
    title = 'Geo-Branding',
    header = header,
    sidebar = sidebar,
    body = body,
    skin = 'purple')