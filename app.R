library(shiny)
library(shinyWidgets)
library(magrittr)
library(DBI)

# create/connect to database
conn = dbConnect(RSQLite::SQLite(), 'bookmarks.sqlite')

ui = fluidPage(
    titlePanel('An app with lots of inputs'),
    sidebarLayout(
        sidebarPanel(
            dateRangeInput('date_range_input', label = 'Date range input'),
            pickerInput('picker_input', label = 'Picker input',
                        choices = LETTERS, multiple = TRUE,
                        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE, size = 5)),
            prettyCheckboxGroup('checkbox_group_input', label = 'Checkbox group input',
                                choices = c('Apple', 'Orange', 'Banana', 'Pear'),
                                selected = c('Apple', 'Pear'), inline = TRUE),
            prettyRadioButtons('radio_buttons_input', label = 'Radio buttons input',
                               choices = c('Cat', 'Dog', 'Fish', 'Rock'),
                                selected = c('Rock'), inline = TRUE),
            actionLink('bookmark', 'Bookmark application', icon = icon('share-alt'))
        ),
        mainPanel(
           verbatimTextOutput('values')
        )
    )
)

server = function(input, output, session) {
    
    # print current input values
    output$values = renderPrint({
        cat('date_range_input : ', paste0(input$date_range_input, collapse = ', '),
            '\npicker_input : ', paste0(input$picker_input, collapse = ', '),
            '\ncheckbox_group_input : ', paste0(input$checkbox_group_input, collapse = ', '),
            '\nradio_buttons_input : ', input$radio_buttons_input, sep = '')
    })
    
    observeEvent(input$bookmark, {
        # snapshot application state
        snapshot_state = reactiveValuesToList(input)
        # only bookmark inputs
        snapshot_state = snapshot_state[stringr::str_detect(names(snapshot_state), '_input')]
        # add the current system time to get a unique hash
        snapshot_state[['time']] = Sys.time()
        # create a state id
        state_id = digest::digest(snapshot_state)
        # create a row to add to the database table
        bookmark_row = tibble::tibble(
            'STATE_ID' = state_id,
            'CREATED_BY' = 'TEST',
            'DESCRIPTION' = 'TEST',
            'STATE_DATA' = jsonlite::toJSON(snapshot_state),
            'CREATED_AT' = Sys.time()
        )
        # write row to database table
        dbWriteTable(conn, 'bookmarks', bookmark_row, append = TRUE)
        # update the query string for the user
        updateQueryString(paste0('?bookmark=', state_id), mode = 'push')
    })
    
    observeEvent(getQueryString(), once = TRUE, {
        # get query string
        hash = getQueryString()
        if(length(hash) > 0) {
            # get bookmark from database table
            bookmark = dbGetQuery(conn, paste0("select state_data from bookmarks where state_id = '", hash$bookmark, "'"))
            # simulate average query time
            Sys.sleep(2)
            # coerce JSON to R list
            bookmark_list = jsonlite::fromJSON(bookmark$STATE_DATA)
            for(id in names(bookmark_list)) {
                value = bookmark_list[[id]]
                if(length(value) == 0) {
                    html_value = ''
                } else {
                    html_value = value
                }
                # update inputs in UI
                session$sendInputMessage(id, list(value = html_value))
            }
        }
    })
    
    # disconnect from database on session ended
    session$onEnded(function() {
        dbDisconnect(db)
    })
    
}

shinyApp(ui = ui, server = server)
