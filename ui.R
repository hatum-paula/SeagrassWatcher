################################################################################
# ui.R
# 
# Initializes the ui. 
# Used to load in your header, sidebar, and body components.
################################################################################
source('./components/header.R')
source('./components/sidebar.R')
source('./components/body.R')

jsCode <- "
$(document).ready(function() {
  $('.ui.menu .item').css({
    'font-size': '14px'
  });
});
"

modalJsCode <- '
$(document).ready(function() {
  $("#go_to_seagrass").on("click", function() {
    $("#seagrass_modal").modal("show");
  });
});
'

customCSS <- "
.ui.vertical.menu {
  width: 150px !important;
}
.ui.sidebar .sidebar-menu .item {
  line-height: 1.5 !important;  
  margin-bottom: 12px !important;  
}
/* Custom CSS to change font size for all buttons */
button {
  font-size: 16px !important;
}
/* Custom CSS to change font size for all selectize inputs */
.selectize-input, .selectize-dropdown {
  font-size: 16px !important;
}
/* Custom CSS to change font size for all Shiny sliders */
.js-irs-0 .irs-single, .js-irs-0 .irs-min, .js-irs-0 .irs-max {
  font-size: 16px !important;
}
/* Custom CSS to change font size for all labels */
label {
  font-size: 16px !important;
}
/* Custom CSS to change font size for all paragraphs */
p {
  font-size: 16px !important;
}
/* Custom CSS to change font size for all bullet points */
ul li {
  font-size: 16px !important;
}
/* Custom CSS to increase font size for Shiny tables */
    .shiny-table {
      font-size: 16px !important;
}
"

ui <- dashboardPage(
  tags$head(
    # JS for sidebar font-size
    tags$script(HTML(jsCode)),
    # JS for the modal
    tags$script(HTML(modalJsCode)),
    # CSS for sidebar width and item spacing
    tags$style(HTML(customCSS)),
    # Additional CSS for h1, h2, and p
    tags$style(HTML("
      h1 { color: #66CC99; }
      h2 { background-color: #f2f2f2; }
      p { font-size: 18px; text-align: justify;}
    ")),
    # Include Semantic UI's CSS and JS
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.js")
  ),
  header = header,
  sidebar = sidebar,
  body = body
)

