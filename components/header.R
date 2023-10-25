################################################################################
# header.R
# 
# Create the header for the ui.
################################################################################
customFontCSS <- "
@font-face {
  font-family: 'Poppins';
  src: url(data:font/truetype;charset=utf-8;base64,your-base64-encoded-font-here) format('truetype');
}
"

header <- dashboardHeader(title = tags$span("SeagrassWatcher",
                                            style = "font-size: 16px;
                                                     color: black;
                                                     font-family: 'Poppins', sans-serif;"))

