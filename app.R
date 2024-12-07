#library(colourpicker)
library(shiny)
# library(reactlog)
library(sf)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)
library(shinyBS)
# library(kableExtra)
# library(rms)
library(svglite)
library(glue)
library(cowplot)
library(janitor)
# library(rms)
# library(Hmisc)
library(cowplot)
# library(assertr)
library(lubridate)
library(shinydashboard)
library(magick)
# library(ggforce)
# library(plotly)
library(redcapAPI)

options(shiny.maxRequestSize = 25*1024^2)

source("jh_functions.R", local = TRUE)
source("compute_segment_angles_function_from_sim_data_2024.R", local = TRUE)
source("function_segment_angles_separated.R", local = TRUE)

source("jh_spine_build_NEW_function.R", local = TRUE)

source("jh_prescribing_alignment_functions.R", local = TRUE)

source("spinal_regional_alignment_analysis_by_vpa.R", local = TRUE)

# source("jh_build_spine_by_vertebral_pelvic_angles_only.R", local = TRUE)
source("jh_build_spine_by_vertebral_pelvic_angles_cleaned.R", local = TRUE)
source("prescribing_alignment_by_matching_unfused.R", local = TRUE)
source("xray_segment_angles_model_functions.R", local = TRUE)
source("build_spine_from_coordinates_functions.R", local = TRUE)




# Define the labels for both modes
get_spine_labels <- function(all_centroids = FALSE) {
  if (all_centroids) {
    return(c("fem_head_center", 
             "s1_anterior_superior", "s1_posterior_superior", 
             "l5_centroid", "l4_centroid", "l3_centroid", "l2_centroid", "l1_centroid", 
             "t12_centroid", "t11_centroid", "t10_centroid", "t9_centroid", 
             "t8_centroid", "t7_centroid", "t6_centroid", "t5_centroid", 
             "t4_centroid", "t3_centroid", "t2_centroid", "t1_centroid", 
             "c7_centroid", "c6_centroid", "c5_centroid", "c4_centroid", "c3_centroid", 
             "c2_centroid"))
  } else {
    return(c("fem_head_center", 
             "s1_anterior_superior", "s1_posterior_superior", 
             "l4_centroid", "l1_centroid", "t9_centroid", "t4_centroid", 
             "t1_centroid", "c2_centroid"))
  }
}

compute_perpendicular_points <- function(x1, y1, x2, y2, distance = 0.01) {
  # Midpoint
  midpoint_x <- (x1 + x2) / 2
  midpoint_y <- (y1 + y2) / 2
  
  # Slope of the line (tangent)
  slope <- (y2 - y1) / (x2 - x1)
  
  # Perpendicular slope (-1 / slope)
  perpendicular_slope <- -1 / slope
  
  # Calculate the change in x and y for perpendicular points
  delta_x <- distance / sqrt(1 + perpendicular_slope^2)
  delta_y <- perpendicular_slope * delta_x
  
  # Two perpendicular points
  point_1 <- c(midpoint_x + delta_x, midpoint_y + delta_y)
  point_2 <- c(midpoint_x - delta_x, midpoint_y - delta_y)
  
  return(tibble(x1 = point_1[1], y1 = point_1[2], x2 = point_2[1], y2 = point_2[2]))
}

calculate_pelvic_incidence_line_coordinates <- function(fem_head_center = c(0,0), 
                                                        s1_anterior, 
                                                        s1_posterior, 
                                                        spine_facing = "left",
                                                        pelvic_tilt = 10, 
                                                        pelvic_incidence_value = 50) {
  
  # Step 1: Calculate the center (midpoint)
  center_x <- (s1_anterior[1] + s1_posterior[1]) / 2
  center_y <- (s1_anterior[2] + s1_posterior[2]) / 2
  center <- c(center_x, center_y)
  
  # Step 2: Calculate the length of the line between s1_anterior and s1_posterior
  line_length <- sqrt((s1_anterior[1] - s1_posterior[1])^2 + (s1_anterior[2] - s1_posterior[2])^2)
  
  # Step 4: Calculate the length of the perpendicular line (5 times the original length)
  extended_length <- 3 * line_length
  
  if (s1_anterior[1] == s1_posterior[1]) {
    # For vertical lines, the perpendicular is horizontal
    dx <- extended_length
    dy <- 0
  } else if (s1_anterior[2] == s1_posterior[2]) {
    # For horizontal lines, the perpendicular is vertical
    dx <- 0
    dy <- extended_length
  } else {
    pt_pi_diff <- pelvic_incidence_value - pelvic_tilt
    
    pt_pi_diff_rad <- pt_pi_diff*(pi/180)
    
    orientation_modifier <- if_else(spine_facing == "left", -1, 1)
    
    dx <- sin(pt_pi_diff_rad)*extended_length*orientation_modifier
    dy <- cos(pt_pi_diff_rad)*extended_length
  }
  
  # Inferior point is displaced from the center by (dx, dy)
  inferior_x <- center[1] - dx
  inferior_y <- center[2] - dy
  inferior <- c(inferior_x, inferior_y)
  
  pi_line_coordinates_df <- tibble(spine_point = c("fem_head_center", "s1_center", "s1_inferior"), 
                                   x = c(fem_head_center[1], 
                                         center[1],
                                         inferior_x),
                                   y = c(fem_head_center[2],
                                         center[2],
                                         inferior_y)
  )
  # Return the center and inferior points as a list
  # return(list(center = center, inferior = inferior))
  pi_line_coordinates_df
}

# all_possible_lumbar_segments_angles_with_lpa_df <- read_csv("all_possible_lumbar_segment_angles_for_lpa.csv")

# reactlog_enable()

spinal_segments_labels_vector <- c('L5-S1', 'L4-L5', 'L3-L4', 'L2-L3', 'L1-L2', 
                                   'T12-L1', 'T11-T12', 'T10-T11', 'T9-T10', 'T8-T9', 'T7-T8', 'T6-T7', 'T5-T6', 'T4-T5', 'T3-T4', 'T2-T3', 'T1-T2',
                                   'C7-T1', 'C6-C7', 'C5-C6', 'C4-C5', 'C3-C4', 'C2-C3', 'C1-C2')

create_spine_rigid_level_input_function <- function(segment_input_label){
  segment_id <- paste0("preop_", str_to_lower(str_replace_all(segment_input_label, pattern = "-", "_")), "_segment")
  rigid_segment_id <- str_replace_all(segment_id, "_segment", "_rigid_xray")
  segment_label <- segment_input_label
  # initial_value <- segment_value_input
  
  # div(
    # class = "segment-input",
    # span(segment_label,
    #      class = "segment-label"),
    div(
      class = "segment-input",
      prettyCheckbox(
        inputId = rigid_segment_id,
        label = segment_label,
        value = FALSE,
        bigger = TRUE,
        status = "danger",
        shape = "curve"
      )
    )
  # )
}


ui <- dashboardPage(
  dashboardHeader(title = "SolaSpine"
                  ),
  dashboardSidebar(
    tags$style(HTML("
  .segment-input {
    display: flex;
    align-items: center; /* Align items to the center vertically */
    justify-content: end; /* Ensure space between label and input */
    margin-bottom: 0px; /* Adjusts the spacing between the inputs */
    font-size: 14px;
    color: black;
  }
  .segment-label {
    margin-right: 5px; /* Slightly increase space for the label */
    white-space: nowrap; /* Prevent labels from wrapping */
    font-size: 12px;
    color: black;
  }
  .segment-input .form-group {
    margin-bottom: 1px; /* Reduce default margin-bottom of form-group */
    color: black;
  }
  .custom-numeric-input {
    padding: 0; /* Remove padding from the numeric input container */
    margin: 0; /* Remove margin from the numeric input container */
    text-align: -webkit-left;
  }
  .custom-numeric-input .form-control {
    padding: 2px 5px; /* Adjust padding inside the numeric input */
    margin-bottom: 0px; /* Ensure no extra margin below the input */
    text-align: -webkit-left;
    width: 50px;
  }
")),
    # fileInput("image", "Upload an Image", accept = c('image/png', 'image/jpeg', 'image/jpg')), 
    fileInput("image", "Upload an Image", accept = 'image/'), 
    br(), 
    conditionalPanel(
      condition = "input.xray_file_uploaded == true",
      h4("Xray Orientation:"),
    actionBttn(
      inputId = "spine_orientation_button",
      label = "Facing LEFT",
      style = "material-flat",
      color = "primary",
      icon = icon("arrow-left")
    )
    ),
    br(),
    textInput(inputId = "redcap_token_password",
              label = "Redcap Password:",
              placeholder = "#Enter Redcap Passphrase#",
              value = "hills"),
    conditionalPanel(
      condition = "input.redcap_token_correct = true",  # JavaScript condition
      actionBttn(inputId = "get_next_redcap_id_button", label = "Get next redcap ID", style = "pill",size = "sm"),
      fluidRow(
        column(width = 8, 
               htmlOutput(outputId = "next_redcap_record_id_needed"), 
               htmlOutput(outputId = "next_10_redcap_record_id_needed")
               ),
        column(width = 4, 
               actionBttn(inputId = "search_for_retrieved_next_record_button", label = "Go", icon = icon("arrow-right"), style = "pill", size = "md"))
      )
      
    ),
    conditionalPanel(
      condition = "input.redcap_token_correct = true",  # JavaScript condition
      sidebarSearchForm(textId = "redcap_record_id",
                        buttonId = "get_redcap_data_button",
                        label = "Image Record ID")
    ),
    conditionalPanel(
      condition = "input.redcap_token_correct = true",  # JavaScript condition
      fluidRow(
        column(width = 6, 
               switchInput(inputId = "record_calibration_measure", label = "Calibration?", value = TRUE, size = "mini")
        ),
        column(width = 6, 
               actionBttn(inputId = "calibrate_button", label = "Calibrate", size = "sm", style = "fill")
        )
      )
    ),
    tableOutput(outputId = "redcap_measures_table"),
    # fluidRow(
    #   box(title = "Recording Alignment:",width = 12,
    #       div(
    #         style = "font-size: 14px; color: black", 
    #         fluidRow(
              # textInput(inputId = "redcap_token_password",
              #           label = "Redcap Password:",
              #           placeholder = "#Enter Redcap Passphrase#",
              #           value = "")
    #         ),
    #         hr(),
    #         fluidRow(
    #           conditionalPanel(
    #             condition = "input.redcap_token_correct = true",  # JavaScript condition
    #             # textInput(inputId = "redcap_record_id",label =  "Record ID:")
    #             sidebarSearchForm(textId = "redcap_record_id",
    #                               buttonId = "get_redcap_data_button",
    #                               label = "Redcap Record ID")
    #           )
    #         ),
    #         fluidRow(
    #           # actionBttn(inputId = "get_redcap_data_button", label = "Get Redcap data"),
    #           tableOutput(outputId = "redcap_measures_table")
    #         )
    #         )
    #     )
    # ),
    fluidRow(
     box(title = "Surgical Planning: (not currently enabled)",
         width = 12, 
         collapsible = TRUE, 
         collapsed = TRUE, 
        conditionalPanel(
      condition = "input.xray_file_uploaded == true",
      h4(strong("Patient Factors:")),
      sliderInput(
        "preop_age",
        "Patient Age:",
        min = 18,
        max = 90,
        value = 60
      ),
      radioGroupButtons(
        inputId = "preop_sex",
        label = NULL,
        individual = TRUE,
        choices = c("Male", "Female"),
        selected = "Female",
        checkIcon = list(
          yes = tags$i(class = "fa fa-check-square", 
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-square-o", 
                      style = "color: steelblue"))
      ),
      hr(),
      
    uiOutput(outputId = "preop_xray_rigid_segments_ui")
    )
    ) 
    ),
    div(
      style = "display: none;",  # Hide the entire div, including the switch
      switchInput(
        inputId = "xray_file_uploaded",
        size = "mini", label = NULL,
        value = FALSE, 
        onLabel = "Y", 
        offLabel = "N",
      ),
      switchInput(
        inputId = "all_points_recorded",
        size = "mini", label = NULL,
        value = FALSE, 
        onLabel = "Y", 
        offLabel = "N",
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
    .nav-tabs-custom > .nav-tabs {
      background-color: #0073e6; /* Set your preferred color */
    }
    .nav-tabs-custom > .nav-tabs > li.active > a, 
    .nav-tabs-custom > .nav-tabs > li.active > a:hover {
      background-color: #005bb5; /* Set a darker color for active tab */
      color: white;
      font-size: 18px; /* Make tab titles larger */
      font-weight: bold; /* Make tab titles bold */
    }
    .nav-tabs-custom > .nav-tabs > li > a {
      color: white;
    }
  "))
    ),
    # Boxes need to be put in a row (or column)
    fluidRow(
      conditionalPanel(
        "input.all_points_recorded == true",
        actionBttn(inputId = "upload_to_redcap", label = "Upload Coordinates to Redcap", icon = icon("upload"), style = "jelly", color = "primary", size = "md")
      ),
      box(width = 3,
          conditionalPanel(
            condition = "input.xray_file_uploaded == true",
            fluidRow(
            class = "d-flex justify-content-center",  # Center content horizontally
                   tags$div(
                     style = "font-size: 20px; 
                 font-weight: bold; 
                 color: yellow; 
                 font-family: arial; 
                 font-style: italic; 
                 text-align: center; 
                 background-color: black; 
                 padding: 3px; 
                 border-radius: 12px;  /* Rounded corners */
                 display: block;
                 margin-left: 10px;
                 margin-right: 10px;
                     box-sizing: border-box;  /* Include padding and border in the element's width */",
                     htmlOutput(outputId = "xray_click_instructions")
                   ),
                   br()
          )
            ),
          conditionalPanel(
            condition = "input.xray_file_uploaded == true & input.all_points_recorded == false",
            # condition = "input.xray_file_uploaded == true",
          fluidRow(
            column(width = 12, 
                   tags$div(
                 id = "image-container",
                 style = "position: relative; width: 350px; height: 700px; overflow: hidden; border: 0px solid #ccc;",
                 tags$img(
                   id = "uploadedImage",
                   src = "",
                   style = "position: absolute; top: 0; left: 0; cursor: crosshair;"
                 )
               ),
               # tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"),
               tags$script(HTML("
       $(document).ready(function() {
  let scale = 1;
  let panX = 0, panY = 0;
  let isPanning = false;
  let startX, startY;

  function updateImageTransform() {
    $('#uploadedImage').css({
      'transform-origin': 'top left',
      'transform': `translate(${panX}px, ${panY}px) scale(${scale})`
    });

    // Update positions of all dots to match the transformation
    $('.dot').each(function() {
      const originalX = $(this).data('orig-x');
      const originalY = $(this).data('orig-y');
      const adjustedX = (originalX * scale) + panX;
      const adjustedY = ((imageHeight - originalY) * scale) + panY;

      $(this).css({
        left: adjustedX + 'px',
        top: adjustedY + 'px'
      });
    });
  }

  let imageHeight = null; // We'll determine the height once the image is loaded

  Shiny.addCustomMessageHandler('load-image', function(data) {
    var img = document.getElementById('uploadedImage');
    img.src = data.src;

    // Once the image loads, set the natural height
    img.onload = function() {
      imageHeight = img.naturalHeight;
      // Reset scaling and position when new image is loaded
      scale = 1;
      panX = 0;
      panY = 0;
      updateImageTransform();

      // Remove existing dots when a new image is loaded
      $('.dot').remove();
    };
  });

  Shiny.addCustomMessageHandler('plot-coordinates', function(data) {
    // Remove existing dots
    $('.dot').remove();

    if (!imageHeight) {
      console.error('Image height not set yet.');
      return;
    }

    // Plot all coordinates
    data.coords.forEach(function(coord, index) {
      console.log(`Plotting point ${index + 1}:`, coord);  // Debugging log for each coordinate

      // Create a new dot element
      const dot = $('<div class=\"dot\"></div>');

      // Adjust Y-coordinate for the Cartesian system
      const adjustedX = (coord.x * scale) + panX;
      const adjustedY = ((imageHeight - coord.y) * scale) + panY;  // Adjusting Y-coordinate

      // Debugging log for adjusted positions
      console.log('Adjusted position for dot:', { adjustedX, adjustedY });
      
      // Adjust the dot's CSS to center it on the point clicked
    const dotSize = 10; // This is the width and height of the dot (in pixels)
    const correctionOffset = 0.5; // Small adjustment if there’s still an offset issue

    dot.css({
      position: 'absolute',
      top: (adjustedY - (dotSize / 2)) + correctionOffset + 'px',
      left: (adjustedX - (dotSize / 2)) + correctionOffset + 'px',
      width: dotSize + 'px',
      height: dotSize + 'px',
      'background-color': 'red',
      'border-radius': '50%',
      'pointer-events': 'none', // Ensures dots don't interfere with panning/zooming
      'z-index': 10 // Ensures the dots are layered above the image
    });

      // Store original coordinates for reference during zoom and pan
      dot.data('orig-x', coord.x);
      dot.data('orig-y', coord.y);

      // Append dot to the image container
      $('#image-container').append(dot);
    });

    // Immediately update dot positions to reflect the current zoom and pan state
    updateImageTransform();
  });

  // Handle zoom with the mouse wheel
  $('#image-container').on('wheel', function(e) {
    e.preventDefault();
    const zoomIntensity = 0.1;
    const delta = e.originalEvent.deltaY > 0 ? -1 : 1;
    const previousScale = scale;

    // Update scale
    scale *= (1 + delta * zoomIntensity);
    scale = Math.min(Math.max(0.5, scale), 5);

    // Calculate new pan to keep the zoom centered at mouse position
    const mouseX = e.pageX - $(this).offset().left;
    const mouseY = e.pageY - $(this).offset().top;

    panX = mouseX - (mouseX - panX) * (scale / previousScale);
    panY = mouseY - (mouseY - panY) * (scale / previousScale);

    updateImageTransform();
  });

  // Handle panning with right-click only
  $('#image-container').on('mousedown', function(e) {
    if (e.which === 3) { // Right-click
      isPanning = true;
      startX = e.pageX - panX;
      startY = e.pageY - panY;
      $(this).css('cursor', 'grabbing');
      return false; // Prevent context menu
    }
  });

  $(document).on('mouseup', function() {
    isPanning = false;
    $('#image-container').css('cursor', 'crosshair');
  });

  $(document).on('mousemove', function(e) {
    if (!isPanning) return;
    panX = e.pageX - startX;
    panY = e.pageY - startY;

    updateImageTransform();
  });

  // Prevent the default context menu from appearing on right-click
  $('#image-container').on('contextmenu', function(e) {
    return false;
  });

  // Record click coordinates on left-click
  $('#image-container').on('click', function(e) {
    if (e.which === 1) { // Left-click
      var img = document.getElementById('uploadedImage');
      const rect = img.getBoundingClientRect(); // Get the image's bounding box relative to the viewport

      // Get the click coordinates relative to the image
      const clickX = e.clientX - rect.left;
      const clickY = e.clientY - rect.top;

      // Adjust the coordinates for the current pan and zoom level to get the original image reference frame
      const adjustedX = clickX / scale;
      const adjustedY = clickY / scale;

      // Correcting Y-coordinate (flipping the y-axis based on the height of the image)
      const correctedY = imageHeight - adjustedY;

      // Debugging log for adjusted click positions
      console.log('Adjusted click position:', { adjustedX, correctedY });

      // Send the corrected click coordinates to the Shiny server
      Shiny.setInputValue('xray_click', {x: adjustedX - 1, y: correctedY + 1}, {priority: 'event'});
    }
  });
});
      "))
        )
          )
          ),
        ############################## COMPLETED COORDINATE COLLECTION ################################
        conditionalPanel(
          condition = "input.xray_file_uploaded == true & input.all_points_recorded == true",
          fluidRow(
            column(width = 12, 
                   tags$div(
                     id = "image-plot-container",
                     style = "position: relative; width: 350px; height: 700px; overflow: hidden; border: 0px solid #ccc;",
                     tags$img(
                       id = "uploadedImagePlot",
                       src = "",
                       style = "position: absolute; top: 0; left: 0; cursor: crosshair;"
                     )
                   ),
                   # tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"),
                   tags$script(HTML("
       $(document).ready(function() {
  let scale = 1;
  let panX = 0, panY = 0;
  let isPanning = false;
  let startX, startY;

  function updateImageTransform() {
    $('#uploadedImagePlot').css({
      'transform-origin': 'top left',
      'transform': `translate(${panX}px, ${panY}px) scale(${scale})`
    });

    // Update positions of all dots to match the transformation
    $('.dot').each(function() {
      const originalX = $(this).data('orig-x');
      const originalY = $(this).data('orig-y');
      const adjustedX = (originalX * scale) + panX;
      const adjustedY = ((imageHeight - originalY) * scale) + panY;

      $(this).css({
        left: adjustedX + 'px',
        top: adjustedY + 'px'
      });
    });
  }

  let imageHeight = null; // We'll determine the height once the image is loaded

  Shiny.addCustomMessageHandler('load-plot-image', function(data) {
    var img = document.getElementById('uploadedImagePlot');
    img.src = data.src;

    // Once the image loads, set the natural height
    img.onload = function() {
      imageHeight = img.naturalHeight;
      // Reset scaling and position when new image is loaded
      scale = 1;
      panX = 0;
      panY = 0;
      updateImageTransform();

      // Remove existing dots when a new image is loaded
      $('.dot').remove();
    };
  });


  // Handle zoom with the mouse wheel
  $('#image-plot-container').on('wheel', function(e) {
    e.preventDefault();
    const zoomIntensity = 0.1;
    const delta = e.originalEvent.deltaY > 0 ? -1 : 1;
    const previousScale = scale;

    // Update scale
    scale *= (1 + delta * zoomIntensity);
    scale = Math.min(Math.max(0.5, scale), 5);

    // Calculate new pan to keep the zoom centered at mouse position
    const mouseX = e.pageX - $(this).offset().left;
    const mouseY = e.pageY - $(this).offset().top;

    panX = mouseX - (mouseX - panX) * (scale / previousScale);
    panY = mouseY - (mouseY - panY) * (scale / previousScale);

    updateImageTransform();
  });

  // Handle panning with right-click only
  $('#image-plot-container').on('mousedown', function(e) {
    if (e.which === 3) { // Right-click
      isPanning = true;
      startX = e.pageX - panX;
      startY = e.pageY - panY;
      $(this).css('cursor', 'grabbing');
      return false; // Prevent context menu
    }
  });

  $(document).on('mouseup', function() {
    isPanning = false;
    $('#image-plot-container').css('cursor', 'crosshair');
  });

  $(document).on('mousemove', function(e) {
    if (!isPanning) return;
    panX = e.pageX - startX;
    panY = e.pageY - startY;

    updateImageTransform();
  });

  // Prevent the default context menu from appearing on right-click
  $('#image-plot-container').on('contextmenu', function(e) {
    return false;
  });

  // Record click coordinates on left-click
  $('#image-plot-container').on('click', function(e) {
    if (e.which === 1) { // Left-click
      var img = document.getElementById('uploadedImagePlot');
      const rect = img.getBoundingClientRect(); // Get the image's bounding box relative to the viewport

      // Get the click coordinates relative to the image
      const clickX = e.clientX - rect.left;
      const clickY = e.clientY - rect.top;

      // Adjust the coordinates for the current pan and zoom level to get the original image reference frame
      const adjustedX = clickX / scale;
      const adjustedY = clickY / scale;

      // Correcting Y-coordinate (flipping the y-axis based on the height of the image)
      const correctedY = imageHeight - adjustedY;

      // Debugging log for adjusted click positions
      console.log('Adjusted click position:', { adjustedX, correctedY });

      // Send the corrected click coordinates to the Shiny server
      Shiny.setInputValue('xray_plot_click', {x: adjustedX - 1, y: correctedY + 1}, {priority: 'event'});
    }
  });
});
      "))
            )
          )
        ),
        conditionalPanel(
          condition = "input.xray_file_uploaded == true",
          fluidRow(
            tags$div(
              "zoom with scroll wheel; pan with right click",
              style = "font-size: 8pt; font-style: italic; color: #555; text-align: center;"
            )
          ),
        fluidRow(
          column(
            width = 6,
            actionBttn(
              inputId = "xray_delete_last_point",
              block = TRUE,
              size = "md",
              label = "Delete Last",
              style = "jelly",
              color = "success",
              icon = icon("delete-left")
            )
          ),
          column(
            width = 6,
            actionBttn(
              size = "md",
              inputId = "xray_reset_points",
              block = TRUE,
              label = "Reset",
              style = "unite",
              color = "danger",
              icon = icon("trash-can")
            )
          ),
        )
        # fluidRow(
        #   conditionalPanel(
        #     condition = "input.xray_file_uploaded == true",
        #     h4("Xray Orientation:"),
        #     actionBttn(
        #       inputId = "spine_orientation_button",
        #       label = "Facing LEFT", 
        #       style = "material-flat",
        #       color = "primary",
        #       icon = icon("arrow-left")
        #     )
        #   )
        # )
        )
      ),
      box(width = 3, 
          tableOutput(outputId = "spine_click_parameters")
          ),
      column(width = 2, 
             conditionalPanel(
        condition = "input.all_points_recorded == true",
        box(title = "Preop Alignment:", 
            width = 12,
            plotOutput(outputId = "preop_spine_simulation_plot",
                       height = "750px"),
            tableOutput("alignment_parameters_df")
        ), 
        box(title = "Alignment Planning:",
              collapsible = TRUE, collapsed = TRUE,
            tableOutput(outputId = "click_coordinates_df"),
            br(),
            textOutput(outputId = "click_coordinates_text"),
            br(),
            hr(),
            h4("Centroid Coordinates:"),
            tableOutput(outputId = "centroid_coordinates_df"), 
            br(), 
            textOutput("xray_centroid_tibble_text")
            )
      )
      ),
      conditionalPanel(
        condition = "input.all_points_recorded == true",
      box(width = 4,
        # title = "Plans",
        actionBttn(
          inputId = "compute_plan_xray",
          label = "Compute Plan",
          style = "unite", 
          color = "danger"
        ),
        hr(),
        fluidRow(
          tabBox(width = 12,
            title = div(style = "float: right;", "Alignment Plans"),  # Moves title to the right
            id = "alignment_plans", 
            tabPanel("Lower T UIV", 
                     # plotOutput(outputId = "spine_plan_lower_t_xray", height = 650), 
                     uiOutput(outputId = "spine_plan_lower_t_ui")
            ),
            tabPanel("Upper T UIV", 
                     # plotOutput(outputId = "spine_plan_upper_t_xray", height = 650),
                     uiOutput(outputId = "spine_plan_upper_t_ui")
            )
          )
        )
      )
      )
    )
  )
)


############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################
############################################################# SERVER ##########################################################

# Server logic
server <- function(input, output, session) {

  observeEvent(input$image, {
    req(input$image)
    updateSwitchInput(session = session, inputId = "xray_file_uploaded", value = TRUE)
  })
  
  spine_orientation <- reactiveVal("left")
  
  observeEvent(input$spine_orientation_button, ignoreInit = TRUE, {
    # Update the spine orientation value
    if (spine_orientation() == "left") {
      spine_orientation("right")
    } else {
      spine_orientation("left")
    }
  })
  
  observeEvent(spine_orientation(), {
    # Update the spine orientation value
    if (spine_orientation() == "left") {
      updateActionButton(session, "spine_orientation_button", label = "Facing LEFT", icon = icon("arrow-left"))
    } else {
      updateActionButton(session, "spine_orientation_button", label = "Facing RIGHT", icon = icon("arrow-right"))
    }
  })
  
  observeEvent(click_coord_reactive_list$coords, {
    if(length(click_coord_reactive_list$coords)==3){
      fem_head_x <- click_coord_reactive_list$coords$fem_head_center$x
      s1_anterior_superior_x <- click_coord_reactive_list$coords$s1_anterior_superior$x
      s1_posterior_superior_x <- click_coord_reactive_list$coords$s1_posterior_superior$x

      if(s1_anterior_superior_x < s1_posterior_superior_x){
        spine_orientation("left")
      }else{
        spine_orientation("right")
      }
    }

    ## extreme scenarios
  # if(length(click_coord_reactive_list$coords)== 4){
  # 
  #   fem_head_x <- click_coord_reactive_list$coords$fem_head_center$x
  #   s1_anterior_superior_x <- click_coord_reactive_list$coords$s1_anterior_superior$x
  #   s1_posterior_superior_x <- click_coord_reactive_list$coords$s1_posterior_superior$x
  # 
  #   if((s1_anterior_superior_x < s1_posterior_superior_x) & click_coord_reactive_list$coords$l4_centroid$x >  s1_anterior_superior_x){
  #     spine_orientation("right")
  #   }
  #   
  #   if((s1_anterior_superior_x > s1_posterior_superior_x) & click_coord_reactive_list$coords$l4_centroid$x <  s1_anterior_superior_x){
  #     spine_orientation("left")
  #   }
  #   
  # }
    
    
  }
  )
  
  
  # observeEvent(input$spine_orientation_button, {
  #   # Update the spine orientation value
  #   if (spine_orientation() == "left") {
  #     spine_orientation("right")
  #     updateActionButton(session, "spine_orientation_button", label = "Facing RIGHT", icon = icon("arrow-right"))
  #   } else {
  #     spine_orientation("left")
  #     updateActionButton(session, "spine_orientation_button", label = "Facing LEFT", icon = icon("arrow-left"))
  #   }
  # })
  # 
  # observeEvent(input$xray_click, {
  #   if(length(click_coord_reactive_list$coords)==3){
  #     
  #     fem_head_x <- click_coord_reactive_list$coords$fem_head_center$x
  #     s1_anterior_superior_x <- click_coord_reactive_list$coords$s1_anterior_superior$x
  #     s1_posterior_superior_x <- click_coord_reactive_list$coords$s1_posterior_superior$x
  # 
  #     if(s1_anterior_superior_x < s1_posterior_superior_x){
  #       # xray_orientation <- "left"
  #       spine_orientation("left")
  #       updateActionButton(session, "spine_orientation_button", label = "Facing LEFT", icon = icon("arrow-left"))
  #     }else{
  #       spine_orientation("right")
  #       updateActionButton(session, "spine_orientation_button", label = "Facing RIGHT", icon = icon("arrow-right"))
  #       # xray_orientation <- "right"
  #     }
  #   }
    # 
    # if(length(click_coord_reactive_list$coords)== 4){
    #   
    #   fem_head_x <- click_coord_reactive_list$coords$fem_head_center$x
    #   s1_anterior_superior_x <- click_coord_reactive_list$coords$s1_anterior_superior$x
    #   s1_posterior_superior_x <- click_coord_reactive_list$coords$s1_posterior_superior$x
    #   
    #   if((s1_anterior_superior_x < s1_posterior_superior_x) & click_coord_reactive_list$coords$l4_centroid$x >  s1_anterior_superior_x){
    #     spine_orientation("right")
    #     updateActionButton(session, "spine_orientation_button", label = "Facing RIGHT", icon = icon("arrow-right"))
    #     
    #   }else{
    #     spine_orientation("left")
    #     updateActionButton(session, "spine_orientation_button", label = "Facing LEFT", icon = icon("arrow-left"))
    #     
    #   }
    # }
  # }
  # )
  

  
  # observeEvent(input$image, {
  observe({
    req(input$image)
    if(xray_instructions_reactiveval() == "Completed"){
      req(input$image)  # Ensure there's an image uploaded
      
      xray <- image_scale(image_read(path = input$image$datapath), "400x")
      
      xray_height <- image_info(xray)$height
      xray_width <- image_info(xray)$width
      
      # Generate the plot
      plot <- xray_reactive_plot()
      
      # Save the plot as a temporary file
      temp_file <- tempfile(fileext = ".jpg")
      ggsave(temp_file, plot = plot, width = xray_width, height = xray_height, units = "px")
      
      
      img_scaled <- image_scale(image_read(temp_file), "400x")  # Scale to 400px width
      
      # Write the scaled image to a temporary file
      temp_file <- tempfile(fileext = ".jpg")
      image_write(img_scaled, path = temp_file, format = "jpeg")
      
      # Encode the scaled image to base64
      img_base64 <- base64enc::dataURI(file = temp_file, mime = "image/jpeg")

      ####### old
      # xray <- image_scale(image_read(path = input$image$datapath), "400x")
      # 
      # xray_height <- image_info(xray)$height
      # xray_width <- image_info(xray)$width
      # 
      # # Generate the plot
      # plot <- xray_reactive_plot()
      # 
      # # Save the plot as a temporary file
      # temp_file <- tempfile(fileext = ".png")
      # ggsave(temp_file, plot = plot, width = xray_width, height = xray_height, units = "px")
      # 
      # # Read the image back and convert it to a base64 string for embedding
      # img <- magick::image_read(temp_file)
      # img_base64 <- base64enc::dataURI(file = temp_file, mime = "image/png")
      # 
      # # Send the image to the UI
      # # session$sendCustomMessage('load-plot-image', list(src = img_base64))
      
      print(paste("Sending load-plot-image message", "Xray height is ", xray_height, "Width is ", xray_width))  # Debugging log
      session$sendCustomMessage('load-plot-image', list(src = img_base64)) 
      
    }else{
      # img <- image_read(input$image$datapath)
      img_scaled <- image_scale(image_read(input$image$datapath), "400x")  # Scale to 400px width
      
      # Write the scaled image to a temporary file
      temp_file <- tempfile(fileext = ".jpg")
      image_write(img_scaled, path = temp_file, format = "jpeg")
      
      # Encode the scaled image to base64
      image_src <- base64enc::dataURI(file = temp_file, mime = "image/jpeg")

      # Send the image URI to the UI
      session$sendCustomMessage('load-image', list(src = image_src)) 
    }
  })
  
  click_coord_reactive_list <- reactiveValues(coords = list(), index = 1)
  
  
  
  # Reset button to clear all points
  observeEvent(input$xray_reset_points, ignoreInit = TRUE, {
    click_coord_reactive_list$coords <- list()
    click_coord_reactive_list$index <- 1
  })
  
  # Button to remove the last recorded point
  observeEvent(input$xray_delete_last_point, ignoreInit = TRUE, {
    if (click_coord_reactive_list$index > 1) {
      click_coord_reactive_list$coords[[click_coord_reactive_list$index - 1]] <- NULL
      click_coord_reactive_list$index <- click_coord_reactive_list$index - 1
    }
    
    if(length(plot_points_coordinates_reactiveval())>0){
      plot_points_list <- plot_points_coordinates_reactiveval()
      
      plot_points_list <- plot_points_list[-length(plot_points_list)]
      
      plot_points_coordinates_reactiveval(plot_points_list)
      
    }
  })
  
  
  plot_points_coordinates_reactiveval <- reactiveVal()
  
  # Store clicks and assign them to the correct label
  observeEvent(list(input$xray_click), ignoreInit = TRUE, {
    spine_input_labels <- get_spine_labels(FALSE)
  
    # Only proceed if there's a label available for the current index
    if (click_coord_reactive_list$index <= length(spine_input_labels)) {
      target_name <- spine_input_labels[click_coord_reactive_list$index]
      # Create a named list with the click coordinates
      new_click <- list(x = input$xray_click$x, y = input$xray_click$y)
      
      # Append the new click as a named list with the label
      click_coord_reactive_list$coords[[target_name]] <- new_click
      click_coord_reactive_list$index <- click_coord_reactive_list$index + 1
    }
    
    # Debugging step to print the structure of click_coord_reactive_list$coords
    # print(str(click_coord_reactive_list$coords))
    
    # Convert the named list to a list of lists
    coords_for_js <- unname(lapply(click_coord_reactive_list$coords, function(coord) {
      list(x = coord$x, y = coord$y)
    }))
    
    # Debugging step to print coords_for_js
    # print(coords_for_js)
    
    plot_points_coordinates_reactiveval(coords_for_js)
    
    # Send the coordinates to JavaScript
    # session$sendCustomMessage('plot-coordinates', list(coords = coords_for_js))
  })
  
  observeEvent(list(input$xray_click, input$xray_delete_last_point, input$xray_reset_points), {
    
    session$sendCustomMessage('plot-coordinates', list(coords = plot_points_coordinates_reactiveval()))
  })
  
  
  
  xray_instructions_reactiveval <- reactiveVal("x")
  
  # Render instructions dynamically based on the number of recorded clicks
  output$xray_click_instructions <- renderText({
    spine_input_labels <- get_spine_labels(FALSE)
    click_count <- length(click_coord_reactive_list$coords)
    # 
    # print(paste("click_count", click_count))
    # print(paste("spine_input_labels", toString(names(click_coord_reactive_list$coords))))
    
    if (click_count < length(spine_input_labels)) {
      instruction <- spine_input_labels[click_count + 1]
      
      instruction <- str_replace_all(instruction, "fem_head_center", "Center of Hips")
      instruction <- str_replace_all(instruction, "_superior", "_superior Corner")
      
      instruction <- str_to_title(str_replace_all(instruction, "_", " "))
      
      instruction <- glue("Click:<br>{instruction}")
      
      xray_instructions_reactiveval("x")
      
      print("correct instructions")
      
    } else {
      print("inccorrect instructions")
      
      instruction <- "All points recorded."
      xray_instructions_reactiveval("Completed")
    }

    HTML("<div>", instruction, "</div>")
  })
  
  
  # Create a reactive table to display coordinates
  click_coordinates_df_reactive <- reactive({
    if (length(click_coord_reactive_list$coords) > 0) {
      # Convert the list to a tibble
      tibble(
        spine_point = names(click_coord_reactive_list$coords),
        x = map_dbl(click_coord_reactive_list$coords, "x"),
        y = map_dbl(click_coord_reactive_list$coords, "y")
      )
    } else {
      tibble(spine_point = character(), x = double(), y = double())
    }
  })
  
  output$click_coordinates_df <- renderTable({
    # click_coordinates_df_reactive()
    
    plotting_coord_list <- plot_points_coordinates_reactiveval()
    
    if (length(plotting_coord_list) > 0) {
      # Convert the list to a tibble
      plotting_coord_df <- tibble(
        spine_point = names(click_coord_reactive_list$coords),
        x = map_dbl(plotting_coord_list, "x"),
        y = map_dbl(plotting_coord_list, "y")
      ) 
      
      plotting_coord_df 
        # mutate(y = abs(y - max(plotting_coord_df$y)))
      
    } else {
      tibble(spine_point = character(), x = double(), y = double())
    }
  })
  
  output$click_coordinates_text <- renderText({
    # click_coordinates_df_reactive()
    
    if (length(plot_points_coordinates_reactiveval()) > 0) {
      # Convert the list to a tibble
      s1_center <- jh_get_point_along_line_function(coord_a = c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y), 
                                       coord_b = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y), 
                                       percent_a_to_b = 0.5)
      
      click_df <- tibble(spine_point = "s1_center", x = s1_center[1], y = s1_center[2]) %>%
        union_all(click_coordinates_df_reactive())

        spine_point_labels <- glue_collapse(click_df$spine_point, sep = "', '")
        
        x_values <- glue_collapse(click_df$x, sep = ", ")
        y_values <- glue_collapse(click_df$y, sep = ", ")
        
        glue("click_df <- tibble(spine_point = c('{spine_point_labels}'), x = c({x_values}), y = c({y_values}))")
      
      
    } else {
      glue("click_df <- tibble(spine_point)")
    }
  })
  

  ############ COMPUTE CENTROIDS #################
  # Reactive tibble for centroid coordinates
  xray_centroid_coordinates_reactive_df <- reactive({
    # Get clicked coordinates dataframe
    xray_click_coordinates_df <- click_coordinates_df_reactive()
    
    # Labels for spine points that we need to have in the final dataframe
    spine_coordinate_labels <- tibble(
      spine_point = c("fem_head_center", "s1_center", "l5_centroid", "l4_centroid", "l3_centroid",
                      "l2_centroid", "l1_centroid", "t12_centroid", "t11_centroid", "t10_centroid",
                      "t9_centroid", "t8_centroid", "t7_centroid", "t6_centroid", "t5_centroid",
                      "t4_centroid", "t3_centroid", "t2_centroid", "t1_centroid", "c7_centroid",
                      "c6_centroid", "c5_centroid", "c4_centroid", "c3_centroid", "c2_centroid")
    )
    
    # Calculate S1 center based on anterior and posterior clicks
    s1_center <- if ("s1_anterior_superior" %in% names(click_coord_reactive_list$coords) && 
                     "s1_posterior_superior" %in% names(click_coord_reactive_list$coords)) {
      s1_anterior <- click_coord_reactive_list$coords$s1_anterior_superior
      s1_posterior <- click_coord_reactive_list$coords$s1_posterior_superior
      c((s1_anterior[[1]] + s1_posterior[[1]]) / 2, (s1_anterior[[2]] + s1_posterior[[2]]) / 2)
    } else {
      c(NA, NA)
    }
    
    # Build the tibble including the S1 center
    current_coords_df <-  tibble(spine_point = "s1_center", x = s1_center[1], y = s1_center[2]) %>%
      bind_rows(xray_click_coordinates_df %>%
                  filter(spine_point != "s1_anterior_superior", 
                         spine_point != "s1_posterior_superior",
                         spine_point != "fem_head_center"))
    
    if(any(xray_click_coordinates_df$spine_point == "c2_centroid")){
      
      spine_coordinate_labels_df <- tibble(
        spine_point = c("s1_center", "l5_centroid",
                        "l4_centroid", "l3_centroid", "l2_centroid", "l1_centroid", "t12_centroid",
                        "t11_centroid", "t10_centroid", "t9_centroid", "t8_centroid", "t7_centroid",
                        "t6_centroid", "t5_centroid", "t4_centroid", "t3_centroid", "t2_centroid",
                        "t1_centroid", "c7_centroid", "c6_centroid", "c5_centroid", "c4_centroid",
                        "c3_centroid", "c2_centroid")
      )%>%
        mutate(index_count = row_number())
      
      spine_coordinates_short_df <- spine_coordinate_labels_df %>%
        left_join(current_coords_df) %>%
        filter(!is.na(x))
      
      l5_centroid_y_adjustment <- (spine_coordinates_short_df %>% filter(spine_point == "s1_center"))$y + ((spine_coordinates_short_df %>% filter(spine_point == "l4_centroid"))$y - (spine_coordinates_short_df %>% filter(spine_point == "s1_center"))$y)*0.35
      
      head_df <- spine_coordinates_short_df %>%
        filter(spine_point == "c2_centroid") %>%
        mutate(y = y*1.1) %>%
        mutate(spine_point = "head") %>%
        mutate(index_count = index_count + 1)
      
      # spine_y_filled_df <- spine_coordinate_labels_df %>%
      #   left_join(spine_coordinates_short_df) %>%
      #   mutate(y = zoo::na.spline(y)) %>%
      #   # mutate(y = zoo::na.approx(y, rule = 2)) %>%
      #   mutate(y = round(y, 3)) %>%
      #   mutate(y = if_else(spine_point == "l5_centroid", l5_centroid_y_adjustment, y))
      # 
      # 
      # final_coords_df <- spine_y_filled_df %>%
      #   # mutate(x = spline(spine_coordinates_short_df$y, spine_coordinates_short_df$x, xout = spine_y_filled_df$y)$y) %>%
      #   mutate(x = zoo::na.spline(x))%>%
      #   select(spine_point, x, y) %>%
      #   mutate(x = round(x, 3))
      
      final_coords_df <- spine_coordinate_labels_df %>%
        left_join(spine_coordinates_short_df) %>%
        union_all(head_df) %>%
        mutate(x = zoo::na.spline(x))%>%
        filter(spine_point != "head") %>%
        mutate(y = zoo::na.spline(y)) %>%
        mutate(y = round(y, 3)) %>%
        mutate(y = if_else(spine_point == "l5_centroid", l5_centroid_y_adjustment, y)) %>%
        select(spine_point, x, y) %>%
        mutate(x = round(x, 3))
      
    }else{
      final_coords_df <- current_coords_df  %>%
        filter(!is.na(x))  # Return what has been clicked so far
      
    }
    
    
    final_coords_df%>%
      filter(!is.na(x)) 
      # mutate(y = abs(y - max(final_coords_df$y)))
  })
  
  output$centroid_coordinates_df <- renderTable({
    xray_centroid_coordinates_reactive_df()
  })
  
  output$xray_centroid_tibble_text <- renderText({
    spine_point_labels <- glue_collapse(xray_centroid_coordinates_reactive_df()$spine_point, sep = "', '")
    
    x_values <- glue_collapse(xray_centroid_coordinates_reactive_df()$x, sep = ", ")
    y_values <- glue_collapse(xray_centroid_coordinates_reactive_df()$y, sep = ", ")
    
    fem_head_center <- c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y)
    # 
    s1_anterior_superior <- c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y)
    s1_posterior_superior <- c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y)
    
    glue("fem_head_center <- c({fem_head_center[1]}, {fem_head_center[2]})\n  s1_anterior_superior <- c({s1_anterior_superior[1]}, {s1_anterior_superior[2]})\n s1_posterior_superior <- c({s1_posterior_superior[1]}, {s1_posterior_superior[2]})\n \n centroid_df <- tibble(spine_point = c('{spine_point_labels}'), x = c({x_values}), y = c({y_values}))")
  })
  
  
  alignment_parameters_reactivevalues_list <- reactiveValues()
  
  actively_computing_parameters_reactive_list <- reactiveValues(alignment_df = tibble())
  
  observeEvent(list(click_coord_reactive_list$coords, spine_orientation()), ignoreInit = TRUE, {
    if(length(click_coord_reactive_list$coords) > 2){
      fem_head_center <- c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y)
      
      s1_post_sup_corner <- c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y)
      s1_ant_sup_corner <- c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y)
      s1_midpoint <- jh_get_point_along_line_function(coord_a = s1_ant_sup_corner, 
                                                      coord_b = s1_post_sup_corner, 
                                                      percent_a_to_b = 0.5)
      
      print(paste("s1_posterior_sup: ", toString(s1_post_sup_corner),
                  "\n s1_anterior_sup: ", toString(s1_ant_sup_corner)))
      
      inf_sacrum_vec <- jh_find_sacrum_inf_point_function(s1_posterior_sup = s1_post_sup_corner, 
                                                          s1_anterior_sup = s1_ant_sup_corner, 
                                                          spine_facing = spine_orientation())
      
      # inf_sacrum_vec <- jh_find_sacrum_inf_point_function(s1_posterior_sup = s1_post_sup_corner,
      #                                                     s1_midpoint = s1_midpoint, 
      #                                                     femoral_heads_center = fem_head_center,
      #                                                     spine_facing = spine_orientation())
      
      # print(paste("S1 midpoint: ", toString(s1_midpoint),
      #             "\n inf_sacrum: ", toString(inf_sacrum_vec), 
      #             "\n fem_head center: ", toString(fem_head_center),
      #             "\n spine orientation: ", spine_orientation()))
      
      pelvic_incidence_value <- jh_calculate_vertex_angle(vertex_coord = s1_midpoint, 
                                         posterior_point_coord = inf_sacrum_vec, 
                                         ventral_point_coord = fem_head_center,
                                         spine_orientation = spine_orientation())
      
      pelvic_tilt_value <- jh_calculate_vertex_angle(vertex_coord = fem_head_center, 
                                                          posterior_point_coord = s1_midpoint, 
                                                          ventral_point_coord = c(fem_head_center[1], s1_midpoint[2]),
                                                          spine_orientation = spine_orientation())
      
      actively_computing_parameters_reactive_list$alignment_df <- tibble(measure = c("PI", "PT"), value = c(pelvic_incidence_value, pelvic_tilt_value))
      
      vert_body_coordinates_df <- click_coordinates_df_reactive() %>%
        filter(str_detect(spine_point, "centroid")) 
      
      if(nrow(vert_body_coordinates_df)>0){
        v_tilt_df <- vert_body_coordinates_df %>%
          mutate(v_tilt = map2(.x = x, .y = y, 
                            .f = ~ jh_calculate_vertex_angle(posterior_point_coord = c(.x, .y),
                                                             ventral_point_coord = c(fem_head_center[1], .y),
                                                             vertex_coord = fem_head_center,
                                                             spine_orientation = spine_orientation()
                            )
          )) %>%
          unnest() 
        
        if(str_to_lower(spine_orientation()) == "left"){
          vpa_df <- v_tilt_df %>%
            mutate(v_tilt = if_else(x < fem_head_center[1], abs(v_tilt), abs(v_tilt)*-1)) %>%
            mutate(value = pelvic_tilt_value + v_tilt) %>%
            mutate(measure = str_to_upper(str_replace_all(spine_point, "_centroid", "pa"))) %>%
            select(measure, value)
        }else{
          vpa_df <- v_tilt_df %>%
            mutate(v_tilt = if_else(x < fem_head_center[1], abs(v_tilt)*-1, abs(v_tilt)))%>%
            mutate(value = pelvic_tilt_value + v_tilt) %>%
            mutate(measure = str_to_upper(str_replace_all(spine_point, "_centroid", "pa"))) %>%
            select(measure, value)
        }
        
        actively_computing_parameters_reactive_list$alignment_df <- actively_computing_parameters_reactive_list$alignment_df %>%
          union_all(vpa_df)
        
        
      }
        

    }
    
  })
  
  # observeEvent(click_coord_reactive_list$coords, ignoreInit = TRUE, {
  #   
  #   click_count <- length(click_coord_reactive_list$coords)
  #   # if(nrow(actively_computing_parameters_reactive_list$alignment_df)>0){
  #   if(click_count > 2 & nrow(actively_computing_parameters_reactive_list$alignment_df)>0){
  #     if(nrow(imported_redcap_data$cleaned_df)>0 & any(names(imported_redcap_data$cleaned_df) == "value")){
  # 
  #       data_unformatted <- actively_computing_parameters_reactive_list$alignment_df %>%
  #         left_join(imported_redcap_data$cleaned_df %>%
  #                     mutate(redcap_val = value) %>%
  #                     select(measure, redcap_val)) %>%
  #         mutate(error = round(value - redcap_val, 2))
  # 
  #       pi_error_df <- data_unformatted %>%
  #         filter(measure == "PI") %>%
  #         filter(abs(error) > 1.5)
  # 
  #       if(nrow(pi_error_df)>0){
  #         recorded_pi <- (data_unformatted %>% filter(measure == "PI"))$redcap_val
  # 
  #         pelvic_thickness <- exportRecordsTyped(rcon = rcon_reactive$rcon, records = input$redcap_record_id,
  #                                         fields = "pelvic_thickness")%>%
  #           as_tibble() %>%
  #           type.convert()
  # 
  #         # fem_head_center <- c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y)
  #         s1_post_sup_corner <- c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y)
  #         s1_ant_sup_corner <- c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y)
  #         s1_midpoint <- jh_get_point_along_line_function(coord_a = s1_ant_sup_corner,
  #                                                         coord_b = s1_post_sup_corner,
  #                                                         percent_a_to_b = 0.5)
  # 
  #         inf_sacrum_vec <- jh_find_sacrum_inf_point_function(s1_posterior_sup = s1_post_sup_corner,
  #                                           s1_anterior_sup = s1_ant_sup_corner,
  #                                           spine_facing = spine_orientation())
  # 
  # 
  #         new_fem_head_coordinates <- jh_find_fem_head_center_given_pi_and_thickness_function(inf_sacrum = inf_sacrum_vec,
  #                                                                 s1_center = s1_midpoint,
  #                                                                 length_s1_fem = pelvic_thickness$pelvic_thickness,
  #                                                                 angle_deg = recorded_pi)
  # 
  #         click_coord_reactive_list$coords$fem_head_center$x <- new_fem_head_coordinates[1]
  #         click_coord_reactive_list$coords$fem_head_center$y <- new_fem_head_coordinates[2]
  # 
  #       }
  # 
  # 
  #     }
  #   }
  # })
  
  
  measurement_error_reactivevalues <- reactiveValues(error_over_2 = TRUE, 
                                                     error_df = tibble(),
                                                     pelvic_thickness_error_value = 99
                                                     )
  
  
  
  observe({
    if(nrow(actively_computing_parameters_reactive_list$alignment_df)>0){
      
      if(nrow(imported_redcap_data$cleaned_df)>0 & any(names(imported_redcap_data$cleaned_df) == "value")){
        
        data_unformatted <- actively_computing_parameters_reactive_list$alignment_df %>%
          left_join(imported_redcap_data$cleaned_df %>%
                      mutate(redcap_val = value) %>%
                      select(measure, redcap_val)) %>%
          mutate(error = round(value - redcap_val, 2)) %>%
          select('Par' = measure, val = value, rec_val = redcap_val, error) 
        
        if(any(abs(data_unformatted$error) > 2)){
          measurement_error_reactivevalues$error_over_2 <- TRUE
        }else{
          measurement_error_reactivevalues$error_over_2 <- FALSE
        }
        
        threshold <- 2
        
        # Apply formatting to the 'error' column based on the value
        data_unformatted$formatted_error <- ifelse(abs(data_unformatted$error) > threshold,
                                                   paste0('<span style="color:red; font-weight:bold;">', data_unformatted$error, '</span>'),
                                                   as.character(data_unformatted$error))
        
        # Remove original 'error' column and replace with formatted column
        error_formatted_red_df <- data_unformatted
        error_formatted_red_df$error <- error_formatted_red_df$formatted_error
        error_formatted_red_df$formatted_error <- NULL
        # error_formatted_red_df
        measurement_error_reactivevalues$error_df <- error_formatted_red_df
      }
    }
    
  })
  
  observeEvent(input$c2_centroid_s1_center_length, ignoreInit = TRUE, {
    if(imported_redcap_data$pelvic_thickness_value_for_qc_check != 0 & as.double(input$c2_centroid_s1_center_length) > 250 & input$all_points_recorded){

      c2_sacrum_xy_pixel_dist <- xray_centroid_coordinates_reactive_df() %>%
        filter(spine_point %in% c("s1_center", "c2_centroid")) %>%
        mutate(point_coord = map2(.x = x, .y = y, .f = ~ c(.x, .y))) %>%
        select(spine_point, point_coord) %>%
        pivot_wider(names_from = spine_point, values_from = point_coord) %>%
        mutate(c2_sacrum_xy_dist = map2(.x = s1_center, .y = c2_centroid,
                                        .f = ~ jh_calculate_distance_between_2_points_function(point_1 = .x, point_2 = .y))) %>%
        unnest(c2_sacrum_xy_dist) %>%
        pull(c2_sacrum_xy_dist)

      scaling_factor <-  as.double(input$c2_centroid_s1_center_length)/c2_sacrum_xy_pixel_dist

      fem_head_center_scaled <- c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y)*scaling_factor

      s1_center_scaled <- c((click_coord_reactive_list$coords$s1_anterior_superior$x + click_coord_reactive_list$coords$s1_posterior_superior$x)/2,
                     (click_coord_reactive_list$coords$s1_anterior_superior$y + click_coord_reactive_list$coords$s1_posterior_superior$y)/2)*scaling_factor

      pelvic_thickness_scaled_computed <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center_scaled, point_2 = s1_center_scaled)

      measurement_error_reactivevalues$pelvic_thickness_error_value <- round(pelvic_thickness_scaled_computed - imported_redcap_data$pelvic_thickness_value_for_qc_check, 1)

    }else{
      measurement_error_reactivevalues$pelvic_thickness_error_value <- 99
    }

  })
  
  
  output$spine_click_parameters <- renderTable({
    measurement_error_reactivevalues$error_df
  }, sanitize.text.function = function(x) x)
  
  observeEvent(list(input$xray_click,
                    spine_orientation()), ignoreInit = TRUE, {

                      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
                      
                      if((any(names(alignment_parameters_list) == "pelvic_incidence") == FALSE) & any(xray_centroid_coordinates_reactive_df()$spine_point == "s1_center")){
                        fem_head_center <- c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y)
                        
                        s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior$x + click_coord_reactive_list$coords$s1_posterior_superior$x)/2,
                                                       (click_coord_reactive_list$coords$s1_anterior_superior$y + click_coord_reactive_list$coords$s1_posterior_superior$y)/2)
                        
                        #   ### COMPUTE PT ###
                          fem_head_to_s1_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
                                                                                                   point_2 = s1_center) ## hypotenuse

                          fem_head_to_s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
                                                                                                     point_2 = c(s1_center[[1]], fem_head_center[[2]])) ## opposite
                        
                            pt_orientation_modifier <- case_when(
                              spine_orientation() == "left" & fem_head_center[[1]] < s1_center[[1]] ~ 1,
                              spine_orientation() == "left" & fem_head_center[[1]] > s1_center[[1]] ~ -1,
                              spine_orientation() == "right" & fem_head_center[[1]] > s1_center[[1]] ~ 1,
                              spine_orientation() == "right" & fem_head_center[[1]] < s1_center[[1]] ~ -1
                            )

                            alignment_parameters_reactivevalues_list$pelvic_tilt <- asin(fem_head_to_s1_x_length/fem_head_to_s1_length)*180/pi*pt_orientation_modifier
                          
                              ### COMPUTE SS ###
                              s1_length <- jh_calculate_distance_between_2_points_function(point_1 = c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y),
                                                                                           point_2 = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y))

                              s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = c(click_coord_reactive_list$coords$s1_anterior_superior$x,
                                                                                                         click_coord_reactive_list$coords$s1_posterior_superior$y),
                                                                                             point_2 = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y))

                              alignment_parameters_reactivevalues_list$sacral_slope <- acos(s1_x_length/s1_length)*180/pi

                              ### COMPUTE PI ###
                              alignment_parameters_reactivevalues_list$pelvic_incidence <- alignment_parameters_reactivevalues_list$pelvic_tilt + alignment_parameters_reactivevalues_list$sacral_slope
                            
                      }
                      
                      ## COMPUTE ALL VPAs ##
                      if(any(xray_centroid_coordinates_reactive_df()$spine_point == "c2_centroid")){
                        s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior$x + click_coord_reactive_list$coords$s1_posterior_superior$x)/2,
                                       (click_coord_reactive_list$coords$s1_anterior_superior$y + click_coord_reactive_list$coords$s1_posterior_superior$y)/2)

                        vpa_df <- xray_centroid_coordinates_reactive_df() %>%
                          filter(spine_point != "s1_center") %>%
                          mutate(vpa = map2(.x = x, .y = y, 
                                            .f = ~ jh_calculate_vertex_angle(posterior_point_coord = s1_center,
                                                                             ventral_point_coord = c(.x, .y),
                                                                             vertex_coord = c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y),
                                                                             spine_orientation = spine_orientation()
                                            )
                          )) %>%
                          # mutate(vpa = map2(.x = x, .y = y, .f = ~ jh_compute_vpa_from_xray_data_function(fem_head_center = c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y),
                          #                                                                                 vertebral_centroid = c(.x, .y),
                          #                                                                                 spine_facing = spine_orientation(),
                          #                                                                                 pelvic_tilt = alignment_parameters_reactivevalues_list$pelvic_tilt
                          # )
                          # )
                          # ) %>%
                          unnest() %>%
                          mutate(vpa_label = str_replace_all(spine_point, "_centroid", "pa")) %>%
                          select(vpa_label, vpa)

                        vpa_list <- as.list(vpa_df$vpa)
                        names(vpa_list) <- vpa_df$vpa_label

                        for (name in names(vpa_list)) {
                          alignment_parameters_reactivevalues_list[[name]] <- vpa_list[[name]]
                        }
                      }

                    }
  )

  
  observeEvent(spine_orientation(), ignoreInit = TRUE, {
                      
                      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
                      
                      if((any(names(alignment_parameters_list) == "pelvic_incidence") == TRUE)){
                        fem_head_center <- c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y)
                        
                        s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior$x + click_coord_reactive_list$coords$s1_posterior_superior$x)/2,
                                       (click_coord_reactive_list$coords$s1_anterior_superior$y + click_coord_reactive_list$coords$s1_posterior_superior$y)/2)
                        
                        #   ### COMPUTE PT ###
                        fem_head_to_s1_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
                                                                                                 point_2 = s1_center) ## hypotenuse
                        
                        fem_head_to_s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
                                                                                                   point_2 = c(s1_center[[1]], fem_head_center[[2]])) ## opposite
                        
                        pt_orientation_modifier <- case_when(
                          spine_orientation() == "left" & fem_head_center[[1]] < s1_center[[1]] ~ 1,
                          spine_orientation() == "left" & fem_head_center[[1]] > s1_center[[1]] ~ -1,
                          spine_orientation() == "right" & fem_head_center[[1]] > s1_center[[1]] ~ 1,
                          spine_orientation() == "right" & fem_head_center[[1]] < s1_center[[1]] ~ -1
                        )
                        
                        alignment_parameters_reactivevalues_list$pelvic_tilt <- asin(fem_head_to_s1_x_length/fem_head_to_s1_length)*180/pi*pt_orientation_modifier
                        
                        ### COMPUTE SS ###
                        s1_length <- jh_calculate_distance_between_2_points_function(point_1 = c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y),
                                                                                     point_2 = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y))
                        
                        s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = c(click_coord_reactive_list$coords$s1_anterior_superior$x,
                                                                                                   click_coord_reactive_list$coords$s1_posterior_superior$y),
                                                                                       point_2 = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y))
                        
                        alignment_parameters_reactivevalues_list$sacral_slope <- acos(s1_x_length/s1_length)*180/pi
                        
                        ### COMPUTE PI ###
                        alignment_parameters_reactivevalues_list$pelvic_incidence <- alignment_parameters_reactivevalues_list$pelvic_tilt + alignment_parameters_reactivevalues_list$sacral_slope
                        
                      }
                      
                      ## COMPUTE ALL VPAs ##
                      if(any(xray_centroid_coordinates_reactive_df()$spine_point == "c2_centroid")){
                        s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior$x + click_coord_reactive_list$coords$s1_posterior_superior$x)/2,
                                       (click_coord_reactive_list$coords$s1_anterior_superior$y + click_coord_reactive_list$coords$s1_posterior_superior$y)/2)
                        
                        vpa_df <- xray_centroid_coordinates_reactive_df() %>%
                          filter(spine_point != "s1_center") %>%
                          mutate(vpa = map2(.x = x, .y = y, 
                                            .f = ~ jh_calculate_vertex_angle(posterior_point_coord = s1_center,
                                                                             ventral_point_coord = c(.x, .y),
                                                                             vertex_coord = c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y),
                                                                             spine_orientation = spine_orientation()
                                            )
                          ))%>%
                          # mutate(vpa = map2(.x = x, .y = y, .f = ~ jh_compute_vpa_from_xray_data_function(fem_head_center = c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y),
                          #                                                                                 vertebral_centroid = c(.x, .y),
                          #                                                                                 spine_facing = spine_orientation(),
                          #                                                                                 pelvic_tilt = alignment_parameters_reactivevalues_list$pelvic_tilt
                          # )
                          # )
                          # ) %>%
                          unnest() %>%
                          mutate(vpa_label = str_replace_all(spine_point, "_centroid", "pa")) %>%
                          select(vpa_label, vpa)
                        
                        vpa_list <- as.list(vpa_df$vpa)
                        names(vpa_list) <- vpa_df$vpa_label
                        
                        for (name in names(vpa_list)) {
                          alignment_parameters_reactivevalues_list[[name]] <- vpa_list[[name]]
                        }
                      }
                      
                    }
  )
  
  # output$alignment_parameters_df_text <- renderText({
  #   
  #   glue_collapse(reactiveValuesToList(alignment_parameters_reactivevalues_list), sep = " \n")
  #   
  # })
  
  output$alignment_parameters_df <- renderTable({

    enframe(reactiveValuesToList(alignment_parameters_reactivevalues_list)) %>%
      mutate(name = str_replace_all(name, "pelvic_tilt", "PT")) %>%
      mutate(name = str_replace_all(name, "pelvic_incidence", "PI")) %>%
      mutate(name = str_replace_all(name, "sacral_slope", "SS")) %>%
      mutate(name = str_to_upper(name))

  })
  
  observeEvent(xray_instructions_reactiveval(), ignoreInit = TRUE, {
    if(xray_instructions_reactiveval() == "Completed"){
    updateSwitchInput(session = session, inputId = "all_points_recorded", value = TRUE)
    }else{
      updateSwitchInput(session = session, inputId = "all_points_recorded", value = FALSE)
      }
  })
  

  
  
  observeEvent(list(xray_instructions_reactiveval(), input$calibrate_button), ignoreInit = TRUE, {
    c2_centroid_s1_center_length_modal_function <- function(c2_centroid_s1_center_length = 0){
      modalDialog(footer = "Redcap Upload", easyClose = TRUE,  size = "l",  
                  box(width = 12, title = "Upload Data to Redcap", footer = NULL, 
                      fluidRow(
                        textInput(inputId = "c2_centroid_s1_center_length", 
                                  label = "Enter C2-centroid to S1 center Length:",
                                  value = c2_centroid_s1_center_length)
                      )
                  )
      )
    }
    
      if(input$record_calibration_measure){
        showModal(
          c2_centroid_s1_center_length_modal_function(c2_centroid_s1_center_length = input$c2_centroid_s1_center_length)
        ) 
      }
  })

  spine_build_list_reactivevalues <- reactiveValues(spine_build_list = list())
  
  # spine_coordinates_reactivevalues <- reactiveValues(spine_build_list = list())
  
  observeEvent(xray_instructions_reactiveval(), ignoreInit = TRUE, {
    spine_build_list <- list()
    # if(any(names(click_coord_reactive_list$coords) == "c2_centroid")){
    if(xray_instructions_reactiveval() == "Completed"){
      spine_build_list <- jh_build_spine_from_coordinates_function(femoral_head_center = c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y), 
                                                                   s1_anterior_superior = c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y), 
                                                                   s1_posterior_superior = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y), 
                                                                   centroid_df = xray_centroid_coordinates_reactive_df(), 
                                                                   spine_facing = spine_orientation())
      
      # spine_build_list$spine_coord_df <- bind_rows(map(.x = names(spine_build_list$vert_coord_list), 
      #                                                                                  .f = ~ jh_extract_coord_from_vert_coord_list_function(vert_coord_level_list = spine_build_list$vert_coord_list[[.x]]) %>%
      #                                                                                    mutate(spine_level = .x)))%>%
      #   select(spine_level, vert_point, x, y) 
      # spine_build_list$spine_coord_df <-   jh_extract_coord_from_vert_coord_list_function(vert_coord_level_list = spine_build_list$vert_coord_list)
      
    }
    
    spine_build_list_reactivevalues$spine_build_list <- spine_build_list
  })
  
  # observeEvent(xray_instructions_reactiveval(), ignoreInit = TRUE, {
  #   if(xray_instructions_reactiveval() == "Completed"& length(names(spine_build_list_reactivevalues$spine_build_list)>0)){
  #     spine_build_list_reactivevalues$spine_build_list$spine_coord_df <- bind_rows(map(.x = names(spine_build_list_reactivevalues$spine_build_list$vert_coord_list), 
  #                                     .f = ~ jh_extract_coord_from_vert_coord_list_function(vert_coord_level_list = spine_build_list_reactivevalues$spine_build_list$vert_coord_list[[.x]]) %>%
  #                                       mutate(spine_level = .x)))%>%
  #       select(spine_level, vert_point, x, y) 
  #     
  #   }
  #   
  # })
  
  # spine_build_from_coordinates_reactive <- reactive({
  # 
  #   
  # })

  
  xray_reactive_plot <- reactive({
    spine_xr_build_list <-  spine_build_list_reactivevalues$spine_build_list
    
    if(xray_instructions_reactiveval() == "Completed"){

      xray <- image_scale(image_read(path = input$image$datapath), "400x")
      
      xray_height <- image_info(xray)$height
      xray_width <- image_info(xray)$width
      
      xlim_left <-0.5 - (xray_width/xray_height)/2
      xlim_right <-0.5 + (xray_width/xray_height)/2
      
      spine_colors_df <- xray_centroid_coordinates_reactive_df() %>%
        mutate(spine_point = str_remove_all(spine_point, "_centroid|_center")) %>%
        filter(spine_point != "s1") %>%
        mutate(spine_color = case_when(
          str_detect(spine_point, "c") ~ "lightblue",
          str_detect(spine_point, "t") ~ "lightgreen",
          str_detect(spine_point, "l") ~ "darkblue"
        )
        )
      
      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
      
      s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior$x + click_coord_reactive_list$coords$s1_posterior_superior$x)/2,
                     (click_coord_reactive_list$coords$s1_anterior_superior$y + click_coord_reactive_list$coords$s1_posterior_superior$y)/2)
      
      pi_df <- calculate_pelvic_incidence_line_coordinates(fem_head_center = c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y),
                                                           s1_anterior = c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y),
                                                           s1_posterior = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y),
                                                           spine_facing = spine_orientation(),
                                                           pelvic_tilt = alignment_parameters_list$pelvic_tilt,
                                                           pelvic_incidence_value = alignment_parameters_list$pelvic_incidence
      )
      
      spine_coordinates_df <- tibble(spine_point = "s1_center",
                                     x = s1_center[[1]],
                                     y = s1_center[[2]]) %>%
        union_all(click_coordinates_df_reactive()) %>%
        filter(spine_point %in% c("fem_head_center", "s1_anterior_superior", "s1_posterior_superior") == FALSE)
      
      l1pa_df <- tibble(x = c(s1_center[[1]],
                              click_coord_reactive_list$coords$fem_head_center$x,
                              click_coord_reactive_list$coords$l1_centroid$x),
                        y = c(s1_center[[2]],
                              click_coord_reactive_list$coords$fem_head_center$y,
                              click_coord_reactive_list$coords$l1_centroid$y))
      
      t4pa_df <- tibble(x = c(s1_center[[1]],
                              click_coord_reactive_list$coords$fem_head_center$x,
                              click_coord_reactive_list$coords$t4_centroid$x),
                        y = c(s1_center[[2]],
                              click_coord_reactive_list$coords$fem_head_center$y,
                              click_coord_reactive_list$coords$t4_centroid$y))
      


      if(length(spine_build_list_reactivevalues$spine_build_list)>0){
        
      
        # spine_build_list_reactivevalues$spine_build_list$spine_coord_df

        inf_endplates_df <- spine_build_list_reactivevalues$spine_build_list$spine_coord_df %>%
          filter(vert_point %in% c("ia", "ip"))
        
        sup_endplates_df <- spine_build_list_reactivevalues$spine_build_list$spine_coord_df %>%
          filter(vert_point %in% c("sa", "sp"))
        
        # sup_endplates_df <- spine_xr_build_list$aligned_vert_geometry_df %>%
        #   select(spine_level, vert_coord_df) %>%
        #   unnest() %>%
        #   filter(vert_point %in% c("sa", "sp")) 
      }else{
        inf_endplates_df <- tibble(spine_level = c(), x = c(), y = c())
        sup_endplates_df <- tibble(spine_level = c(), x = c(), y = c())
      }

      

      # xray_plot <-  ggdraw(xlim = c(0, xray_width), ylim = c(0, xray_height)) +
      xray_plot <- ggdraw() +
        draw_image(
          scale = xray_height,
                   xray,
                   x = 0,
                   halign = 0, 
                   valign = 0,
                   y = 0,
                   width = 1,
                   # height = 1,
                   clip = FALSE
        ) +
        geom_path(data = xray_centroid_coordinates_reactive_df(),
                  aes(x = x, y = y), color = "lightblue", size = 0.2) +
        geom_point(data = spine_colors_df,
                   aes(x = x, y = y, color = spine_color, fill = spine_color),size = 0.2) +
        scale_fill_identity() +
        scale_color_identity()+
        geom_path(data = pi_df,
                  aes(x = x, y = y), color = "darkgreen", size = 0.25)+
        # geom_sf(data = spine_build_list_reactivevalues$spine_build_list$lines_list$l1pa, linewidth = 0.5, color = "darkblue") +
        # geom_sf(data = spine_build_list_reactivevalues$spine_build_list$lines_list$t4pa, linewidth = 0.5, color = "darkblue") +
        # geom_sf(data = spine_build_list_reactivevalues$spine_build_list$lines_list$l1pa, linewidth = 0.5, color = "darkblue") +   THESE WILL NOT BE UPDATED
        
        geom_path(data = l1pa_df,
                  aes(x = x, y = y),
                  color = "darkblue", size = 0.25)+
        geom_path(data = t4pa_df,
                  aes(x = x, y = y),
                  color = "purple", size = 0.25) +
        coord_fixed(xlim = c(0, xray_width), ylim = c(0, xray_height))
      
      if(nrow(sup_endplates_df)>1){
        xray_plot <- xray_plot +
        geom_line(data = sup_endplates_df, aes(x = x, y = y, group = spine_level), color = "red", size = 0.2) +
        geom_line(data = inf_endplates_df, aes(x = x, y = y, group = spine_level), color = "green", size = 0.2) 
      }
      
      xray_plot
      
    }
    
   
    
    
  })
  
  # output$xray_plot_complete <- renderPlot({
  #   xray_reactive_plot()
  # }
  # )
  
  
  observeEvent(input$xray_plot_click, {
    spine_xr_build_list <-  spine_build_list_reactivevalues$spine_build_list
    
    # Assume these are the clicked coordinates from the plot
    clicked_x <- input$xray_plot_click$x
    clicked_y <- input$xray_plot_click$y

    
    if(length(spine_xr_build_list)>0){

      nearest_point <- spine_xr_build_list$spine_coord_df %>%
        rowwise() %>%
        mutate(distance = sqrt((x - clicked_x)^2 + (y - clicked_y)^2)) %>%
        ungroup() %>%
        arrange(distance) %>%
        slice(1) 
      
      spine_level_to_mod <- nearest_point$spine_level[[1]]
      spine_point_to_mod <- nearest_point$vert_point[[1]]
      # if(nrow())
      spine_xr_build_list$vert_coord_list[[spine_level_to_mod]][[spine_point_to_mod]] <- c(clicked_x,clicked_y)
      
      # spine_build_list_reactivevalues$spine_build_list$vert_coord_list[[nearest_point$spine_level[[1]]]][[nearest_point$vert_point[[1]]]] <- c(nearest_point$x[[1]], nearest_point$y[[1]])
      
      new_geom <- jh_construct_vert_polygon_from_coordinates_list_function(vert_list = spine_xr_build_list$vert_coord_list[[spine_level_to_mod]], 
                                                                           buffer_amount = spine_build_list_reactivevalues$spine_build_list$buffer_amount)
      
      spine_xr_build_list$vert_geom_list[[spine_level_to_mod]] <- new_geom 
      spine_build_list_reactivevalues$spine_build_list <- spine_xr_build_list
      
      new_vert_point_coord_df <- tibble(spine_level = spine_level_to_mod, vert_point = spine_point_to_mod, x = clicked_x, y = clicked_y)
      
      spine_build_list_reactivevalues$spine_build_list$spine_coord_df <- spine_build_list_reactivevalues$spine_build_list$spine_coord_df %>% 
        rows_upsert(new_vert_point_coord_df, by = c("spine_level", "vert_point"))
      
    }else{
      print(names(spine_build_list), "Reactive values to list results with names: names(reactiveValuesToList(spine_build_list_reactivevalues))", names(reactiveValuesToList(spine_build_list_reactivevalues)))
    }
    
  })
  
  
  output$xray_plot_click_coordinates <- renderTable({
    spine_xr_build_list <-  spine_build_list_reactivevalues$spine_build_list
    # print(paste0(input$xray_plot_click))
    # coord_clicked <- paste("X:", click_coords$x, "Y:", click_coords$y)
    
    xray_click_tibble <- tibble(contents_of_list = names(spine_xr_build_list))
     
    if(length(spine_xr_build_list)>0){
      # Assume these are the clicked coordinates from the plot

      # spine_coord_df <- tibble(spine_level = c("s1", "s1"),
      #                          vert_point = c("sa", "sp"),
      #                          x = c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$x),
      #                          y = c(click_coord_reactive_list$coords$s1_anterior_superior$y, click_coord_reactive_list$coords$s1_posterior_superior$y),
      # ) %>%
      #   union_all(spine_xr_build_list$aligned_vert_geometry_df %>%
      #               select(spine_level, vert_coord_df) %>%
      #               unnest(vert_coord_df)) %>%
      #   # mutate(spine_point = paste(str_to_upper(spine_level), str_to_upper(vert_point))) %>%
      #   select(spine_level, vert_point, x, y)

      clicked_x <- input$xray_plot_click$x
      clicked_y <- input$xray_plot_click$y

      nearest_point <- spine_xr_build_list$spine_coord_df %>%
        rowwise() %>%
        mutate(distance = sqrt((x - clicked_x)^2 + (y - clicked_y)^2)) %>%
        ungroup() %>%
        arrange(distance) %>%
        slice(1)

      xray_click_tibble <- nearest_point %>%
        select(spine_level, vert_point, x, y) %>%
        mutate(x_click = clicked_x,
               y_click = clicked_y) %>%
        pivot_longer(cols = c(x, y, x_click, y_click), names_to = "coord", values_to = "val")
      # xray_click_tibble <- nearest_point
    }
    
    print(xray_click_tibble)
    xray_click_tibble
  }) %>%
    bindEvent(input$xray_plot_click)
  
  
    # observe({
    #   if(xray_instructions_reactiveval() == "Completed"){
    #     req(input$image)  # Ensure there's an image uploaded
    #     
    #     # Generate the plot
    #     plot <- xray_reactive_plot()
    #     
    #     # Save the plot as a temporary file
    #     temp_file <- tempfile(fileext = ".png")
    #     # ggsave(temp_file, plot = plot, width = 8, height = 10, dpi = 150)
    #     ggsave(temp_file, plot = plot, width = 350, height = 700, units = "px")
    #     
    #     # Read the image back and convert it to a base64 string for embedding
    #     img <- magick::image_read(temp_file)
    #     img_base64 <- base64enc::dataURI(file = temp_file, mime = "image/png")
    #     
    #     # Send the image to the UI
    #     # session$sendCustomMessage('load-plot-image', list(src = img_base64))
    #     session$sendCustomMessage('uploadedImage', list(src = img_base64)) 
    #   }
    # 
    # })

  
  
  output$preop_xray_rigid_segments_ui <- renderUI({
    # preop_segment_angles_list <- preop_segment_angles_list_reactive()
    
    segment_angles_input_list <- rev(map(spinal_segments_labels_vector, function(label) {
      create_spine_rigid_level_input_function(segment_input_label = label)
    }))
    # create_spine_rigid_level_input_function
    
    # column(width = 5,
    fluidRow(
      column(width = 12,
      h4("Select Any Fused Levels:"),
      box(width = 12,
          title = "Cervical", 
          collapsible = TRUE, 
          collapsed = TRUE,
          # h5("Check box if Rigid Level"),
          segment_angles_input_list[1:6] %>% tagList()
      ),
      box(width = 12,title = "Thoracic", 
          collapsible = TRUE, 
          collapsed = TRUE,
          # h5("Check box if Rigid Level"),
          segment_angles_input_list[7:19] %>% tagList()
      ),
      box(width = 12,title = "Lumbar", 
          collapsible = TRUE, 
          collapsed = FALSE,
          # h5("Check box if Rigid Level"),
          segment_angles_input_list[20:24]%>% tagList()
      )
    )
    )
  })
  
  preop_rigid_levels_vector_reactive_xray <- reactive({
    # segment_id <- paste0("preop_", str_to_lower(str_replace_all(segment_input_label, pattern = "-", "_")), "_rigid")
    rigid_levels <- c("na")
    if(input$preop_l5_s1_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L5-S1")} 
    if(input$preop_l4_l5_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L4-L5")} 
    if(input$preop_l3_l4_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L3-L4")} 
    if(input$preop_l2_l3_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L2-L3")} 
    if(input$preop_l1_l2_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L1-L2")} 
    if(input$preop_t12_l1_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T12-L1")} 
    if(input$preop_t11_t12_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T11-T12")} 
    if(input$preop_t10_t11_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T10-T11")} 
    if(input$preop_t9_t10_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T10-T9")} 
    if(input$preop_t8_t9_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T8-T9")} 
    if(input$preop_t7_t8_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T7-T8")} 
    if(input$preop_t6_t7_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T6-T7")} 
    if(input$preop_t5_t6_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T5-T6")} 
    if(input$preop_t4_t5_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T4-T5")} 
    if(input$preop_t3_t4_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T3-T4")} 
    if(input$preop_t2_t3_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T2-T3")} 
    if(input$preop_t1_t2_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T1-T2")} 
    if(input$preop_c7_t1_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C7-T1")} 
    if(input$preop_c6_c7_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C6-C7")} 
    if(input$preop_c5_c6_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C5-C6")} 
    if(input$preop_c4_c5_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C4-C5")} 
    if(input$preop_c3_c4_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C3-C4")} 
    if(input$preop_c2_c3_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C2-C3")} 
    if(input$preop_c1_c2_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C1-C2")} 
    # set_names(map_chr(spinal_segments_labels_vector, ~ paste0(str_to_lower(strsplit(.x, "-")[[1]][1]), "_segment_angle")))
    if(length(rigid_levels)>0){
      rigid_levels <- map_chr(rigid_levels, ~ paste0(str_to_lower(strsplit(.x, "-")[[1]][1]), "_segment"))
    }
    rigid_levels
  })
  


  
  output$preop_spine_simulation_plot <- renderPlot({
    spine_build_list <-  spine_build_list_reactivevalues$spine_build_list
    
    if(xray_instructions_reactiveval() == "Completed"){
      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
      

      if(length(spine_build_list)>0){
        ggplot() +
        geom_sf(data = st_sfc(spine_build_list$vert_geom_list), 
                color = "black",
                fill = "grey90", 
                alpha = 0.8) +
        # geom_sf(data = spine_build_list$vert_geom_list$c1, 
        #         fill = "grey90",
        #         alpha = 0.5) +
        geom_path(data =  spine_build_list$spine_coord_df %>%
                    filter(vert_point %in% c("center", "centroid")), aes(x = x, y = y)) +
        geom_sf(data = spine_build_list$fem_head_sf, 
                fill = "grey90") +
        # geom_sf(data = spine_build_list$sacrum_sf, 
        #         fill = "grey90", 
        #         alpha = 0.8) +
        geom_sf(data = spine_build_list$lines_list$l1pa, 
                color = "blue")+
        geom_sf(data = spine_build_list$lines_list$t9pa, 
                color = "orange") +
        geom_sf(data = spine_build_list$lines_list$t4pa, 
                color = "purple") +
        geom_sf(data = spine_build_list$lines_list$c2pa, 
                color = "darkgreen") +
        theme_void() +
        labs(title = "Preop Alignment") +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(
            size = 16,
            hjust = 0.5,
            vjust = -0.5,
            face = "bold.italic"
          ),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA)
        )
      }
      
       

      
      
    }
    
    # if(any(names(alignment_parameters_list) == "c2pa")){
      
      # if(any(names(click_coord_reactive_list$coords) == "c2_centroid")){
        
        # xray_centroid_coordinates_reactive_df()
        
        # spine_build_list <- jh_build_spine_from_coordinates_function(femoral_head_center = c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y), 
        #                                                  s1_anterior_superior = c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y), 
        #                                                  s1_posterior_superior = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y), 
        #                                                  centroid_df = xray_centroid_coordinates_reactive_df(), 
        #                                                  spine_facing = spine_orientation())
        
        
        # spine_simulation_list <- build_full_spine_from_vertebral_pelvic_angles_function(pelv_inc_value = alignment_parameters_list$pelvic_incidence,
        #                                                                                 pt_value = alignment_parameters_list$pelvic_tilt,
        #                                                                                 l1pa_value_input = alignment_parameters_list$l1pa,
        #                                                                                 # l1s1_value_input = alignment_parameters_list$preop_l1s1,
        #                                                                                 # t10_l2_value_input = alignment_parameters_list$preop_t10_l2,
        #                                                                                 t9pa_value_input = alignment_parameters_list$t9pa,
        #                                                                                 t4pa_value_input = alignment_parameters_list$t4pa,
        #                                                                                 c2pa_value_input = alignment_parameters_list$c2pa,
        #                                                                                 # c2_c7_value_input = alignment_parameters_list$preop_c2c7,
        #                                                                                 # input_segment_angles = "yes",
        #                                                                                 # segment_angles_input = segment_angles_list,
        #                                                                                 spine_faces = spine_orientation()
        # )
        # 
        # spine_geoms_df <- spine_simulation_list$spine_df %>%
        #   select(object, geom, geom_alpha)
        
        # measurements_df <- spine_simulation_list$measurements_df
        
        # ggplot() +
        #   geom_sf(data = spine_geoms_df,
        #           color = "black",
        #           aes(geometry = geom,
        #               alpha = geom_alpha,
        #               fill = "grey90")) +
        #   # ylim(-20, 110) +
        #   theme_void() +
        #   labs(title = "Preop Alignment") +
        #   theme(
        #     axis.text = element_blank(),
        #     axis.title = element_blank(),
        #     plot.title = element_text(
        #       size = 16,
        #       hjust = 0.5,
        #       vjust = -0.5,
        #       face = "bold.italic"
        #     ),
        #     plot.background = element_rect(fill = "transparent", colour = NA),
        #     panel.background = element_rect(fill = "transparent", colour = NA)
        #   ) +
        #   draw_text(text = spine_simulation_list$measurements_df$label,
        #             x = spine_simulation_list$measurements_df$x,
        #             y = spine_simulation_list$measurements_df$y, size = 11) +
        #   scale_fill_identity() +
        #   scale_alpha_identity()+
        #   xlim(-30, 30)
      # } 
      # coord_fixed()
      
    # }
    
    
  })
  
  
  
  spine_plan_lower_t_uiv_option_1_reactive <- eventReactive(input$compute_plan_xray, {
    alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)

    # spine_build_list <- spine_build_from_coordinates_reactive()
    spine_build_list <-  spine_build_list_reactivevalues$spine_build_list

    build_t11_spine_plot_function(pso_option_number = 1,
                                  preop_age = input$preop_age,
                                  preop_sex = input$preop_sex,
                                  preop_pelvic_incidence = alignment_parameters_list$pelvic_incidence,
                                  preop_pt = alignment_parameters_list$pelvic_tilt,
                                  preop_l1pa = alignment_parameters_list$l1pa,
                                  preop_t9pa = alignment_parameters_list$t9pa,
                                  preop_t4pa = alignment_parameters_list$t4pa,
                                  preop_c2pa = alignment_parameters_list$c2pa,
                                  # l1pa_line_color = input$l1pa_line_color,
                                  # t4pa_line_color = input$t4pa_line_color,
                                  # c2pa_line_color = input$c2pa_line_color,
                                  # preop_segment_angles_input_list_reactive = spine_build_list$segment_angles_list,
                                  preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive_xray(),
                                  return_list_or_plot = "list"
    )
  })


  spine_plan_lower_t_uiv_option_2_reactive <- eventReactive(input$compute_plan_xray, {
    alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)

    # spine_build_list <- spine_build_from_coordinates_reactive()
    spine_build_list <-  spine_build_list_reactivevalues$spine_build_list

    build_t11_spine_plot_function(pso_option_number = 2,
                                  preop_age = input$preop_age,
                                  preop_sex = input$preop_sex,
                                  preop_pelvic_incidence = alignment_parameters_list$pelvic_incidence,
                                  preop_pt = alignment_parameters_list$pelvic_tilt,
                                  preop_l1pa = alignment_parameters_list$l1pa,
                                  preop_t9pa = alignment_parameters_list$t9pa,
                                  preop_t4pa = alignment_parameters_list$t4pa,
                                  preop_c2pa = alignment_parameters_list$c2pa,
                                  # l1pa_line_color = input$l1pa_line_color,
                                  # t4pa_line_color = input$t4pa_line_color,
                                  # c2pa_line_color = input$c2pa_line_color,
                                  # preop_segment_angles_input_list_reactive = spine_build_list$segment_angles_list,
                                  preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive_xray(),
                                  return_list_or_plot = "list"
    )


  })
  
  output$spine_plan_lower_t_uiv_option_1_plot <- renderPlot({
    spine_plan_lower_t_uiv_option_1_reactive()$prescribed_plot_no_targets
  })
  
  output$spine_plan_lower_t_uiv_option_1_table <- renderTable({
    spine_plan_lower_t_uiv_option_1_reactive()$regional_targets %>%
      select("Parameter" = name, "Target" = value)

  })
  
  output$spine_plan_lower_t_uiv_option_1_table_2 <- renderTable({
    spine_plan_lower_t_uiv_option_1_reactive()$alignment_measures_df %>%
      filter(name %in% c("C2 Tilt", "C2PA", "T4PA", "PT"))%>%
      mutate(value = paste0(value, "º")) %>%
      select("Parameter" = name, "Expected" = value) 
  })
  

  
  output$spine_plan_lower_t_uiv_option_2_plot <- renderPlot({
    spine_plan_lower_t_uiv_option_2_reactive()$prescribed_plot_no_targets
  })
  
  output$spine_plan_lower_t_uiv_option_2_table <- renderTable({
    spine_plan_lower_t_uiv_option_2_reactive()$regional_targets %>%
      select("Parameter" = name, "Target" = value)

  })
  output$spine_plan_lower_t_uiv_option_2_table_2 <- renderTable({
    spine_plan_lower_t_uiv_option_2_reactive()$alignment_measures_df %>%
      filter(name %in% c("C2 Tilt", "C2PA", "T4PA", "PT"))%>%
      mutate(value = paste0(value, "º")) %>%
      select("Parameter" = name, "Expected" = value) 
  })

  
  output$spine_plan_lower_t_ui <-  renderUI({

    if(spine_plan_lower_t_uiv_option_1_reactive()$pso_level != "none"){
    
      fluidRow(
        column(width = 6, 
               plotOutput(outputId = "spine_plan_lower_t_uiv_option_1_plot"),
               h4("Regional Targets:"),
               tableOutput(outputId = "spine_plan_lower_t_uiv_option_1_table"),
               h4("Expected Global Alignment & Balance"),
               tableOutput(outputId = "spine_plan_lower_t_uiv_option_1_table_2")
        ),
        column(width = 6, 
               plotOutput(outputId = "spine_plan_lower_t_uiv_option_2_plot"),
               h4("Regional Targets:"),
               tableOutput(outputId = "spine_plan_lower_t_uiv_option_2_table"),
               h4("Expected Global Alignment & Balance"),
               tableOutput(outputId = "spine_plan_lower_t_uiv_option_2_table_2")
               )
      )
    }else{
      fluidRow(
        column(width = 6, 
               plotOutput(outputId = "spine_plan_lower_t_uiv_option_1_plot"),
               h4("Regional Targets:"),
               tableOutput(outputId = "spine_plan_lower_t_uiv_option_1_table"),
               h4("Expected Global Alignment & Balance"),
               tableOutput(outputId = "spine_plan_lower_t_uiv_option_1_table_2")
        )
        )
      }
    
  })
  
  
  
  
  ################
  spine_plan_upper_t_uiv_option_1_reactive <- eventReactive(input$compute_plan_xray, {
    alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
    
    # spine_build_list <- spine_build_from_coordinates_reactive()
    spine_build_list <-  spine_build_list_reactivevalues$spine_build_list
    
    build_upper_t_uiv_spine_plot_function(pso_option_number = 1,
                                  preop_age = input$preop_age,
                                  preop_sex = input$preop_sex,
                                  preop_pelvic_incidence = alignment_parameters_list$pelvic_incidence,
                                  preop_pt = alignment_parameters_list$pelvic_tilt,
                                  preop_l1pa = alignment_parameters_list$l1pa,
                                  preop_t9pa = alignment_parameters_list$t9pa,
                                  preop_t4pa = alignment_parameters_list$t4pa,
                                  preop_c2pa = alignment_parameters_list$c2pa,
                                  # l1pa_line_color = input$l1pa_line_color,
                                  # t4pa_line_color = input$t4pa_line_color,
                                  # c2pa_line_color = input$c2pa_line_color,
                                  # preop_segment_angles_input_list_reactive = spine_build_list$segment_angles_list,
                                  preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive_xray(),
                                  return_list_or_plot = "list"
    )
  })
  
  
  spine_plan_upper_t_uiv_option_2_reactive <- eventReactive(input$compute_plan_xray, {
    alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
    
    # spine_build_list <- spine_build_from_coordinates_reactive()
    spine_build_list <-  spine_build_list_reactivevalues$spine_build_list
    
    build_upper_t_uiv_spine_plot_function(pso_option_number = 2,
                                  preop_age = input$preop_age,
                                  preop_sex = input$preop_sex,
                                  preop_pelvic_incidence = alignment_parameters_list$pelvic_incidence,
                                  preop_pt = alignment_parameters_list$pelvic_tilt,
                                  preop_l1pa = alignment_parameters_list$l1pa,
                                  preop_t9pa = alignment_parameters_list$t9pa,
                                  preop_t4pa = alignment_parameters_list$t4pa,
                                  preop_c2pa = alignment_parameters_list$c2pa,
                                  # l1pa_line_color = input$l1pa_line_color,
                                  # t4pa_line_color = input$t4pa_line_color,
                                  # c2pa_line_color = input$c2pa_line_color,
                                  # preop_segment_angles_input_list_reactive = spine_build_list$segment_angles_list,
                                  preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive_xray(),
                                  return_list_or_plot = "list"
    )
    
    
  })
  
  output$spine_plan_upper_t_uiv_option_1_plot <- renderPlot({
    spine_plan_upper_t_uiv_option_1_reactive()$prescribed_plot_no_targets
  })
  
  output$spine_plan_upper_t_uiv_option_1_table <- renderTable({
    spine_plan_upper_t_uiv_option_1_reactive()$regional_targets %>%
      select("Parameter" = name, "Target" = value)

  })
  
  output$spine_plan_upper_t_uiv_option_1_table_2 <- renderTable({
    spine_plan_upper_t_uiv_option_1_reactive()$alignment_measures_df %>%
      filter(name %in% c("C2 Tilt", "C2PA", "PT"))%>%
      mutate(value = paste0(value, "º")) %>%
      select("Parameter" = name, "Expected" = value) 
  })
  
  
  output$spine_plan_upper_t_uiv_option_2_plot <- renderPlot({
    spine_plan_upper_t_uiv_option_2_reactive()$prescribed_plot_no_targets
  })
  
  output$spine_plan_upper_t_uiv_option_2_table <- renderTable({
    spine_plan_upper_t_uiv_option_2_reactive()$regional_targets %>%
      select("Parameter" = name, "Target" = value)
  })
  
  output$spine_plan_upper_t_uiv_option_2_table_2 <- renderTable({
    spine_plan_upper_t_uiv_option_2_reactive()$alignment_measures_df %>%
      filter(name %in% c("C2 Tilt", "C2PA", "PT"))%>%
      mutate(value = paste0(value, "º")) %>%
      select("Parameter" = name, "Expected" = value) 
  })
  
  
  output$spine_plan_upper_t_ui <-  renderUI({

    if(spine_plan_upper_t_uiv_option_1_reactive()$pso_level != "none"){
      
      fluidRow(
        column(width = 6, 
               plotOutput(outputId = "spine_plan_upper_t_uiv_option_1_plot"),
               h4("Regional Targets:"),
               tableOutput(outputId = "spine_plan_upper_t_uiv_option_1_table"),
               h4("Expected Global Alignment & Balance"),
               tableOutput(outputId = "spine_plan_upper_t_uiv_option_1_table_2")
        ),
        column(width = 6, 
               plotOutput(outputId = "spine_plan_upper_t_uiv_option_2_plot"),
               h4("Regional Targets:"),
               tableOutput(outputId = "spine_plan_upper_t_uiv_option_2_table"),
               h4("Expected Global Alignment & Balance"),
               tableOutput(outputId = "spine_plan_upper_t_uiv_option_2_table_2")
        )
      )
    }else{
      fluidRow(
        column(width = 6, 
               plotOutput(outputId = "spine_plan_upper_t_uiv_option_1_plot"),
               h4("Regional Targets:"),
               tableOutput(outputId = "spine_plan_upper_t_uiv_option_1_table"),
               h4("Expected Global Alignment & Balance"),
               tableOutput(outputId = "spine_plan_upper_t_uiv_option_1_table_2")
        )
      )
    }
    
  })
  
  

redcap_tables <- reactiveValues()
  
observeEvent(input$upload_to_redcap, ignoreInit = TRUE, {
  redcap_tables$spine_coordinates_df <-   tibble(spine_level = "femoral_heads", 
           vert_point = "center", 
           x = click_coord_reactive_list$coords$fem_head_center$x[1], 
           y = click_coord_reactive_list$coords$fem_head_center$y[1]) %>%
    union_all(spine_build_list_reactivevalues$spine_build_list$spine_coord_df)    %>%
    select(spine_level_coord = spine_level, vert_point_coord = vert_point, x_coord = x, y_coord = y) %>%
    mutate(redcap_repeat_instance = row_number()) %>%
    mutate(spine_coordinates_complete = "Complete")
  
  redcap_tables$spine_calibration <- tibble(c2_centroid_s1_center_length = input$c2_centroid_s1_center_length, 
                                            spine_calibration_complete = "Complete")
  
})
  

  output$spine_coord_output <- renderTable({
    
    if(measurement_error_reactivevalues$error_over_2){
      measurement_error_reactivevalues$error_df
    }else{
      if(length(spine_build_list_reactivevalues$spine_build_list)>1){
        redcap_tables$spine_coordinates_df 
      } 
    }
  }, sanitize.text.function = function(x) x)
  
  output$spine_coord_for_redcap_plot <- renderPlot({
    
    if(length(spine_build_list_reactivevalues$spine_build_list)>1){
      spine_coord_indexed_df <- spine_build_list_reactivevalues$spine_build_list$spine_coord_df %>%
        select(spine_level) %>%
        distinct() %>%
        mutate(spine_index = row_number()) %>%
        left_join(spine_build_list_reactivevalues$spine_build_list$spine_coord_df)%>%
        filter(vert_point %in% c("sp", "sa", "ia", "ip"))  %>%
        group_by(spine_level) %>%
        mutate(vert_point_index = row_number()) %>%
        ungroup()
      
      spine_coord_indexed_for_plotting_df <- spine_coord_indexed_df %>%
        filter(vert_point == "sp") %>%
        mutate(vert_point_index = 5) %>%
        union_all(spine_coord_indexed_df) %>%
        arrange(spine_index, vert_point_index) %>%
        select(spine_level, vert_point, x, y)
    
      spine_coord_indexed_for_plotting_df %>%
        group_by(spine_level) %>%
        ggplot() + 
        geom_path(aes(x = x, y = y, group = spine_level)) +
        coord_fixed() +
        theme_void() +
        labs(title = "Spine Coordinates")
    }
  })
  
  observeEvent(input$upload_to_redcap, {
    print(measurement_error_reactivevalues$error_over_2)
    if(measurement_error_reactivevalues$error_over_2){
      showModal(
        modalDialog(footer = "Redcap Upload", easyClose = TRUE,  size = "l",  
                    box(width = 12, title = "Error in Measurements", footer = NULL, 
                        fluidRow(
                          h4("Error in measurements below exceed acceptable value.\nPlease adjust centroids or restart."),
                          tableOutput(outputId = "spine_coord_output")
                        )
                    )
        )
      )
    }else if(abs(measurement_error_reactivevalues$pelvic_thickness_error_value)>10){
      showModal(
        modalDialog(footer = "Redcap Upload", easyClose = TRUE,  size = "l",  
                    box(width = 12, title = "Error in Calibration Value", footer = NULL, 
                        fluidRow(
                          h4("The recorded calibration measure is inconsistent with the redcap value."),
                          hr(),
                          h4(glue("Pelvic thickness (S1 center to fem heads) error based on current calibration = {measurement_error_reactivevalues$pelvic_thickness_error_value}mm")),
                          hr(),
                          h3("Double Check the calibration line is from C2 to S1 center, and is entered correctly.")
                        )
                    )
        )
      )
      
      }else{
      showModal(
        modalDialog(footer = "Redcap Upload", easyClose = TRUE,  size = "l",  
                    box(width = 12, title = "Upload Data to Redcap", footer = NULL, 
                        fluidRow(
                          column(6, 
                                 conditionalPanel(
                                   condition = "input.redcap_record_id.length > 0",  # JavaScript condition
                                   tableOutput(outputId = "spine_coord_output")
                                 )
                          ),
                          column(6, 
                                 conditionalPanel(
                                   condition = "input.redcap_record_id.length > 0",  # JavaScript condition
                                   plotOutput(outputId = "spine_coord_for_redcap_plot")
                                 )
                          )
                        ),
                        fluidRow(
                          conditionalPanel(
                            condition = "input.redcap_record_id.length > 0",  # JavaScript condition
                            actionBttn(inputId = "confirm_upload_final",
                                       label = "Confirmed, Upload to Redcap",
                                       style = "simple", color = "primary")
                          )
                        ),
                        br(),
                        textOutput(outputId = "redcap_upload_status"),
                        div(
                          style = "display: none;",  # Hide the entire div, including the switch
                          switchInput(
                            inputId = "redcap_token_correct",
                            size = "mini", label = NULL,
                            value = FALSE, 
                            onLabel = "Y", 
                            offLabel = "N",
                          )
                        )
                    )
        )
        
      ) 
    }
  })
  
  observeEvent(input$redcap_token_password, ignoreInit = TRUE, {
    
    if(str_to_lower(input$redcap_token_password) == "hills"){
      updateSwitchInput(session = session, inputId = "redcap_token_correct", value = TRUE)
    }
    
    
  })
  
  
  rcon_reactive <- reactiveValues(rcon = " ")
  
  # observeEvent(input$redcap_token_password, ignoreInit = TRUE, {
  #   if(str_to_lower(input$redcap_token_password) == "hills"){
  #     redcap_token <- "6DDC3E7AB46DC526ECF3F1A366A4A7DD"
  #     redcap_url <-  'https://redcap.uthscsa.edu/REDCap/api/'
  #     rcon_reactive$rcon <- redcapConnection(url = redcap_url, token = redcap_token, config =  httr::config(ssl_verifypeer = FALSE))     
  #   }
  # })
  observe({
    if(str_to_lower(input$redcap_token_password) == "hills"){
      redcap_token <- "6DDC3E7AB46DC526ECF3F1A366A4A7DD"
      redcap_url <-  'https://redcap.uthscsa.edu/REDCap/api/'
      rcon_reactive$rcon <- redcapConnection(url = redcap_url, token = redcap_token, config =  httr::config(ssl_verifypeer = FALSE))     
    }
  })
  
  next_redcap_record_id_needed_reactiveval <- reactiveValues(next_redcap_record = "", 
                                                             next_10_redcap_records = "")
  
  observeEvent(input$get_next_redcap_id_button, ignoreInit = TRUE, {
    
    last_completed_id <- exportRecordsTyped(rcon = rcon_reactive$rcon) %>%
      filter(spine_coordinates_complete == "Complete") %>%
      as_tibble() %>%
      select(record_id) %>%
      distinct() %>%
      mutate(record_id = as.character(record_id)) %>%
      tail(1) %>%
      pull(record_id)

    all_recs_df <- exportRecordsTyped(rcon = rcon_reactive$rcon, fields = "record_id")

    next_record_index <- which(all_recs_df$record_id == last_completed_id) +1

    next_redcap_record_id_needed_reactiveval$next_redcap_record <-  all_recs_df$record_id[next_record_index]
    
    next_10_records_indices <- which(all_recs_df$record_id == last_completed_id) + c(1:10)
    
    next_redcap_record_id_needed_reactiveval$next_10_redcap_records <-  all_recs_df$record_id[next_10_records_indices]
  })
  
  output$next_redcap_record_id_needed <- renderText({
    
    HTML(glue("<span style='text-align: center; font-size:20px; font-weight:bold; color:green; font-family:sans-serif; font-style:italic;'>{next_redcap_record_id_needed_reactiveval$next_redcap_record}</span>"))
    
  })
  
  output$next_10_redcap_record_id_needed <- renderText({
    
    # glue_collapse(next_redcap_record_id_needed_reactiveval$next_10_redcap_records, sep = "\n")
    
    HTML(glue("<span style='text-align: center; font-size:12px; font-weight:bold; color:green; font-family:sans-serif; font-style:italic;'>{glue_collapse(next_redcap_record_id_needed_reactiveval$next_10_redcap_records, sep = '\n')}</span>"))
    
  })
  
  imported_redcap_data <- reactiveValues(imported_data = tibble(), 
                                         pelvic_thickness_value_for_qc_check = 0)
  
  
  observeEvent(next_redcap_record_id_needed_reactiveval$next_redcap_record, ignoreInit = TRUE, {
    updateSearchInput(session = session, 
                      inputId = "redcap_record_id", 
                      value = next_redcap_record_id_needed_reactiveval$next_redcap_record
    )
  })
  
  observeEvent(list(input$get_redcap_data_button, input$search_for_retrieved_next_record_button), ignoreInit = TRUE, {
    id_length <- str_length(input$redcap_record_id)

    if(id_length > 0){
      redcap_record_id <- case_when(
        id_length == 1 ~ paste0("000", input$redcap_record_id),
        id_length == 2 ~ paste0("00", input$redcap_record_id),
        id_length == 3 ~ paste0("0", input$redcap_record_id),
        id_length == 4 ~ paste0(input$redcap_record_id)
      )

      record_redcap_export_df <- exportRecordsTyped(rcon = rcon_reactive$rcon, records = paste0(redcap_record_id)) %>%
        type.convert() %>%
        as_tibble()
      
      imported_redcap_data$imported_data <- record_redcap_export_df %>%
        select(record_id, pelvic_incidence, pelvic_tilt, contains("pelvic_angle")) %>%
        pivot_longer(cols = -record_id, names_to = "measure", values_to = "value") %>%
        filter(!is.na(value))%>%
        mutate(measure = str_replace_all(measure, "_pelvic_angle", "PA")) %>%
        mutate(measure = str_replace_all(measure, "pelvic_incidence", "PI")) %>%
        mutate(measure = str_replace_all(measure, "pelvic_tilt", "PT")) %>%
        mutate(measure = str_to_upper(measure)) %>%
        select(id = record_id, measure, value)
      
      imported_redcap_data$pelvic_thickness_value_for_qc_check <- record_redcap_export_df %>%
        pull(pelvic_thickness) %>%
        as.double()

    }

  })
  
  output$redcap_measures_table <- renderTable({
    if(nrow(imported_redcap_data$imported_data)>0){
      imported_redcap_data$cleaned_df <- imported_redcap_data$imported_data %>%
        filter(measure %in% c("PI", "PT")) %>%
        union_all(imported_redcap_data$imported_data %>%
                    filter(measure %in% c("PI", "PT") == FALSE) %>%
                    arrange(rev(row_number())))
      
      imported_redcap_data$cleaned_df
    }
  })
  
  
  observeEvent(input$confirm_upload_final, ignoreInit = TRUE, {
    
    if(redcapAPI::exportNextRecordName(rcon = rcon_reactive$rcon)>1){
      
      if(is.na(input$redcap_record_id)){
        redcap_record_id <- exportNextRecordName(rcon = rcon_reactive$rcon)
      }else{
        id_length <- str_length(input$redcap_record_id)
        
        redcap_record_id <- case_when(
          id_length == 1 ~ paste0("000", input$redcap_record_id), 
          id_length == 2 ~ paste0("00", input$redcap_record_id), 
          id_length == 3 ~ paste0("0", input$redcap_record_id), 
          id_length == 4 ~ paste0(input$redcap_record_id)
        ) 
      }
      
      redcap_upload_df <- redcap_tables$spine_coordinates_df %>%
        mutate(record_id = redcap_record_id, 
               redcap_repeat_instrument = "spine_coordinates")
      
      redcap_calibration_upload_df <- redcap_tables$spine_calibration %>%
        mutate(record_id = redcap_record_id) %>%
        select(record_id, everything())
      
      withProgress(message = 'Uploading Data', value = 0, {
        number_of_steps <- 4
        
        incProgress(1/number_of_steps, detail = paste("Uploading Coordinates"))
      
        importRecords(rcon = rcon_reactive$rcon, data = redcap_upload_df, returnContent = "count")
        
        incProgress(1/number_of_steps, detail = paste("Uploading Calibration"))
        
        importRecords(rcon = rcon_reactive$rcon, data = redcap_calibration_upload_df, returnContent = "count")
        
        incProgress(1/number_of_steps, detail = paste("Completing Upload & retrieving Next Record ID"))
        
        record_ids_df <- exportRecordsTyped(rcon = rcon_reactive$rcon, fields = "record_id")
        
        next_rec_id <- record_ids_df$record_id[which(record_ids_df$record_id == redcap_record_id)+1]
        
        if(length(next_rec_id)>0){
          completion_text <- paste("Tables were successfully uploaded. Next Record is:", next_rec_id ) 
        }else{
          completion_text <- paste("Tables were successfully uploaded. Unable to determine next Record number") 
        }
      }
      )
      
      
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = completion_text,
        type = "success"
      )
      
    }
  }
  )
  
  final_upload_reactive_count <- reactiveValues()
  final_upload_reactive_count$count <- 0
  observeEvent(input$confirm_upload_final, {
    final_upload_reactive_count$count <- final_upload_reactive_count$count + 1
  }
  )
  
  
}
  
  # Run the app
shinyApp(ui = ui, server = server)
