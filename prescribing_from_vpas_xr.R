
jh_rotate_vert_for_vpa_correction_function <- function(vert_coord_list, 
                                                       inf_vert_sp,
                                                       centroid_target,
                                                       initial_translation_correction = c(0,0)) {
  
  original_sp <- vert_coord_list$sp
  
  # Apply initial translation correction
  vert_coord_list <- lapply(vert_coord_list, function(p) p - initial_translation_correction)
  
  # Adjust rotation center
  c_rot_center <- inf_vert_sp - initial_translation_correction
  
  # Extract current centroid and target centroid
  c_current <- vert_coord_list$centroid
  c_target <- centroid_target - initial_translation_correction
  
  # Compute vectors from rotation center to centroids
  v_current <- c_current - c_rot_center
  v_target <- c_target - c_rot_center
  
  # Compute angles of these vectors
  angle_current <- atan2(v_current[2], v_current[1])
  angle_target <- atan2(v_target[2], v_target[1])
  
  # Compute the rotation angle needed
  theta <- angle_target - angle_current
  
  # Normalize theta to be between -pi and pi
  theta <- atan2(sin(theta), cos(theta))
  
  # Function to rotate a point around the rotation center
  rotate_point <- function(p, center, theta) {
    # Translate point to rotation center
    p_translated <- p - center
    # Rotation matrix
    R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
    # Rotate the point
    p_rotated <- R %*% p_translated
    # Translate back
    p_rotated <- p_rotated + center
    return(as.numeric(p_rotated))
  }
  
  # Rotate all points in the vert_coord_list
  new_coord_list <- lapply(vert_coord_list, function(p) {
    rotate_point(p, c_rot_center, theta)
  })
  
  # Preserve the names of the list elements
  names(new_coord_list) <- names(vert_coord_list)
  
  # Compute new centroid after rotation
  c_new <- new_coord_list$centroid
  
  # Compute translation vector to move centroid to target
  translation_vector <- c_target - c_new
  
  # Apply translation to the rotated points
  new_coord_list <- lapply(new_coord_list, function(p) p + translation_vector)
  
  # Since we translated the shape, adjust c_rot_center accordingly
  c_rot_center_new <- c_rot_center + translation_vector
  
  # Build the new geometry
  new_geom <- st_polygon(list(rbind(
    new_coord_list$sp, 
    new_coord_list$sa, 
    new_coord_list$ia, 
    new_coord_list$ip, 
    new_coord_list$sp)))
  
  sp_translation <- original_sp - (new_coord_list$sp + initial_translation_correction)
  
  # Calculate the slope change if required
  slope_change <- jh_calculate_vertex_angle(
    vertex_coord = new_coord_list$ip,
    posterior_point_coord = new_coord_list$ia,
    ventral_point_coord = vert_coord_list$ia)
  
  # Check if centroid matches target
  centroid_difference <- round(c_target, 6) - round(new_coord_list$centroid, 6)
  if(any(centroid_difference != 0)){
    print("Centroid is not at the target position after transformation.")
  }
  
  return(list(
    vert_coord_list = new_coord_list,
    vert_geom = new_geom,
    slope_change = slope_change,
    sp_translation = sp_translation,
    rotation_center = c_rot_center_new))
}



jh_compute_vertebra_centroid_target_by_vpa_target_function <- function(vertebra_coordinates_list, 
                                                                       femoral_head_center, 
                                                                       spine_orientation = "right", 
                                                                       current_vpa = 10, 
                                                                       vpa_target = 100
                                                                       # inferior_vert_geom
                                                                       ){
  
  center_of_rotation <- vertebra_coordinates_list$inferior_posterior_disc
  
  vert_to_hips_line <- st_linestring(rbind(vertebra_coordinates_list$centroid, 
                                           femoral_head_center))
  
  vert_tilt_angle <- jh_calculate_vertex_angle(vertex_coord = femoral_head_center,
                                               posterior_point_coord = c(femoral_head_center[1], vertebra_coordinates_list$centroid[2]), 
                                               ventral_point_coord = vertebra_coordinates_list$centroid, 
                                               spine_orientation = spine_orientation)
  
  
  vert_tilt_target <- vert_tilt_angle  + (vpa_target - current_vpa)
  vert_tilt_target_radian <- vert_tilt_target*pi/180
  
  new_vert_tilt_hyp <- jh_calculate_distance_between_2_points_function(point_1 = vertebra_coordinates_list$centroid, 
                                                                       point_2 = femoral_head_center)*1.25
  
  vert_hip_target_line_point <- c(
    femoral_head_center[1] + sin(vert_tilt_target_radian)*new_vert_tilt_hyp,
    femoral_head_center[2] + cos(vert_tilt_target_radian)*new_vert_tilt_hyp 
  )
  
  new_vert_to_hips_target_line <- st_linestring(rbind(vert_hip_target_line_point, femoral_head_center))  
  
  
  vert_centroid_ip_length <- jh_calculate_distance_between_2_points_function(point_1 = center_of_rotation,
                                                                             point_2 = vertebra_coordinates_list$centroid)
  
  # vert_rotation_circle  <- st_buffer(st_point(center_of_rotation), dist = vert_centroid_ip_length)
  
  vert_rotation_circle  <- st_difference(x = st_buffer(st_point(center_of_rotation), dist = vert_centroid_ip_length),
                                              y = st_buffer(st_point(center_of_rotation), dist = vert_centroid_ip_length*0.99))
  
  # vert_rotation_circle <- st_difference(x = vert_rotation_circle_full, y = inferior_vert_geom)
  
  # new_vert_to_hips_target_line_cropped <- st_crop(x = st_sfc(new_vert_to_hips_target_line), y = c(xmin = -9999, xmax = 9999, ymax = 999999, ymin =  vertebra_coordinates_list$centroid[2]))
  # vert_new_centroid_target_full
  new_vert_to_hips_target_line_cropped <- st_crop(x = st_sfc(new_vert_to_hips_target_line), 
                                                  y = c(xmin = -9999, xmax = 9999, ymax = 999999, 
                                                        ymin =  mean(vertebra_coordinates_list$ip[2], vertebra_coordinates_list$ia[2])))
  
  # Assuming new_vert_to_hips_target_line_cropped and vert_rotation_circle are sf objects
  vert_new_centroid_target <- st_intersection(new_vert_to_hips_target_line_cropped, vert_rotation_circle)
  
  vert_new_centroid_target_full <- vert_new_centroid_target
  # Check if the intersection result contains points
  if(nrow(st_coordinates(vert_new_centroid_target)) > 0) {
    
    if(spine_orientation == "right"){
      if(vpa_target < current_vpa){
        target_centroid_df <- clean_names(as_tibble(st_coordinates(vert_new_centroid_target))) %>%
          select(x, y) %>%
          filter(x == min(x))
        
        target_centroid <- c(target_centroid_df$x[1], target_centroid_df$y[1]) 
      }else{
        target_centroid_df <- clean_names(as_tibble(st_coordinates(vert_new_centroid_target))) %>%
          select(x, y) %>%
          filter(x == max(x))
        
        target_centroid <- c(target_centroid_df$x[1], target_centroid_df$y[1]) 
      }
      
    }else{
      if(vpa_target < current_vpa){
        target_centroid_df <- clean_names(as_tibble(st_coordinates(vert_new_centroid_target))) %>%
          select(x, y) %>%
          filter(x == max(x))
        
        target_centroid <- c(target_centroid_df$x[1], target_centroid_df$y[1]) 
      }else{
        target_centroid_df <- clean_names(as_tibble(st_coordinates(vert_new_centroid_target))) %>%
          select(x, y) %>%
          filter(x == min(x))
        
        target_centroid <- c(target_centroid_df$x[1], target_centroid_df$y[1]) 
      }
    }
    
    
    return(list(centroid_target = target_centroid,
                vert_new_centroid_target_full = vert_new_centroid_target_full,
                vert_rotation_circle = vert_rotation_circle,
                new_vert_to_hips_target_line_cropped = new_vert_to_hips_target_line_cropped
    ))
    
    # Print the intersection coordinates
    # print(vert_new_centroid_target)
  } else {
    print("No intersection found.")
    
    return(list(
      # centroid_target = target_centroid,
      # vert_new_centroid_target_full = vert_new_centroid_target_full,
      vert_rotation_circle = vert_rotation_circle,
      new_vert_to_hips_target_line_cropped = new_vert_to_hips_target_line_cropped
    ))
  }
  
  # return(list(centroid_target = vert_new_centroid_target, 
  #             vert_rotation_circle = vert_rotation_circle, 
  #             new_vert_to_hips_target_line = new_vert_to_hips_target_line))
  
  
}

# 
# jh_compute_vertebra_centroid_target_by_vpa_target_function <- function(vertebra_coordinates_list, 
#                                                                        center_of_rotation,
#                                                                        femoral_head_center, 
#                                                                        spine_orientation = "right", 
#                                                                        current_vpa = 10, 
#                                                                        vpa_target = 100, 
#                                                                        inferior_vert_geom){
#   
#   vert_to_hips_line <- st_linestring(rbind(vertebra_coordinates_list$centroid, 
#                                            femoral_head_center))
#   
#   vert_tilt_angle <- jh_calculate_vertex_angle(vertex_coord = femoral_head_center,
#                                                posterior_point_coord = c(femoral_head_center[1], vertebra_coordinates_list$centroid[2]), 
#                                                ventral_point_coord = vertebra_coordinates_list$centroid, 
#                                                spine_orientation = "right")
#   
#   
#   vert_tilt_target <- vert_tilt_angle  + (vpa_target - current_vpa)
#   vert_tilt_target_radian <- vert_tilt_target*pi/180
#   
#   new_vert_tilt_hyp <- jh_calculate_distance_between_2_points_function(point_1 = vertebra_coordinates_list$centroid, 
#                                                                        point_2 = femoral_head_center)*1.25
#   
#   vert_hip_target_line_point <- c(
#     femoral_head_center[1] + sin(vert_tilt_target_radian)*new_vert_tilt_hyp,
#     femoral_head_center[2] + cos(vert_tilt_target_radian)*new_vert_tilt_hyp 
#   )
#   
#   new_vert_to_hips_target_line <- st_linestring(rbind(vert_hip_target_line_point, femoral_head_center))  
# 
# 
#   vert_centroid_ip_length <- jh_calculate_distance_between_2_points_function(point_1 = center_of_rotation,
#                                                                              point_2 = vertebra_coordinates_list$centroid)
#   
#   # vert_rotation_circle  <- st_buffer(st_point(center_of_rotation), dist = vert_centroid_ip_length)
# 
#   vert_rotation_circle_full  <- st_difference(x = st_buffer(st_point(center_of_rotation), dist = vert_centroid_ip_length),
#                 y = st_buffer(st_point(center_of_rotation), dist = vert_centroid_ip_length*0.99))
#   
#   vert_rotation_circle <- st_difference(x = vert_rotation_circle_full, y = inferior_vert_geom)
#   
#   # new_vert_to_hips_target_line_cropped <- st_crop(x = st_sfc(new_vert_to_hips_target_line), y = c(xmin = -9999, xmax = 9999, ymax = 999999, ymin =  vertebra_coordinates_list$centroid[2]))
#   # vert_new_centroid_target_full
#   new_vert_to_hips_target_line_cropped <- st_crop(x = st_sfc(new_vert_to_hips_target_line), 
#              y = c(xmin = -9999, xmax = 9999, ymax = 999999, 
#                    ymin =  mean(vertebra_coordinates_list$ip[2], vertebra_coordinates_list$ia[2])))
#   
#   # Assuming new_vert_to_hips_target_line_cropped and vert_rotation_circle are sf objects
#   vert_new_centroid_target <- st_intersection(new_vert_to_hips_target_line_cropped, vert_rotation_circle)
#   
#   vert_new_centroid_target_full <- vert_new_centroid_target
#   # Check if the intersection result contains points
#   if(nrow(st_coordinates(vert_new_centroid_target)) > 0) {
# 
#     if(spine_orientation == "right"){
#       if(vpa_target < current_vpa){
#         target_centroid_df <- clean_names(as_tibble(st_coordinates(vert_new_centroid_target))) %>%
#           select(x, y) %>%
#           filter(x == min(x))
#         
#         target_centroid <- c(target_centroid_df$x[1], target_centroid_df$y[1]) 
#       }else{
#         target_centroid_df <- clean_names(as_tibble(st_coordinates(vert_new_centroid_target))) %>%
#           select(x, y) %>%
#           filter(x == max(x))
#         
#         target_centroid <- c(target_centroid_df$x[1], target_centroid_df$y[1]) 
#       }
#       
#     }else{
#       if(vpa_target < current_vpa){
#         target_centroid_df <- clean_names(as_tibble(st_coordinates(vert_new_centroid_target))) %>%
#           select(x, y) %>%
#           filter(x == max(x))
#         
#         target_centroid <- c(target_centroid_df$x[1], target_centroid_df$y[1]) 
#       }else{
#         target_centroid_df <- clean_names(as_tibble(st_coordinates(vert_new_centroid_target))) %>%
#           select(x, y) %>%
#           filter(x == min(x))
#         
#         target_centroid <- c(target_centroid_df$x[1], target_centroid_df$y[1]) 
#       }
#     }
#      
#     
#     return(list(centroid_target = target_centroid,
#                 vert_new_centroid_target_full = vert_new_centroid_target_full,
#                 vert_rotation_circle = vert_rotation_circle,
#                 new_vert_to_hips_target_line_cropped = new_vert_to_hips_target_line_cropped
#     ))
#     
#     # Print the intersection coordinates
#     # print(vert_new_centroid_target)
#   } else {
#     print("No intersection found.")
#     
#     return(list(
#       # centroid_target = target_centroid,
#                 # vert_new_centroid_target_full = vert_new_centroid_target_full,
#                 vert_rotation_circle = vert_rotation_circle,
#                 new_vert_to_hips_target_line_cropped = new_vert_to_hips_target_line_cropped
#     ))
#   }
#   
#   # return(list(centroid_target = vert_new_centroid_target, 
#   #             vert_rotation_circle = vert_rotation_circle, 
#   #             new_vert_to_hips_target_line = new_vert_to_hips_target_line))
#   
# 
# }

#########

jh_rotate_vert_for_vpa_correction_function <-  function(vert_list, 
                                                        # center_of_rotation, 
                                                        target_centroid, 
                                                        starting_segment_angle = 5
                                                        # superior_posterior_disc_point
                                                        ) {
  # library(sf)
  
  
  # starting_slope <- jh_calculate_vertex_angle(vertex_coord = vert_list$sp,
  #                                               posterior_point_coord = vert_list$sa,
  #                                               ventral_point_coord = c(vert_list$sa[1], vert_list$sp[2]))
  
  
  # Ensure coordinates are numeric vectors of length 2
  sp <- as.numeric(vert_list$sp)     # to be rotated
  sa <- as.numeric(vert_list$sa)     # to be rotated
  ia <- as.numeric(vert_list$ia)     # to be rotated
  ip <- as.numeric(vert_list$ip)     # to be rotated
  inferior_posterior_disc <- as.numeric(vert_list$inferior_posterior_disc)     # CONSTANT CENTER OF ROTATION
  superior_posterior_disc <- as.numeric(vert_list$superior_posterior_disc)     # to be rotated
  
  current_centroid <- as.numeric(vert_list$centroid)     # to be rotated
  
  # center_of_rotation <- as.numeric(center_of_rotation)   # remains constant
  # superior_posterior_disc_point <- as.numeric(superior_posterior_disc_point)
  
  # Check that all coordinates are length 2
  if (!all(sapply(list(sp, sa, ia, ip, inferior_posterior_disc, superior_posterior_disc, current_centroid), function(x) length(x) == 2))) {
    stop("All coordinate vectors must be of length 2.")
  }
  
  # Construct vert_body (without center_of_rotation)
  # coords <- rbind(sp, sa, ia, ip, sp)
  
  # Create the polygon geometry
  # vert_body <- st_polygon(list(coords))
  
  # Compute the centroid of vert_body
  # current_centroid <- st_coordinates(st_centroid(st_sfc(vert_body)))[1, ]
  
  # Compute vectors from center_of_rotation to centroids
  vector_current <- current_centroid - inferior_posterior_disc
  vector_target <- target_centroid - inferior_posterior_disc
  
  # Compute the angle between the vectors
  angle_current <- atan2(vector_current[2], vector_current[1])
  angle_target <- atan2(vector_target[2], vector_target[1])
  angle_rotation <- angle_target - angle_current
  
  final_segment_angle = starting_segment_angle + angle_rotation*180/pi
  
  if(final_segment_angle > 35){
    angle_rotation <- ((35 - starting_segment_angle)*pi)/180
  }
  
  angle_return_list <- list()
  angle_return_list$angle_current <- angle_current
  angle_return_list$angle_target <- angle_target
  angle_return_list$angle_rotation <- angle_rotation
  
  # Rotation function (operating on vectors)
  rotate_point <- function(pt, center, angle) {
    s <- sin(angle)
    c <- cos(angle)
    pt_translated <- pt - center
    xnew <- pt_translated[1] * c - pt_translated[2] * s
    ynew <- pt_translated[1] * s + pt_translated[2] * c
    pt_rotated <- c(xnew, ynew) + center
    return(pt_rotated)
  }
  
  # Rotate sa, ia, ip around center_of_rotation
  new_sp <- rotate_point(sp, inferior_posterior_disc, angle_rotation)
  new_sa <- rotate_point(sa, inferior_posterior_disc, angle_rotation)
  new_ia <- rotate_point(ia, inferior_posterior_disc, angle_rotation)
  new_ip <- rotate_point(ip, inferior_posterior_disc, angle_rotation)
  
  new_superior_posterior_disc <- rotate_point(superior_posterior_disc, inferior_posterior_disc, angle_rotation)
 
  # Construct new_vert_list
  new_vert_list <- list()
  new_vert_list$sp <- new_sp    # remains the same
  new_vert_list$sa <- new_sa
  new_vert_list$ia <- new_ia
  # new_vert_list$center_of_rotation <- center_of_rotation   # remains the same
  new_vert_list$inferior_posterior_disc <- inferior_posterior_disc
  
  new_vert_list$superior_posterior_disc <- new_superior_posterior_disc
  
  new_vert_geom <- st_polygon(list(rbind(new_sp, new_sa, new_ia, new_ip, new_sp)))
  
  new_vert_list$centroid <- st_coordinates(st_centroid(st_sfc(new_vert_geom)))[1, ]
  
  
  # Check that centroids match (after rounding)
  if (!all(round(new_vert_list$centroid, 2) == round(target_centroid, 2))) {
    warning("Centroid does not match target centroid after rotation.")
  }
  
  
  # Construct new_geom_extended (with center_of_rotation)
  # new_geom_extended <- st_polygon(list(rbind(new_sp, new_sa, new_ia, center_of_rotation, new_ip, new_sp)))
  
  # Construct new_vert_geom (without center_of_rotation)
  # new_vert_geom <- new_vert_body  # already constructed
  
  
  # Return the results
  result <- list()
  result$vert_coord_list <- new_vert_list
  # result$new_geom_extended <- new_geom_extended
  result$vert_geom <- new_vert_geom

  # result$new_slope <- jh_calculate_vertex_angle(vertex_coord = new_vert_list$sp,
  #                                        posterior_point_coord = new_vert_list$sa,
  #                                        ventral_point_coord = c(new_vert_list$sa[1], new_vert_list$sp[2]))
  # 
  # result$slope_change <- result$new_slope - starting_slope
  
  result$slope_change <- jh_calculate_segment_angle_between_vertebrae_from_coordinates_function(sp_upper = new_vert_list$sp, 
                                                                         sa_upper = new_vert_list$sa,
                                                                         sp_lower = vert_list$sp,
                                                                         sa_lower = vert_list$sa)
  
  result$angle_return_list <- angle_return_list
  
  return(result)
}



####
# perform_rotation_and_translation <- function(vert_list, 
#                                              point_of_rotation, 
#                                              slope_adjustment, 
#                                              new_inf_disc_rotation_point, 
#                                              starting_superior_disc_point) {

perform_rotation_and_translation <- function(vert_list, 
                                             slope_adjustment, 
                                             translated_inferior_vert_coord_list) {
  
  new_inf_disc_rotation_point <- translated_inferior_vert_coord_list$superior_posterior_disc
  
  # inferior_posterior_disc
  # superior_posterior_disc
  # Convert slope_adjustment from degrees to radians
  angle_rad <- slope_adjustment * pi / 180
  
  # Extract the coordinates
  sp <- as.numeric(vert_list$sp)
  sa <- as.numeric(vert_list$sa)
  ia <- as.numeric(vert_list$ia)
  ip <- as.numeric(vert_list$ip)
  
  superior_posterior_disc <- as.numeric(vert_list$superior_posterior_disc)
  inferior_posterior_disc <- as.numeric(vert_list$inferior_posterior_disc)
  
  # # Ensure all coordinates are length-2 numeric vectors
  # if (!all(sapply(list(sp, sa, ia, ip, point_of_rotation, new_inf_disc_rotation_point), function(x) length(x) == 2))) {
  #   stop("All coordinate vectors must be of length 2.")
  # }
  # Ensure all coordinates are length-2 numeric vectors
  if (!all(sapply(list(sp, sa, ia, ip, superior_posterior_disc, inferior_posterior_disc, new_inf_disc_rotation_point), function(x) length(x) == 2))) {
    stop("All coordinate vectors must be of length 2.")
  }
  
  # Rotation function (operating on vectors)
  rotate_point <- function(pt, center, angle) {
    s <- sin(angle)
    c <- cos(angle)
    pt_translated <- pt - center
    xnew <- pt_translated[1] * c - pt_translated[2] * s
    ynew <- pt_translated[1] * s + pt_translated[2] * c
    pt_rotated <- c(xnew, ynew) + center
    return(pt_rotated)
  }
  

  # Rotate the points around point_of_rotation by slope_adjustment degrees
  new_sp <- rotate_point(sp, inferior_posterior_disc, angle_rad)
  new_sa <- rotate_point(sa, inferior_posterior_disc, angle_rad)
  new_ia <- rotate_point(ia, inferior_posterior_disc, angle_rad)
  new_ip <- rotate_point(ip, inferior_posterior_disc, angle_rad)
  # Note: point_of_rotation remains the same during rotation
  
  new_superior_posterior_disc <- rotate_point(superior_posterior_disc, inferior_posterior_disc, angle_rad)
  
  # Now translate the rotated geometry so that point_of_rotation moves to new_inf_disc_rotation_point
  # translation_vector <- new_inf_disc_rotation_point - point_of_rotation
  translation_vector <- new_inf_disc_rotation_point - inferior_posterior_disc
  
  # Apply the translation to the rotated points
  new_sp <- new_sp + translation_vector
  new_sa <- new_sa + translation_vector
  new_ia <- new_ia + translation_vector
  new_ip <- new_ip + translation_vector
  # new_point_of_rotation <- point_of_rotation + translation_vector  # Should equal new_inf_disc_rotation_point
  new_superior_posterior_disc <- new_superior_posterior_disc + translation_vector
  new_inferior_posterior_disc <- inferior_posterior_disc + translation_vector
  
  
  new_vert_list <- list()
  new_vert_list$sp <- new_sp
  new_vert_list$sa <- new_sa
  new_vert_list$ia <- new_ia
  new_vert_list$ip <- new_ip
  new_vert_list$new_superior_posterior_disc <- new_superior_posterior_disc  
  new_vert_list$inferior_posterior_disc <- inferior_posterior_disc # Should be equal to new_inf_disc_rotation_point
  
  # Construct new_vert_body
  new_geom <- st_polygon(list(rbind(new_sp, new_sa, new_ia, new_ip, new_sp)))
  
  # Construct new_geom as per your requirement (using original sp)
  # new_geom <- st_polygon(list(rbind(new_sp, new_sa, new_ia, new_ip, new_sp)))
  
  # Compute the centroid of the new geometry
  new_vert_list$centroid <- st_coordinates(st_centroid(st_sfc(new_geom)))[1, ]
  
  # Return the results
  result <- list()
  result$vert_coord <- new_vert_list
  result$vert_geom <- new_geom
  # result$new_superior_disc_point <- new_superior_disc_point
  return(result)
}











