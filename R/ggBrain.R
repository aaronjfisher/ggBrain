rm(list = ls())
library(WhiteStripe)
library(neurobase)
library(dplyr)
library(ggplot2)
library(tidyr)
fname = ws_img_data()[1]
x = readnii(fname)
mask = NULL
mask = x > 0
vdim = NULL
xyz = NULL
breaks = NULL
zlim = NULL
col = gray(0:64/64)

zlim = zlimmer(x, zlim = zlim)
if (is.null(breaks)) {
  breaks <- c(min(x, zlim, na.rm = TRUE), 
              seq(min(zlim, na.rm = TRUE),
                  max(zlim, na.rm = TRUE), 
                  length = length(col) - 1),
              max(x, zlim, na.rm = TRUE))
}
breaks = unique(breaks)

# function(x) {
dims = dim(x)

if (is.null(xyz)) {
  xyz <- ceiling(dims/2)
}


vdims = try({
  voxdim(x)
}, silent = TRUE)
if (inherits(vdims, "try-error")) {
  if (is.null(vdim)) {
    warning("No Voxel dimensions are given, using default as 1mm!")
    vdims = rep(1, 3)
  } else {
    if (length(vdim) != 3) {
      stop("Voxel dimensions must be 3 numbers")
    }
    vdims = vdim
  }
}
all_dims = 1:3
names(dims) = names(vdims) = paste0("dim", all_dims)


mat_to_cols = function(mat, plane, vox) {
  nc = dim(mat)
  res = cbind(
    x = rep(seq(nc[1]), times = nc[2]),
    y = rep(seq(nc[2]), each = nc[1]),
    vox_x = vox[1],
    vox_y = vox[2],
    min_x = vox[1],
    min_y = vox[2],
    max_x = nc[1] * vox[1],
    max_y = nc[2] * vox[2],
    plane = plane,
    value = c(mat)
  )
  return(res)
}

img = x
xyz_img_to_df = function(img, xyz, vdims){
  v1 = mat_to_cols(img[, xyz[2],], 
                   plane = 2,
                   vox = vdims[c(1,3)])
  v2 = mat_to_cols(img[xyz[1], ,], 
                   plane = 1,
                   vox = vdims[c(2,3)])
  v3 = mat_to_cols(img[, ,xyz[3]], 
                   plane = 3,
                   vox = vdims[c(1,2)])
  L = list(v1, v2, v3)
  L = do.call("rbind", L)
  L = as.data.frame(L)
  L$voxel_x = L$x * L$vox_x
  L$voxel_y = L$y * L$vox_y
  return(L)
}

res = xyz_img_to_df(img = x, xyz = xyz, vdims = vdims)
# 
# xyz_img_to_df = function(xyz = NULL){
#   if (is.null(xyz)) {
#     xyz <- ceiling(dims/2)
#   }
#   
# }
# 
# if (is.null(mask)) {
#   L = lapply(dims, seq)
#   names(L) = paste0("dim", seq(length(dims)))
#   ind = expand.grid(L)
#   ind = as.matrix(ind)
# } else { 
#   check_mask_fail(mask)
#   ind = which(mask > 0, arr.ind = TRUE)
# }
# 
# ind = ind[, 1:3]
# 
# 
# eg = c(2, 1, 3)
# idim = eg[1]
# split_up = lapply(eg, function(idim) {
#   
#   cn = paste0("dim", idim)
#   keep = ind[, cn] == xyz[idim]
#   res = ind[keep,]
#   
#   # can do x and y here
#   value = x[res]
#   cn = setdiff(paste0("dim", all_dims), cn)
#   res = ind[keep, ]
#   vres = t( t(res) * vdims )
#   
#   res = res[, cn]
#   vres = vres[, cn]
#   colnames(res) = c("x", "y")
#   colnames(vres) = paste0("voxel_", c("x", "y"))
#   
#   res = cbind(res, 
#               vres,
#               plane = idim, value = value)
#   res = as.data.frame(res)
#   res$dim_x = vdims[cn[1]]
#   res$dim_y = vdims[cn[2]]
#   res$min_x = vdims[cn[1]]
#   res$min_y = vdims[cn[2]]
#   # 
#   res$max_x = vdims[cn[1]] * dims[cn[1]]
#   res$max_y = vdims[cn[2]] * dims[cn[2]]
#   return(res)
# })
# 
# res = do.call("rbind", split_up)
# 

res$plane = factor(res$plane, levels = c(2, 1, 3))
# res$voxel_x = res$dim_x

#####################################
# Start at min_x,min_x and then max_x, max_y
#####################################
max_vals = res %>% 
  group_by(plane) %>% 
  summarise(
    min_x = min(min_x),
    max_x = max(max_x),
    min_y = min(min_y),
    max_y = max(max_y)            
  )
max_vals = gather(max_vals, coord, voxel, -plane)
max_vals = separate(max_vals, coord, 
                    into = c("type", "axis"), 
                    sep = "_")
max_vals$axis = paste0("voxel_", max_vals$axis)
max_vals = spread(max_vals, key = axis, value = voxel)
max_vals$value = min(res$value)

res = select(res, -max_x, -max_y, -min_x, -min_y)

theme_black = 
  theme(panel.background = element_rect(fill = "black"), 
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "null"), 
        # axis.text = element_text(
        #   margin = unit(0, "null")
        # ),
        axis.text = element_blank(),
        axis.line = element_blank(), 
        ##################
        # from SO 31254533
        #################
        plot.margin = unit(c(0,0,0,0), "null"),
        panel.spacing = unit(0, "null"),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0,
                               unit = "null")
  )

theme_black_legend = theme_black + 
  theme(legend.position = c(0.75, 0.25),
        legend.text = element_text(colour = "white"),
        legend.title = element_text(colour = "white"))

gray_gradient = scale_fill_gradient(
    low = gray(0),
    high = gray(1),
    na.value = "black")  

gray_gradientn = scale_fill_gradientn(
  colours = col,
  na.value = "black")  


# res$plane = factor(res$plane, levels = eg)


out = ggplot(
  data = res, 
  aes(x = voxel_x, y = voxel_y, fill = value)) + 
  geom_tile() + 
  # geom_point(data = max_vals, alpha = 0,
  #            fill = NA) +
  facet_wrap(~ plane, nrow = 2, ncol = 2, 
             scales = "free")
# out + theme_black_legend
# out + theme_black_legend + gray_gradientn
out = out + theme_black_legend + gray_gradient
print(out)
}
# 
# res$val = cut(res$value, breaks = breaks,
#               include.lowest = TRUE)
# out = ggplot(
#   data = res, 
#   aes(x = voxel_x, y = voxel_y, fill = val)) + 
#   geom_tile() + 
#   # geom_point(data = max_vals, alpha = 0,
#   #            fill = NA) +
#   facet_wrap(~ plane, nrow = 2, ncol = 2, 
#              scales = "free")
# out + scale_fill_manual(
#   values = col,
#   breaks = breaks, drop = FALSE)



######################################################
# this needs to be better in shiny - with changing xyz 
######################################################
# vox_ind$keep = vox_ind$dim1 %in% xyz[1] | 
#     vox_ind$dim2 %in% xyz[2] | 
#     vox_ind$dim3 %in% xyz[3]

# vox_ind = vox_ind[keep, ]
# eg = rbind(c(1, 3),
#            c(2, 3),
#            c(1, 2))
# split_up = apply(eg, 1, function(x) {
#   cn = paste0("dim", x)
#   keep = t( t(vox_ind[, cn]) == xyz[x])
#   keep = keep[, 1] & keep[,2]
#   res = vox_ind[keep,]
# })








