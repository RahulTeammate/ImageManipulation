#This destroys all variables from a prior session
rm(list=ls())
setwd("C:/Users/Rahul/Desktop/img_tool")

#Source: http://stackoverflow.com/questions/14769628/how-to-get-pixel-data-from-an-image-using-r
#        https://cran.r-project.org/web/packages/png/png.pdf
library('png')
img = readPNG('elcapitan3.png')
#gather the pixel data------------------------------------
#image has 512 width and 340 height
#image[height,width,RGBA]
TOT_SAMPLES = 340*512
HEIGHT = 340
WIDTH = 512
#The first dimension is the pixel's index in the samples
#The second dimension is (R,G,B,orig_height,orig_width)
data1 = array(data=NA,dim=c(TOT_SAMPLES,5))
data2 = array(data=NA,dim=c(TOT_SAMPLES,5))
data3 = array(data=NA,dim=c(TOT_SAMPLES,5))
datasize1 = TOT_SAMPLES
datasize2 = 0
datasize3 = 0
#Just fill up the first data set
for (h in 1:HEIGHT) {
  for (w in 1:WIDTH) {
    data1[WIDTH*(h-1)+w,1] = img[h,w,1]
    data1[WIDTH*(h-1)+w,2] = img[h,w,2]
    data1[WIDTH*(h-1)+w,3] = img[h,w,3]
    data1[WIDTH*(h-1)+w,4] = h
    data1[WIDTH*(h-1)+w,5] = w
  }
}

#k-means clustering---------------------------------------
#Choose k points to act as cluster centers
randomthreepoints = sample(TOT_SAMPLES,3,replace=FALSE)
clustermean1 = c(data1[randomthreepoints[1],1],data1[randomthreepoints[1],2],data1[randomthreepoints[1],3])
clustermean2 = c(data1[randomthreepoints[2],1],data1[randomthreepoints[2],2],data1[randomthreepoints[2],3])
clustermean3 = c(data1[randomthreepoints[3],1],data1[randomthreepoints[3],2],data1[randomthreepoints[3],3])

changeinvalue = as.double(10.0) #Force changeinvalue into a double.
#Also, initialize it to a large number so it falls in the while loop.

#Until the cluster centers change very little
while (changeinvalue > 1e-2) {
  #Allocate each data point to the nearest clustermean.
  newdata1 = array(data=NA,dim=c(TOT_SAMPLES,5))
  newdata2 = array(data=NA,dim=c(TOT_SAMPLES,5))
  newdata3 = array(data=NA,dim=c(TOT_SAMPLES,5))
  index1 = 1
  index2 = 1
  index3 = 1
  yc1 = c(clustermean1[1],clustermean1[2],clustermean1[3])
  yc2 = c(clustermean2[1],clustermean2[2],clustermean2[3])
  yc3 = c(clustermean3[1],clustermean3[2],clustermean3[3])
  
  #Go through data1
  if (datasize1 >= 1) {
    for (i in 1:datasize1) {
      xc1 = c(data1[i,1],data1[i,2],data1[i,3])
    
      smallest_dist1 = as.double(dist(rbind(xc1,yc1),method="euclidean"))
      smallest_dist2 = as.double(dist(rbind(xc1,yc2),method="euclidean"))
      smallest_dist3 = as.double(dist(rbind(xc1,yc3),method="euclidean"))
    
      if ((smallest_dist1 < smallest_dist2) && (smallest_dist1 < smallest_dist3)) {
        newdata1[index1,] = data1[i,]
        index1 = index1 + 1
      }
      else if ((smallest_dist2 < smallest_dist1) && (smallest_dist2 < smallest_dist3)) {
        newdata2[index2,] = data1[i,]
        index2 = index2 + 1
      }
      else { #smallest_dist3 is the smallest
        newdata3[index3,] = data1[i,]
        index3 = index3 + 1
      }
    }
  }
  #Go through data2
  if (datasize2 >= 1) {
    for (i in 1:datasize2) {
      xc2 = c(data2[i,1],data2[i,2],data2[i,3])
    
      smallest_dist1 = as.double(dist(rbind(xc2,yc1),method="euclidean"))
      smallest_dist2 = as.double(dist(rbind(xc2,yc2),method="euclidean"))
      smallest_dist3 = as.double(dist(rbind(xc2,yc3),method="euclidean"))
    
      if ((smallest_dist1 < smallest_dist2) && (smallest_dist1 < smallest_dist3)) {
        newdata1[index1,] = data2[i,]
        index1 = index1 + 1
      }
      else if ((smallest_dist2 < smallest_dist1) && (smallest_dist2 < smallest_dist3)) {
        newdata2[index2,] = data2[i,]
        index2 = index2 + 1
      }
      else { #smallest_dist3 is the smallest
        newdata3[index3,] = data2[i,]
        index3 = index3 + 1
      }
    }
  }
  #Go through data3
  if (datasize3 >= 1) {
    for (i in 1:datasize3) {
      xc3 = c(data3[i,1],data3[i,2],data3[i,3])
    
      smallest_dist1 = as.double(dist(rbind(xc3,yc1),method="euclidean"))
      smallest_dist2 = as.double(dist(rbind(xc3,yc2),method="euclidean"))
      smallest_dist3 = as.double(dist(rbind(xc3,yc3),method="euclidean"))
    
      if ((smallest_dist1 < smallest_dist2) && (smallest_dist1 < smallest_dist3)) {
        newdata1[index1,] = data3[i,]
        index1 = index1 + 1
      }
      else if ((smallest_dist2 < smallest_dist1) && (smallest_dist2 < smallest_dist3)) {
        newdata2[index2,] = data3[i,]
        index2 = index2 + 1
      }
      else { #smallest_dist3 is the smallest
        newdata3[index3,] = data3[i,]
        index3 = index3 + 1
      }
    }
  }
  
  #make sure each cluster has at least 1 element
  if (index1 == 1) {
    #Randomly pick one element from another cluster and move it.
    randi2 = sample(index2,size=1,replace=TRUE)
    randi3 = sample(index3,size=1,replace=TRUE)
    coinflip = sample(2,size=1,replace=TRUE)
    
    #The extra condition is 
    #just in case we aren't taking from a 1-elem set and moving it to a 0-elem set
    if (coinflip == 1 && index2 > 1) {
      newdata1[index1,] = newdata2[randi2,]
      index1 = index1 + 1
      newdata2[randi2,] = c(NA,NA,NA,NA,NA)
      index2 = index2 - 1
      newdata2 = na.omit(newdata2)
    }
    else if (coinflip == 1 && index2 == 1) {
      newdata1[index1,] = newdata3[randi3,]
      index1 = index1 + 1
      newdata3[randi3,] = c(NA,NA,NA,NA,NA)
      index3 = index3 - 1
      newdata3 = na.omit(newdata3)
    }
    else if (coinflip == 2 && index3 > 1) {
      newdata1[index1,] = newdata3[randi3,]
      index1 = index1 + 1
      newdata3[randi3,] = c(NA,NA,NA,NA,NA)
      index3 = index3 - 1
      newdata3 = na.omit(newdata3)
    }
    else { #coinflip == 2 && index3 == 1
      newdata1[index1,] = newdata2[randi2,]
      index1 = index1 + 1
      newdata2[randi2,] = c(NA,NA,NA,NA,NA)
      index2 = index2 - 1
      newdata2 = na.omit(newdata2)
    }
  }
  if (index2 == 1) {
    #Randomly pick one element from another cluster and move it.
    randi1 = sample(index1,size=1,replace=TRUE)
    randi3 = sample(index3,size=1,replace=TRUE)
    coinflip = sample(2,size=1,replace=TRUE)
    
    #The extra condition is 
    #just in case we aren't taking from a 1-elem set and moving it to a 0-elem set
    if (coinflip == 1 && index1 > 1) {
      newdata2[index2,] = newdata1[randi1,]
      index2 = index2 + 1
      newdata1[randi1,] = c(NA,NA,NA,NA,NA)
      index1 = index1 - 1
      newdata1 = na.omit(newdata1)
    }
    else if (coinflip == 1 && index1 == 1) {
      newdata2[index2,] = newdata3[randi3,]
      index2 = index2 + 1
      newdata3[randi3,] = c(NA,NA,NA,NA,NA)
      index3 = index3 - 1
      newdata3 = na.omit(newdata3)
    }
    else if (coinflip == 2 && index3 > 1) {
      newdata2[index2,] = newdata3[randi3,]
      index2 = index2 + 1
      newdata3[randi3,] = c(NA,NA,NA,NA,NA)
      index3 = index3 - 1
      newdata3 = na.omit(newdata3)
    }
    else { #coinflip == 2 && index3 == 1
      newdata2[index2,] = newdata1[randi1,]
      index2 = index2 + 1
      newdata1[randi1] = c(NA,NA,NA,NA,NA)
      index1 = index1 - 1
      newdata1 = na.omit(newdata1)
    }
  }
  if (index3 == 1) {
    #Randomly pick one element from another cluster and move it.
    randi1 = sample(index1,size=1,replace=TRUE)
    randi2 = sample(index2,size=1,replace=TRUE)
    coinflip = sample(2,size=1,replace=TRUE)
    
    #The extra condition is 
    #just in case we aren't taking from a 1-elem set and moving it to a 0-elem set
    if (coinflip == 1 && index1 > 1) {
      newdata3[index3,] = newdata1[randi1,]
      index3 = index3 + 1
      newdata1[randi1,] = c(NA,NA,NA,NA,NA)
      index1 = index1 - 1
      newdata1 = na.omit(newdata1)
    }
    else if (coinflip == 1 && index1 == 1) {
      newdata3[index3,] = newdata2[randi2,]
      index3 = index3 + 1
      newdata2[randi2,] = c(NA,NA,NA,NA,NA)
      index2 = index2 - 1
      newdata2 = na.omit(newdata2)
    }
    else if (coinflip == 2 && index2 > 1) {
      newdata3[index3,] = newdata2[randi2,]
      index3 = index3 + 1
      newdata2[randi2,] = c(NA,NA,NA,NA,NA)
      index2 = index2 - 1
      newdata2 = na.omit(newdata2)
    }
    else { #coinflip == 2 && index3 == 1
      newdata3[index3,] = newdata1[randi1,]
      index3 = index3 + 1
      newdata1[randi1,] = c(NA,NA,NA,NA,NA)
      index1 = index1 - 1
      newdata1 = na.omit(newdata1)
    }
  }
  
  #now replace the cluster means with the mean of the clusters you calculated.
  data1 = na.omit(newdata1)
  data2 = na.omit(newdata2)
  data3 = na.omit(newdata3)
  
  newmean1 = c(mean(data1[,1]),mean(data1[,2]),mean(data1[,3]))
  newmean2 = c(mean(data2[,1]),mean(data2[,2]),mean(data2[,3]))
  newmean3 = c(mean(data3[,1]),mean(data3[,2]),mean(data3[,3]))
  
  changeinvalue = abs(as.double(dist(rbind(clustermean1,newmean1),method="euclidean")))
  + abs(as.double(dist(rbind(clustermean2,newmean2),method="euclidean")))
  + abs(as.double(dist(rbind(clustermean3,newmean3),method="euclidean")))
  
  clustermean1 = newmean1
  clustermean2 = newmean2
  clustermean3 = newmean3
  datasize1 = length(data1[,1])
  datasize2 = length(data2[,1])
  datasize3 = length(data3[,1])
}

cat(paste("Cluster mean 1:",clustermean1))
cat(paste("Cluster mean 2:",clustermean2))
cat(paste("Cluster mean 3:",clustermean3))

if (datasize1 >= 1) {
  for (i in 1:datasize1) {
    h = data1[i,4]
    w = data1[i,5]
    
    #What is the closest mean?
    xc1 = c(data1[i,1],data1[i,2],data1[i,3])
    
    smallest_dist1 = as.double(dist(rbind(xc1,clustermean1),method="euclidean"))
    smallest_dist2 = as.double(dist(rbind(xc1,clustermean2),method="euclidean"))
    smallest_dist3 = as.double(dist(rbind(xc1,clustermean3),method="euclidean"))
    
    if ((smallest_dist1 < smallest_dist2) && (smallest_dist1 < smallest_dist3)) {
      img[h,w,] = c(clustermean1[1],clustermean1[2],clustermean1[3],1.0)
    }
    else if ((smallest_dist2 < smallest_dist1) && (smallest_dist2 < smallest_dist3)) {
      img[h,w,] = c(clustermean2[1],clustermean2[2],clustermean2[3],1.0)
    }
    else { #smallest_dist3 is the smallest
      img[h,w,] = c(clustermean3[1],clustermean3[2],clustermean3[3],1.0)
    }
  }
}

if (datasize2 >= 1) {
  for (i in 1:datasize2) {
    h = data2[i,4]
    w = data2[i,5]
    
    #What is the closest mean?
    xc2 = c(data2[i,1],data2[i,2],data2[i,3])
    
    smallest_dist1 = as.double(dist(rbind(xc2,clustermean1),method="euclidean"))
    smallest_dist2 = as.double(dist(rbind(xc2,clustermean2),method="euclidean"))
    smallest_dist3 = as.double(dist(rbind(xc2,clustermean3),method="euclidean"))
    
    if ((smallest_dist1 < smallest_dist2) && (smallest_dist1 < smallest_dist3)) {
      img[h,w,] = c(clustermean1[1],clustermean1[2],clustermean1[3],1.0)
    }
    else if ((smallest_dist2 < smallest_dist1) && (smallest_dist2 < smallest_dist3)) {
      img[h,w,] = c(clustermean2[1],clustermean2[2],clustermean2[3],1.0)
    }
    else { #smallest_dist3 is the smallest
      img[h,w,] = c(clustermean3[1],clustermean3[2],clustermean3[3],1.0)
    }
  }
}

if (datasize3 >= 1) {
  for (i in 1:datasize3) {
    h = data3[i,4]
    w = data3[i,5]
    
    #What is the closest mean?
    xc3 = c(data3[i,1],data3[i,2],data3[i,3])
    
    smallest_dist1 = as.double(dist(rbind(xc3,clustermean1),method="euclidean"))
    smallest_dist2 = as.double(dist(rbind(xc3,clustermean2),method="euclidean"))
    smallest_dist3 = as.double(dist(rbind(xc3,clustermean3),method="euclidean"))
    
    if ((smallest_dist1 < smallest_dist2) && (smallest_dist1 < smallest_dist3)) {
      img[h,w,] = c(clustermean1[1],clustermean1[2],clustermean1[3],1.0)
    }
    else if ((smallest_dist2 < smallest_dist1) && (smallest_dist2 < smallest_dist3)) {
      img[h,w,] = c(clustermean2[1],clustermean2[2],clustermean2[3],1.0)
    }
    else { #smallest_dist3 is the smallest
      img[h,w,] = c(clustermean3[1],clustermean3[2],clustermean3[3],1.0)
    }
  }
}

new_img = writePNG(img,target='output.png')