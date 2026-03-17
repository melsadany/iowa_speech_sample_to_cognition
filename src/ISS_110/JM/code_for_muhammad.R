####################################################################
### functions
####################################################################
library(tuneR) ## pretty sure this is the only dependency


### find audio peaks
find_peaks <- function(vector, thresh, result = 'center') {
  # Calculate the differences
  diffs <- diff(vector)
  
  # Identify where the diffs change from positive to negative, indicating a peak
  peak_indices <- which(diffs[-length(diffs)] > 0 & diffs[-1] < 0) + 1
  peak_indices <- peak_indices[vector[peak_indices] > thresh]
  if(length(peak_indices)==0) return(NA)
  if (result == 'center') {
    return(peak_indices)
  } else if (result == 'onset') {
    onset_indices <- numeric(length(peak_indices))
    for (i in 1:length(peak_indices)) {
      peak_idx <- peak_indices[i]
      # Scan backwards to find the onset
      onset_idx <- peak_idx
      while (onset_idx > 1 && vector[onset_idx - 1] < vector[peak_idx] && vector[onset_idx - 1] > thresh) {
        onset_idx <- onset_idx - 1
      }
      onset_indices[i] <- onset_idx
    }
    return(onset_indices)
  } else {
    stop("Invalid 'result' parameter value. Use 'center' or 'onset'.")
  }
}

### align wav (q1) to template (idx)
align_to_template = function(q1,idx=NULL,t1=NULL,maxlen=21368275){
 s = seq(0,30*44100,1000)
 if(!is.null(t1) & is.null(idx)){
  idx = which(abs(t1@left)>20000)
  idx = (idx-idx[1])[-1]
  idx = idx[c(TRUE,diff(idx) > 10)]
 }
 
 chk = sapply(s,function(x,y) sum(y[idx + x]),abs(q1@left))
 #s[which.max(chk)]
 ### scan +/- 1000 samples of the best coarse match
 sfine = c(-1000:1000) + s[which.max(chk)]
 chkfine = sapply(sfine,function(x,y) sum(y[idx + x]),abs(q1@left))
 best_fine = sfine[which.max(chkfine)]
 interval = best_fine:(best_fine+maxlen)
 print(paste("start estimated at",round(best_fine/44100,2),"seconds"))
 return(q1[interval])
}

### function to get reading times
### q1a is aligned "query" WAV/MP3
### ann is the "annotation" of the template
reading_time = function(q1a,ann,plot.it=TRUE,drop_before=250){
 r = q1a[ann[[1]][50]:ann[[2]][99]]
 r = abs(r@left) + abs(r@right)
 words = rep(1:50,each=44100*1.2)
 ll = tapply(r[1:length(words)],words,function(x) lowess(x,f=1/10)$y[seq(1,52920,length.out=1200)])
 foo = do.call('rbind',ll)
 foo[foo<(median(r,na.rm=T)+mad(r,na.rm=T))] = 0
 front0 = apply(foo,1,function(x) which(x==0)[1])
 front0[is.na(front0)] = 1
 for(i in 1:50){
  foo[i,1:front0[i]] = 0
  }
 rownames(foo) = ann$prompt[50:99]
 foo[,1:(drop_before-1)] = 0
 if(plot.it){
 img(foo,xlab="milliseconds",do.breaks=F)
  abline(h=c(10,20,30,40),col='white')
  points(apply(foo,1,function(x) which(x>0)[1]),(1:50)-0.5,col='white')
  axis(1,at=c(0,300,600,900,1200),labels=c(0,300,600,900,1200))
  mtext(1,at=apply(foo,1,function(x) which(x>0)[1]),text="|",col='grey')
 }
 starts = apply(foo,1,function(x) which(x>0)[1])
 out = c(median=median(starts),mad=mad(starts))
 return(starts)
 return(out)
}


## auditory-oral reaction time
ao_rxn = function(q1a,ann,thresh=2000,plot.it=TRUE){
 r = normalize(q1a[ann[[1]][14]:ann[[2]][18]],unit="16")
 r = abs(r@left) + abs(r@right)
 words = rep(1:5,ann[[1]][15:19]-ann[[1]][14:18])[1:length(r)]
 ll = tapply(r[1:length(words)],words,function(x) approx(lowess(x,f=1/100)$y,xout=seq(1,44100*2,length.out=2000))$y)
 foo = do.call('rbind',ll)
 foo[is.na(foo)] = 0
 #print(range(foo))
 #return(foo)
 peaks = apply(foo,1,function(x) diff(find_peaks(x,thresh=thresh)[1:2]))
 # xx=peaks[1]
 # while (xx<100) {
 #   peaks = apply(foo,1,function(x) diff(find_peaks(x,thresh=thresh-100)[1:2]))
 #   xx=peaks[1]
 # }
 #print(peaks)
 if(sum(is.na(peaks))>1) stop("not enough peaks detected...maybe adjust threshold?")
 out = c(
  median=median(peaks,na.rm=T),
  mad=mad(peaks,na.rm=T)
 )
 if(plot.it){
  img(foo)
  abline(v=out[1]+c(-3*out[2],0,3*out[2]),lty=2,col='white')
 }
 return(out)
}

## visual-oral reaction time
vo_rxn = function(q1a,ann,thresh=2000,plot.it=T){
 r = normalize(q1a[ann[[1]][8]:ann[[2]][12]],unit="16")
 r = abs(r@left) + abs(r@right)
 words = rep(1:5,ann[[1]][9:13]-ann[[1]][8:12])[1:length(r)]
 ll = tapply(r[1:length(words)],words,function(x) approx(lowess(x,f=1/100,delta=100)$y,xout=seq(1,44100*2,length.out=2000))$y)
 foo = do.call('rbind',ll)
 foo[is.na(foo)] = 0
 peaks = apply(foo,1,function(x) find_peaks(x,thresh=thresh,result="onset")[1])
 # xx=peaks[1]
 # while (xx<100) {
 #   thresh = thresh - 100
 #   peaks = apply(foo,1,function(x) diff(find_peaks(x,thresh)[1:2]))
 #   xx=peaks[1]
 #   print(paste0("tring threshold: ", thresh))
 # }
 if(sum(is.na(peaks))>1) stop("not enough peaks detected...maybe adjust threshold?")
 out = c(
  median=median(peaks,na.rm=T),
  mad=mad(peaks,na.rm=T)
 )
 if(plot.it){
  img(foo)
  abline(v=out[1]+c(-3*out[2],0,3*out[2]),lty=2,col='white')
 }
 return(out)
}

###################################################################
###################################################################
###################################################################
# p.mp3 <- readMP3()
# q1a = align_to_template(q1,idx=idx)
# plot(q1a[ann[[1]][4]:ann[[2]][4]])
# plot(q1a[ann[[1]][50]:ann[[2]][99]])
# abline(v=ann$seconds[50:99]-424.5,col='red')
# 
# reading_time(q1a,ann)
# abline(v=231,col='white',lty=2)
# abline(v=270,col='white',lty=2)
# 
# ### reaction time
# plot(q1a[ann[[1]][8]:ann[[2]][12]])
# abline(v=ann$seconds[8:12]-41.71,col='red')
# plot(q1a[ann[[1]][14]:ann[[2]][18]])
# abline(v=ann$seconds[14:18]-55.8,col='red')
# 
# vo_rxn(q1a,ann)
# ao_rxn(q1a,ann)
# 
# #ff = list.files("v3/",full.names=T,pattern="mp3")
# #qs = lapply(ff,readMP3)
# #qsa = lapply(qs,align_to_template,idx)
# 
# ff = list.files("/wdata/msmuhammad/projects/RPOE/language/data/raw/ISS",full.names=T,pattern="mp3")
# qs = lapply(ff,readMP3)
# qsa = lapply(qs,align_to_template,idx)[51:66] ### I only know these to be v1.10
# 
# visual = sapply(c(5000,2000,1000,500,200),function(y) as.numeric(sapply(qsa,function(x) try(vo_rxn(x,ann,y,F),silent=T)[1])))
# auditory = sapply(c(5000,2000,1000,500,200),function(y) as.numeric(sapply(qsa,function(x) try(ao_rxn(x,ann,y,F),silent=T)[1])))
# 
# 
# plot(auditory[,3],visual[,2],ylim=c(0,600),xlim=c(0,600),ylab="visual-vocal RT (median ms)",
#      xlab="auditory-vocal RT (median ms)")
# abline(0,1,col='grey')
# abline(lm(visual[,2]~auditory[,3]),col='orangered',lty=2)


###################################################################
###################################################################