txt.main <-"
# Script: IWATCH-reader-006.R
# Created: January  9, 2021
# Revised: January 10, 2021
# Name   : Claus E. Andersen (clan@dtu.dk)

# Use cases of this script: 

# 1. Visualize certain radiation physics interactions for teaching such as:
#    (a) Impact of magnetic field on individual electrons.
#    (b) Electrons passing through air cavity vs. water cavity.

# 2. Illustrate EGSnrc for debugging and teachning purposes such as:
#    (a) The stack (NP)
#    (b) What happens when Estep is changed?
#    (c) Do electrons go straight if we stop straggling?
#    (d) What paths will be taken if we enforce CSDA?

# 3. Compute certain interesting features like:
#    (a) Projected or maximum range of electrons.
#    (b) Electron backscatter fraction.

# Background:
# The RZ-user codes in EGSnrc includes an option to create files with particle/history information.
# Select the I/O-control, and set iwatch = graph. Minimize the number of histories to 1000 or such
# (as the files can become very large).

# The files read by this script have the extension egsgph.

# Note that we have NP (= stack counter) in the egsgph file. We take the
# interpretation that this is the 'generation'. This may not be 
# correct.

# We compute the loss of energy at each point (dE = E - lead(E)). This value is also 
# subject to interpretation.  

# Looking at the nrcaux.mortran code, we can identify the format of 
# the egsgph-file: 

# IF((IWATCH = 4) & (IARG >= 0) & (IARG ~= 5)) [ 'GRAPHICS OUTPUT'
# IF( graph_unit < 0 ) graph_unit = egs_open_file(ku,kr,ka,'.egsgph');
# WRITE(graph_unit,:GRAPHICS_FORMAT:) NP,IQ(NP),IR(NP),X(NP),Y(NP),Z(NP),E(NP);
# :GRAPHICS_FORMAT:FORMAT(2I4,1X,I6,4G15.8,I12);

# There is also a special line with zeros and the value of JHSTRY:
# IF(IWATCH = 4) [
#  IF( graph_unit < 0 ) [
#    graph_unit = egs_open_file(ku,kr,ka,'.egsgph');
#  ]
#  WRITE(graph_unit,:GRAPHICS_FORMAT:) 0,0,0,0.0,0.0,0.0,0.0,JHSTRY;
#  JHSTRY=JHSTRY+1;
# ]

# NP = stack counter
# IR = region number
# IQ = charge

# Detail: How did I find that file?
# I ran the code: 
#    grep -rnw 'EGSnrc/HEN_HOUSE' -e 'egsgph'
# to identify all files containing the text string 'egshph'.
"

require(dplyr)
require(lattice)
require(clanTools)
require(clanLattice)


####################################################################################
# Main user input
####################################################################################
plotid <- "001"
run.note <- "Test case"

main.txt <- "2 MeV electron beam on 1 cm thick Al slab"

# Give directory and file name for the egsgph-file.
path.base <- "~/EGSnrc/egs_home/dosrznrc/"
fn <- "IWATCH100"
fn.ext <- "egsgph"

# Some points are only shown if dE is larger than the cut-energy:
dE.cut.points <- 0.00015

# Only show data for the following histories:
hist.no.selected <- c(1,3,5,17,25,54)

xlim.sel <-c(-0.5,0.5)
ylim.sel <-c(0,1)

cex.lab <- 1.5

plots.wanted <- TRUE

print.details <- FALSE
plot.first.particle <- TRUE
plot.last.particle <- TRUE
plot.deepest.primary.electron <- TRUE

e.primary.wanted <- TRUE
e.primary.point.cex <- 0.5
e.primary.point.pch <- 16
e.primary.point.col <- 'blue'

e.secondary.wanted <- TRUE
e.secondary.point.cex <- 1.0
e.secondary.point.pch <- 4
e.secondary.point.col <- 'black'

ph.primary.wanted <- TRUE
ph.primary.point.cex <- 1
ph.primary.point.pch <- 20
ph.primary.point.col <- 'green'

ph.secondary.wanted <- TRUE
ph.secondary.point.cex <- 1
ph.secondary.point.pch <- 14
ph.secondary.point.col <- 'green'

pos.primary.wanted <- TRUE
pos.primary.point.cex <- 1
pos.primary.point.pch <- 20
pos.primary.point.col <- 'red'

pos.secondary.wanted <- TRUE
pos.secondary.point.cex <- 1
pos.secondary.point.pch <- 14
pos.secondary.point.col <- 'red'

# To convert the electron (tital) to kinetic energy we need:
MeV.rest.electron <- 0.51099895000

plotfilename <- paste("IWATCH-analysis-results",fn,plotid,sep="-")


#######################################################################
# Time of analysis
#######################################################################
time.analysis <- Sys.time()
time.analysis.formated <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")


df.parm <- rbind(
  data.frame(name="Time of analysis",value=as.character(time.analysis.formated),stringsAsFactors=FALSE),
  data.frame(name="Note",value=as.character(run.note),stringsAsFactors=FALSE),
  data.frame(name="Input file name",value=as.character(fn.full),stringsAsFactors=FALSE),
  data.frame(name="Input file folder",value=as.character(path.base),stringsAsFactors=FALSE),
  data.frame(name="Output file",value=as.character(plotfilename),stringsAsFactors=FALSE),
  data.frame(name="Electrons, primary (NP=1). color",  value=as.character(e.primary.point.col),stringsAsFactors=FALSE),
  data.frame(name="Electrons, secondary (NP>1). color",value=as.character(e.secondary.point.col),stringsAsFactors=FALSE),
  data.frame(name="Photons, primary (NP=1). color",    value=as.character(ph.primary.point.col),stringsAsFactors=FALSE),
  data.frame(name="Photons, secondary (NP>1). color",  value=as.character(ph.secondary.point.col),stringsAsFactors=FALSE),
  data.frame(name="Positrons, primary (NP=1). color",  value=as.character(pos.primary.point.col),stringsAsFactors=FALSE),
  data.frame(name="Positrons, secondary (NP>1). color",value=as.character(pos.secondary.point.col),stringsAsFactors=FALSE)
)

N.dots <- 40
df.parm$name <- substring.with.dots(df.parm$name,n=N.dots+1)
N.dots <- 46
df.parm$value <- substring.with.dots(df.parm$value,n=N.dots+1)


close.device.wanted <- FALSE

if(FALSE){
png.fac <- 1.5
png(filename = paste(plotfilename,"%03d.png",sep=""),
   width = png.fac*20, height = png.fac*13, units = "cm", pointsize = png.fac*10,
    bg = "white", res = 600, family = "arial", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png")[1])
close.device.wanted <- TRUE
}


if(!FALSE){
inch.fac <- 2.54 / 1.2
pdf(paste(plotfilename,".pdf",sep=""), width = 29.7/inch.fac, height = 21/inch.fac,pointsize=19,family="Courier")
#dev.off()
close.device.wanted <- TRUE
}

if(FALSE){
postscript(paste(plotfilename,".ps",sep=""),onefile=TRUE) 
close.device.wanted <- TRUE
}

txtplot(txt.main,plot.new=TRUE,cex=0.6)


#############################################	#################
line.height=line.height0 <- 0.022
line.height=line.height0 <- 0.026
PP <- list(0.05,0.97,1)


PP <- txtplot(paste("Main parameters used in this analysis: \n",sep=""), new=TRUE, PP, cex=0.7,line.height=line.height0)
PP <- txtplot(data.frame(df.parm),new=FALSE,PP,cex=0.6,col.sign="",line.height=line.height0)

####################################################################################
# Read file and do data manipulations
####################################################################################
# Read file as text. Why? because it includes a somewhat non-standard foemat.
fn.full <- paste(fn,".",fn.ext,sep="")
path.full <- paste(path.base,fn.full,sep="")
df <- readLines(path.full)


# First extract the fixed-width format data:
xx1 <- substring(df,1,4)
xx2 <- substring(df,5,8)
xx3 <- substring(df,9,15)
xx4 <- substring(df,16,30)
xx5 <- substring(df,31,45)
xx6 <- substring(df,46,60)
xx7 <- substring(df,61,76)
xx8 <- substring(df,77,87)

df2 <- data.frame(generation=as.numeric(xx1),IQ=as.numeric(xx2),IRL=as.numeric(xx3),x=as.numeric(xx4),y=as.numeric(xx5),z=as.numeric(xx6),MeV=as.numeric(xx7),no=as.numeric(xx8))

# First convert missing no to 0.
ok <- is.na(df2$no)
df2$no[ok] <- 0

# Second, make a new coloumn for the particle number
df2$no2 <- rep(0,nrow(df2))

# Third, make a new particle num ber (no2)
ok <- df2$no>0
df2$no2[ok] <- rep(1,sum(ok))
df2$no2 <- cumsum(df2$no2)
df2$no2 <- df2$no2 + 1

# Finally, delete the rows with the old particle numbers.
ok <- df2$no > 0 
df2 <- df2[!ok,]

df2 %>%
select(no2,generation,IQ,IRL,x,y,z,MeV) %>% 
mutate(MeV=ifelse(IQ==-1,MeV-MeV.rest.electron,MeV)) %>%
mutate(r=(x^2 + y^2)^0.5) %>%
mutate(R=(x^2 + y^2 +  z^2)^0.5) %>%rename(hist.no=no2) %>%
data.frame(.) -> df2

df2 %>%
ungroup() %>%
mutate(particle=c(0,abs(diff(generation))+abs(diff(IQ)))) %>%
mutate(particle = ifelse(particle==0,0,1)) %>%
mutate(particle=cumsum(particle)) %>%
data.frame(.) -> df2

df2 %>%
ungroup() %>%
mutate(MeV.old = ifelse(particle==lag(particle),lag(MeV),NA)) %>%
mutate(MeV.next = ifelse(particle==lead(particle),lead(MeV),NA)) %>%
group_by(particle) %>%
mutate(dE.old = MeV - MeV.old) %>%
mutate(dE = MeV - MeV.next) %>%
mutate(dE.old = ifelse(is.na(dE.old),0,dE.old)) %>%
mutate(dE = ifelse(is.na(dE),0,dE)) %>%
data.frame(.) -> df2

# The final results are in df2

####################################################################################
# Plots
####################################################################################
if(plots.wanted){

plt <- xyplot(z~r|paste("no =",hist.no),data=df2,subset=IQ==-1 & generation==1 & hist.no %in% hist.no.selected,type="l",
ylab=list("z [cm]",cex=cex.lab),
xlab=list("r [cm]",cex=cex.lab),
main=main.txt,
scales=list(x="same"),
panel=function(x,y,...){
panel.xyplot(x,y,...)  
panel.points(x[1],y[2],col='black',pch=16,cex=1)
panel.points(last.element(x),last.element(y),col='black',pch=16,cex=0.5)
panel.abline(h=max(y),lty="dashed")
})

print(plt)

plt <- xyplot(z~x,groups=paste("no =",hist.no),data=df2,subset=IQ==-1 & generation==1 & hist.no %in% hist.no.selected,type="b",
       ylab=list("z [cm]",cex=cex.lab),
       xlab=list("x [cm]",cex=cex.lab),
       main=main.txt,
       xlim=xlim.sel,
       ylim=ylim.sel,
       scales=list(x="same"),
       panel=function(x,y,...){
         panel.xyplot(x,y,...)  
         panel.points(x[1],y[2],col='black',pch=16,cex=1)
         panel.points(last.element(x),last.element(y),col='black',pch=16,cex=0.5)
         panel.abline(h=max(y),lty="dashed")
       })

print(plt)


plt <- xyplot(z~x,groups=paste("no =",hist.no),data=df2,subset=IQ==-1 & generation==1 & hist.no %in% hist.no.selected,type="b",
       main=main.txt,
       ylab=list("z [cm]",cex=cex.lab),
       xlab=list("x [cm]",cex=cex.lab),
      scales=list(x="same"),
       panel=function(x,y,...){
         panel.xyplot(x,y,...)  
         panel.points(x[1],y[2],col='black',pch=16,cex=1)
         panel.points(last.element(x),last.element(y),col='black',pch=16,cex=0.5)
         panel.abline(h=max(y),lty="dashed")
       })

print(plt)


plt <- xyplot(MeV~z|paste("no =",hist.no),groups=paste("gen=",generation),data=df2,
       ylab=list("Energy  [MeV]",cex=cex.lab),
       xlab=list("z [cm]",cex=cex.lab),
       main=main.txt,
       subset=hist.no %in% hist.no.selected ,type="p",
       scales=list(x="same"),
       panel=function(x,y,...){
         panel.xyplot(x,y,...)  
         panel.points(x[1],y[2],col='black',pch=16,cex=1)
         panel.points(last.element(x),last.element(y),col='black',pch=16,cex=0.5)
         panel.abline(h=max(y),lty="dashed")
       })

print(plt)


plt <- xyplot(MeV~I((x^2+y^2+z^2)^0.5)|paste("no =",hist.no),groups=paste("no=",generation),data=df2,
       ylab=list("Energy [MeV]",cex=cex.lab),
       xlab=list("(x^2+y^2+z^2)^0.5 [cm]",cex=cex.lab),
       main=main.txt,
       subset=IQ%in%c(-1 ,0)& hist.no%in%hist.no.selected & generation%in%c(1:1),type="l",
       scales=list(x="same"),
       panel=function(x,y,...){
         panel.xyplot(x,y,...)  
         panel.points(x[1],y[2],col='black',pch=16,cex=1)
         panel.points(last.element(x),last.element(y),col='black',pch=16,cex=0.5)
         panel.abline(h=max(y),lty="dashed")
       })

print(plt)


plt <- xyplot(z~x|reorder.for.trellis(paste("no =",hist.no)),
              groups=generation,data=df2,subset= IRL > 0 & hist.no %in% hist.no.selected,type="b",
       scales=list(x="same"),
       main=main.txt,
       ylab=list("z [cm]",cex=cex.lab),
       xlab=list("x [cm]",cex=cex.lab),
       panel=function(x,y,groups,subscripts,...){
         df0 <- df2[subscripts,]

         if(print.details){print(df0)}
         
         # Primary electrons
         ok <- df0$IQ==-1 & df0$generation==1
         if(e.primary.wanted && sum(ok)>0){
            particle.vec <- unique(df0$particle[ok])
            for(particle0 in particle.vec){
             ok <- df0$particle==particle0
             panel.points(df0$x[ok],df0$z[ok],type="l",col="blue")
             ok2 <- ok & abs(df0$dE)>=dE.cut.points
             panel.points(df0$x[ok2],df0$z[ok2],type="b",col=e.primary.point.col,cex=e.primary.point.cex,pch=e.primary.point.pch)
           }
         }

                      
           
         # Secondary electrons
         ok <- df0$IQ==-1 & df0$generation>1
         if(e.secondary.wanted && sum(ok)>0){
           particle.vec <- unique(df0$particle[ok])
           for(particle0 in particle.vec){
             ok <- df0$particle==particle0
             panel.points(df0$x[ok],df0$z[ok],type="l",col="black")
             ok2 <- ok & df0$dE>=dE.cut.points
             panel.points(df0$x[ok2],df0$z[ok2],type="p",col=e.secondary.point.col,cex=e.secondary.point.cex,pch=e.secondary.point.pch)
           }
         }
         
         
        # Photons
         ok <- df0$IQ==0  & df0$generation==1
         if(ph.primary.wanted && sum(ok)>0){
            particle.vec <- unique(df0$particle[ok])
            for(particle0 in particle.vec){
             ok <- df0$particle==particle0
             panel.points(df0$x[ok],df0$z[ok],type="b",col=ph.primary.point.col,cex=ph.primary.point.cex,pch=ph.primary.point.pch)
             }
         }
         
         
         # Secondary photons
         ok <- df0$IQ==0 & df0$generation>1
         if(ph.secondary.wanted && sum(ok)>0){
           particle.vec <- unique(df0$particle[ok])
           for(particle0 in particle.vec){
             ok <- df0$particle==particle0
             panel.points(df0$x[ok],df0$z[ok],type="l",col=ph.secondary.point.col)
             ok2 <- ok & df0$dE>=dE.cut.points
             panel.points(df0$x[ok2],df0$z[ok2],type="p",col=ph.secondary.point.col,cex=ph.secondary.point.cex,pch=ph.secondary.point.pch)
           }
         }
  
         
         
         # Primary positrons
         ok <- df0$IQ==1 & df0$generation==1
         if(e.primary.wanted && sum(ok)>0){
           particle.vec <- unique(df0$particle[ok])
           for(particle0 in particle.vec){
             ok <- df0$particle==particle0
             panel.points(df0$x[ok],df0$z[ok],type="l",col=pos.primary.point.col)
             ok2 <- ok & abs(df0$dE)>=dE.cut.points
             panel.points(df0$x[ok2],df0$z[ok2],type="b",col=pos.primary.point.col,cex=pos.primary.point.cex,pch=pos.primary.point.pch)
           }
         }
         
         
         
         # Secondary positrons
         ok <- df0$IQ==1 & df0$generation>1
         if(e.secondary.wanted && sum(ok)>0){
           particle.vec <- unique(df0$particle[ok])
           for(particle0 in particle.vec){
             ok <- df0$particle==particle0
             panel.points(df0$x[ok],df0$z[ok],type="l",col=pos.secondary.point.col)
             ok2 <- ok & df0$dE>=dE.cut.points
             panel.points(df0$x[ok2],df0$z[ok2],type="b",col=pos.secondary.point.col,cex=pos.secondary.point.cex,pch=pos.secondary.point.pch)
           }
         }
         
         
       
        if(plot.first.particle){          
         panel.points(x[1],y[1],col='black',pch=16,cex=1)
        }
         
         if(plot.last.particle){
         panel.points(last.element(x),last.element(y),col='black',pch=16,cex=0.5)
         }
         
         ok <- df0$IQ==-1 & df0$generation==1
         if(plot.deepest.primary.electron & sum(ok) > 0) {
           panel.abline(h=max(df0$z[ok],na.rm=TRUE),lty="dashed")
         }
          
        })


print(plt)

plt <- update(plt,xlim=xlim.sel,ylim=ylim.sel)

print(plt)

} # plots.wanted block


# Close device (normally this will close the pdf file with plots)
if(close.device.wanted) dev.off()
print(paste(plotfilename,"was closed (dev.off()."))

####################################################################################
# Projected and maximum range computations (if meaningful)
####################################################################################
# Computation of the projected range
# Requires that the bean is in the z-direction and that the
# slab is sufficiently thick.
# Some workers state that CSDA is required.
df2 %>%
filter(IQ==-1) %>%
filter(generation==1) %>%
group_by(particle) %>%
summarize(z.proj=max(z)) %>%
ungroup() %>%
summarize(z.proj.min=min(z.proj),z.proj.max=max(z.proj),z.proj.mean=mean(z.proj),z.proj.sd=sd(z.proj),N=n()) -> df.projected


print(head(df2,12))

df.projected

df.projected$z.proj.mean 
df.projected$z.proj.max 



  