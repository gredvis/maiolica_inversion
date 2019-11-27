;+
; NAME:
;
;   read_flexpart_header
;
; PURPOSE:
;
;   Read FLEXPART header file for an MAIOLICA2 simulation
;   Output is generated in FLEXPART by the routine concoutput.f
; 
; CATEGORY:
;
;   trajectory, atmospheric transport, dispersion modelling
;
; CALLING SEQUENCE:
;
;  read_flexpart_header,filename=filename,header=header
;
; INPUTS:
;
;       filename: the name of the binary output file
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;       header: the header structure with details on the model, output
;              grid, and release locations and times.
; 
; COMMON BLOCKS:
;
;        none
;
; SIDE EFFECTS:
;
;        none
;
; RESTRICTIONS:
;
;        none
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
; (c) Dominik Brunner, Empa, Switzerland: First implementation March 2007
;                     based on R program read.grid.time by Stephan Henne
;     28 April 2010 (DB):
;                     Bug in reading headers for simulations with multiple
;                     release points fixed, and conc array extended to 5
;                     dimensions (x,y,z,nageclass,nreleasepoint)
;     19 June 2010 (DB):
;                     Bug eliminated which prevented the correct reading of
;                     of files containing wet and/or dry deposition fields.
;                     The fields wd and dd are now returned by the program, too.
;     07 August 2010 (DB):
;                     Latitude order is now reversed not only for COSMO-FLEXPART
;                     output but for all FLEXPART output. Dry and wet depo
;                     fields are now also reversed.
;     23 Nov 2017 (DB):
;                     Changed routine name from read_header_maiolica2 to
;                     read_flexpart_header
;-

;--------------------------------------------------------------------

PRO read_flexpart_header,filename=filename,header=header

date=0L
time=0L
; define some temporary variables for reading
bt13=bytarr(13) & bt12=bytarr(12) & bt3=bytarr(3) ; byte arrays
bt4=bytarr(4) & bt40=bytarr(40)
l1=0L & l2=0L & l3=0L           ; long integer numbers
s1=0                            ; short integer
f1=0. & f2=0. & f3=0. & f4=0. & f5=0. & f6=0.     ; floating point numbers

; open the file
openr,lun,filename,/get_lun,/f77_unformatted

; read date and time information
readu,lun,date,time,bt13
model=string(bt13)
isflex8 = STRMID(model,0,11) EQ 'FLEXPART V8'
iscosmo = STRMID(model,0,13) EQ 'FLEXPART V8.C'
yyyymmdd=STRTRIM(date,2)
hhmmss=STRTRIM(time,2)
s=STRLEN(hhmmss)
IF s LT 6 THEN hhmmss=string(bytarr(6-s)+48B)+hhmmss
dtg=yyyymmdd+hhmmss

; read info on output interval, averaging time, sampling time
loutstep=0L & loutaver=0L & loutsample=0L
readu,lun,loutstep,loutaver,loutsample

; read info on output grid setup
outlon0=0. & outlat0=0. & numxgrid=0L & numygrid=0L & dxout=0. & dyout=0.
p0lon=0. & p0lat=90.

IF iscosmo THEN BEGIN
   readu,lun,outlon0,outlat0,numxgrid,numygrid,dxout,dyout,p0lon,p0lat
   IF p0lat NE 90. OR p0lon NE 0. THEN $
      print,'grid in rotated COSMO coordinates pollon/pollat =',p0lon,p0lat
ENDIF ELSE BEGIN
   readu,lun,outlon0,outlat0,numxgrid,numygrid,dxout,dyout
ENDELSE

numzgrid=0L
point_lun,-lun,pos
readu,lun,numzgrid
outheight=FltArr(numzgrid)
point_lun,lun,pos
readu,lun,numzgrid,outheight
readu,lun,date,time
yyyymmdd=STRTRIM(date,2)
hhmmss=STRTRIM(time,2)
s=STRLEN(hhmmss)
IF s LT 6 THEN hhmmss=string(bytarr(6-s)+48B)+hhmmss

; Read number of species, and name for each species (+extra name for depositions)
; Indicate the dimension of the fields (i.e., 1 for deposition fields, numzgrid for
; concentration fields
nspec=0L
IF isflex8 THEN BEGIN
   maxpointspec_act = 0L
   readu,lun,nspec,maxpointspec_act
ENDIF ELSE BEGIN
   readu,lun,nspec
ENDELSE
nspec=nspec/3
bt8=BytArr(8) & bt10=BytArr(10) & bt18=BytArr(18)
species=StrArr(nspec)

FOR i=0,nspec-1 DO BEGIN
   readu,lun,l1,bt10 ;& print,string(bt10)
   readu,lun,l1,bt10 ;& print,string(bt10)
   readu,lun,l1,bt18 & species[i]=string(bt18) ;& print,string(bt8)
ENDFOR

; Read information on release points: total number, then for each point:
; start, end, coordinates, # of particles, name, mass
numpoint=0L
readu,lun,numpoint
relstart=StrArr(numpoint) & relend=StrArr(numpoint) & relkindz=LonArr(numpoint)
rellng1=FltArr(numpoint) & rellat1=FltArr(numpoint) 
rellng2=FltArr(numpoint) & rellat2=FltArr(numpoint) 
relzz1=FltArr(numpoint) & relzz2=FltArr(numpoint) 
npart=LonArr(numpoint) & relcom=StrArr(numpoint)

; get 3D release domain of each release point
; Note: kindz = 2 if z-coordinate is in "m ASL", otherwise z-coordinate is in "m AMG"
FOR i=0,numpoint-1 DO BEGIN
 ;  IF isflex8 THEN BEGIN
      ; start and end of release relative to simulation start/end
      readu,lun,l1,l2,s1
      relkindz[i]=s1
 ;  ENDIF ELSE BEGIN
 ;     readu,lun,l1,l2,l3
 ;     relkindz[i]=l3
 ;  ENDELSE
   relstart[i]=gvtime2dtg(dtg2gvtime(dtg)+l1/86400D)
   relend[i]=gvtime2dtg(dtg2gvtime(dtg)+l2/86400D)
   readu,lun,f1,f2,f3,f4,f5,f6
   rellng1[i]=f1 & rellat1[i]=f2
   rellng2[i]=f3 & rellat2[i]=f4
   relzz1[i]=f5 & relzz2[i]=f6
   readu,lun,l1,l2 & npart[i]=l1
   readu,lun,bt40 & relcom[i]=strtrim(bt40,2)
   ;; now a strange loop supposed to read in each species mass
   FOR k=0,nspec-1 DO BEGIN
      readu,lun,f1
      readu,lun,f2
      readu,lun,f3
   ENDFOR
ENDFOR

; read information on some model switches (method, lsubgrid,
; lconvection)
method=0L & lsubgrid=0L & lconvection=0L & ind_source=0L & ind_receptor=0L
IF isflex8 THEN readu,lun,method,lsubgrid,lconvection,ind_source,ind_receptor $
ELSE readu,lun,method,lsubgrid,lconvection

; read age class information
nageclass=0L
point_lun,-lun,pos
readu,lun,nageclass
lage=LonArr(nageclass)
point_lun,lun,pos
readu,lun,nageclass,lage

; read topography
topo=FltArr(numxgrid,numygrid)
rec=FltArr(numygrid)
FOR i=0,numxgrid-1 DO BEGIN
   readu,lun,rec
   topo[i,*]=rec
ENDFOR
   
free_lun,lun

header={ model:model,$
         numxgrid:numxgrid,$
         numygrid:numygrid,$
         numzgrid:numzgrid,$
         dxout:dxout,$
         dyout:dyout,$
         outheight:outheight,$
         numpoint:numpoint,$
         nspec:nspec,$
         species:species,$
         relcom:relcom,$
         npart:npart,$
         rellat1:rellat1,$
         rellat2:rellat2,$
         rellng1:rellng1,$
         rellng2:rellng2,$
         relzz1:relzz1,$
         relzz2:relzz2,$
         relkindz:relkindz,$
         relstart:relstart,$
         relend:relend,$
         nageclass:nageclass,$ 
         outlon0:outlon0,$
         outlat0:outlat0,$
         loutstep:loutstep,$
         loutaver:loutaver,$
         loutsample:loutsample,$
         method:method,$
         lsubgrid:lsubgrid,$
         lconvection:lconvection,$
         ind_source:ind_source,$
         ind_receptor:ind_receptor,$
         p0lon:p0lon,$
         p0lat:p0lat,$
         dtg:dtg,$
         topo:topo}

END


