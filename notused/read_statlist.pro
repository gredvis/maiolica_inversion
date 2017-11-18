;+
; NAME:
;
;   READ_STATLIST
;
; PURPOSE:
;
;    Read list of GAW or TTORCH stations etc.
;
; CATEGORY:
;
;    ESA GHG CCI
;
; CALLING SEQUENCE:
;
;    statstr=read_statlist(/co2,/ch4,/ttorch,/gaw)
;
; INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;    /co2: set this keyword to read list of continuous CO2 stations
;    /ch4: continuous CH4 stations
;    /ttorch: TTORCH tall tower sites
;    /gaw: GAW sites only (43 sites)
;    /all: Read all sites
;    /flex: Read sites used as receptors in the MAIOLICA-2 simulations
;
; OUTPUTS:
;
;      statstr : structure array
;              {name:'', ; the station code
;               lon:0.,  ; station longitude
;               lat:0.,  ; station latitude
;               elev:0.} ; station height above sea level 
;
; COMMON BLOCKS:
;
;  none
;
; SIDE EFFECTS:
;
;  none
;
; RESTRICTIONS:
;
;  none
;
; PROCEDURE:
;
;  none
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
; 
;   (c) Dominik Brunner
;   Swiss Federal Laboratories for Materials Science and Technology
;   Empa Duebendorf, Switzerland
;
;   DB, 10 Nov 2012:  first implementation
;
;-
FUNCTION read_statlist,co2=co2,ch4=ch4,ttorch=ttorch,gaw=gaw,flex=flex,all=all

; get stations
  allsites=0
  line=''
  iname=0 & ilat=1 & ilon=2 & ielev=3

  ;basedir = '/home/brd134/IDL/esa_cci/'
  basedir = '/home/arf/pers/IDL/urmel/INVERSION/FINAL/'

  CASE 1 OF
     keyword_set(co2):    statfile=basedir + 'co2_processing/statlist_co2.txt'
     keyword_set(ch4):    statfile=basedir + 'ch4_processing/statlist.txt'
     keyword_set(ttorch): statfile=basedir + 'co2_processing/statlist_ttorch.txt'
     keyword_set(gaw):    statfile=basedir + 'co2_processing/statlist_gaw.txt'
     keyword_set(all): BEGIN
        statfile=basedir+'all_sites_ext.txt'
        allsites=1
        ilongname=1 & ilat=2 & ilon=3 & ielev=4
     END
     keyword_set(flex): BEGIN
        statfile=basedir+'flexpart_receptor_sites_20150109.csv'
        allsites=1
        ilongname=1 & ilat=3 & ilon=2 & ielev=4
        iecmwf_elev=5
     END
     ELSE: BEGIN
        message,'no keyword set',/continue
        RETURN,-1
     END
  ENDCASE

  nstat=file_lines(statfile)
  IF keyword_set(flex) THEN BEGIN
     rec = {name:'',longname:'',lat:0.,lon:0.,elev:0.,ecmwf_elev:0.,contri:'',type:'',title:'',stind:0}
  ENDIF ELSE BEGIN
     rec = {name:'',longname:'',lat:0.,lon:0.,elev:0.,contri:'',type:'',title:'',stind:0}
  ENDELSE
  statstr = replicate(rec,nstat)
  
  openr, lun, statfile, /get_lun
  IF keyword_set(flex) THEN BEGIN
     tab = ',' 
     ;; read header line
     readf,lun,line
     nstat = nstat - 1
  ENDIF ELSE BEGIN
     tab=STRING(9B)
  ENDELSE
  FOR i=0,nstat-1 DO BEGIN
     readf, lun, line
     IF keyword_set(flex) THEN line = strjoin(strsplit(line,'"',/extract),'')
     a=strsplit(line, tab, /extract)
     statstr[i].name=a[iname]
     IF allsites THEN statstr[i].longname=a[ilongname]
     statstr[i].lat=float(a[ilat])
     statstr[i].lon=float(a[ilon])
     statstr[i].elev=float(a[ielev])
     ;; additional information needed for Tina's WDCGG reading routines
     found = 1
     stind = 0
     type = 'hourly'
     CASE strlowcase(statstr[i].name) OF
        'mnm': BEGIN 
           title  = 'Minamitorishima, Japan'
           contri = 'jma'
        END
        'alt': BEGIN 
           title  = 'Alert, Canada'
           contri = 'ec'
        END
        'brw': BEGIN
           title  = 'Barrow, Alaska'
           contri = 'noaa'
        END
        'cpt': BEGIN 
           title  = 'Cape Point, South Africa'
           contri = 'saws'
        END
        'yon': BEGIN 
           title  = 'Yonagunijima, Japan' 
           contri = 'jma'
        END
        'mlo': BEGIN 
           title  = 'Mauna Loa, Hawaii' 
           contri = 'noaa'
           stind  = 12
        END
        'gsn': BEGIN
           title  = 'Gosan, South Korea'
           contri = 'gerc'
        END
        'ryo': BEGIN
           title  = 'Ryori, Japan'
           contri = 'jma'
        END
        'amy': BEGIN
           title  = 'Anmyeon-do, South Korea'
           contri = 'kma'
        END
        'izo': BEGIN
           title  = 'Izana, Spain'
           contri = 'aemet'
        END
        'jfj': BEGIN
           title  = 'Jungfraujoch, Switzerland'
           contri = 'empa'
           stind  = 6
        END
        'ngl': BEGIN
           title  = 'Neuglobsow, Germany'
           contri = 'ubag'
        END
        'prs': BEGIN
           title  = 'Plateau Rosa, Italy'
           contri = 'rse'
           stind  = 6
        END
        'deu': BEGIN
           title  = 'Deuselbach, Germany'
           contri = 'ubag'
        END
        'kmw': BEGIN
           title  = 'Kollumerwaard, Netherlands'
           contri = 'rivm'
        END 
        'ssl': BEGIN
           title  = 'Schauinsland, Germany'
           contri = 'ubag'
           stind  = 2
        END
        'zsf': BEGIN
           title  = 'Zugspitze, Germany'
           contri = 'ubag'
           stind  = 4
        END
        'amy': BEGIN
           title  = 'Anmyeon-do, South Korea'
           contri = 'kma'
           stind  = 0
        END
        'gsn': BEGIN
           title  = 'Gosan, South Korea'
           contri = 'gerc'
        END
        'cdl': BEGIN
           title  = 'Candle Lake, Canada'
           contri = 'ec'
        END
        'etl': BEGIN
           title  = 'East Trout Lake, Canada'
           contri = 'ec'
        END
        'fsd': BEGIN
           title  = 'Fraserdale, Canada'
           contri = 'ec'
        END
        'egb': BEGIN
           title  ='Egert, Canada'
           contri = 'ec'
        END
        'wsa': BEGIN
           title  ='Sable Island, Canada'
           contri = 'ec'
        END
        'bik': BEGIN
           title = 'Bialystok, Poland'
           contri = 'INGOS'
        END
        'cbw': BEGIN
           title = 'Cabauw, Netherlands'
           contri = 'INGOS'
        END
        'gif': BEGIN
           title = 'Gif sur Yvette, France'
           contri = 'INGOS'
        END
        'hei': BEGIN
           title = 'Heidelberg, Germany'
           contri = 'INGOS'
        END
        'hun': BEGIN
           title = 'Hegyhatsal, Hungary'
           contri = 'INGOS'
        END
        'ipr': BEGIN
           title = 'Ispra, Italy'
           contri = 'INGOS'
        END
        'jfj': BEGIN
           title = 'Jungfraujoch, Switzerland'
           contri = 'INGOS'
        END
        'kas': BEGIN
           title = 'Kasprowy Wierch, Poland'
           contri = 'INGOS'
        END
        'mhd': BEGIN
           title = 'Mace Head, Ireland'
           contri = 'INGOS'
        END
        'oxk': BEGIN
           title = 'Ochsenkopf, Germany'
           contri = 'INGOS'
        END
        'pal': BEGIN
           title = 'Pallas, Finland'
           contri = 'INGOS'
        END
        'ssl': BEGIN
           title = 'Schauinsland, Germany'
           contri = 'INGOS'
        END
        'trn': BEGIN
           title = 'Trainou, France'
           contri = 'INGOS'
        END
        'tta': BEGIN
           title = 'TTA, XXX'
           contri = 'INGOS'
        END
        'amt': BEGIN
           title = 'Argyle, USA'
           contri = 'NOAA-GMD'
        END
        'bao': BEGIN
           title = 'Boulder Atmospheric Observatory, USA'
           contri = 'NOAA-GMD'
        END
        'lef': BEGIN
           title = 'Park Falls, USA'
           contri = 'NOAA-GMD'
        END
        'sct': BEGIN
           title = 'Beach Island South Carolina Tower, USA'
           contri = 'NOAA-GMD'
        END
        'str': BEGIN
           title = 'San Francisco Sutro Tower, USA'
           contri = 'NOAA-GMD'
        END
       'snp': BEGIN
           title = 'Shenandoah National Park, USA'
           contri = 'NOAA-GMD'
        END
        'wbi': BEGIN
           title = 'West Branch, USA'
           contri = 'NOAA-GMD'
        END
        'wgc': BEGIN
           title = 'Walnut Grove, USA'
           contri = 'NOAA-GMD'
        END
        'wkt': BEGIN
           title = 'Moody WKT Tower, USA'
           contri = 'NOAA-GMD'
        END
        'zot': BEGIN
           title = 'Zotto, Russia'
           contri = 'MPI-Jena'
        END
        ELSE: found = 0
     ENDCASE
     IF found THEN BEGIN
        statstr[i].contri = STRUPCASE(contri)
        statstr[i].type =type
        statstr[i].title = title
        statstr[i].stind = stind
     ENDIF

     IF keyword_set(flex) THEN BEGIN
        statstr[i].name = strlowcase(statstr[i].name)
        statstr[i].ecmwf_elev = float(a[iecmwf_elev])
     ENDIF
  ENDFOR
  free_lun, lun

  return,statstr

END
