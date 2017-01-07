FUNCTION station_info,statid
  
  statrec = {id:'',name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
  
  CASE strlowcase(statid) OF
     'mnm': statrec={id:statid,name:'Minamitorishima',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'yon': statrec={id:statid,name:'Yonaginijima',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'alt': statrec={id:statid,name:'Alert',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'jfj': statrec={id:statid,name:'Jungfraujoch',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} 
     'brw': statrec={id:statid,name:'Barrow',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} 
     'smo': statrec={id:statid,name:'Samoa',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'izo': statrec={id:statid,name:'Izana',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'cpt': statrec={id:statid,name:'Cape Point',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'wsa': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Sable Island'
     'etl': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'East Trout Lake'
     'egb': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Egbert'
     'mhd': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Mace Head'
     'kmw': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Kollumerwaard'
     'bal': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Baltic Sea'
     'hun': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Hegyhatsal'
     'ngl': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Neuglobsow'
     'ssl': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Schauinsland'
     'bkt': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Bukit Koto Tabang'
     'oxk': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Ochsenkopf'
     'key': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Key Biscayne'
     'ask': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Assekrem'
     'hun': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Heyghatsal'
     'amy': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Anmyeon-do'
     'ams': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Amsterdam Island'
     'nmb': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Namibia'
     'wlg': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Mt. Waliguan, China'
     'sgp': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Southern Great Plains'
     'kzm': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Plateau Assy, Kazakhstan'
     'lef': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Park Falls, USA'
     'bsc': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Black Sea, Romania'
     'puy': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Puy de Dome, France'
     'amt': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Argyle, USA'
     'llb': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Lac La Biche, Canada'
     'tap': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Tae-ahn Peninsula, South Korea'
     'wkt': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Moody, USA'
     'cgo': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Cape Grim, Australia'
     'cfa': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Cape Ferguson, Australia'
     'chr': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'CHR'
     'nwr': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Niwot Ridge, USA'
     'gmi': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'GMI'
     'cba': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Cold Bay'
     'cdl': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'CDL'
     'zgt': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'ZGT'
     'deu': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'DEU'
     'fsd': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'FSD'
     'zsf': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'ZSF'
     'prs': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'PRS'
     'tkb': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'TKB'
     'kzd': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'KZD'
     'kzm': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'KZM'
     'sey': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'Seychelles'
     'cri': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'CRI'
     'rpb': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'RPB'
     'ter': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'TER'
     'mkn': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'MKN'
     'hba': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'HBA'
     'syo': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'SYO'
     'maa': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'MAA'
     'shm': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'SHM'
     'azr': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'AZR'
     'mlo': statrec={id:statid,name:'Mauna Loa',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'asc': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'ASC'
     'psa': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'PSA'
     'thd': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'THD'
     'crz': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'CRZ'
     'tdf': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'TDF'
     'mqa': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'MQA'
     
     'cpt': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'CPT'
     'lpo': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'LPO'
     'zep': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'ZEP'
     'sum': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'SUM'
     'pal': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'PAL'
     'esp': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'ESP'
     'hpb': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'HPB'
     'uum': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'UUM'
     'pdm': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'PDM'
     'bgu': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'BGU'
     'uta': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'UTA'
     'pta': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'PTA'
     'lmp': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'LMP'
     'bmw': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'BMW'
     'bme': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'BME'
     'wis': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'WIS'
     'lln': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'LLN'
     'kum': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'KUM'
     'abp': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'ABP'
     'eic': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'EIC'
     'arh': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'ARH'
     'bhd': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'BHD'
     'cya': statrec={id:statid,name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} ;'CYA'
     ELSE: BEGIN
        message,'station '+statid+' not found',/cont
        RETURN,''
     END

  ENDCASE

  ;; define y-axis range for plotting purposes
  yrange = [1750.,1980.]

  IF statid eq 'chr' THEN yrange = [1600.,1800.]
  IF statid eq 'gmi' THEN yrange = [1600.,1850.]
  IF statid eq 'cgo' THEN yrange = [1600.,1850.]
  IF statid eq 'cpt' THEN yrange = [1600.,1800.]
  IF statid eq 'smo' THEN yrange = [1600.,1850.]
  IF statid eq 'sey' THEN yrange = [1600.,1850.]
  IF statid eq 'nwr' THEN yrange = [1650.,1900.]
  IF statid eq 'alt ' THEN yrange = [1750.,1950.]
  IF statid eq 'hba' THEN yrange = [1600.,1800.]
  IF statid eq 'syo' THEN yrange = [1600.,1800.]
  IF statid eq 'ams' THEN yrange = [1600.,1800.]
  IF statid eq 'maa' THEN yrange = [1600.,1800.]
  IF statid eq 'mlo' THEN yrange = [1650.,1900.]
  IF statid eq 'asc' THEN yrange = [1600.,1800.]
  IF statid eq 'psa' THEN yrange = [1600.,1800.]
  IF statid eq 'nmb' THEN yrange = [1600.,1800.]
  IF statid eq 'crz' THEN yrange = [1600.,1800.]
  IF statid eq 'tdf' THEN yrange = [1600.,1800.]
  IF statid eq 'mqa' THEN yrange = [1600.,1800.]
  IF statid eq 'rpb' THEN yrange = [1650.,1900.]
  IF statid eq 'izo' THEN yrange = [1700.,1950.]
  
  statrec.yrange = yrange

  return,statrec

END
