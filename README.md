*******************************************************************************
Note: the program was developed in IDL 8.0, it should be executed in 8.0 or upper version of IDL programing platform. This program includes 4 subroutines, which were list in this document.
The Savitzky-Golay filter and linear-interpolation functions were embed in IDL function library.
Users are expected to have sufficient knowledge in remote sensing image processing, and basic of IDL programing. 
To execute this IDL program, these 4 subroutines are put in the same document folder; call the Main procedure PRO TEST_POINT to run the program.

Functionalities: 
 

Enjoy your research! (^_^)
*******************************************************************************

(1)	Main procedure:

pro test_point 
    file = 'E:\test.img'  ;set your time-series remote sensing data to be processed. 
    headinfo=read_ENVIHdr(file)
    imgdata = read_Hdrimage(file,headinfo)
    ns = headinfo.samples
    nl = headinfo.lines
    nb = headinfo.bands
    map_info = headinfo.mapinfo 
    wavelength = headinfo.wavelength
    ;help,imgdata
    savgolfilter=savgol(16,16,0,4)    ;set your smoothing parameters for S-G filter
    New_savgolfilter=dblarr(1,1,33)
    New_savgolfilter[0,0,*]=savgolfilter
    outname = 'E:\try\test365_NAN'
    openw,lun,outname,/get_lun
    
    wtlb = WIDGET_BASE(map = 0)
    WIDGET_CONTROL,wtlb,/Realize
    CENTERTLB,wtlb
    process = Idlitwdprogressbar(GROUP_LEADER=wTlb,TIME=0,TITLE='')
    Idlitwdprogressbar_setvalue, process, 0
    aa = double(99)/nl    
    
    for i=0, nl-1 do begin
       for j=0, ns-1 do begin
          imgdata[j,i,*] = convol(imgdata[j,i,*],New_savgolfilter,/EDGE_TRUNCATE)
          ;print,imgdata[j,i,*]
          tmpdata = interpol(imgdata[j,i,*],365,/LSQUADRATIC) ;set your interpolation strategy
       
          ;help,tmpdata
          writeu,lun,tmpdata
       endfor
       WAIT,.5
       Idlitwdprogressbar_setvalue, process,1+aa*i
    endfor
    free_lun, lun
    
    WIDGET_CONTROL,process,/Destroy
    WIDGET_CONTROL,wTlb, /Destroy
    
    openw,unit,outname+'.hdr',/get_lun
    printf,unit,'ENVI'
    printf,unit,'description = {}'
    printf,unit,'samples = '+ strcompress(ns,/remove)
    printf,unit,'lines = '+ strcompress(nl,/remove)
    printf,unit,'bands = '+ strcompress(365,/remove)
    ;printf,unit,'header offset = 0'
    printf,unit,'file type = ENVI Standard'
    printf,unit,'data type = 4'
    printf,unit,'interleave = bip'
    printf,unit,'sensor type = Unknown'
    
    free_lun,unit
    ;print,'end!!!'
    Result = DIALOG_MESSAGE("END!!",/INFORMATION)        
    
End


(2)	Read ENVI image dataset (1)

FUNCTION read_ENVIHdr,infile
  header = strsplit(infile,'.',/extract)
  CASE N_ELEMENTS(header) OF
    1: header = header+'.hdr'
    2: header = header[N_ELEMENTS(header)-2]+'.hdr'
    ELSE:BEGIN
    ok=DIALOG_MESSAGE('Header files are missing!'+STRING(13B)+'Please reselect files', title='Errors in files',/error)
    RETURN,-1
  ENDELSE
ENDCASE
PRINT,header
status=FILE_TEST(header)
IF(~status) THEN  BEGIN
  PRINT,status
  ok=DIALOG_MESSAGE('Header files do not exist!' + STRING(13B)+'',title='Error in selecting files',/error)
  RETURN,-1
ENDIF
OPENR, unit, header, /get_lun
Hdr_Label=''
READF, unit, Hdr_Label
Hdr_Label= STRLOWCASE(Hdr_Label);
Hdr_Label=STRTRIM(Hdr_Label);
IF( Hdr_Label NE 'envi') THEN BEGIN
  temp = DIALOG_MESSAGE('Errors in file format！',/Warning)
  RETURN,-1
ENDIF
header_line = ''
band_name=''
wavelength=''
WHILE NOT EOF(unit) DO BEGIN
  READF, unit, header_line
  argument=''
  argument=STRMID(header_line,0,STRPOS(header_line,'='))
  argument=STRTRIM(argument,2);
  argument=STRLOWCASE(argument)
  tmp = STRSPLIT(header_line, '=', /extract)
  IF argument EQ 'samples' THEN samples  = LONG(tmp[1])
  IF argument EQ 'lines' THEN lines = LONG(tmp[1])
  IF argument EQ 'bands' THEN bands = LONG(tmp[1])
  IF argument EQ 'header offset' THEN offset = LONG(tmp[1])
  IF argument EQ 'data type' THEN type = LONG(tmp[1])
  IF argument EQ 'interleave' THEN interleave =STRING(tmp[1])
  IF argument EQ 'wavelength units ' THEN wavelengthunits  =STRING(tmp[1])
  IF argument EQ 'band names' THEN BEGIN
    count=1
    WHILE (N_ELEMENTS(band_name) NE (bands+1))DO BEGIN
      IF(N_ELEMENTS(band_name) EQ (bands+1)) THEN BEGIN
        BREAK
      ENDIF
      IF(STRPOS(header_line,'}') NE -1) THEN BEGIN
        currline=STRMID(header_line,0,STRPOS(header_line,'}',/REVERSE_SEARCH))
        IF(STRPOS(currline,',') NE -1) THEN BEGIN
          tmp_bandName =STRTRIM(STRSPLIT(currline, ',', /extract),2)
          band_name=[band_name,tmp_bandName]
        ENDIF ELSE  BEGIN
          IF(N_ELEMENTS(currline) NE 0)THEN   BEGIN
            band_name=[band_name,STRTRIM(currline,2)]
          ENDIF
        ENDELSE
        BREAK
      ENDIF
      IF(STRPOS(tmp[1],'{') NE -1) THEN BEGIN
        IF(STRPOS(tmp[1],'{') NE (STRLEN(tmp[1])-1) ) THEN BEGIN
          tmpStr=STRMID(tmp[1],STRPOS(tmp[1],'{')+1)
          IF(STRPOS(tmp[1],',') NE -1) THEN BEGIN
            tmp_bandName= STRSPLIT(STRTRIM(tmpStr,2), ',', /EXTRACT)
            band_name=[band_name,tmp_bandName]
          ENDIF
        ENDIF
        tmp=['','']
        READF, unit, header_line
      ENDIF ELSE  BEGIN
        currline=header_line
        IF(STRPOS(currline,',') NE -1) THEN BEGIN
          tmp_bandName = STRSPLIT(STRTRIM(currline,2), ',', /extract)
          band_name=[band_name,tmp_bandName]
        ENDIF
        READF, unit, header_line
      ENDELSE
    ENDWHILE
  ENDIF
  IF argument EQ 'wavelength' THEN BEGIN
    count=1
    WHILE (N_ELEMENTS(wavelength) NE (bands+1))DO BEGIN
      IF(N_ELEMENTS(wavelength) EQ (bands+1)) THEN BEGIN
        BREAK
      ENDIF
      IF(STRPOS(header_line,'}') NE -1) THEN BEGIN
        currline=STRMID(header_line,0,STRPOS(header_line,'}',/REVERSE_SEARCH))
        IF(STRPOS(currline,',') NE -1) THEN BEGIN
          tmp_wavelength = STRTRIM(STRSPLIT(currline, ',', /extract),2)
          wavelength=[wavelength,tmp_wavelength]
        ENDIF ELSE  BEGIN
          IF(N_ELEMENTS(currline) NE 0)THEN   BEGIN
            wavelength=[wavelength,tmp_wavelength]
          ENDIF
        ENDELSE
        BREAK
      ENDIF
      IF(STRPOS(tmp[1],'{') NE -1) THEN BEGIN
        IF(STRPOS(tmp[1],'{') NE (STRLEN(tmp[1])-1) ) THEN BEGIN
          tmpStr=STRMID(tmp[1],STRPOS(tmp[1],'{')+1)
          IF(STRPOS(tmp[1],',') NE -1) THEN BEGIN
            tmp_wavelength= STRSPLIT(STRTRIM(tmpStr,2), ',', /EXTRACT)
            wavelength=[wavelength,tmp_wavelength]
          ENDIF
        ENDIF
        tmp=['','']
        READF, unit, header_line
      ENDIF ELSE  BEGIN
        currline=header_line
        IF(STRPOS(currline,',') NE -1) THEN BEGIN
          tmp_wavelength = STRSPLIT(STRTRIM(currline,2), ',', /extract)
          wavelength=[wavelength,tmp_wavelength]
        ENDIF
        READF, unit, header_line
      ENDELSE
    ENDWHILE
  ENDIF
  
  IF argument EQ 'map info' THEN BEGIN
    mapinfo_tmp=strsplit(tmp[1],'{',/extract)
    mapinfo_tmp=strsplit(mapinfo_tmp[1],',',/extract)
    mapinfo={ulx:0.,uly:0.,spacing:0.}
    mapinfo.ULX=mapinfo_tmp(3)
    mapinfo.ULY=mapinfo_tmp(4)
    mapinfo.SPACING=mapinfo_tmp(5)
  ENDIF
ENDWHILE
band_name=(N_ELEMENTS(band_name) EQ 1)? 'undefined':band_name[1:bands]
wavelength=(N_ELEMENTS(wavelength) EQ 1)? 'undefined':double(wavelength[1:bands])
mapinfo=(SIZE(mapinfo,/type) EQ 0)?'undefined':mapinfo

CLOSE,unit & FREE_LUN, unit
HeaderInfo={samples:samples , $
  lines:lines, $
  bands:bands, $
  type:type, $
  offset:offset,$
  interleave:interleave,$
  band_name:band_name,$
  wavelength:wavelength,$
  mapinfo:mapinfo}
RETURN,HeaderInfo
END

(3)	Read ENVI image dataset (2)
FUNCTION read_Hdrimage,infile,headerinfo
  column = headerinfo.SAMPLES
  row = headerinfo.LINES
  band = headerinfo.BANDS
  interleave=headerinfo.INTERLEAVE
  type=headerinfo.TYPE
  offset=headerinfo.OFFSET
  band_name=headerinfo.band_name
    
  openr,unit,infile,/get_lun
  point_lun,unit,offset
  
  CASE STRUPCASE(STRTRIM(interleave,2)) OF
    'BSQ':BEGIN
    image_size = [column, row, band]
    image = MAKE_ARRAY(column, row, band, value = 0,type=type)
    readu,unit,image
    ;image = read_binary(infile, data_dims = image_size, data_type = type,DATA_START=offset)
    END
    'BIL':BEGIN
    image_size = [column, band, row]
    image = MAKE_ARRAY(column, band, row, value = 0,type=type)
    PRINT,type
    readu,unit,image
    ;image = read_binary(infile, data_dims = image_size, data_type = type,DATA_START=offset)
;    HELP,image,/structure
    image= TRANSPOSE(image, [0, 2, 1])
;    HELP,image,/structure
    END
    'BIP':BEGIN
    image_size = [band, column, row]
    image = MAKE_ARRAY(band, column, row, value = 0,type=type)
    readu,unit,image
    ;image = read_binary(infile, data_dims = image_size, data_type = type,DATA_START=offset)
    image= TRANSPOSE(image, [1,2,0])
    END
  ELSE:  ok=DIALOG_MESSAGE('Please check file storage format! (BSQ，BIL，BIP)',title='Error in reading file.',/error)
  ENDCASE
  free_lun,unit
  image = reform(image)
  RETURN,image
END

(4)	Write ENVI file
pro write_envi, file, img, NO_REVERSE=NO_REVERSE, $
                map_info=map_info, $
                description=description
  if n_elements(description) eq 0 then $
    description='Written using "write_envi.pro". '+systime()

  xdr = 1

  openw,lun, file,/get_lun, xdr=xdr
  writeu,   lun, img

;  if keyword_set(NO_REVERSE) then $
;    writeu,   lun, img $
;  else $
;    writeu,  lun, reverse(img,size(img,/n_dim))
  free_lun, lun

; Write ENVI header file
  siz = size(img)
  if siz[0] lt 2 then message,'::write_envi:: expected at least 2d image!'
  ydim = siz[siz[0]]
  xdim = siz[siz[0]-1]
  if siz[0] eq 2 then vzdim=1 $
  else vzdim=product(siz[1:siz[0]-2],/int)
  var = siz[siz[0]+1]

  openw,lun,file+'.hdr',/get_lun
  printf,lun,'ENVI'
  printf,lun,'description = {'
  printf,lun, description+'.}'
  printf,lun,'samples = '+strcompress(xdim,/remove)
  printf,lun,'lines   = '+strcompress(ydim,/remove)
  printf,lun,'bands   = '+strcompress(vzdim,/remove)
  printf,lun,'header offset = 0'
  if var le 3 then $
    printf,lun,'file type = ENVI Classification' $
  else printf,lun,'file type = ENVI Standard'
  if keyword_set(xdr) then $
    printf,lun,'data type = '+strcompress((var EQ 2) ? 3 : var,/remove) $
  else $
  printf,lun,'data type = '+strcompress(var,/remove)
  printf,lun,'interleave = bip'
  printf,lun,'sensor type = Unknown'
  printf,lun,'byte order = '+strcompress(keyword_set(xdr),/r)
  if (var eq 6) then printf,lun,'complex function = Power'
  if n_elements(map_info) eq 1 then $
    printf, lun, map_info
  free_lun,lun
end
