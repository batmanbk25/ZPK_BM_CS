*&---------------------------------------------------------------------*
*& Include          ZIN_BM_CS_UPLOADF01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form 1000_PAI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 1000_PAI .
  DATA:
    LW_FILENAME TYPE STRING,
    LW_RESULT   TYPE XMARK.

  LW_FILENAME = P_FILENM.
  IF LW_FILENAME IS NOT INITIAL.
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_EXIST
      EXPORTING
        FILE                 = LW_FILENAME
      RECEIVING
        RESULT               = LW_RESULT
      EXCEPTIONS
        CNTL_ERROR           = 1
        ERROR_NO_GUI         = 2
        WRONG_PARAMETER      = 3
        NOT_SUPPORTED_BY_GUI = 4
        OTHERS               = 5.
    IF SY-SUBRC <> 0 OR LW_RESULT IS INITIAL.
      MESSAGE S001(ZMS_BM_CS) DISPLAY LIKE 'E' WITH LW_FILENAME.
      SET CURSOR FIELD 'P_FILENM'.
      STOP.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0000_MAIN_PROC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0000_MAIN_PROC .
  CASE 'X'.
    WHEN P_UPLOAD.
      PERFORM 0050_CHECK_FILE.
      PERFORM 0010_GET_CONFIG.
      PERFORM 0020_UPLOAD.
      PERFORM 0030_DISPLAY.
    WHEN P_DIPLAY.
      CLEAR
      GV_FILE.
*      PERFORM PROCESS_DATA.
*      PERFORM 0070_CHECK_FILE.
      PERFORM 0060_GET_CONFIG.
      PERFORM 0040_DISPLAY.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0010_Get_Config
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0010_GET_CONFIG.
  DATA:
    LS_DATA_RAW TYPE ZST_BM_CS_DATA_RAW.

  SELECT
    ZTB_BM_CSPJ_C~PROJID,
    ZTB_BM_CSPJ_C~MODLID,
    ZTB_BM_CSPJ_C~CSFUNC,
    ZTB_BM_CSTR_C~STTURE,
    ZTB_BM_CSPJ_C~VCL_NAME,
    ZTB_BM_CSPJ_C~VIEWNAME,
    ZTB_BM_CSPJ_C~SUMREC,
    ZTB_BM_CSPJ_C~COMREC,
    ZTB_BM_CSPJ_C~CSFSTS
    FROM ZTB_BM_CSPJ_C
    LEFT JOIN ZTB_BM_CSTR_C
    ON   ZTB_BM_CSTR_C~MODLID = ZTB_BM_CSPJ_C~MODLID
    AND  ZTB_BM_CSTR_C~CSFUNC = ZTB_BM_CSPJ_C~CSFUNC
    INTO CORRESPONDING FIELDS OF TABLE @GT_CSPJ_C
   WHERE PROJID = @P_PROJID.
  SORT GT_CSPJ_C BY PROJID MODLID CSFUNC.

  SELECT *
    FROM ZTB_BM_CSPJ_D
    INTO TABLE GT_CSPJ_D_ORG
   WHERE PROJID = P_PROJID.

  LOOP AT GT_CSPJ_C INTO DATA(LS_CSPJ_C).
    MOVE-CORRESPONDING LS_CSPJ_C TO LS_DATA_RAW.
    IF LS_CSPJ_C-STTURE IS INITIAL.
      CREATE DATA LS_DATA_RAW-REFDATA TYPE TABLE OF (LS_CSPJ_C-VIEWNAME).
    ELSE.
      CREATE DATA LS_DATA_RAW-REFDATA TYPE TABLE OF (LS_CSPJ_C-STTURE).
    ENDIF.
    APPEND LS_DATA_RAW TO GT_DATA_RAW.
  ENDLOOP.

  IF GT_DATA_RAW[] IS NOT INITIAL.
    SELECT *
      FROM ZTB_BM_CSTR_C
      INTO TABLE @GT_CSTR_C
      FOR ALL ENTRIES IN @GT_DATA_RAW
      WHERE MODLID = @GT_DATA_RAW-MODLID
        AND CSFUNC = @GT_DATA_RAW-CSFUNC.
    SORT GT_CSTR_C BY MODLID CSFUNC.
  ELSE.
    MESSAGE 'No data!' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form 0060_GET_CONFIG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 0060_GET_CONFIG .
  DATA:
    LS_DATA_RAW TYPE ZST_BM_CS_DATA_RAW.

  IF P_PROJID IS NOT INITIAL.
    SELECT *
      FROM  ZTB_BM_CSPJ_L
      INTO CORRESPONDING FIELDS OF TABLE GT_ALV
      WHERE PROJID = P_PROJID
        AND FILENAME IN S_FILE[].

  ELSE.
    SELECT *
      FROM  ZTB_BM_CSPJ_L
      INTO CORRESPONDING FIELDS OF TABLE GT_ALV
      WHERE FILENAME IN S_FILE[].

  ENDIF.

  IF GT_ALV[] IS NOT INITIAL.
    SELECT *
      FROM ZTB_BM_CSTR_C
      INTO TABLE @GT_CSTR_C
      FOR ALL ENTRIES IN @GT_ALV
      WHERE MODLID = @GT_ALV-MODLID
        AND CSFUNC = @GT_ALV-CSFUNC.
    SORT GT_CSTR_C BY MODLID CSFUNC.
  ELSE.
    MESSAGE 'No data!' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form 0020_Upload
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0020_UPLOAD .
  DATA:
    LS_DATA_RAW TYPE ZST_BM_CS_DATA_RAW,
    LW_FILENAME LIKE  RLGRAP-FILENAME,
    LT_FCAT     TYPE LVC_T_FCAT,
    LT_FCAT_ST  TYPE LVC_T_FCAT,
    LS_CSPJ_D   TYPE ZTB_BM_CSPJ_D,
    LW_POS      TYPE I,
    LS_USRLOG   TYPE ZST_BM_USRLOG,
    LT_INPUT    LIKE DD02L-TABNAME.

  LW_FILENAME = P_FILENM.

  CALL FUNCTION 'ZFM_BM_CS_EXCEL_UPLOAD'
    EXPORTING
      I_LINE_HEADER     = 6
      I_FILENAME        = LW_FILENAME
    CHANGING
      CT_DATA_RAW       = GT_DATA_RAW
    EXCEPTIONS
      CONVERSION_FAILED = 1
      OTHERS            = 2.

  LOOP AT GT_DATA_RAW INTO LS_DATA_RAW.
    IF LS_DATA_RAW-REFDATA->* IS NOT INITIAL.
      CLEAR LT_FCAT.
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          I_STRUCTURE_NAME   = LS_DATA_RAW-VIEWNAME
          I_INTERNAL_TABNAME = LS_DATA_RAW-VIEWNAME
          I_BYPASSING_BUFFER = 'X'
        CHANGING
          CT_FIELDCAT        = LT_FCAT.

      IF LS_DATA_RAW-STTURE IS NOT INITIAL.
        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            I_STRUCTURE_NAME = LS_DATA_RAW-STTURE
          CHANGING
            CT_FIELDCAT      = LT_FCAT_ST.
      ENDIF.

      ASSIGN LS_DATA_RAW-REFDATA->* TO FIELD-SYMBOL(<LFT_DATA_D>).
      IF SY-SUBRC IS INITIAL.
        LOOP AT <LFT_DATA_D> ASSIGNING FIELD-SYMBOL(<LF_DATA_D>).
          CLEAR: LS_CSPJ_D, LW_POS.
          MOVE-CORRESPONDING LS_DATA_RAW TO LS_CSPJ_D.
          LS_CSPJ_D-CSFSTS = GC_CSFSTS-NOT_YET.
          LS_CSPJ_D-MANDT = SY-MANDT.

          LOOP AT LT_FCAT INTO DATA(LS_FCAT) WHERE KEY = 'X'.
            ASSIGN COMPONENT LS_FCAT-FIELDNAME OF STRUCTURE <LF_DATA_D>
              TO FIELD-SYMBOL(<LF_KEY>).
            IF SY-SUBRC IS INITIAL.
              IF LS_FCAT-DATATYPE = 'CLNT' AND <LF_KEY> IS INITIAL.
                <LF_KEY> = SY-MANDT.
              ENDIF.

              LS_CSPJ_D-TABKEY+LW_POS(LS_FCAT-INTLEN) = <LF_KEY>.
              LW_POS = LW_POS + LS_FCAT-INTLEN.
            ENDIF.
          ENDLOOP.

          READ TABLE GT_CSPJ_D_ORG INTO DATA(LS_CSPJ_D_ORG)
            WITH KEY PROJID = LS_CSPJ_D-PROJID
                      MODLID = LS_CSPJ_D-MODLID
                      CSFUNC = LS_CSPJ_D-CSFUNC
                      VIEWNAME = LS_CSPJ_D-VIEWNAME
                      TABKEY = LS_CSPJ_D-TABKEY.
          IF SY-SUBRC IS INITIAL.
            CLEAR LS_USRLOG.
            MOVE-CORRESPONDING LS_CSPJ_D_ORG TO LS_USRLOG.
            MOVE-CORRESPONDING LS_USRLOG TO LS_CSPJ_D.
            LS_CSPJ_D-CSFSTS = LS_CSPJ_D_ORG-CSFSTS.
          ELSE.
            LS_CSPJ_D-CRUSR = SY-UNAME.
            LS_CSPJ_D-CRDAT = SY-DATUM.
            LS_CSPJ_D-CRTIM = SY-UZEIT.
          ENDIF.

          CALL TRANSFORMATION ID
            SOURCE DATA = <LF_DATA_D>
            RESULT XML LS_CSPJ_D-XMLSTR.

          READ TABLE GT_CSPJ_D_ORG INTO DATA(LS_CSPJ_D_O)
            WITH KEY PROJID = LS_CSPJ_D-PROJID
                     MODLID = LS_CSPJ_D-MODLID
                     CSFUNC = LS_CSPJ_D-CSFUNC
                     VIEWNAME = LS_CSPJ_D-VIEWNAME
                     TABKEY = LS_CSPJ_D-TABKEY.
          APPEND LS_CSPJ_D TO GT_CSPJ_D_UP.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.
  MESSAGE 'File upload successful' TYPE 'S'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0030_Display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0030_DISPLAY.

  CALL SCREEN 0100.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0100_PBO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0100_PBO .
  DATA:
    LT_FCAT    TYPE LVC_T_FCAT,
    LS_CSPJ_D  TYPE ZTB_BM_CSPJ_D,
    LS_LAYOUT  TYPE LVC_S_LAYO,
    LS_VARIANT TYPE DISVARIANT.

  LS_LAYOUT-CWIDTH_OPT = 'X'.
  LS_VARIANT-REPORT = SY-REPID.
  LS_VARIANT-HANDLE = SY-DYNNR.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME   = 'ZTB_BM_CSPJ_D'
      I_INTERNAL_TABNAME = 'ZTB_BM_CSPJ_D'
    CHANGING
      CT_FIELDCAT        = LT_FCAT.
  LOOP AT LT_FCAT ASSIGNING FIELD-SYMBOL(<LF_FCAT>).
    CASE <LF_FCAT>-FIELDNAME.
      WHEN 'XMLSTR' OR 'TABKEY'.
        <LF_FCAT>-HOTSPOT = 'X'.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.


  IF GO_ALV_0100 IS INITIAL.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CALLBACK_PROGRAM      = SY-REPID
*       I_CALLBACK_BUTTON_CLICK = '0100_BUTTON_CLICK'
        I_CALLBACK_HOSPOT_CLICK = '0100_HOSPOT_CLICK'
        I_CUS_CONTROL_NAME      = 'CUS_ALV'
        IS_VARIANT              = LS_VARIANT
        I_SAVE                  = 'A'
        IS_LAYOUT               = LS_LAYOUT
*       IT_TOOLBAR_EXCLUDING    =
      IMPORTING
        E_ALV_GRID              = GO_ALV_0100
      CHANGING
        IT_OUTTAB               = GT_CSPJ_D_UP
        IT_FIELDCATALOG         = LT_FCAT.
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = GO_ALV_0100.

  ENDIF.

*  PERFORM 0100_HOSPOT_CLICK.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0100_SAVE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0100_SAVE .
  DATA:
    LS_CSPJ_D TYPE ZTB_BM_CSPJ_D.

  LOOP AT GT_CSPJ_D_UP INTO LS_CSPJ_D.
    AT NEW CSFUNC.
      READ TABLE GT_CSPJ_C ASSIGNING FIELD-SYMBOL(<LF_CSPJ_C>)
        WITH KEY PROJID = LS_CSPJ_D-PROJID
                 MODLID = LS_CSPJ_D-MODLID
                 CSFUNC = LS_CSPJ_D-CSFUNC BINARY SEARCH.
      IF SY-SUBRC IS INITIAL.
        CLEAR <LF_CSPJ_C>-SUMREC.
      ENDIF.
    ENDAT.
    ADD 1 TO <LF_CSPJ_C>-SUMREC.

  ENDLOOP.

  MODIFY ZTB_BM_CSPJ_C FROM TABLE GT_CSPJ_C.

  CALL FUNCTION 'ZFM_DATA_UPDATE_TABLE'
    EXPORTING
      I_STRUCTURE            = 'ZTB_BM_CSPJ_D'
*     T_FIELDCAT             =
      T_TABLE_CHANGED        = GT_CSPJ_D_UP
      T_TABLE_ORIGINAL       = GT_CSPJ_D_ORG
*     I_GET_ORG_DATA         =
*      I_DEL_FIELD            =  "'DELETED''
    EXCEPTIONS
      NO_STRUCTURE           = 1
      CONFLICT_STRUCTURE     = 2
      NO_ORIGINAL_DATA       = 3
      CONFLICT_ORIGINAL_DATA = 4
      NO_DATA                = 5
      UPDATE_ERROR           = 6
      INSERT_ERROR           = 7
      DELETE_ERROR           = 8
      DEL_FIELD_NOT_EXISTS   = 9
      OTHERS                 = 10.

*---------------------------------------------------------------------
*---------------------------------------------------------------------
*Author: DongVC
*Date: 14.09.2022
*Purpose: Write log of Upload Data
*---------------------------------------------------------------------
*----------------------------- Begin of change -----------------------
  DATA:
    LV_NUMBERUP TYPE INT4.


  SELECT  SINGLE MAX( LINEN ) AS LINEN
        FROM  ZTB_BM_CSPJ_L
        INTO  @DATA(LV_LINEN)
        WHERE PROJID = @P_PROJID.
  IF  SY-SUBRC IS INITIAL.
    LV_NUMBERUP = LV_LINEN + 1.
  ELSE.
    LV_NUMBERUP = 1.
  ENDIF.

  LOOP AT GT_CSPJ_D_UP INTO LS_CSPJ_D .
    CLEAR: GS_CSPJ_L.
    GS_CSPJ_L-LINEN = LV_NUMBERUP.
    GS_CSPJ_L-MANDT = LS_CSPJ_D-MANDT.
    GS_CSPJ_L-PROJID = LS_CSPJ_D-PROJID.
    GS_CSPJ_L-MODLID = LS_CSPJ_D-MODLID.
    GS_CSPJ_L-CSFUNC = LS_CSPJ_D-CSFUNC.
    GS_CSPJ_L-VIEWNAME = LS_CSPJ_D-VIEWNAME.
    GS_CSPJ_L-TABKEY = LS_CSPJ_D-TABKEY.
    GS_CSPJ_L-XMLSTR = LS_CSPJ_D-XMLSTR.
    GS_CSPJ_L-CRTIM = SY-UZEIT.
    GS_CSPJ_L-CRDAT = SY-DATUM.
    GS_CSPJ_L-CRUSR = SY-UNAME.
    GS_CSPJ_L-CHTIM = SY-UZEIT.
    GS_CSPJ_L-CHDAT = SY-DATUM.
    GS_CSPJ_L-CHUSR = SY-UNAME.
*-----------------------------------File upload--------------
    SPLIT P_FILENM AT '\' INTO: TABLE DATA(LT_SPLIT).
    READ TABLE LT_SPLIT INTO DATA(LS_ITAB) INDEX  LINES( LT_SPLIT ).
    IF  SY-SUBRC IS INITIAL.
      GS_CSPJ_L-FILENAME = LS_ITAB.
    ENDIF.
*---------------------------------------------------------------
    INSERT ZTB_BM_CSPJ_L FROM GS_CSPJ_L.
  ENDLOOP.
  MESSAGE 'Save Successfully' TYPE 'S'.
*----------------------------- End of change ----------------------------
*------------------------------------------------------------------------
ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0100_HOSPOT_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0100_HOSPOT_CLICK
  USING LPS_COLUMN    TYPE LVC_S_COL
        LPS_ROW_NO    TYPE LVC_S_ROID.
  DATA:
    LR_DATA    TYPE REF TO DATA,
    LT_FCAT    TYPE LVC_T_FCAT,
    LW_XSTRING TYPE XSTRING.
  FIELD-SYMBOLS:
    <LFT_DATA> TYPE STANDARD TABLE.

  CASE LPS_COLUMN-FIELDNAME.
    WHEN 'TABKEY'.
      READ TABLE GT_CSPJ_D_UP INTO DATA(LS_CSPJ_D) INDEX LPS_ROW_NO-ROW_ID.
      IF SY-SUBRC IS INITIAL.

        CREATE DATA LR_DATA TYPE TABLE OF (LS_CSPJ_D-VIEWNAME).
        CREATE DATA LR_DATA TYPE (LS_CSPJ_D-VIEWNAME).

        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            I_STRUCTURE_NAME   = LS_CSPJ_D-VIEWNAME
            I_INTERNAL_TABNAME = LS_CSPJ_D-VIEWNAME
          CHANGING
            CT_FIELDCAT        = LT_FCAT.

        CALL TRANSFORMATION ID
          SOURCE XML LS_CSPJ_D-XMLSTR
          RESULT DATA = LR_DATA->*.

        CALL FUNCTION 'ZFM_BM_ALV_ITEM_DETAIL'
          EXPORTING
            IT_FCAT   = LT_FCAT
            IS_DETAIL = LR_DATA->*.
      ENDIF.
    WHEN 'XMLSTR'.
      READ TABLE GT_CSPJ_D_UP INTO LS_CSPJ_D INDEX LPS_ROW_NO-ROW_ID.
      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            TEXT   = LS_CSPJ_D-XMLSTR
          IMPORTING
            BUFFER = LW_XSTRING.

        CALL FUNCTION 'DISPLAY_XML_STRING'
          EXPORTING
            XML_STRING      = LW_XSTRING
          EXCEPTIONS
            NO_XML_DOCUMENT = 1
            OTHERS          = 2.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*---------------------------------------------------------------------
*Author: DongVC
*Date: 14.09.2022
*Purpose: Write log of Upload Data
*---------------------------------------------------------------------
FORM 1000_PBO.
  IF P_UPLOAD = 'X'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'GR1'.
        SCREEN-ACTIVE = '0'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'GR2'.
        SCREEN-ACTIVE   = '1'.
        SCREEN-REQUIRED = '2'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'GR3'.
        SCREEN-REQUIRED = '2'.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ELSEIF P_DIPLAY = 'X'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'GR1'.
        SCREEN-ACTIVE = '1'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'GR2'.
        SCREEN-ACTIVE   = '0'.
        SCREEN-REQUIRED = '0'.
        MODIFY SCREEN.
      ELSEIF SCREEN-GROUP1 = 'GR3'.
        SCREEN-REQUIRED = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
FORM 0050_CHECK_FILE.
  DATA:
    LV_CSPJ_L TYPE TABLE OF ZTB_BM_CSPJ_L,
    LV_FILE   TYPE LOCALFILE.

  IF P_PROJID IS INITIAL OR P_FILENM IS INITIAL .
    MESSAGE 'Bạn nhập thiếu dữ liệu' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  SPLIT P_FILENM AT '\' INTO: TABLE DATA(LT_SPLIT).
  READ TABLE LT_SPLIT INTO DATA(LS_ITAB) INDEX  LINES( LT_SPLIT ).
  IF  SY-SUBRC IS INITIAL.
    LV_FILE = LS_ITAB.
    SELECT *
           FROM ZTB_BM_CSPJ_L
           INTO  TABLE LV_CSPJ_L
           WHERE PROJID = P_PROJID
                 AND FILENAME =   LV_FILE .
    IF SY-SUBRC IS INITIAL.
      MESSAGE 'File đã được upload' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 0070_CHECK_FILE .

*  DATA:
*    LV_CSPJ_L TYPE TABLE OF ZTB_BM_CSPJ_L.

*  IF P_PROJID IS INITIAL OR P_FILENM IS INITIAL OR S_DIDATE IS INITIAL.
*    MESSAGE 'Bạn nhập thiếu dữ liệu' TYPE 'S' DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*  SPLIT P_FILENM AT '\' INTO: TABLE DATA(LT_SPLIT).
*  READ TABLE LT_SPLIT INTO DATA(LS_ITAB) INDEX  LINES( LT_SPLIT ).
*  IF  SY-SUBRC IS INITIAL.
*  GV_FILE = LS_ITAB.
*  IF P_PROJID IS NOT INITIAL.
*    SELECT *
*         FROM ZTB_BM_CSPJ_L
*         INTO  TABLE LV_CSPJ_L
*         WHERE PROJID   = P_PROJID
*           AND FILENAME IN S_FILE[].
*    IF SY-SUBRC IS INITIAL.
*    ELSE .
*      MESSAGE 'File chưa được upload' TYPE 'S' DISPLAY LIKE 'E'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*  ENDIF.

*      AND CRDAT = S_DIDATE.

*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form 0200_PBO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form 0040_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 0040_DISPLAY .
  SORT GT_ALV BY LINEN.
  CALL SCREEN 0200.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------
*Author: DongVC
*Date: 14.09.2022
*Purpose: Write log of Upload Data
*---------------------------------------------------------------------
FORM 0200_PBO .
  DATA: LW_COLTEXT TYPE TEXT255,
        LW_POS     TYPE INT4,
        LS_FCAT    LIKE LINE OF GT_FIELDCAT.
  DATA:
    LS_CSPJ_L  TYPE ZTB_BM_CSPJ_L,
    LS_VARIANT TYPE DISVARIANT.

  CLEAR:
        GS_LAYOUT.
  GS_LAYOUT-ZEBRA      = 'X'.
  GS_LAYOUT-CWIDTH_OPT = 'X'.
  GS_LAYOUT-SEL_MODE = 'A'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'ZST_BM_CSPJ_L'
    CHANGING
      CT_FIELDCAT            = GT_FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    LOOP AT GT_FIELDCAT ASSIGNING FIELD-SYMBOL(<FS_FIELDCAT>).
      CLEAR
      <FS_FIELDCAT>-TECH.
      CASE <FS_FIELDCAT>-FIELDNAME.
        WHEN 'PROJID'.
          <FS_FIELDCAT>-COL_POS       = 1.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'MODLID'.
          <FS_FIELDCAT>-COL_POS       = 2.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'CSFUNC'.
          <FS_FIELDCAT>-COL_POS       = 3.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'VIEWNAME'.
          <FS_FIELDCAT>-COL_POS       = 4.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'TABKEY'.
          <FS_FIELDCAT>-COL_POS       = 5.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-HOTSPOT = 'X'.
        WHEN 'LINEN'.
          <FS_FIELDCAT>-COL_POS       = 6.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COLTEXT = 'Number of Uploads'.
        WHEN 'XMLSTR'.
          <FS_FIELDCAT>-COL_POS       = 11.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-HOTSPOT = 'X'.
        WHEN 'FILENAME'.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 7.
        WHEN 'CRUSR'.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 8.
        WHEN 'CRDAT'.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 9.
          <FS_FIELDCAT>-TECH          = ''.
        WHEN 'CRTIM'.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 10.
          <FS_FIELDCAT>-TECH          = ''.
        WHEN OTHERS.
          <FS_FIELDCAT>-TECH          = 'X'.
          <FS_FIELDCAT>-NO_OUT        = 'X'.
      ENDCASE.
    ENDLOOP.
  ENDIF.


  IF G_ALV_GRID  IS INITIAL.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CALLBACK_PROGRAM      = SY-REPID
*       I_CALLBACK_BUTTON_CLICK = '0200_BUTTON_CLICK'
        I_CALLBACK_HOSPOT_CLICK = '0200_HOSPOT_CLICK'
        I_CUS_CONTROL_NAME      = 'CONTAINER'
        IS_VARIANT              = LS_VARIANT
        I_SAVE                  = 'A'
        IS_LAYOUT               = GS_LAYOUT
*       IT_TOOLBAR_EXCLUDING    =
      IMPORTING
        E_ALV_GRID              = G_ALV_GRID
      CHANGING
        IT_OUTTAB               = GT_ALV
        IT_FIELDCATALOG         = GT_FIELDCAT.
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = G_ALV_GRID.


  ENDIF.

ENDFORM.

*---------------------------------------------------------------------
*Author: DongVC
*Date: 14.09.2022
*Purpose: Write log of Upload Data
*---------------------------------------------------------------------
FORM 0200_HOSPOT_CLICK
  USING LPS_COLUMN    TYPE LVC_S_COL
        LPS_ROW_NO    TYPE LVC_S_ROID.
  DATA:
    LS_DETAIL  TYPE ZST_LVC_S_DETA,

    LR_DATA    TYPE REF TO DATA,
    LT_FCAT    TYPE LVC_T_FCAT,
    LW_XSTRING TYPE XSTRING,
    LW_VIM     TYPE VIM_NAME.
  FIELD-SYMBOLS:
    <LFT_DATA>    TYPE STANDARD TABLE,
    <LF_NEST_VAL> TYPE ANY.

  CASE LPS_COLUMN-FIELDNAME.
    WHEN 'TABKEY'.
      READ TABLE GT_ALV INTO DATA(LS_ALV) INDEX LPS_ROW_NO-ROW_ID.
      IF SY-SUBRC IS INITIAL.
        LW_VIM = LS_ALV-VIEWNAME.
        READ TABLE GT_CSTR_C INTO DATA(LS_CSTR_C)
          WITH KEY MODLID = LS_ALV-MODLID
                   CSFUNC = LS_ALV-CSFUNC
                   BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          IF LS_CSTR_C-STTURE IS NOT INITIAL.
            LW_VIM = LS_CSTR_C-STTURE.
          ENDIF.
        ENDIF.

        CREATE DATA LR_DATA TYPE TABLE OF (LW_VIM).
        CREATE DATA LR_DATA TYPE (LW_VIM).

        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING
            I_STRUCTURE_NAME   = LW_VIM
            I_INTERNAL_TABNAME = LW_VIM
          CHANGING
            CT_FIELDCAT        = LT_FCAT.

        CALL TRANSFORMATION ID
          SOURCE XML LS_ALV-XMLSTR
          RESULT DATA = LR_DATA->*.

        CLEAR: GT_DETAIL.
        LOOP AT LT_FCAT INTO DATA(LS_FCAT).
          CLEAR: LS_DETAIL.
          LS_DETAIL-FIELDNAME = LS_FCAT-FIELDNAME.
          IF LS_FCAT-SCRTEXT_L IS INITIAL.
            LS_DETAIL-COLUMNTEXT = LS_FCAT-FIELDNAME.
          ELSE.
            LS_DETAIL-COLUMNTEXT = LS_FCAT-SCRTEXT_L.
          ENDIF.
          ASSIGN COMPONENT LS_FCAT-FIELDNAME OF
            STRUCTURE LR_DATA->* TO <LF_NEST_VAL>.

          MOVE <LF_NEST_VAL> TO LS_DETAIL-VALUE.
          CONDENSE LS_DETAIL-VALUE.
          APPEND LS_DETAIL TO GT_DETAIL.
        ENDLOOP.

        CALL SCREEN 0300 STARTING AT 5 5 ENDING AT 100 20.
*        CALL FUNCTION 'ZFM_BM_ALV_ITEM_DETAIL'
*          EXPORTING
*            IT_FCAT   = LT_FCAT
*            IS_DETAIL = LR_DATA->*.
      ENDIF.
    WHEN 'XMLSTR'.
      READ TABLE GT_ALV INTO LS_ALV INDEX LPS_ROW_NO-ROW_ID.
      IF SY-SUBRC IS INITIAL.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            TEXT   = LS_ALV-XMLSTR
          IMPORTING
            BUFFER = LW_XSTRING.

        CALL FUNCTION 'DISPLAY_XML_STRING'
          EXPORTING
            XML_STRING      = LW_XSTRING
          EXCEPTIONS
            NO_XML_DOCUMENT = 1
            OTHERS          = 2.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*FORM BUTTON_CLICK  USING    P_ES_COL_ID TYPE LVC_S_COL
*                            P_ES_ROW_NO TYPE LVC_S_ROID.
*  DATA: LV_COL_ID TYPE LVC_S_ROW.
*  DATA: LV_ROW_NO TYPE LVC_S_ROID.
*
*  LV_COL_ID = P_ES_COL_ID. "Cot nhan duoc
*  LV_ROW_NO = P_ES_ROW_NO. "Hang nhan duoc
*ENDFORM.
*---------------------------------------------------------------------
*---------------------------------------------------------------------
*Author: DongVC
*Date: 14.09.2022
*Purpose: Write log of Upload Data
*---------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Form 0300_PBO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 0300_PBO .
  DATA: LW_COLTEXT TYPE TEXT255,
        LW_POS     TYPE INT4,
        LS_FCAT    LIKE LINE OF GT_FIELDCAT.
  DATA:
    LS_CSPJ_L  TYPE ZTB_BM_CSPJ_L,
    LS_VARIANT TYPE DISVARIANT.

  CLEAR:
        GS_LAYOUT,
        GT_FIELDCAT_D.
  GS_LAYOUT-ZEBRA      = 'X'.
  GS_LAYOUT-CWIDTH_OPT = 'X'.
  GS_LAYOUT-SEL_MODE = 'A'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'ZST_LVC_S_DETA'
    CHANGING
      CT_FIELDCAT            = GT_FIELDCAT_D[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
  ENDIF.
  IF G_ALV_GRID_D  IS INITIAL.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR'
      EXPORTING
        I_CALLBACK_PROGRAM = SY-REPID
        I_CUS_CONTROL_NAME = 'CONTAINER_D'
        IS_VARIANT         = LS_VARIANT
        I_SAVE             = 'A'
        IS_LAYOUT          = GS_LAYOUT
      IMPORTING
        E_ALV_GRID         = G_ALV_GRID_D
      CHANGING
        IT_OUTTAB          = GT_DETAIL
        IT_FIELDCATALOG    = GT_FIELDCAT_D.
  ELSE.
    CALL FUNCTION 'ZFM_ALV_DISPLAY_SCR_REFRESH'
      EXPORTING
        I_ALV_GRID = G_ALV_GRID_D.
  ENDIF.
ENDFORM.
