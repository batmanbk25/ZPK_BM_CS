*&---------------------------------------------------------------------*
*&  Include           ZPG_BM_TRAN_TAB
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  MAIN_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MAIN_PROC .
  CASE 'X'.
    WHEN P_PDFCS.
      CASE 'X'.
        WHEN P_TRATR.
          PERFORM TRANS_CUS_TABLE.
        WHEN P_TRADL.
          PERFORM DOWNLOAD_CUS_TABLE.
        WHEN P_TRAUL.
          PERFORM UPLOAD_CUS_TABLE.
      ENDCASE.
    WHEN P_STABL.
      PERFORM TRANS_SPEC_TABLE.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " MAIN_PROC

*&---------------------------------------------------------------------*
*&      Form  TRANS_CUS_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TRANS_CUS_TABLE .
  DATA:
    LS_CS_MAP TYPE ZST_BM_CS_MAP,
    LT_CS_MAP TYPE ZTT_BM_CS_MAP,
    LT_CSFUNC TYPE ZTT_BM_CSTR_C,
    LT_CSKEY  TYPE ZTT_BM_CSTR_K,
    LS_KEY    TYPE E071K,
    LS_OBJ    TYPE KO200,
    LT_KEY    TYPE APB_LPD_T_E071K,
    LT_OBJ    TYPE /SAPCND/T_KO200,
    LS_ERROR  TYPE IWERRORMSG.

  SELECT *
    FROM ZTB_BM_CSTR_C AS C
    INTO TABLE LT_CSFUNC
   WHERE MODLID IN S_MODLID
     AND CSFUNC IN S_CSFUNC.
  SORT LT_CSFUNC BY MODLID CSFUNC.

*  SELECT *
*    FROM ZTB_BM_CSTR_K
*    INTO TABLE LT_CSKEY
*   WHERE MODLID IN S_MODLID
*     AND CSFUNC IN S_CSFUNC.
*  SORT LT_CSKEY BY VIEWNAME TABKEY.

  CALL FUNCTION 'ZFM_BM_CS_COLLECT_KEYS'
    EXPORTING
      IT_CSFUNC = LT_CSFUNC
    IMPORTING
      ET_CS_MAP = LT_CS_MAP
      ET_CSTR_K = LT_CSKEY.

  LOOP AT LT_CS_MAP INTO LS_CS_MAP WHERE TABNAME IS NOT INITIAL.
    LS_KEY-TABKEY = LS_CS_MAP-TABKEY.
    LS_KEY-OBJECT   = 'TABU'.
    LS_KEY-OBJNAME  = LS_CS_MAP-TABNAME.
    LS_KEY-VIEWNAME = LS_CS_MAP-VIEWNAME.

*   Set other attribute to transport
    LS_KEY-PGMID    = 'R3TR'.
    IF LS_CS_MAP-VCL_NAME IS NOT INITIAL.
      LS_KEY-MASTERNAME   = LS_CS_MAP-VCL_NAME.
      LS_KEY-MASTERTYPE   = 'CDAT'.
    ELSEIF LS_CS_MAP-VIEWNAME IS NOT INITIAL.
      LS_KEY-MASTERNAME   = LS_CS_MAP-VIEWNAME.
      LS_KEY-MASTERTYPE   = 'VDAT'.
    ELSE.
      LS_KEY-MASTERNAME = LS_KEY-OBJNAME.
      LS_KEY-MASTERTYPE   = 'TABU'.
    ENDIF.

    MOVE-CORRESPONDING LS_KEY TO LS_OBJ.
    LS_OBJ-OBJFUNC = 'K'.
    LS_OBJ-OBJ_NAME = LS_KEY-MASTERNAME.
    LS_OBJ-OBJECT = LS_KEY-MASTERTYPE.
    APPEND LS_OBJ TO GT_OBJ.

    APPEND LS_KEY TO GT_KEY.
  ENDLOOP.
  SORT GT_OBJ BY PGMID OBJECT OBJ_NAME.
  DELETE ADJACENT DUPLICATES FROM GT_OBJ COMPARING PGMID OBJECT OBJ_NAME.

  IF GT_KEY[] IS NOT INITIAL.
    CALL FUNCTION 'IW_C_APPEND_OBJECTS_TO_REQUEST'
      IMPORTING
*       TRANSPORT_ORDER       =
*       IS_CANCELLED          =
        ERROR_MSG = LS_ERROR
      TABLES
        OBJECTS   = GT_OBJ
        KEYS      = GT_KEY.
  ENDIF.
  REFRESH: GT_KEY, GT_OBJ.
ENDFORM.                    " TRANS_CUS_TABLE


*&---------------------------------------------------------------------*
*&      Form  TRANS_SPEC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TRANS_SPEC_TABLE .
  DATA:
    LT_OBJECT        TYPE TABLE OF GTY_OBJECT,
    LS_KEY           TYPE E071K,
    LS_OBJ           TYPE KO200,
    LS_ERROR         TYPE IWERRORMSG,
    LR_DATA          TYPE REF TO DATA,
    LT_FCAT          TYPE LVC_T_FCAT,
    LW_KEY_LENG      TYPE NUMC06,
    LS_DATA_RAW      TYPE ZST_BM_DATA_RAW,
    LT_DATA_RAW      TYPE TABLE OF ZST_BM_DATA_RAW,
    LT_TABNAMES      TYPE DDTABNAMES,
    LT_WHERE_CLAUSES TYPE RSDS_WHERE_TAB.      "Where clause.

  FIELD-SYMBOLS:
    <LF_E071K>       TYPE E071K,
    <LFT_DATA_TABLE> TYPE TABLE.

  IF S_TABNM[] IS NOT INITIAL.
    IF LINES( S_TABNM[] ) = 1.
      CALL FUNCTION 'ZFM_DATTAB_FREE_SELECTION'
        EXPORTING
          I_TABLE         = S_TABNM-LOW
          I_MAXSELECT     = 12
        IMPORTING
          T_WHERE_CLAUSES = LT_WHERE_CLAUSES
        EXCEPTIONS
          ERROR           = 1
          OTHERS          = 2.

      CREATE DATA LR_DATA TYPE TABLE OF (S_TABNM-LOW).
      ASSIGN LR_DATA->* TO <LFT_DATA_TABLE>.
      CALL FUNCTION 'ZFM_DATTAB_GET'
        EXPORTING
          I_TABLE         = S_TABNM-LOW
          T_WHERE_CLAUSES = LT_WHERE_CLAUSES
        IMPORTING
          T_TABLE_DATA    = <LFT_DATA_TABLE>.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          I_STRUCTURE_NAME       = S_TABNM-LOW
        CHANGING
          CT_FIELDCAT            = LT_FCAT
        EXCEPTIONS
          INCONSISTENT_INTERFACE = 1
          PROGRAM_ERROR          = 2
          OTHERS                 = 3.
      LOOP AT LT_FCAT INTO DATA(LS_FCAT)
        WHERE KEY = 'X'.
        LW_KEY_LENG = LW_KEY_LENG + LS_FCAT-INTLEN.
      ENDLOOP.

      IF P_TRATR IS NOT INITIAL.
        LS_OBJ-PGMID      = 'R3TR'.
        LS_OBJ-OBJECT     = 'TABU'.
        LS_OBJ-OBJFUNC    = 'K'.
        LS_OBJ-OBJ_NAME   = S_TABNM-LOW.
        APPEND LS_OBJ TO GT_OBJ.
        LOOP AT <LFT_DATA_TABLE> ASSIGNING FIELD-SYMBOL(<LF_DATA_LINE>).
          MOVE-CORRESPONDING LS_OBJ TO LS_KEY.
          LS_KEY-OBJNAME    = S_TABNM-LOW.
          LS_KEY-MASTERNAME = LS_KEY-OBJNAME.
          LS_KEY-MASTERTYPE = LS_KEY-OBJECT.
*          CONCATENATE SY-MANDT '*' INTO LS_KEY-TABKEY.
          LS_KEY-TABKEY = <LF_DATA_LINE>(LW_KEY_LENG).
          APPEND LS_KEY TO GT_KEY.
        ENDLOOP.
      ELSEIF P_TRADL IS NOT INITIAL.
        LS_DATA_RAW-TABNM = S_TABNM-LOW.
        LS_DATA_RAW-REFDATA = LR_DATA.
        APPEND LS_DATA_RAW TO LT_DATA_RAW.

        CALL FUNCTION 'ZFM_BM_DATA2XML_FILE'
          EXPORTING
            IT_DATA_RAW = LT_DATA_RAW
            I_OPENFILE  = 'X'.

      ENDIF.
    ELSE.
      SELECT TABNAME
        FROM DD02L
        INTO TABLE LT_TABNAMES
       WHERE TABNAME IN S_TABNM.

      IF P_TRATR IS NOT INITIAL.
        LOOP AT LT_TABNAMES INTO DATA(LS_TABNAME).
          LS_KEY-PGMID = 'R3TR'.
          LS_KEY-OBJECT = 'TABU'.
          LS_KEY-OBJNAME = LS_TABNAME.
          LS_KEY-MASTERNAME = LS_KEY-OBJNAME.
          LS_KEY-MASTERTYPE = LS_KEY-OBJECT.
          CONCATENATE SY-MANDT '*' INTO LS_KEY-TABKEY.
          MOVE-CORRESPONDING LS_KEY TO LS_OBJ.
          LS_OBJ-OBJFUNC = 'K'.
          LS_OBJ-OBJ_NAME = LS_KEY-OBJNAME.
          APPEND LS_OBJ TO GT_OBJ.
          APPEND LS_KEY TO GT_KEY.
        ENDLOOP.
      ELSEIF P_TRADL IS NOT INITIAL.
        CALL FUNCTION 'ZFM_BM_DATA2XML_FILE'
          EXPORTING
            IT_TABNAME = LT_TABNAMES
            I_OPENFILE = 'X'.

      ENDIF.
    ENDIF.
  ENDIF.

  IF P_TRATR IS NOT INITIAL AND GT_KEY[] IS NOT INITIAL.
    CALL FUNCTION 'IW_C_APPEND_OBJECTS_TO_REQUEST'
      IMPORTING
*       TRANSPORT_ORDER       =
*       IS_CANCELLED          =
        ERROR_MSG = LS_ERROR
      TABLES
        OBJECTS   = GT_OBJ
        KEYS      = GT_KEY.
  ENDIF.
  REFRESH: GT_KEY, GT_OBJ.
ENDFORM.                    " TRANS_SPEC_TABLE

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_CUS_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DOWNLOAD_CUS_TABLE .
  DATA:
    LT_TABNAME TYPE DDTABNAMES,
    LT_CS_MAP  TYPE ZTT_BM_CS_MAP,
    LT_CSFUNC  TYPE ZTT_BM_CSTR_C,
    LT_CSKEY   TYPE ZTT_BM_CSTR_K,
    LS_KEY     TYPE E071K,
    LS_OBJ     TYPE KO200,
    LT_KEY     TYPE APB_LPD_T_E071K,
    LT_OBJ     TYPE /SAPCND/T_KO200,
    LS_ERROR   TYPE IWERRORMSG,
    BEGIN OF LS_TABKEY,
      TABNAME TYPE TABNAME,
      TABKEY  TYPE TABKEY120,
    END OF LS_TABKEY,
    LT_TABKEY LIKE TABLE OF LS_TABKEY.

  SELECT *
    FROM ZTB_BM_CSTR_C AS C
    INTO TABLE LT_CSFUNC
   WHERE MODLID IN S_MODLID
     AND CSFUNC IN S_CSFUNC.
  SORT LT_CSFUNC BY MODLID CSFUNC.

  CALL FUNCTION 'ZFM_BM_CS_COLLECT_KEYS'
    EXPORTING
      IT_CSFUNC = LT_CSFUNC
    IMPORTING
      ET_CS_MAP = LT_CS_MAP
      ET_CSTR_K = LT_CSKEY.

  MOVE-CORRESPONDING LT_CS_MAP TO LT_TABKEY.
  SORT LT_TABKEY  BY TABNAME.

  DATA:
    LR_DATA      TYPE REF TO DATA,
    LR_LINE      TYPE REF TO DATA,
    LW_KEY       TYPE STRING,
    LS_WHERE     TYPE RSDSWHERE,
    LT_WHERE     TYPE RSDS_WHERE_TAB,
    LW_WHERE_STR TYPE STRING,
    LW_STINDEX   TYPE I,
    LS_DATA_RAW  TYPE ZST_BM_DATA_RAW,
    LT_DATA_RAW  TYPE TABLE OF ZST_BM_DATA_RAW,
    LT_FIELDCAT  TYPE LVC_T_FCAT.

  LOOP AT LT_TABKEY INTO LS_TABKEY WHERE TABNAME IS NOT INITIAL.
    AT NEW TABNAME.
      CLEAR: LS_DATA_RAW, LT_FIELDCAT.
      LS_DATA_RAW-TABNM = LS_TABKEY-TABNAME.
      CREATE DATA LS_DATA_RAW-REFDATA TYPE TABLE OF (LS_TABKEY-TABNAME) WITH DEFAULT KEY.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          I_STRUCTURE_NAME       = LS_TABKEY-TABNAME
          I_INTERNAL_TABNAME     = LS_TABKEY-TABNAME
        CHANGING
          CT_FIELDCAT            = LT_FIELDCAT
        EXCEPTIONS
          INCONSISTENT_INTERFACE = 1
          PROGRAM_ERROR          = 2
          OTHERS                 = 3.
      LW_STINDEX = 0.

    ENDAT.

    CLEAR: LT_WHERE, LW_STINDEX.
    LOOP AT LT_FIELDCAT INTO DATA(LS_FCAT)
      WHERE KEY = 'X'.
      LW_KEY = LS_TABKEY-TABKEY+LW_STINDEX(LS_FCAT-DD_OUTLEN).
      LW_STINDEX = LW_STINDEX + LS_FCAT-DD_OUTLEN.
      IF LW_KEY IS NOT INITIAL AND LW_KEY <> '*' AND LS_FCAT-DATATYPE <> 'CLNT'.
        LW_KEY = '''' && LW_KEY && ''''.

        CONCATENATE LS_FCAT-FIELDNAME '=' LW_KEY
               INTO LS_WHERE SEPARATED BY SPACE.
        APPEND LS_WHERE TO LT_WHERE.
      ENDIF.

    ENDLOOP.

    CONCATENATE LINES OF LT_WHERE INTO LW_WHERE_STR SEPARATED BY ' AND '.

*   Get data from database
    SELECT *
      FROM (LS_TABKEY-TABNAME)
      WHERE (LW_WHERE_STR)
      APPENDING TABLE @LS_DATA_RAW-REFDATA->*.

    AT END OF TABNAME.
      SORT LS_DATA_RAW-REFDATA->*.
      DELETE ADJACENT DUPLICATES FROM LS_DATA_RAW-REFDATA->*.
      APPEND LS_DATA_RAW TO LT_DATA_RAW.
    ENDAT.
  ENDLOOP.

  CALL FUNCTION 'ZFM_BM_DATA2XML_FILE'
    EXPORTING
      IT_DATA_RAW = LT_DATA_RAW
      I_OPENFILE  = 'X'.
ENDFORM.                    " DOWNLOAD_CUS_TABLE

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_CUS_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM UPLOAD_CUS_TABLE .
  DATA:
    LT_TABNAME       TYPE DDTABNAMES .

  CALL FUNCTION 'ZFM_BM_DATA_FROM_XML_FILE'
    EXPORTING
      I_SHOWDATA  = 'X'
      I_UPDATE_DB = ''.
ENDFORM.                    " UPLOAD_CUS_TABLE


*&---------------------------------------------------------------------*
*& Form 1000_PBO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 1000_PBO .
  CASE 'X'.
    WHEN P_TRAUL.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'TCS'
        OR SCREEN-GROUP1 = 'STY'
        OR SCREEN-GROUP1 = 'TTB' .
          SCREEN-ACTIVE = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN OTHERS.
      CASE 'X'.
        WHEN P_PDFCS.
          LOOP AT SCREEN.
            IF SCREEN-GROUP1 CS 'TTB'.
              SCREEN-ACTIVE = '0'.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        WHEN P_STABL.
          LOOP AT SCREEN.
            IF SCREEN-GROUP1 CS 'TCS'.
              SCREEN-ACTIVE = '0'.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.
        WHEN OTHERS.
      ENDCASE.
  ENDCASE.
ENDFORM.
