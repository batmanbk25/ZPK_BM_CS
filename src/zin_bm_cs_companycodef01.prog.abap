*&---------------------------------------------------------------------*
*& Include          ZIN_BM_CS_COMPANYCODEF01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form MAIN_PROCESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM MAIN_PROCESS .
  CALL SCREEN 1000.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADDRES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ADDRES .
  IF GV_BUKRS IS INITIAL OR GV_BUTXT IS INITIAL OR GV_SPRAS IS INITIAL.
    MESSAGE ' Bạn nhập thiếu dữ liệu !' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ELSE.
    CLEAR
G_MODE.
    CALL SCREEN 1001.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESS_DATA .
  DATA:
    LV_ADDR_NUM TYPE ADRC-ADDRNUMBER,
    LS_T001     TYPE T001,
    ET_RETURN   TYPE  BAPIRET2_T.
  CLEAR:
  GT_RETURN,
   ADTEL[],
   ADFAX[],
   ADSMTP[],
  GT_T001.
  IF GV_BUKRS IS INITIAL OR GV_BUTXT IS INITIAL OR GV_SPRAS IS INITIAL OR GV_TITLE_MEDI IS INITIAL OR GV_COUNTRY IS INITIAL OR GV_LANGU IS INITIAL.
    MESSAGE ' Bạn nhập thiếu dữ liệu !' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.

  ENDIF.
  SELECT SINGLE
   *
     FROM T001
     INTO  CORRESPONDING FIELDS OF GT_T001
     WHERE BUKRS = GV_BUKRS.
  IF  SY-SUBRC IS INITIAL.

    G_MODE = 'UPDATE'.


    PERFORM CHECK .
    ET_RETURN = GT_RETURN.
    IF ET_RETURN IS NOT INITIAL .
      RETURN.
    ENDIF.
    CASE GT_T001-ADRNR.
      WHEN '' .
        PERFORM  INSERT_ADDR CHANGING LV_ADDR_NUM.
        GT_T001-ADRNR = LV_ADDR_NUM.
      WHEN OTHERS.
        PERFORM UPDATE_ADDR CHANGING GT_T001.
    ENDCASE.
    GT_T001-BUTXT = GV_BUTXT.
    GT_T001-ORT01 = GV_ORT01.
    GT_T001-LAND1 = GV_LAND1.
    GT_T001-WAERS = GV_WAERS.
    GT_T001-SPRAS = GV_SPRAS.
    UPDATE  T001 FROM GT_T001.
    GV_ADRNR = LV_ADDR_NUM.
    MESSAGE 'UPDATE SUCCESSFUL'  TYPE 'S'.
  ELSE.
    G_MODE = 'INSERT'.
    PERFORM CHECK.

    ET_RETURN = GT_RETURN.
    IF ET_RETURN IS NOT INITIAL .
      RETURN.
    ENDIF.
    PERFORM  INSERT_ADDR CHANGING LV_ADDR_NUM.
    GV_ADRNR = LV_ADDR_NUM.
    GT_T001-BUKRS = GV_BUKRS.
    GT_T001-BUTXT = GV_BUTXT.
    GT_T001-ORT01 = GV_ORT01.
    GT_T001-LAND1 = GV_LAND1.
    GT_T001-WAERS = GV_WAERS.
    GT_T001-SPRAS = GV_SPRAS.
    GT_T001-ADRNR = GV_ADRNR.
    INSERT T001 FROM GT_T001.
    MESSAGE 'Successful new creation' TYPE 'S'.
    G_MODE = 'UPDATE'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form INSERT_ADDR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_ADDR_NUM
*&---------------------------------------------------------------------*
FORM INSERT_ADDR  CHANGING LV_ADDR_NUM  TYPE ADRC-ADDRNUMBER.
  DATA:

    LV_ADDR1_DATA TYPE ADDR1_DATA,

    LV_ADDR_REF   TYPE ADDR_REF,

    LV_ADDR_DATA  TYPE ADDR1_DATA.

  LV_ADDR1_DATA-TITLE = GV_TITLE_MEDI.
  LV_ADDR1_DATA-NAME1 = GV_NAME1.
  LV_ADDR1_DATA-NAME2 = GV_NAME2.
  LV_ADDR1_DATA-SORT1 = GV_SORT1.
  LV_ADDR1_DATA-SORT2 = GV_SORT2.
  LV_ADDR1_DATA-STR_SUPPL1 = GV_STR_SUPPL1.
  LV_ADDR1_DATA-STREET = GV_STREET.
  LV_ADDR1_DATA-POST_CODE1 = GV_POST_CODE1.
  LV_ADDR1_DATA-CITY1 = GV_CITY1.
  LV_ADDR1_DATA-COUNTRY = GV_COUNTRY.
  LV_ADDR1_DATA-REGION = GV_REGION.
  LV_ADDR1_DATA-TIME_ZONE = GV_TIME_ZONE.
  LV_ADDR1_DATA-PO_BOX = GV_PO_BOX.
  LV_ADDR1_DATA-POST_CODE2 = GV_POST_CODE2.
  LV_ADDR1_DATA-POST_CODE3 = GV_POST_CODE3.
  LV_ADDR1_DATA-LANGU = GV_LANGU.
  LV_ADDR1_DATA-DEFLT_COMM = GV_DEFLT_COMM.
  LV_ADDR1_DATA-REMARK = GV_REMARK.


  LV_ADDR_REF-ADDR_GROUP = 'CA01'.

*wa_addr_ref-owner = 'X'.

  LV_ADDR_REF-APPL_TABLE = 'ADRC'.

  LV_ADDR_REF-APPL_FIELD = 'ADDRNUMBER'.

  CALL FUNCTION 'ADDR_INSERT'
    EXPORTING
      ADDRESS_DATA    = LV_ADDR1_DATA
      ADDRESS_GROUP   = 'CA01'
      ADDRESS_HANDLE  = 'Address '
*     DATE_FROM       = '00010101'
*     LANGUAGE        = SY-LANGU
*     CHECK_EMPTY_ADDRESS = 'X'
*     CHECK_ADDRESS   = 'X'
    IMPORTING
      ADDRESS_DATA    = LV_ADDR_DATA
*     RETURNCODE      =
* TABLES
*     ERROR_TABLE     =
    EXCEPTIONS
      ADDRESS_EXISTS  = 1
      PARAMETER_ERROR = 2
      INTERNAL_ERROR  = 3
      OTHERS          = 4.

  IF SY-SUBRC <> 0.

* Implement suitable error handling here

  ENDIF.

  IF GV_TEL_NUMBER IS NOT INITIAL.
    PERFORM ADD_TEL CHANGING LV_ADDR_NUM.
  ENDIF.
  IF GV_FAX_NUMBER IS NOT INITIAL.
    PERFORM ADD_FAX CHANGING LV_ADDR_NUM.
  ENDIF.
  IF GV_SMTP_ADDR IS NOT INITIAL.
    PERFORM ADD_SMTP CHANGING LV_ADDR_NUM.
  ENDIF.

  CALL FUNCTION 'ADDR_NUMBER_GET'
    EXPORTING
      ADDRESS_HANDLE           = 'Address '
      ADDRESS_REFERENCE        = LV_ADDR_REF
      PERSONAL_ADDRESS         = ' '
*     NUMBERRANGE_NUMBER       = '01'
*     E071K_WA                 =
*     GENERATE_TRANSPORT_ENTRIES =
*     OWNER                    = 'X'
*     TABLE_NAME               =
*     FIELD_NAME               =
*     OBJKEY                   =
    IMPORTING
      ADDRESS_NUMBER           = LV_ADDR_NUM
*     RETURNCODE_NUMBERRANGE   =
* TABLES
*     E071K_TAB                =
    EXCEPTIONS
      ADDRESS_HANDLE_NOT_EXIST = 1
      INTERNAL_ERROR           = 2
      PARAMETER_ERROR          = 3
      OTHERS                   = 4.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'ADDR_MEMORY_SAVE'
    EXPORTING
      EXECUTE_IN_UPDATE_TASK = ' '
    EXCEPTIONS
      ADDRESS_NUMBER_MISSING = 1
      PERSON_NUMBER_MISSING  = 2
      INTERNAL_ERROR         = 3
      DATABASE_ERROR         = 4
      REFERENCE_MISSING      = 5
      OTHERS                 = 6.

  IF SY-SUBRC <> 0.

    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO

    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

  COMMIT WORK AND WAIT.

  CALL FUNCTION 'ADDR_MEMORY_CLEAR'
    EXPORTING
      FORCE              = ' '
    EXCEPTIONS
      UNSAVED_DATA_EXIST = 1
      INTERNAL_ERROR     = 2
      OTHERS             = 3.

  IF SY-SUBRC <> 0.

    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO

    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_ADDR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GT_T001
*&---------------------------------------------------------------------*
FORM UPDATE_ADDR  CHANGING LPS_T001  TYPE T001.
  DATA:
    LV_ADDR_NUM       TYPE ADRC-ADDRNUMBER,
    LV_ADDR1_DATA     TYPE ADDR1_DATA,
    LV_RETURNCODE     TYPE AD_RETCODE,
    LS_ADDR1_SEL      LIKE  ADDR1_SEL,
    LT_ADDR_ERROR     TYPE TABLE OF ADDR_ERROR,
    LV_ADDR1_OLD_VERS TYPE TABLE OF ADDR_VERS,
    LS_AD1_FLAGS      LIKE   AD1_FLAGS,
    LV_ADDR_GET       LIKE  ADDR1_VAL.

  LV_ADDR_NUM = LPS_T001-ADRNR.

  LS_ADDR1_SEL-ADDRNUMBER = LV_ADDR_NUM.

  CALL FUNCTION 'ADDR_GET'
    EXPORTING
      ADDRESS_SELECTION       = LS_ADDR1_SEL
    IMPORTING
      ADDRESS_VALUE           = LV_ADDR_GET
      ADDRESS_ADDITIONAL_INFO = LS_AD1_FLAGS            "*1181i
*     returncode              = returncode              "*963d
      RETURNCODE              = LV_RETURNCODE           "*963i
    TABLES
      ERROR_TABLE             = E_TABLE
      VERSIONS                = LV_ADDR1_OLD_VERS
    EXCEPTIONS
      PARAMETER_ERROR         = 1
      ADDRESS_NOT_EXIST       = 2
      VERSION_NOT_EXIST       = 3
      INTERNAL_ERROR          = 4
      OTHERS                  = 5.
  DATA:
    LT_T001 TYPE T001,
    I_DATA  TYPE  ZST_BM_V_T001_ADDRESS.

  MOVE-CORRESPONDING LV_ADDR_GET TO LV_ADDR1_DATA.

  I_DATA = GS_DATA.
  SELECT SINGLE
    *
      FROM T001
      INTO CORRESPONDING FIELDS OF  LT_T001
      WHERE BUKRS = GV_BUKRS.

  IF SY-SUBRC IS INITIAL .
    IF  GV_TITLE_MEDI  IS NOT INITIAL.
      LV_ADDR1_DATA-TITLE = GV_TITLE_MEDI .
    ENDIF.
    IF  GV_NAME1  IS NOT INITIAL.
      LV_ADDR1_DATA-NAME1 = GV_NAME1 .
    ENDIF.
    IF  GV_NAME2  IS NOT INITIAL.
      LV_ADDR1_DATA-NAME2 = GV_NAME2 .
    ENDIF.
    IF  GV_SORT1  IS NOT INITIAL.
      LV_ADDR1_DATA-SORT1 = GV_SORT1 .
    ENDIF.
    IF  GV_SORT2  IS NOT INITIAL.
      LV_ADDR1_DATA-SORT2 = GV_SORT2 .
    ENDIF.
    IF  GV_STR_SUPPL1  IS NOT INITIAL.
      LV_ADDR1_DATA-STR_SUPPL1 = GV_STR_SUPPL1 .
    ENDIF.
    IF  GV_STREET  IS NOT INITIAL.
      LV_ADDR1_DATA-STREET = GV_STREET .
    ENDIF.
    IF  GV_POST_CODE1  IS NOT INITIAL.
      LV_ADDR1_DATA-POST_CODE1 = GV_POST_CODE1 .
    ENDIF.
    IF  GV_CITY1  IS NOT INITIAL.
      LV_ADDR1_DATA-CITY1 = GV_CITY1 .
    ENDIF.
    IF  GV_COUNTRY  IS NOT INITIAL.
      LV_ADDR1_DATA-COUNTRY = GV_COUNTRY .
    ENDIF.
    IF  GV_REGION IS NOT INITIAL.
      LV_ADDR1_DATA-REGION = GV_REGION .
    ENDIF.
    IF  GV_TIME_ZONE  IS NOT INITIAL..
      LV_ADDR1_DATA-TIME_ZONE = GV_TIME_ZONE .
    ENDIF.
    IF  GV_PO_BOX  IS NOT INITIAL.
      LV_ADDR1_DATA-PO_BOX = GV_PO_BOX .
    ENDIF.
    IF  GV_POST_CODE2  IS NOT INITIAL.
      LV_ADDR1_DATA-POST_CODE2 = GV_POST_CODE2 .
    ENDIF.
    IF  GV_POST_CODE3  IS NOT INITIAL.
      LV_ADDR1_DATA-POST_CODE3 = GV_POST_CODE3 .
    ENDIF.
    IF  GV_LANGU  IS NOT INITIAL.
      LV_ADDR1_DATA-LANGU = GV_LANGU .
    ENDIF.
    IF  GV_DEFLT_COMM  IS NOT INITIAL.
      LV_ADDR1_DATA-DEFLT_COMM = GV_DEFLT_COMM .
    ENDIF.
    IF  GV_REMARK  IS NOT INITIAL.
      LV_ADDR1_DATA-REMARK = GV_REMARK .
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ADDR_UPDATE'
    EXPORTING
      ADDRESS_DATA      = LV_ADDR1_DATA
      ADDRESS_NUMBER    = LV_ADDR_NUM
*     DATE_FROM         = '00010101'
*     LANGUAGE          = SY-LANGU
*     NATION            = ' '
*     CHECK_EMPTY_ADDRESS        = 'X'
*     CHECK_ADDRESS     = 'X'
*     CHECK_OTHER_VERSIONS       = 'X'
*     BLK_EXCPT         =
    IMPORTING
*     ADDRESS_DATA      =
      RETURNCODE        = LV_RETURNCODE
*     DATA_HAS_CHANGED  =
    TABLES
      ERROR_TABLE       = LT_ADDR_ERROR
    EXCEPTIONS
      ADDRESS_NOT_EXIST = 1
      PARAMETER_ERROR   = 2
      VERSION_NOT_EXIST = 3
      INTERNAL_ERROR    = 4
      ADDRESS_BLOCKED   = 5
      OTHERS            = 6.
  IF SY-SUBRC <> 0.

    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO

    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

  IF GV_TEL_NUMBER IS NOT INITIAL OR GV_MOB_NUMBER IS NOT INITIAL.
    PERFORM ADD_TEL CHANGING LV_ADDR_NUM.
  ENDIF.
  IF GV_FAX_NUMBER IS NOT INITIAL.
    PERFORM ADD_FAX CHANGING LV_ADDR_NUM.
  ENDIF.
  IF GV_SMTP_ADDR IS NOT INITIAL.
    PERFORM ADD_SMTP CHANGING LV_ADDR_NUM.
  ENDIF.

  CALL FUNCTION 'ADDR_MEMORY_SAVE'
    EXPORTING
      EXECUTE_IN_UPDATE_TASK = ' '
    EXCEPTIONS
      ADDRESS_NUMBER_MISSING = 1
      PERSON_NUMBER_MISSING  = 2
      INTERNAL_ERROR         = 3
      DATABASE_ERROR         = 4
      REFERENCE_MISSING      = 5
      OTHERS                 = 6.

  IF SY-SUBRC <> 0.

    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO

    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

  CALL FUNCTION 'ADDR_MEMORY_CLEAR'
    EXPORTING
      FORCE              = ' '
    EXCEPTIONS
      UNSAVED_DATA_EXIST = 1
      INTERNAL_ERROR     = 2
      OTHERS             = 3.

  IF SY-SUBRC <> 0.

    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO

    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK .
  CASE G_MODE.
    WHEN 'UPDATE'. " update
      PERFORM CHECK_UPDATE.
    WHEN 'INSERT'. " insert
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_UPDATE .
  DATA:
    LT_T001 TYPE T001,
    I_DATA  TYPE  ZST_BM_V_T001_ADDRESS.

  I_DATA = GS_DATA.
  SELECT SINGLE
    BUKRS
      FROM T001
      INTO CORRESPONDING FIELDS OF  LT_T001
      WHERE BUKRS = GV_BUKRS.

  IF SY-SUBRC IS INITIAL .
    IF  GV_BUTXT  IS INITIAL
       AND  GV_TITLE_MEDI IS INITIAL
       AND  GV_NAME1 IS INITIAL
       AND  GV_NAME2 IS INITIAL
       AND  GV_SORT1 IS INITIAL
       AND  GV_SORT2 IS INITIAL
       AND  GV_STR_SUPPL1 IS INITIAL
       AND  GV_STREET IS INITIAL
       AND  GV_POST_CODE1 IS INITIAL
       AND  GV_CITY1 IS INITIAL
       AND  GV_COUNTRY IS INITIAL
       AND  GV_REGION IS INITIAL
       AND  GV_TIME_ZONE IS INITIAL
       AND  GV_PO_BOX IS INITIAL
       AND  GV_POST_CODE2 IS INITIAL
       AND  GV_POST_CODE3 IS INITIAL
       AND  GV_LANGU IS INITIAL
       AND  GV_DEFLT_COMM IS INITIAL
       AND  GV_REMARK IS INITIAL.
    ELSE.
      LT_T001-BUTXT = GV_BUTXT.
      LT_T001-ORT01 = GV_ORT01.
      LT_T001-LAND1 = GV_LAND1.
      LT_T001-WAERS = GV_WAERS.
      LT_T001-SPRAS = GV_SPRAS.
      UPDATE  T001 FROM LT_T001.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_TEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_ADDR_NUM
*&---------------------------------------------------------------------*
FORM ADD_TEL  CHANGING LV_ADDR_NUM TYPE ADRC-ADDRNUMBER.
  DATA:
    LV_TABLE  LIKE SZAD_FIELD-TABLE_TYPE,
    LV_INDEX  TYPE INT4,
    LV_HANDLE TYPE SZAD_FIELD-HANDLE,
    LS_ADR2   TYPE TABLE OF ADR2.
  DATA: LV_RETURNCODE TYPE AD_RETCODE.
  IF GV_TEL_NUMBER IS NOT INITIAL.
    IF G_MODE = 'UPDATE'.
      LV_INDEX += 1.
      ADTEL-CONSNUMBER = LV_INDEX.
    ENDIF.

    ADTEL-TEL_NUMBER = GV_TEL_NUMBER.
    ADTEL-TEL_EXTENS = GV_TEL_EXTENS.
    ADTEL-COUNTRY    = GV_LAND1.
    ADTEL-FLGDEFAULT = 'X'.
    ADTEL-HOME_FLAG = 'X'.
    ADTEL-R3_USER    = '1'.
    IF G_MODE = 'UPDATE'.
      SELECT *
        FROM ADR2
        INTO TABLE LS_ADR2
        WHERE ADDRNUMBER = LV_ADDR_NUM.
      IF  SY-SUBRC IS INITIAL.
        ADTEL-UPDATEFLAG = 'U'.
      ELSE.
        ADTEL-UPDATEFLAG = 'I'.
      ENDIF.
    ELSE.
      ADTEL-UPDATEFLAG = 'I'.
      LV_HANDLE = 'Address'.
    ENDIF.
*  ADTEL-TEL_EXTENS = GS_DATA-TEL_EXTENS.
    APPEND ADTEL.
  ENDIF.

  IF GV_MOB_NUMBER IS NOT INITIAL.
    IF G_MODE = 'UPDATE'.
      LV_INDEX += 1.
      ADTEL-CONSNUMBER = LV_INDEX.
    ENDIF.
    ADTEL-TEL_NUMBER = GV_MOB_NUMBER.
    ADTEL-TEL_EXTENS = GV_MOB_NUMBER.
    ADTEL-COUNTRY    = GV_LAND1.
    ADTEL-FLGDEFAULT = ''.
    ADTEL-HOME_FLAG = ''.
    ADTEL-R3_USER    = '3'.
    IF G_MODE = 'UPDATE'.

      SELECT *
          FROM ADR2
          INTO TABLE LS_ADR2
          WHERE ADDRNUMBER = LV_ADDR_NUM.
      IF  SY-SUBRC IS INITIAL.
        ADTEL-UPDATEFLAG = 'U'.
      ELSE.
        ADTEL-UPDATEFLAG = 'I'.
      ENDIF.
    ELSE.
      ADTEL-UPDATEFLAG = 'I'.
      LV_HANDLE = 'Address'.
    ENDIF.
*  ADTEL-TEL_EXTENS = GS_DATA-TEL_EXTENS.
    APPEND ADTEL.
  ENDIF.

  LV_TABLE = 'ADTEL'.
  CALL FUNCTION 'ADDR_COMM_MAINTAIN'
    EXPORTING
      ADDRESS_NUMBER    = LV_ADDR_NUM
      ADDRESS_HANDLE    = LV_HANDLE
*     DATE_FROM         = '00010101'
*     LANGUAGE          = SY-LANGU
      TABLE_TYPE        = LV_TABLE
*     SUBSTITUTE_ALL_COMM_DATA             = ' '
*     ACCEPT_TELNR_CALL = ' '
*     CONSIDER_CONSNUMBER_FOR_INSERT       = ' '
*     CHECK_ADDRESS     = 'X'
*     IV_TIME_DEPENDENCE                   = ' '
*     IV_CONSIDER_ADRCOMC_FOR_INSERT       = 'X'
*     BLK_EXCPT         =
    IMPORTING
      RETURNCODE        = LV_RETURNCODE
    TABLES
      COMM_TABLE        = ADTEL
      ERROR_TABLE       = E_TABLE
*     COMM_TABLE_OUT    =
*     IT_USAGE          =
*     IT_USAGE_UUID     =
*     ET_USAGE_OUT      =
*     ET_USAGE_UUID_OUT =
*     ET_CREATED_USAGE_UUID                =
    EXCEPTIONS
      PARAMETER_ERROR   = 1
      ADDRESS_NOT_EXIST = 2
      INTERNAL_ERROR    = 3
      ADDRESS_BLOCKED   = 4
      OTHERS            = 5.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_FAX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_ADDR_NUM
*&---------------------------------------------------------------------*
FORM ADD_FAX  CHANGING LV_ADDR_NUM TYPE ADRC-ADDRNUMBER.
  DATA:
    LV_TABLE  LIKE SZAD_FIELD-TABLE_TYPE,
    LV_INDEX  TYPE INT4,
    LV_HANDLE TYPE SZAD_FIELD-HANDLE,
    LS_ADR3   TYPE TABLE OF ADR3.
  DATA: LV_RETURNCODE TYPE AD_RETCODE.

  IF G_MODE = 'UPDATE'.
    LV_INDEX += 1.
    ADFAX-CONSNUMBER = LV_INDEX.
  ENDIF.

  ADFAX-FAX_NUMBER = GV_FAX_NUMBER.
  ADFAX-FAX_EXTENS = GV_FAX_EXTENS.
  ADFAX-COUNTRY    = GV_LAND1.
  ADFAX-FLGDEFAULT = 'X'.
  ADFAX-HOME_FLAG = 'X'.
  ADFAX-R3_USER    = '1'.
  IF G_MODE = 'UPDATE'.
    SELECT *
           FROM ADR3
           INTO TABLE LS_ADR3
           WHERE ADDRNUMBER = LV_ADDR_NUM.
    IF  SY-SUBRC IS INITIAL.
      ADFAX-UPDATEFLAG = 'U'.
    ELSE.
      ADFAX-UPDATEFLAG = 'I'.
    ENDIF.
  ELSE.
    ADFAX-UPDATEFLAG = 'I'.
    LV_HANDLE = 'Address'.
  ENDIF.
*  ADTEL-TEL_EXTENS = GS_DATA-TEL_EXTENS.
  APPEND ADFAX.

  LV_TABLE = 'ADFAX'.
  CALL FUNCTION 'ADDR_COMM_MAINTAIN'
    EXPORTING
      ADDRESS_HANDLE    = LV_HANDLE
      ADDRESS_NUMBER    = LV_ADDR_NUM
*     DATE_FROM         = '00010101'
*     LANGUAGE          = SY-LANGU
      TABLE_TYPE        = LV_TABLE
*     SUBSTITUTE_ALL_COMM_DATA             = ' '
*     ACCEPT_TELNR_CALL = ' '
*     CONSIDER_CONSNUMBER_FOR_INSERT       = ' '
*     CHECK_ADDRESS     = 'X'
*     IV_TIME_DEPENDENCE                   = ' '
*     IV_CONSIDER_ADRCOMC_FOR_INSERT       = 'X'
*     BLK_EXCPT         =
    IMPORTING
      RETURNCODE        = LV_RETURNCODE
    TABLES
      COMM_TABLE        = ADFAX
      ERROR_TABLE       = E_TABLE
*     COMM_TABLE_OUT    =
*     IT_USAGE          =
*     IT_USAGE_UUID     =
*     ET_USAGE_OUT      =
*     ET_USAGE_UUID_OUT =
*     ET_CREATED_USAGE_UUID                =
    EXCEPTIONS
      PARAMETER_ERROR   = 1
      ADDRESS_NOT_EXIST = 2
      INTERNAL_ERROR    = 3
      ADDRESS_BLOCKED   = 4
      OTHERS            = 5.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_SMTP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_ADDR_NUM
*&---------------------------------------------------------------------*
FORM ADD_SMTP  CHANGING LV_ADDR_NUM TYPE ADRC-ADDRNUMBER.
  DATA:
    LV_TABLE  LIKE SZAD_FIELD-TABLE_TYPE,
    LV_INDEX  TYPE INT4,
    LV_HANDLE TYPE SZAD_FIELD-HANDLE,
    LS_ADR6   TYPE TABLE OF ADR6.
  DATA: LV_RETURNCODE TYPE AD_RETCODE.

  IF G_MODE = 'UPDATE'.
    LV_INDEX += 1.
    ADSMTP-CONSNUMBER = LV_INDEX.
  ENDIF.

  ADSMTP-SMTP_ADDR = GV_SMTP_ADDR.
*  ADSMTP-SMTP_SRCH = GS_DATA-SMTP_SRCH.
*  ADSMTP-COUNTRY    = GS_DATA-LAND1.
  ADSMTP-FLGDEFAULT = 'X'.
  ADSMTP-HOME_FLAG = 'X'.
  ADSMTP-R3_USER    = '1'.
  IF G_MODE = 'UPDATE'.
    SELECT *
           FROM ADR6
           INTO TABLE LS_ADR6
           WHERE ADDRNUMBER = LV_ADDR_NUM.
    IF  SY-SUBRC IS INITIAL.
      ADSMTP-UPDATEFLAG = 'U'.
    ELSE.
      ADSMTP-UPDATEFLAG = 'I'.
    ENDIF.
  ELSE.
    ADSMTP-UPDATEFLAG = 'I'.
    LV_HANDLE = 'Address'.
  ENDIF.
*  ADTEL-TEL_EXTENS = GS_DATA-TEL_EXTENS.
  APPEND ADSMTP.

  LV_TABLE = 'ADSMTP'.
  CALL FUNCTION 'ADDR_COMM_MAINTAIN'
    EXPORTING
      ADDRESS_HANDLE    = LV_HANDLE
      ADDRESS_NUMBER    = LV_ADDR_NUM
*     DATE_FROM         = '00010101'
*     LANGUAGE          = SY-LANGU
      TABLE_TYPE        = LV_TABLE
*     SUBSTITUTE_ALL_COMM_DATA             = ' '
*     ACCEPT_TELNR_CALL = ' '
*     CONSIDER_CONSNUMBER_FOR_INSERT       = ' '
*     CHECK_ADDRESS     = 'X'
*     IV_TIME_DEPENDENCE                   = ' '
*     IV_CONSIDER_ADRCOMC_FOR_INSERT       = 'X'
*     BLK_EXCPT         =
    IMPORTING
      RETURNCODE        = LV_RETURNCODE
    TABLES
      COMM_TABLE        = ADSMTP
      ERROR_TABLE       = E_TABLE
*     COMM_TABLE_OUT    =
*     IT_USAGE          =
*     IT_USAGE_UUID     =
*     ET_USAGE_OUT      =
*     ET_USAGE_UUID_OUT =
*     ET_CREATED_USAGE_UUID                =
    EXCEPTIONS
      PARAMETER_ERROR   = 1
      ADDRESS_NOT_EXIST = 2
      INTERNAL_ERROR    = 3
      ADDRESS_BLOCKED   = 4
      OTHERS            = 5.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

FORM CHECKMODE .
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'GV_BUKRS'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ELSEIF
      SCREEN-NAME = 'GV_ADRNR'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form LOAD_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM LOAD_DATA .
  DATA:
    LS_ADRC  TYPE ADRC,
    LS_ADRCT TYPE ADRCT,
    LT_ADR2  TYPE TABLE OF ADR2,
    LS_ADR3  TYPE ADR3,
    LS_ADR6  TYPE ADR6.
  SELECT SINGLE
    *
      FROM T001
      INTO  CORRESPONDING FIELDS OF GT_T001
      WHERE BUKRS = GV_BUKRS.
  IF SY-SUBRC IS INITIAL.
    MESSAGE 'Đã tồn tại!' TYPE 'S'.
    GV_BUKRS = GT_T001-BUKRS.
    GV_BUTXT = GT_T001-BUTXT.
    GV_ORT01 = GT_T001-ORT01.
    GV_LAND1 = GT_T001-LAND1.
    GV_WAERS = GT_T001-WAERS.
    GV_SPRAS = GT_T001-SPRAS.
    GV_ADRNR = GT_T001-ADRNR.
  ENDIF.

  IF GV_ADRNR IS  INITIAL.
  ELSE.
    SELECT SINGLE
      *
      FROM ADRC
      INTO CORRESPONDING FIELDS OF LS_ADRC
      WHERE  ADDRNUMBER = GV_ADRNR.
    IF SY-SUBRC IS INITIAL.
      GV_TITLE_MEDI = LS_ADRC-TITLE.
      GV_NAME1 = LS_ADRC-NAME1.
      GV_NAME2 = LS_ADRC-NAME2.
      GV_SORT1 = LS_ADRC-SORT1.
      GV_SORT2 = LS_ADRC-SORT2.
      GV_STR_SUPPL1 = LS_ADRC-STR_SUPPL1.
      GV_STREET = LS_ADRC-STREET.
      GV_POST_CODE1 = LS_ADRC-POST_CODE1 .
      GV_CITY1 = LS_ADRC-CITY1.
      GV_COUNTRY = LS_ADRC-COUNTRY.
      GV_REGION = LS_ADRC-REGION.
      GV_TIME_ZONE = LS_ADRC-TIME_ZONE.
      GV_PO_BOX = LS_ADRC-PO_BOX.
      GV_POST_CODE2 = LS_ADRC-POST_CODE2.
      GV_POST_CODE3 = LS_ADRC-POST_CODE3.
      GV_LANGU = LS_ADRC-LANGU.
      GV_DEFLT_COMM = LS_ADRC-DEFLT_COMM.
    ENDIF.
    SELECT SINGLE
      *
      FROM ADRCT
      INTO CORRESPONDING FIELDS OF LS_ADRCT
      WHERE  ADDRNUMBER = GV_ADRNR.
    IF SY-SUBRC IS INITIAL.
      GV_REMARK = LS_ADRCT-REMARK.
    ENDIF.
    SELECT
      *
      FROM ADR2
      INTO TABLE LT_ADR2
      WHERE  ADDRNUMBER = GV_ADRNR.
    IF SY-SUBRC IS INITIAL.
      READ TABLE LT_ADR2 INTO DATA(LS_ADR2) WITH KEY  R3_USER = 1 .
      IF SY-SUBRC IS INITIAL.
        GV_TEL_NUMBER = LS_ADR2-TEL_NUMBER.
        GV_TEL_EXTENS = LS_ADR2-TEL_EXTENS.
      ENDIF.
      READ TABLE LT_ADR2 INTO LS_ADR2 WITH KEY  R3_USER = 3 .
      IF SY-SUBRC IS INITIAL.
        GV_MOB_NUMBER = LS_ADR2-TEL_NUMBER.
      ENDIF.

    ENDIF.
    SELECT SINGLE
    *
    FROM ADR3
    INTO CORRESPONDING FIELDS OF LS_ADR3
    WHERE  ADDRNUMBER = GV_ADRNR.
    IF SY-SUBRC IS INITIAL.
      GV_FAX_NUMBER = LS_ADR3-FAX_NUMBER.
      GV_FAX_EXTENS = LS_ADR3-FAX_EXTENS.
    ENDIF.
    SELECT SINGLE
   *
   FROM ADR6
   INTO CORRESPONDING FIELDS OF LS_ADR6
   WHERE  ADDRNUMBER = GV_ADRNR.
    IF SY-SUBRC IS INITIAL.
      GV_SMTP_ADDR = LS_ADR6-SMTP_ADDR.
    ENDIF.
  ENDIF.
ENDFORM.
FORM LIST.
  DATA: GT_LIST     TYPE VRM_VALUES.
  DATA: WA_LIST TYPE VRM_VALUE.

  SELECT TSAC~COMM_TYPE,
    TSACT~COMM_TEXT
    FROM TSAC
    INNER JOIN TSACT ON TSAC~COMM_TYPE = TSACT~COMM_TYPE
    WHERE LANGU = @SY-LANGU
    INTO TABLE @DATA(LT_TSAC).
  LOOP AT LT_TSAC INTO DATA(LS_TSAC).
    WA_LIST-KEY  = LS_TSAC-COMM_TYPE.
    WA_LIST-TEXT = LS_TSAC-COMM_TEXT.
    APPEND WA_LIST TO GT_LIST.
  ENDLOOP.
*hien thi list
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = 'GV_DEFLT_COMM'
      VALUES          = GT_LIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
ENDFORM.
