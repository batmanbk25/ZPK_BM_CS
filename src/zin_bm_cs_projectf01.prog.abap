*&---------------------------------------------------------------------*
*& Include          ZIN_BM_CS_PROJECTF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form MAIN_PROCESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM  MAIN_PROCESS .
  PERFORM GET_DATA.
  PERFORM PROCESS_DATA.
  CALL SCREEN 1001.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA.
  SELECT *
    FROM ZTB_BM_CSPJ_P
    INTO CORRESPONDING FIELDS OF TABLE GT_ALV
    WHERE
    ZTB_BM_CSPJ_P~PROJID IN S_PROJID.
  IF GT_ALV IS INITIAL.
    MESSAGE 'No data' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  SORT GT_ALV BY PROJID.
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
  LOOP AT GT_ALV ASSIGNING FIELD-SYMBOL(<LFS_ALV>).
    <LFS_ALV>-DETAIL_BTN = 'Detail'.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BUILD_FCAT .

  DATA: LW_COLTEXT TYPE TEXT255,
        LW_POS     TYPE INT4,
        LS_FCAT    LIKE LINE OF GT_FIELDCAT.

  CLEAR:
        GS_LAYOUT.
  GS_LAYOUT-ZEBRA      = 'X'.
  GS_LAYOUT-CWIDTH_OPT = 'X'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'ZST_BM_CSPJ_P'
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
      CASE <FS_FIELDCAT>-FIELDNAME.
        WHEN 'PROJID'.
          <FS_FIELDCAT>-COL_POS       = 0.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'DESCR'.
          <FS_FIELDCAT>-COL_POS       = 1.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'DATBE'.
          <FS_FIELDCAT>-COL_POS       = 2.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'DATEN'.
          <FS_FIELDCAT>-COL_POS       = 3.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'DETAIL_BTN'.
          <FS_FIELDCAT>-COLTEXT       = 'Button'.
          <FS_FIELDCAT>-STYLE         = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 4.
        WHEN OTHERS.
          <FS_FIELDCAT>-TECH          = 'X'.
          <FS_FIELDCAT>-NO_OUT        = 'X'.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM BUTTON_CLICK  USING    P_ES_COL_ID TYPE LVC_S_COL
                            P_ES_ROW_NO TYPE LVC_S_ROID.
  DATA: LV_COL_ID TYPE LVC_S_ROW.
  DATA: LV_ROW_NO TYPE LVC_S_ROID.

  LV_COL_ID = P_ES_COL_ID. "Cot nhan duoc
  LV_ROW_NO = P_ES_ROW_NO. "Hang nhan duoc
  READ TABLE GT_ALV INTO DATA(LS_ALV)
  INDEX  P_ES_ROW_NO-ROW_ID.
  IF SY-SUBRC = 0 .

    SUBMIT ZPG_BM_CS_REPORT  AND RETURN
     WITH P_STATUS = '*'
     WITH S_PROJID-LOW = LS_ALV-PROJID.
    IF SY-SUBRC > 0.
      MESSAGE 'No data' TYPE 'S' DISPLAY LIKE 'E'.                             "error processing
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PROJECT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_PROJECT.
  DATA:
        LV_RESULT TYPE CHAR1.
  PERFORM LIST CHANGING LV_RESULT.
  IF LV_RESULT = 'A'.
  ELSE.
    G_MODE = '1'.
    CLEAR
    GV_PROJID.
    CLEAR
    GV_DESCR.
    CLEAR
    GV_DATBE.
    CLEAR
    GV_DATEN .

    CALL SCREEN 1002.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form list
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM LIST  CHANGING LV_RESULT TYPE CHAR1.
  DATA: LT_LIST   TYPE  CCSEQ_T_VALUES,
        LT_CSTR_M TYPE TABLE OF ZTB_BM_CSTR_M,
        WA_SPOPLI TYPE SPOPLI.

*  CLEAR: WA_SPOPLI,
*         GT_LIST.
*  WA_SPOPLI-VAROPTION = 'FI'.
*  APPEND WA_SPOPLI TO LT_LIST.
*
*  CLEAR: WA_SPOPLI.
*  WA_SPOPLI-VAROPTION = 'CO'.
*  APPEND WA_SPOPLI TO LT_LIST.
*
*  CLEAR: WA_SPOPLI.
*  WA_SPOPLI-VAROPTION = 'MM'.
*  APPEND WA_SPOPLI TO LT_LIST.
*
*  CLEAR: WA_SPOPLI.
*  WA_SPOPLI-VAROPTION = 'SD'.
*  APPEND WA_SPOPLI TO LT_LIST.


  SELECT *
    FROM  ZTB_BM_CSTR_M
    INTO TABLE LT_CSTR_M.
  LOOP AT LT_CSTR_M INTO DATA(LS_CSTR_M).
    WA_SPOPLI-VAROPTION = LS_CSTR_M-MODLID.
    APPEND WA_SPOPLI TO LT_LIST.
  ENDLOOP.
  CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
    EXPORTING
      MARK_FLAG          = 'X'
      MARK_MAX           = '100'
      TITEL              = 'Modules'
      TEXTLINE1          = 'Choose Modules'
    IMPORTING
      ANSWER             = LV_RESULT
    TABLES
      T_SPOPLI           = LT_LIST
    EXCEPTIONS
      NOT_ENOUGH_ANSWERS = 1
      TOO_MUCH_ANSWERS   = 2
      TOO_MUCH_MARKS     = 3
      OTHERS             = 4.
  .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  GT_LIST = LT_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE .
  DATA:
    WA        TYPE ZST_BM_CSPJ_P,
    WA1       TYPE ZTB_BM_CSPJ_C,
    LS_ALV    TYPE ZTB_BM_CSPJ_P,
    LT_CSTR_C TYPE TABLE OF  ZTB_BM_CSTR_C,
    LT_QTT    TYPE TABLE OF  ZTB_BM_CSPJ_C,
    LS_QTT    TYPE   ZTB_BM_CSPJ_C,
    WA2       TYPE ZTB_BM_CSPJ_D.
  CASE  G_MODE.
*MODE UPDATE
    WHEN '1'.
      G_HIDE = '1'.
      SELECT SINGLE
          PROJID
          DESCR
          DATBE
          DATEN
      FROM ZTB_BM_CSPJ_P
      INTO CORRESPONDING FIELDS OF  LS_ALV
      WHERE PROJID = GV_PROJID.
      IF SY-SUBRC IS INITIAL.
        MESSAGE  ' Project đã tồn tại !' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        IF GV_PROJID IS INITIAL OR GV_DATBE IS INITIAL OR GV_DATEN IS INITIAL.
          MESSAGE ' Bạn nhập thiếu dữ liệu !' TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          IF SY-UCOMM = '&DATA_SAVE'.
            WA-PROJID = GV_PROJID.
            WA-DESCR  = GV_DESCR.
            WA-DATBE  = GV_DATBE.
            WA-DATEN  = GV_DATEN.
            INSERT  ZTB_BM_CSPJ_P FROM WA.
            LOOP AT GT_LIST INTO DATA(LS_LIST)
             WHERE SELFLAG = 'X'.
              LS_QTT-MODLID =  LS_LIST-VAROPTION.
              APPEND LS_QTT TO LT_QTT.
            ENDLOOP.
            SELECT *
            FROM ZTB_BM_CSTR_C
              INTO  TABLE @LT_CSTR_C
               FOR ALL ENTRIES IN @LT_QTT
          WHERE
                 MODLID = @LT_QTT-MODLID.
            LOOP AT LT_CSTR_C INTO DATA(LS_CSTR_C).
              WA1-PROJID = GV_PROJID.
              WA1-MODLID = LS_CSTR_C-MODLID.
              WA1-CSFUNC = LS_CSTR_C-CSFUNC.
              WA1-VIEWNAME = LS_CSTR_C-VIEWNAME.
              WA1-CSFSTS = '1'.
              INSERT  ZTB_BM_CSPJ_C FROM WA1.
              WA2-PROJID = GV_PROJID.
              WA2-MODLID = LS_CSTR_C-MODLID.
              WA2-CSFUNC = LS_CSTR_C-CSFUNC.
              WA2-VIEWNAME = LS_CSTR_C-VIEWNAME.
              WA2-CSFSTS = '1'.
              INSERT  ZTB_BM_CSPJ_D FROM WA2.
            ENDLOOP.
            MESSAGE 'Create successful'   TYPE 'S'.
            COMMIT WORK AND WAIT.
            WA-PROJID = GV_PROJID.
            WA-DESCR = GV_DESCR.
            WA-DATBE = GV_DATBE.
            WA-DATEN = GV_DATEN.
            WA-DETAIL_BTN = 'Detail'.
            APPEND WA TO GT_ALV.
            SORT GT_ALV BY PROJID.
            G_MODE = '2'.

          ENDIF.
        ENDIF.
      ENDIF.
*MODE INSERT
    WHEN '2'.
      CLEAR:
      WA.
      WA-PROJID = GV_PROJID.
      WA-DESCR = GV_DESCR.
      WA-DATBE = GV_DATBE.
      WA-DATEN = GV_DATEN.
      MODIFY ZTB_BM_CSPJ_P FROM WA.

      CALL METHOD G_ALV_GRID1->CHECK_CHANGED_DATA( ).

      LOOP AT GT_RESELECT INTO DATA(LS_RESELECT)
               WHERE CHECK = 'X'.
        LS_QTT-MODLID =  LS_RESELECT-MODLID.
        APPEND LS_QTT TO LT_QTT.
      ENDLOOP.
      SELECT *
      FROM ZTB_BM_CSTR_C
        INTO  TABLE @LT_CSTR_C
         FOR ALL ENTRIES IN @LT_QTT
    WHERE
           MODLID = @LT_QTT-MODLID.
      LOOP AT LT_CSTR_C INTO LS_CSTR_C .
        WA1-PROJID = GV_PROJID.
        WA1-MODLID = LS_CSTR_C-MODLID.
        WA1-CSFUNC = LS_CSTR_C-CSFUNC.
        WA1-VIEWNAME = LS_CSTR_C-VIEWNAME.
        WA1-CSFSTS = '1'.
        INSERT  ZTB_BM_CSPJ_C FROM WA1.
        WA2-PROJID = GV_PROJID.
        WA2-MODLID = LS_CSTR_C-MODLID.
        WA2-CSFUNC = LS_CSTR_C-CSFUNC.
        WA2-VIEWNAME = LS_CSTR_C-VIEWNAME.
        WA2-CSFSTS = '1'.
        INSERT  ZTB_BM_CSPJ_D FROM WA2.
      ENDLOOP.
      READ TABLE GT_ALV ASSIGNING FIELD-SYMBOL(<LS_ALV>) WITH KEY PROJID = GV_PROJID.
      <LS_ALV>-PROJID = GV_PROJID.
      <LS_ALV>-DESCR = GV_DESCR.
      <LS_ALV>-DATBE = GV_DATBE.
      <LS_ALV>-DATEN = GV_DATEN.

      MESSAGE 'UPDATE successful'   TYPE 'S'.
      COMMIT WORK AND WAIT.
      SORT GT_ALV BY PROJID.
    WHEN OTHERS.
  ENDCASE.

  DATA: IS_STABLE TYPE LVC_S_STBL.
  IS_STABLE-ROW = 'X'.
  IS_STABLE-COL = 'X'.

  CALL METHOD G_ALV_GRID->REFRESH_TABLE_DISPLAY(
    EXPORTING
      IS_STABLE = IS_STABLE
    EXCEPTIONS
      FINISHED  = 1
      OTHERS    = 2 ).
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
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'GV_PROJID'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EDIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM EDIT.
  DATA:
    LT_SEL_ROWS TYPE LVC_T_ROID,
    LS_SEL_ROW  TYPE LVC_S_ROID.
  CALL METHOD G_ALV_GRID->CHECK_CHANGED_DATA.
  CALL METHOD G_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_ROW_NO = LT_SEL_ROWS.

  IF LT_SEL_ROWS[] IS INITIAL.
    MESSAGE 'Hãy chọn ít nhất  một dòng' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF LINES( LT_SEL_ROWS ) > 1.
    MESSAGE 'Chỉ được chọn một dòng' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ELSE.
    G_MODE = '2'.
    LOOP AT LT_SEL_ROWS INTO LS_SEL_ROW.
      READ TABLE GT_ALV INTO DATA(LS_ALV) INDEX LS_SEL_ROW-ROW_ID.
      GV_PROJID = LS_ALV-PROJID.
      GV_DESCR  = LS_ALV-DESCR.
      GV_DATBE  = LS_ALV-DATBE.
      GV_DATEN  = LS_ALV-DATEN.
    ENDLOOP.
    CALL SCREEN 1002.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form RESELCET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM RESELCET .
*  DATA: LT_LIST1  TYPE  CCSEQ_T_VALUES,
*        LT_CSTR_M TYPE TABLE OF ZTB_BM_CSTR_M,
*        LT_CSPJ_C TYPE TABLE OF ZTB_BM_CSPJ_C,
*        WA_SPOPLI TYPE SPOPLI,
*        LV_RESULT TYPE CHAR1.
*
*  SELECT *
*     FROM ZTB_BM_CSPJ_C
*    INTO TABLE LT_CSPJ_C
*    WHERE
*    PROJID = GV_PROJID.
*
*  SELECT *
*    FROM  ZTB_BM_CSTR_M
*    INTO TABLE LT_CSTR_M.
*
*  LOOP AT LT_CSTR_M INTO DATA(LS_CSTR_M).
*    CLEAR:
*    WA_SPOPLI.
*    READ TABLE LT_CSPJ_C INTO DATA(LS_CSPJ_C)
*    WITH KEY PROJID = GV_PROJID
*             MODLID = LS_CSTR_M-MODLID.
*    IF SY-SUBRC IS INITIAL.
*      WA_SPOPLI-VAROPTION = LS_CSTR_M-MODLID.
*      WA_SPOPLI-INACTIVE  = 'X'.
*      WA_SPOPLI-SELFLAG   = 'X'.
*    ELSE.
*      WA_SPOPLI-VAROPTION = LS_CSTR_M-MODLID.
*    ENDIF.
*    APPEND WA_SPOPLI TO LT_LIST1.
*  ENDLOOP.
*  CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
*    EXPORTING
*      MARK_FLAG          = 'X'
*      MARK_MAX           = '100'
*      TITEL              = 'Modules'
*      TEXTLINE1          = 'Choose Modules'
*    IMPORTING
*      ANSWER             = LV_RESULT
*    TABLES
*      T_SPOPLI           = LT_LIST1
*    EXCEPTIONS
*      NOT_ENOUGH_ANSWERS = 1
*      TOO_MUCH_ANSWERS   = 2
*      TOO_MUCH_MARKS     = 3
*      OTHERS             = 4.
*  .
*  IF SY-SUBRC <> 0.
** Implement suitable error handling here
*  ENDIF.
*  GT_LIST1 = LT_LIST1.
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form Detail
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DETAIL .
  SUBMIT ZPG_BM_CS_REPORT  AND RETURN
   WITH P_STATUS = '*'
   WITH S_PROJID-LOW = GV_PROJID.
ENDFORM.
FORM  RESELECT.
  DATA:
    LT_CSTR_M TYPE TABLE OF ZTB_BM_CSTR_M,
    LT_CSPJ_C TYPE TABLE OF ZTB_BM_CSPJ_C.
  CLEAR
  GT_RESELECT.
  SELECT *
    FROM ZTB_BM_CSTR_M
    INTO CORRESPONDING FIELDS OF TABLE   GT_RESELECT.
  IF SY-SUBRC = 0.
    SELECT *
     FROM ZTB_BM_CSPJ_C
    INTO TABLE LT_CSPJ_C
    WHERE
    PROJID = GV_PROJID.
    LOOP AT GT_RESELECT ASSIGNING FIELD-SYMBOL(<LS_RESELECT>).
      IF G_MODE = '2'.
        READ TABLE LT_CSPJ_C INTO DATA(LS_CSPJ_C)
     WITH KEY PROJID = GV_PROJID
              MODLID = <LS_RESELECT>-MODLID.
        IF SY-SUBRC IS INITIAL.
          GS_STYLE-STYLE =  CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
          <LS_RESELECT>-CHECK = 'X'.
        ELSE.
          GS_STYLE-STYLE =  CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ENDIF.
      ELSEIF G_MODE = '3'.
        READ TABLE LT_CSPJ_C INTO LS_CSPJ_C
      WITH KEY PROJID = GV_PROJID
               MODLID = <LS_RESELECT>-MODLID.
        IF SY-SUBRC IS INITIAL.
          GS_STYLE-STYLE =  CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
          <LS_RESELECT>-CHECK = 'X'.
        ELSE.
          GS_STYLE-STYLE =  CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        ENDIF.
      ENDIF.
      APPEND GS_STYLE TO <LS_RESELECT>-STYLE.
    ENDLOOP.
  ENDIF.
  CLEAR:
        GS_LAYOUT.
  GS_LAYOUT-CWIDTH_OPT = 'X'.
  GS_LAYOUT-NO_TOOLBAR = 'X'.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'ZST_BM_CSTR_M'
    CHANGING
      CT_FIELDCAT            = GT_FIELDCAT1[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    LOOP AT GT_FIELDCAT1 ASSIGNING FIELD-SYMBOL(<FS_FIELDCAT1>).
      CASE <FS_FIELDCAT1>-FIELDNAME.
        WHEN 'CHECK'.
          <FS_FIELDCAT1>-COL_POS       = 0.
          <FS_FIELDCAT1>-CHECKBOX = 'X'.
          <FS_FIELDCAT1>-EDIT = 'X'.
        WHEN 'MODLID'.
          <FS_FIELDCAT1>-COL_POS       = 1.
          <FS_FIELDCAT1>-OUTPUTLEN     = 10.

        WHEN 'MODTXT'.
          <FS_FIELDCAT1>-COL_POS       = 2.
          <FS_FIELDCAT1>-OUTPUTLEN     = 10.
        WHEN OTHERS.

          <FS_FIELDCAT1>-TECH          = 'X'.
          <FS_FIELDCAT1>-NO_OUT        = 'X'.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HIDE_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM HIDE_ALV .
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'RESELECT'.
      SCREEN-ACTIVE = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF G_CUSTOM_CONTAINER1 IS NOT INITIAL.
    G_ALV_GRID1->FREE( ).
    G_CUSTOM_CONTAINER1->FREE( ).
    GV_FREE1 = '1'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXPORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM EXPORT .
  DATA:
    LS_EXCEL  TYPE ZST_BM_EXCEL,
    LS_DSCH   TYPE  ZST_BM_EXCEL_DETAIL,
    LT_DSCH   TYPE TABLE OF  ZST_BM_EXCEL_DETAIL,
    LT_T001   TYPE TABLE OF T001,
    LS_DETAIL TYPE ZST_BM_EXCEL2,
    LS_VALUE  TYPE ZST_BM_VALUA,
    LS_VALUE1 TYPE ZST_BM_VALUA1,
    LS_VALUE2 TYPE ZTT_BM_LINE,
    LS_VALUE3 TYPE ZTT_BM_LINE1,
    LV_C      TYPE INT4,
    LV_D      TYPE INT4,
    LV_VCD    TYPE DD02L-TABNAME,
    LT_CSPJ_C TYPE TABLE OF GTY_CSPJ_C,
    LT_CSTR_C TYPE TABLE OF ZTB_BM_CSTR_C.
  DATA: LT_FIELDNAME   TYPE TABLE OF ZST_BM_VALUA,
        LT_DESCRIPTION TYPE TABLE OF ZST_BM_VALUA1.

  LS_EXCEL-DESCR = GV_PROJID && '-' && GV_DESCR.

  SELECT
    ZTB_BM_CSPJ_C~PROJID,
    ZTB_BM_CSPJ_C~MODLID,
    ZTB_BM_CSPJ_C~CSFUNC,
    ZTB_BM_CSTR_C~STTURE,
    ZTB_BM_CSTR_C~TCODE,
    ZTB_BM_CSPJ_C~VCL_NAME,
    ZTB_BM_CSPJ_C~VIEWNAME,
    ZTB_BM_CSPJ_C~SUMREC,
    ZTB_BM_CSPJ_C~COMREC,
    ZTB_BM_CSPJ_C~CSFSTS,
    ZTB_BM_CSTR_C~DESCR
       FROM ZTB_BM_CSPJ_C
    LEFT OUTER JOIN ZTB_BM_CSTR_C
     ON ZTB_BM_CSTR_C~MODLID = ZTB_BM_CSPJ_C~MODLID
     AND ZTB_BM_CSTR_C~CSFUNC = ZTB_BM_CSPJ_C~CSFUNC
      INTO CORRESPONDING FIELDS OF TABLE @LT_CSPJ_C
        WHERE
        PROJID = @GV_PROJID.
  SORT LT_CSPJ_C BY CSFUNC MODLID.
  LOOP AT LT_CSPJ_C  INTO DATA(LS_CSPJ_C).
    LV_C = LV_C + 1.
    LV_D = LV_C + 4.
    LS_DSCH-CSFUNC = LS_CSPJ_C-CSFUNC.
    LS_DSCH-MODLID = LS_CSPJ_C-MODLID.
    LS_DSCH-DESCR = GV_DESCR.
    LS_DSCH-TCODE = LS_CSPJ_C-TCODE.
    LS_DSCH-DESCR_DETAIL = LS_CSPJ_C-DESCR.
    LS_DSCH-VIEWNAME = LS_CSPJ_C-VIEWNAME.
    LS_DSCH-NAME = LS_CSPJ_C-CSFUNC && '-' && LS_CSPJ_C-VIEWNAME.
    LS_DSCH-STT = LV_C.
    LS_DSCH-LINK = `=HYPERLINK("#"&"'" & C`&& LV_D  && `& "'!A1",C` && LV_D  && `)`.
*    =HYPERLINK("#"&"'" & C6 & "'!A1";C6)
*Dong,Vu Uu tien lay trong structure truoc-------------------------
    IF  LS_CSPJ_C-STTURE IS NOT INITIAL.
      LV_VCD = LS_CSPJ_C-STTURE.
    ELSEIF LS_CSPJ_C-VIEWNAME IS NOT INITIAL.
      LV_VCD = LS_CSPJ_C-VIEWNAME.
    ELSEIF LS_CSPJ_C-VCL_NAME IS NOT INITIAL.
      LV_VCD = LS_CSPJ_C-VCL_NAME.
    ENDIF.
*Dong,Vu------------------------------end---------------------------
    CLEAR
      GT_FIELDCAT1.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        I_STRUCTURE_NAME       = LV_VCD
      CHANGING
        CT_FIELDCAT            = GT_FIELDCAT1[]
      EXCEPTIONS
        INCONSISTENT_INTERFACE = 1
        PROGRAM_ERROR          = 2
        OTHERS                 = 3.
    LOOP AT  GT_FIELDCAT1 INTO DATA(LFS_FIELDCAT1).
      LS_VALUE-VALUE = LFS_FIELDCAT1-FIELDNAME .
      APPEND LS_VALUE TO LS_DSCH-FIELDNAME.
      LS_VALUE1-VALUE = LFS_FIELDCAT1-SCRTEXT_L.
      APPEND LS_VALUE1 TO LS_DSCH-DESCRIPTION.
      LS_VALUE2-VALUE = ''.
      APPEND LS_VALUE2 TO LS_DSCH-LINE.
      LS_VALUE3-VALUE = ''.
      APPEND LS_VALUE3 TO LS_DSCH-LINE1.
    ENDLOOP.
    APPEND LS_DSCH TO LS_EXCEL-DSCH.
    CLEAR
     LS_DSCH.
  ENDLOOP.
  SORT LS_EXCEL-DSCH BY STT .
  CALL FUNCTION 'ZXLWB_CALLFORM'
    EXPORTING
      IV_FORMNAME    = 'ZFN_PROJECT'
      IV_CONTEXT_REF = LS_EXCEL
    EXCEPTIONS
      OTHERS         = 2.
  IF SY-SUBRC NE 0 .
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 .
  ENDIF.
ENDFORM.
*& Form LOCK_PROJECT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM LOCK_PROJECT .
  DATA:
    LV_GARG   LIKE SEQG3-GARG,
    LV_ENQ    LIKE STANDARD TABLE OF SEQG3 WITH HEADER LINE,
    LV_PROJID TYPE ZDD_BM_CS_PROJID,
    LV_MSG    TYPE STRING.
  DATA:
    LT_SEL_ROWS TYPE LVC_T_ROID,
    LS_SEL_ROW  TYPE LVC_S_ROID.
  CALL METHOD G_ALV_GRID->CHECK_CHANGED_DATA.
  CALL METHOD G_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_ROW_NO = LT_SEL_ROWS.
  IF LINES( LT_SEL_ROWS ) = 1.
    READ TABLE LT_SEL_ROWS INTO LS_SEL_ROW INDEX 1.
    READ TABLE GT_ALV INTO DATA(LS_ALV)  INDEX LS_SEL_ROW-ROW_ID.
    LV_PROJID = LS_ALV-PROJID.
  ENDIF.
  CALL FUNCTION 'ENQUEUE_EZ_LOCK_PROJECT'
    EXPORTING
      MODE_ZTB_BM_CSPJ_P = 'E'
      MANDT              = SY-MANDT
      PROJID             = LV_PROJID
*     X_PROJID           = ' '
*     _SCOPE             = '2'
*     _WAIT              = ' '
*     _COLLECT           = ' '
    EXCEPTIONS
      FOREIGN_LOCK       = 1
      SYSTEM_FAILURE     = 2
      OTHERS             = 3.
  IF SY-SUBRC = 1.
    CONCATENATE SY-MANDT S_PROJID INTO LV_GARG.
    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        GCLIENT               = SY-MANDT
        GNAME                 = 'EZ_LOCK_PROJECT'
        GARG                  = LV_GARG
*       GUNAME                = SY-UNAME
*       LOCAL                 = ' '
*       FAST                  = ' '
*       GARGNOWC              = ' '
*     IMPORTING
*       NUMBER                =
*       SUBRC                 =
      TABLES
        ENQ                   = LV_ENQ
      EXCEPTIONS
        COMMUNICATION_FAILURE = 1
        SYSTEM_FAILURE        = 2
        OTHERS                = 3.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
    READ TABLE LV_ENQ INDEX 1.
    CONCATENATE ' The object  is lock  by other user' SY-UNAME  INTO LV_MSG SEPARATED BY SPACE.
    MESSAGE LV_MSG TYPE 'S' DISPLAY LIKE 'E' .
    G_LOCK = '0'.
  ELSE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form UNLOCK_PROJECT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM UNLOCK_PROJECT .
  CALL FUNCTION 'DEQUEUE_EZ_LOCK_PROJECT'
    EXPORTING
      MODE_ZTB_BM_CSPJ_P = 'E'
      MANDT              = SY-MANDT
      PROJID             = GV_PROJID
*     X_PROJID           = ' '
*     _SCOPE             = '3'
*     _SYNCHRON          = ' '
*     _COLLECT           = ' '
    .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY .
  DATA:
    LT_SEL_ROWS TYPE LVC_T_ROID,
    LS_SEL_ROW  TYPE LVC_S_ROID.
  CALL METHOD G_ALV_GRID->CHECK_CHANGED_DATA.
  CALL METHOD G_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_ROW_NO = LT_SEL_ROWS.
  IF LINES( LT_SEL_ROWS ) = 1.
    G_MODE = '3'.
    LOOP AT LT_SEL_ROWS INTO LS_SEL_ROW.
      READ TABLE GT_ALV INTO DATA(LS_ALV) INDEX LS_SEL_ROW-ROW_ID.
      GV_PROJID = LS_ALV-PROJID.
      GV_DESCR  = LS_ALV-DESCR.
      GV_DATBE  = LS_ALV-DATBE.
      GV_DATEN  = LS_ALV-DATEN.
    ENDLOOP.
    CALL SCREEN 1002.
  ELSE.
  ENDIF.
ENDFORM.
FORM CHECK2.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'GV_PROJID'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF SCREEN-NAME = 'GV_DESCR'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF SCREEN-NAME = 'GV_DATBE'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
    IF SCREEN-NAME = 'GV_DATEN'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.

*-----DongVu--- Xóa project-----------------
FORM DELETE_PROJECT .
  DATA:
    LT_SEL_ROWS TYPE LVC_T_ROID,
    LS_SEL_ROW  TYPE LVC_S_ROID,
    LV_ERR      TYPE CHAR1.
  PERFORM CHECK_DELETEPROJECT CHANGING LV_ERR.
  CHECK LV_ERR IS INITIAL.
  CALL METHOD G_ALV_GRID->CHECK_CHANGED_DATA.
  CALL METHOD G_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_ROW_NO = LT_SEL_ROWS.
  IF LT_SEL_ROWS[] IS INITIAL.
    MESSAGE 'Hãy chọn ít nhất  một dòng' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF LINES( LT_SEL_ROWS ) > 1.
    MESSAGE 'Chỉ được chọn một dòng' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ELSE.
    LOOP AT LT_SEL_ROWS INTO LS_SEL_ROW.
      READ TABLE GT_ALV INTO DATA(LS_ALV) INDEX LS_SEL_ROW-ROW_ID.
      IF  SY-SUBRC IS INITIAL.
        DELETE FROM ZTB_BM_CSPJ_P WHERE PROJID EQ LS_ALV-PROJID.
        DELETE FROM ZTB_BM_CSPJ_C WHERE PROJID EQ LS_ALV-PROJID.
        DELETE FROM ZTB_BM_CSPJ_D WHERE PROJID EQ LS_ALV-PROJID.
        DELETE GT_ALV WHERE PROJID EQ LS_ALV-PROJID.
      ELSE.
        MESSAGE 'No data' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
      MESSAGE 'Xóa thành công ' TYPE 'S'.
    ENDLOOP.
  ENDIF.
ENDFORM.
*-----DongVu--- Xóa project-----------------------------------------

*-----Check xem project đã được upload dữ liệu chưa?----------------
FORM CHECK_DELETEPROJECT CHANGING LPV_ERR TYPE CHAR1.
  DATA:
        LV_DELETE TYPE ZDD_BM_CS_PROJID.
  DATA:
    LT_SEL_ROWS TYPE LVC_T_ROID,
    LS_SEL_ROW  TYPE LVC_S_ROID.
  CALL METHOD G_ALV_GRID->CHECK_CHANGED_DATA.
  CALL METHOD G_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_ROW_NO = LT_SEL_ROWS.
  LOOP AT LT_SEL_ROWS INTO LS_SEL_ROW.
    READ TABLE GT_ALV INTO DATA(LS_ALV) INDEX LS_SEL_ROW-ROW_ID.
    IF SY-SUBRC IS INITIAL.
      LV_DELETE =  LS_ALV-PROJID.
    ENDIF.
  ENDLOOP.

  SELECT *
    FROM ZTB_BM_CSPJ_D
    INTO CORRESPONDING FIELDS OF TABLE GT_CSPJ_D
    WHERE PROJID = LV_DELETE.
  IF  SY-SUBRC IS INITIAL.
    LOOP AT GT_CSPJ_D INTO DATA(LS_CSPJ_D).
      IF LS_CSPJ_D-XMLSTR IS NOT INITIAL.
        MESSAGE ' Project này đã upload dữ liệu, không thể xóa ' TYPE 'S' DISPLAY LIKE 'E'.
        LPV_ERR = 'X'.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*-----Check xem project đã được upload dữ liệu chưa?----------------
