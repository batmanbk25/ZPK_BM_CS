*&---------------------------------------------------------------------*
*& Include          ZIN_BM_CS_REPORTF01
*&---------------------------------------------------------------------*

FORM MAIN_PROCESS .
  PERFORM GET_DATA.
  PERFORM PROCESS_DATA.
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM LIST_STATUS .
*list
  DATA: GT_LIST     TYPE VRM_VALUES.
  DATA: WA_LIST    TYPE VRM_VALUE.
  DATA:
        LT_DD07V TYPE  DD07V_TAB .
  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      DOMNAME        = 'ZDO_BM_CS_CSFSTS'
      TEXT           = 'X'
      LANGU          = SY-LANGU
*     BYPASS_BUFFER  = ' '
*   IMPORTING
*     RC             =
    TABLES
      DD07V_TAB      = LT_DD07V
    EXCEPTIONS
      WRONG_TEXTFLAG = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  WA_LIST-KEY  = '*'.
  WA_LIST-TEXT = 'All '.
  APPEND WA_LIST TO GT_LIST.
  LOOP AT LT_DD07V INTO DATA(LS_DD07V).
    WA_LIST-KEY  = LS_DD07V-DOMVALUE_L.
    WA_LIST-TEXT = LS_DD07V-DDTEXT.
    APPEND WA_LIST TO GT_LIST.
  ENDLOOP.
*hien thi list
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = 'P_STATUS'
      VALUES          = GT_LIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
ENDFORM.

FORM GET_DATA .
  CASE P_STATUS.
    WHEN '*'.
      SELECT *
      FROM  ZTB_BM_CSPJ_C
      INTO CORRESPONDING FIELDS OF  TABLE GT_ALV
      WHERE
              ZTB_BM_CSPJ_C~PROJID IN S_PROJID
          AND ZTB_BM_CSPJ_C~MODLID IN S_MODLID
          AND ZTB_BM_CSPJ_C~CSFUNC IN S_CSFUNC.
    WHEN OTHERS.
      SELECT *
        FROM  ZTB_BM_CSPJ_C
        INTO CORRESPONDING FIELDS OF  TABLE GT_ALV
        WHERE
              ZTB_BM_CSPJ_C~PROJID IN S_PROJID
          AND ZTB_BM_CSPJ_C~MODLID IN S_MODLID
          AND ZTB_BM_CSPJ_C~CSFUNC IN S_CSFUNC
          AND ZTB_BM_CSPJ_C~CSFSTS = P_STATUS.
  ENDCASE.
  SORT GT_ALV BY PROJID MODLID CSFUNC.

  IF GT_ALV IS INITIAL.
    MESSAGE 'No data' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT
      ZTB_BM_CSPJ_D~PROJID,
      ZTB_BM_CSPJ_D~MODLID,
      ZTB_BM_CSPJ_D~CSFUNC,
      ZTB_BM_CSPJ_D~VIEWNAME,
      ZTB_BM_CSPJ_D~TABKEY,
      ZTB_BM_CSPJ_D~CSFSTS,
      ZTB_BM_CSPJ_D~XMLSTR,
      ZTB_BM_CSPJ_D~CHUSR,
      ZTB_BM_CSPJ_D~CHDAT,
      ZTB_BM_CSPJ_D~CHTIM,
      ZTB_BM_CSTR_C~DESCR
    FROM ZTB_BM_CSPJ_D
      LEFT JOIN ZTB_BM_CSTR_C
    ON ZTB_BM_CSTR_C~MODLID = ZTB_BM_CSPJ_D~MODLID
      AND ZTB_BM_CSTR_C~CSFUNC = ZTB_BM_CSPJ_D~CSFUNC
      FOR ALL ENTRIES IN @GT_ALV
    WHERE   ZTB_BM_CSPJ_D~PROJID = @GT_ALV-PROJID
        AND ZTB_BM_CSPJ_D~MODLID = @GT_ALV-MODLID
        AND ZTB_BM_CSPJ_D~CSFUNC = @GT_ALV-CSFUNC
        AND DELETED = @SPACE
            INTO CORRESPONDING FIELDS OF TABLE @GT_DETAIL.
*    SELECT *
*      FROM ZTB_BM_CSPJ_D
*      FOR ALL ENTRIES IN @GT_ALV
*      WHERE PROJID = @GT_ALV-PROJID
*        AND MODLID = @GT_ALV-MODLID
*        AND CSFUNC = @GT_ALV-CSFUNC
*        AND DELETED = @SPACE
*      INTO CORRESPONDING FIELDS OF TABLE @GT_DETAIL.
*    SORT GT_DETAIL BY PROJID MODLID CSFUNC TABKEY.

    SELECT *
      FROM ZTB_BM_CSTR_C
      FOR ALL ENTRIES IN @GT_ALV
      WHERE MODLID = @GT_ALV-MODLID
        AND CSFUNC = @GT_ALV-CSFUNC
      INTO TABLE @GT_CSTR_C.
    SORT GT_CSTR_C BY MODLID CSFUNC.
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
  LOOP AT GT_ALV ASSIGNING FIELD-SYMBOL(<LFS_ALV>).
    CASE <LFS_ALV>-CSFSTS.
      WHEN 1.
        <LFS_ALV>-ICON   = '@EB@'. "Light out; undefined
      WHEN 2.
        <LFS_ALV>-ICON   = '@09@'. "yellow
      WHEN 3.
        <LFS_ALV>-ICON   = '@0A@'. "Red
      WHEN 4.
        <LFS_ALV>-ICON   = '@08@'. "green
      WHEN 5.
        <LFS_ALV>-ICON   = '@09@'. "green
      WHEN OTHERS.
    ENDCASE.
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
      I_STRUCTURE_NAME       = 'ZST_BM_CSPJ_C'
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
        WHEN 'MODLID'.
          <FS_FIELDCAT>-COL_POS       = 1.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'CSFUNC'.
          <FS_FIELDCAT>-COL_POS       = 2.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'VIEWNAME'.
          <FS_FIELDCAT>-COL_POS       = 3.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'SUMREC'.
          <FS_FIELDCAT>-COL_POS       = 4.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'COMREC'.
          <FS_FIELDCAT>-COL_POS       = 5.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'CSFSTS'.
          <FS_FIELDCAT>-COL_POS       = 6.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
        WHEN 'ICON'.
          <FS_FIELDCAT>-COLTEXT       = 'Trạng Thái'.
          <FS_FIELDCAT>-ICON          = 'X'.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 7.
        WHEN 'CHUSR'.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 8.
          <FS_FIELDCAT>-TECH          = ''.
        WHEN 'CHDAT'.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 9.
          <FS_FIELDCAT>-TECH          = ''.
        WHEN 'CHTIM'.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 10.
          <FS_FIELDCAT>-TECH          = ''.
        WHEN 'DETAIL_BTN'.
          <FS_FIELDCAT>-COLTEXT       = 'Button'.
          <FS_FIELDCAT>-STYLE         = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 10.
        WHEN OTHERS.
          <FS_FIELDCAT>-TECH          = 'X'.
          <FS_FIELDCAT>-NO_OUT        = 'X'.
      ENDCASE.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUTTON_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW
*&      --> E_COLUMN
*&      --> C
*&---------------------------------------------------------------------*
FORM BUTTON_CLICK  USING    P_ES_COL_ID TYPE LVC_S_COL
                            P_ES_ROW_NO TYPE LVC_S_ROID.
  DATA: LV_COL_ID TYPE LVC_S_ROW.
  DATA: LV_ROW_NO TYPE LVC_S_ROID.

  LV_COL_ID = P_ES_COL_ID. "Cot nhan duoc
  LV_ROW_NO = P_ES_ROW_NO. "Hang nhan duoc

  READ TABLE GT_ALV INTO DATA(LS_ALV)
  INDEX  P_ES_ROW_NO-ROW_ID.
  IF SY-SUBRC = 0 .
    PERFORM DISPLAY_ALV1 USING LS_ALV.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV1 USING LS_ALV LIKE LINE OF GT_ALV.
  DATA: LS_LAYOUT TYPE LVC_S_LAYO,
        LS_ALV1   TYPE ZST_BM_CSPJ_D.
*  SELECT *
**    VIEWNAME
**    TABKEY
**    CSFSTS
*  FROM  ZTB_BM_CSPJ_D
*  WHERE    PROJID   = LS_ALV-PROJID
*      AND  MODLID   = LS_ALV-MODLID
*      AND  CSFUNC   = LS_ALV-CSFUNC.
  CLEAR: GT_ALV1.
  LOOP AT GT_DETAIL INTO DATA(LS_DETAIL)
    WHERE PROJID    = LS_ALV-PROJID
      AND  MODLID   = LS_ALV-MODLID
      AND  CSFUNC   = LS_ALV-CSFUNC.
    APPEND LS_DETAIL TO GT_ALV1.
  ENDLOOP.
*PROCESS_DATA
  LOOP AT GT_ALV1 ASSIGNING FIELD-SYMBOL(<LFS_ALV>).
    PERFORM SET_STATUS CHANGING <LFS_ALV>.
  ENDLOOP.
*BUILD_FIELDCAT
  DATA: LW_COLTEXT TYPE TEXT255,
        LW_POS     TYPE INT4,
        LS_FCAT    LIKE LINE OF GT_FIELDCAT.

  CLEAR:
  GS_LAYOUT.
  GS_LAYOUT-ZEBRA           = 'X'.
  GS_LAYOUT-CWIDTH_OPT      = 'X'.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME       = 'ZST_BM_CSPJ_D'
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

    LOOP AT GT_FIELDCAT1 ASSIGNING FIELD-SYMBOL(<FS_FIELDCAT>).
      CASE <FS_FIELDCAT>-FIELDNAME.
        WHEN 'VIEWNAME'.
          <FS_FIELDCAT>-COL_POS   = 0.
          <FS_FIELDCAT>-OUTPUTLEN = 10.
        WHEN 'TABKEY'.
          <FS_FIELDCAT>-COL_POS   = 1.
          <FS_FIELDCAT>-OUTPUTLEN = 10.
        WHEN 'CSFSTS'.
          <FS_FIELDCAT>-COL_POS   = 2.
          <FS_FIELDCAT>-OUTPUTLEN = 10.

        WHEN 'ICON'.
          <FS_FIELDCAT>-COLTEXT   = 'Trạng Thái'.
          <FS_FIELDCAT>-ICON      = 'X'.
          <FS_FIELDCAT>-OUTPUTLEN = 10.
          <FS_FIELDCAT>-COL_POS   = 3.
        WHEN 'CHUSR'.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 4.
          <FS_FIELDCAT>-TECH          = ''.
          <FS_FIELDCAT>-NO_OUT    = ''.
        WHEN 'CHDAT'.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 5.
          <FS_FIELDCAT>-TECH          = ''.
          <FS_FIELDCAT>-NO_OUT    = ''.
        WHEN 'CHTIM'.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 6.
          <FS_FIELDCAT>-TECH          = ''.
          <FS_FIELDCAT>-NO_OUT    = ''.
        WHEN 'DESCR'.
          <FS_FIELDCAT>-OUTPUTLEN     = 10.
          <FS_FIELDCAT>-COL_POS       = 7.
          <FS_FIELDCAT>-TECH          = ''.
          <FS_FIELDCAT>-NO_OUT    = ''.
        WHEN OTHERS.
          <FS_FIELDCAT>-TECH      = 'X'.
          <FS_FIELDCAT>-NO_OUT    = 'X'.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  LS_LAYOUT-SEL_MODE = 'A'.
  LS_LAYOUT-ZEBRA    = 'A'.
*  GS_LAYOUT- = 'A'.

*call alv
  CLEAR : GT_FIELDCAT.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IT_FIELDCAT_LVC          = GT_FIELDCAT1
      I_SAVE                   = 'A'
      IS_LAYOUT_LVC            = LS_LAYOUT
    TABLES
      T_OUTTAB                 = GT_ALV1
    EXCEPTIONS
      PROGRAM_ERROR            = 1.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE   SY-MSGTY NUMBER SY-MSGNO
       WITH SY-MSGV1  SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
FORM PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'ZALV2'." EXCLUDING RT_EXTAB.
ENDFORM.

FORM USER_COMMAND USING P_UCOMM TYPE SY-UCOMM
                        P_SELFLD TYPE SLIS_SELFIELD.
  DATA : L_REP_MODE,
         LT_SEL_ROWS      TYPE LVC_T_ROID,
         LS_SEL_ROW       TYPE LVC_S_ROID,
         LT_ALV1          LIKE GT_ALV1,
         REF1             TYPE REF TO CL_GUI_ALV_GRID,
         LT_ZTB_BM_CSPJ_D TYPE TABLE OF ZST_BM_CSPJ_D,
         LV_RESULT        TYPE C.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = REF1.
  CALL METHOD REF1->CHECK_CHANGED_DATA.
  CASE P_UCOMM.
    WHEN '&EDIT'.
      CLEAR: GV_CANCEL.
      CALL METHOD REF1->GET_SELECTED_ROWS
        IMPORTING
          ET_ROW_NO = LT_SEL_ROWS.

      IF LT_SEL_ROWS[] IS INITIAL.
        MESSAGE 'Hãy chọn dòng muốn sửa?' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
      CALL SCREEN 0200 STARTING AT 20 10.

      IF GV_CANCEL IS INITIAL.

        LOOP AT LT_SEL_ROWS INTO LS_SEL_ROW.
          READ TABLE GT_ALV1 ASSIGNING FIELD-SYMBOL(<LFS_ALV1>) INDEX LS_SEL_ROW-ROW_ID.
          <LFS_ALV1>-CSFSTS = GV_CSFSTS.
*          <LFS_ALV1>-CHUSR  = SY-UNAME.
*          <LFS_ALV1>-CHDAT  = SY-DATUM.
*          <LFS_ALV1>-CHTIM  = SY-UZEIT.


          READ TABLE GT_DETAIL ASSIGNING FIELD-SYMBOL(<LFS_DETAIL>)
            WITH KEY  PROJID    = <LFS_ALV1>-PROJID
                      MODLID    = <LFS_ALV1>-MODLID
                      CSFUNC    = <LFS_ALV1>-CSFUNC
                      VIEWNAME  = <LFS_ALV1>-VIEWNAME
                      TABKEY    = <LFS_ALV1>-TABKEY.


          IF SY-SUBRC IS INITIAL.
            <LFS_DETAIL>-CSFSTS = GV_CSFSTS.
            <LFS_DETAIL>-CHUSR  = SY-UNAME.
            <LFS_DETAIL>-CHDAT  = SY-DATUM.
            <LFS_DETAIL>-CHTIM  = SY-UZEIT.
          ENDIF.

          PERFORM SET_STATUS CHANGING <LFS_ALV1>.
        ENDLOOP.

        DATA:
          LV_COUNT  TYPE P,
          LV_COUNT2 TYPE P,
          LV_COUNT3 TYPE P,
          LV_PROJID TYPE ZDD_BM_CS_PROJID,
          LV_MODLID TYPE ZDD_BM_MODULE,
          LV_CSFUNC TYPE ZDD_BM_CSFUNC,
          LV_CSFSTS TYPE ZDD_BM_CS_CSFSTS.
        LOOP AT GT_ALV1 INTO DATA(GS_ALV1).
          LV_PROJID = GS_ALV1-PROJID.
          LV_MODLID = GS_ALV1-MODLID.
          LV_CSFUNC = GS_ALV1-CSFUNC.
        ENDLOOP.

        LOOP AT GT_ALV1 ASSIGNING FIELD-SYMBOL(<LFS_ALV2>).
*  LOOP AT GT_ALV1 TRANSPORTING NO FIELDS.
          CASE <LFS_ALV2>-CSFSTS.
            WHEN '4'.
              LV_COUNT   = LV_COUNT + 1.
            WHEN '1'.
              LV_COUNT2  = LV_COUNT2 + 1.
            WHEN '3'.
              LV_COUNT3  = LV_COUNT3 + 1.
            WHEN OTHERS.
          ENDCASE.

*    WHERE CSFSTS = '4'.
*    COUNT = COUNT + 1.
        ENDLOOP.

        READ TABLE GT_ALV ASSIGNING FIELD-SYMBOL(<LFS_ALV>)
          WITH KEY PROJID = LV_PROJID
                   MODLID = LV_MODLID
                   CSFUNC = LV_CSFUNC.
        IF SY-SUBRC IS INITIAL .

          <LFS_ALV>-COMREC = LV_COUNT.

          IF <LFS_ALV>-SUMREC = <LFS_ALV>-COMREC.
            IF <LFS_ALV>-COMREC <> 0.
              <LFS_ALV>-CSFSTS  = '4'.
              <LFS_ALV>-ICON    = '@08@'. "green
            ELSE.
              <LFS_ALV>-CSFSTS  = '1'.
              <LFS_ALV>-ICON    = '@EB@'. "
            ENDIF.

          ELSEIF <LFS_ALV>-SUMREC  = LV_COUNT2.
            IF LV_COUNT2 <> 0.
              <LFS_ALV>-CSFSTS  = '1'.
              <LFS_ALV>-ICON    = '@EB@'. "Light out; undefined
            ELSE.
              <LFS_ALV>-CSFSTS  = '1'.
              <LFS_ALV>-ICON    = '@EB@'. "
            ENDIF.

          ELSEIF <LFS_ALV>-SUMREC  = LV_COUNT3.
            IF LV_COUNT3 <> 0.
              <LFS_ALV>-CSFSTS  = '3'.
              <LFS_ALV>-ICON    = '@0A@'. "Red
            ELSE.
              <LFS_ALV>-CSFSTS  = '1'.
              <LFS_ALV>-ICON    = '@EB@'. "
            ENDIF.

          ELSE.
            <LFS_ALV>-CSFSTS  = '2'.
            <LFS_ALV>-ICON    = '@09@'. "yellow
          ENDIF.
          LV_CSFSTS = <LFS_ALV>-CSFSTS.
        ENDIF.
      ENDIF.

      P_SELFLD-REFRESH = 'X'.

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
    WHEN '&DATA_SAVE'.
      PERFORM CHANGE_DATA.
  ENDCASE.
  P_SELFLD-REFRESH = 'X'.                    "user_command
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LFS_ALV>
*&---------------------------------------------------------------------*
FORM SET_STATUS  CHANGING LPS_ALV TYPE ZST_BM_CSPJ_D.
  CASE LPS_ALV-CSFSTS.
    WHEN 1.
      LPS_ALV-ICON  = '@EB@'. "Light out; undefined
    WHEN 2.
      LPS_ALV-ICON  = '@09@'. "yellow
    WHEN 3.
      LPS_ALV-ICON  = '@0A@'. "Red
    WHEN 4.
      LPS_ALV-ICON  = '@08@'. "green
    WHEN 5.
      LPS_ALV-ICON  = '@09@'. "green
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
FORM CHANGE_DATA.

  DATA:
    LV_PROJID TYPE ZDD_BM_CS_PROJID,
    LV_MODLID TYPE ZDD_BM_MODULE,
    LV_CSFUNC TYPE ZDD_BM_CSFUNC,
    LV_CSFSTS TYPE ZDD_BM_CS_CSFSTS,
    LV_CHUSR  TYPE CHUSER,
    LV_CHDAT  TYPE CHDATE,
    LV_CHTIM  TYPE CHTIME_KK,
    LT_ALV1   TYPE TABLE OF ZST_BM_CSPJ_D.
  SELECT *
      FROM ZTB_BM_CSPJ_D
      INTO CORRESPONDING FIELDS OF TABLE LT_ALV1.

  LV_CHUSR  = SY-UNAME.
  LV_CHDAT  = SY-DATUM.
  LV_CHTIM  = SY-UZEIT.
  "Update detail
  LOOP AT GT_ALV1 ASSIGNING FIELD-SYMBOL(<LFS_ALV1>).
    READ TABLE LT_ALV1 INTO DATA(LS_ALV1)
    WITH KEY PROJID   = <LFS_ALV1>-PROJID
          MODLID   = <LFS_ALV1>-MODLID
          CSFUNC   = <LFS_ALV1>-CSFUNC
          VIEWNAME = <LFS_ALV1>-VIEWNAME
          TABKEY   = <LFS_ALV1>-TABKEY.
    IF SY-SUBRC IS INITIAL.
      IF LS_ALV1-CSFSTS <> <LFS_ALV1>-CSFSTS .
        UPDATE ZTB_BM_CSPJ_D
               SET CSFSTS   = <LFS_ALV1>-CSFSTS
                   CHUSR    = LV_CHUSR
                   CHDAT    = LV_CHDAT
                   CHTIM    = LV_CHTIM
             WHERE PROJID   = <LFS_ALV1>-PROJID
               AND MODLID   = <LFS_ALV1>-MODLID
               AND CSFUNC   = <LFS_ALV1>-CSFUNC
               AND VIEWNAME = <LFS_ALV1>-VIEWNAME
               AND TABKEY   = <LFS_ALV1>-TABKEY.
        LV_PROJID = <LFS_ALV1>-PROJID.
        LV_MODLID = <LFS_ALV1>-MODLID.
        LV_CSFUNC = <LFS_ALV1>-CSFUNC.


        <LFS_ALV1>-CHUSR = LV_CHUSR.
        <LFS_ALV1>-CHDAT = LV_CHDAT.
        <LFS_ALV1>-CHTIM = LV_CHTIM.
        READ TABLE GT_DETAIL ASSIGNING FIELD-SYMBOL(<LFS_DETAIL>)
        WITH KEY  PROJID   = <LFS_ALV1>-PROJID
                  MODLID   = <LFS_ALV1>-MODLID
                  CSFUNC   = <LFS_ALV1>-CSFUNC
                  VIEWNAME = <LFS_ALV1>-VIEWNAME
                  TABKEY   = <LFS_ALV1>-TABKEY.
        IF SY-SUBRC IS INITIAL .
          <LFS_DETAIL>-CHUSR = LV_CHUSR.
          <LFS_DETAIL>-CHDAT = LV_CHDAT.
          <LFS_DETAIL>-CHTIM = LV_CHTIM.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDLOOP.

  READ TABLE GT_ALV ASSIGNING FIELD-SYMBOL(<LFS_ALV>)
    WITH KEY  PROJID    = LV_PROJID
              MODLID    = LV_MODLID
              CSFUNC    = LV_CSFUNC.
  IF SY-SUBRC IS INITIAL.
    UPDATE ZTB_BM_CSPJ_C
         SET COMREC = <LFS_ALV>-COMREC
             CSFSTS = <LFS_ALV>-CSFSTS
             CHUSR  = LV_CHUSR
             CHDAT  = LV_CHDAT
             CHTIM  = LV_CHTIM
       WHERE PROJID = LV_PROJID
         AND MODLID = LV_MODLID
         AND CSFUNC = LV_CSFUNC.

    <LFS_ALV>-CHUSR = LV_CHUSR.
    <LFS_ALV>-CHDAT = LV_CHDAT.
    <LFS_ALV>-CHTIM = LV_CHTIM.

    IF SY-SUBRC IS INITIAL.
      MESSAGE 'Update successful'   TYPE 'S'.
    ELSE.
      MESSAGE 'Update unsuccessful' TYPE 'E'.
    ENDIF.


  ENDIF.
  COMMIT WORK AND WAIT.
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
*& Form CHANGE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SAVE.

  DATA:
    LV_CHUSR TYPE CHUSER,
    LV_CHDAT TYPE CHDATE,
    LV_CHTIM TYPE CHTIME_KK,
    LT_ALV   TYPE TABLE OF ZST_BM_CSPJ_C,
    LT_ALV1  TYPE TABLE OF ZST_BM_CSPJ_D.

  LV_CHUSR  = SY-UNAME.
  LV_CHDAT  = SY-DATUM.
  LV_CHTIM  = SY-UZEIT.

  SELECT *
      FROM ZTB_BM_CSPJ_C
      INTO CORRESPONDING FIELDS OF TABLE LT_ALV.

  "Update detail
  LOOP AT GT_ALV ASSIGNING FIELD-SYMBOL(<LFS_ALV>).
    READ TABLE LT_ALV INTO DATA(LS_ALV)
    WITH KEY
          PROJID   = <LFS_ALV>-PROJID
          MODLID   = <LFS_ALV>-MODLID
          CSFUNC   = <LFS_ALV>-CSFUNC.
    IF SY-SUBRC IS INITIAL.
      IF LS_ALV-CSFSTS <> <LFS_ALV>-CSFSTS
        OR LS_ALV-COMREC <> <LFS_ALV>-COMREC .
        UPDATE ZTB_BM_CSPJ_C
         SET
             COMREC = <LFS_ALV>-COMREC
             CSFSTS = <LFS_ALV>-CSFSTS
             CHUSR  = LV_CHUSR
             CHDAT  = LV_CHDAT
             CHTIM  = LV_CHTIM
       WHERE PROJID = <LFS_ALV>-PROJID
         AND MODLID = <LFS_ALV>-MODLID
         AND CSFUNC = <LFS_ALV>-CSFUNC.
        <LFS_ALV>-CHUSR = LV_CHUSR.
        <LFS_ALV>-CHDAT = LV_CHDAT.
        <LFS_ALV>-CHTIM = LV_CHTIM.
      ENDIF.
    ENDIF.
  ENDLOOP.
  SELECT *
        FROM ZTB_BM_CSPJ_D
        INTO CORRESPONDING FIELDS OF TABLE LT_ALV1.
  LOOP AT GT_DETAIL ASSIGNING FIELD-SYMBOL(<LFS_DETAIL>).
    READ TABLE LT_ALV1 INTO DATA(LS_ALV1)
    WITH KEY
          PROJID   = <LFS_DETAIL>-PROJID
          MODLID   = <LFS_DETAIL>-MODLID
          CSFUNC   = <LFS_DETAIL>-CSFUNC
          VIEWNAME = <LFS_DETAIL>-VIEWNAME
          TABKEY   = <LFS_DETAIL>-TABKEY.
    IF SY-SUBRC IS INITIAL.
      IF LS_ALV1-CSFSTS <> <LFS_DETAIL>-CSFSTS .
        UPDATE ZTB_BM_CSPJ_D
             SET
             CSFSTS = <LFS_DETAIL>-CSFSTS
             CHUSR  = LV_CHUSR
             CHDAT  = LV_CHDAT
             CHTIM  = LV_CHTIM

           WHERE PROJID   = <LFS_DETAIL>-PROJID
             AND MODLID   = <LFS_DETAIL>-MODLID
             AND CSFUNC   = <LFS_DETAIL>-CSFUNC
             AND VIEWNAME = <LFS_DETAIL>-VIEWNAME
             AND TABKEY   = <LFS_DETAIL>-TABKEY.
        <LFS_DETAIL>-CHUSR = LV_CHUSR.
        <LFS_DETAIL>-CHDAT = LV_CHDAT.
        <LFS_DETAIL>-CHTIM = LV_CHTIM.
      ENDIF.
    ENDIF.
  ENDLOOP.
  COMMIT WORK AND WAIT.
*  IF SY-SUBRC IS INITIAL.
  MESSAGE 'Update successful'   TYPE 'S'.
*  ELSE.
*    MESSAGE 'Update unsuccessful' TYPE 'E'.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CUSTOMIZIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM 0100_FC_GOTOCS.
  DATA:
    LS_CSTR     TYPE ZTB_BM_CSTR_C,
    LT_SEL_ROWS TYPE LVC_T_ROID,
    COUNT       TYPE P,
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
    LOOP AT LT_SEL_ROWS INTO LS_SEL_ROW.
      READ TABLE GT_ALV INTO DATA(LS_ALV) INDEX LS_SEL_ROW-ROW_ID.
      SELECT SINGLE
        TCODE
        VIEWNAME
        VCL_NAME
    FROM ZTB_BM_CSTR_C
    INTO CORRESPONDING FIELDS OF  LS_CSTR
        WHERE MODLID = LS_ALV-MODLID
        AND   CSFUNC = LS_ALV-CSFUNC.
    ENDLOOP.

    IF LS_CSTR-VCL_NAME IS NOT INITIAL..
      CALL FUNCTION 'VIEWCLUSTER_MAINTENANCE_CALL'
        EXPORTING
          VIEWCLUSTER_NAME             = LS_CSTR-VCL_NAME
          MAINTENANCE_ACTION           = 'S'
*         CORR_NUMBER                  = ' '
*       TABLES
*         DBA_SELLIST                  =
*         DBA_SELLIST_CLUSTER          =
        EXCEPTIONS
          CLIENT_REFERENCE             = 1
          FOREIGN_LOCK                 = 2
          VIEWCLUSTER_NOT_FOUND        = 3
          VIEWCLUSTER_IS_INCONSISTENT  = 4
          MISSING_GENERATED_FUNCTION   = 5
          NO_UPD_AUTH                  = 6
          NO_SHOW_AUTH                 = 7
          OBJECT_NOT_FOUND             = 8
          NO_TVDIR_ENTRY               = 9
          NO_CLIENTINDEP_AUTH          = 10
          INVALID_ACTION               = 11
          SAVING_CORRECTION_FAILED     = 12
          SYSTEM_FAILURE               = 13
          UNKNOWN_FIELD_IN_DBA_SELLIST = 14
          MISSING_CORR_NUMBER          = 15
          OTHERS                       = 16.
      RETURN.
    ENDIF.


    IF LS_CSTR-VIEWNAME IS NOT INITIAL.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          ACTION                       = 'U'
*         CORR_NUMBER                  = '          '
          VIEW_NAME                    = LS_CSTR-VIEWNAME
*       TABLES
*         DBA_SELLIST                  =
*         EXCL_CUA_FUNCT               =
        EXCEPTIONS
          CLIENT_REFERENCE             = 1
          FOREIGN_LOCK                 = 2
          INVALID_ACTION               = 3
          NO_CLIENTINDEPENDENT_AUTH    = 4
          NO_DATABASE_FUNCTION         = 5
          NO_EDITOR_FUNCTION           = 6
          NO_SHOW_AUTH                 = 7
          NO_TVDIR_ENTRY               = 8
          NO_UPD_AUTH                  = 9
          ONLY_SHOW_ALLOWED            = 10
          SYSTEM_FAILURE               = 11
          UNKNOWN_FIELD_IN_DBA_SELLIST = 12
          VIEW_NOT_FOUND               = 13
          MAINTENANCE_PROHIBITED       = 14
          OTHERS                       = 15.
      RETURN.
    ENDIF.

    IF LS_CSTR-TCODE IS NOT INITIAL.
      CALL TRANSACTION LS_CSTR-TCODE.
      RETURN.
    ENDIF.

    IF LS_CSTR-TCODE IS INITIAL
      AND LS_CSTR-VIEWNAME IS INITIAL
      AND LS_CSTR-VCL_NAME IS INITIAL   .
      MESSAGE 'No data' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0100_FC_APPLY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0100_FC_APPLY .
  DATA:
    LS_SELECT_FUNC TYPE ZST_BM_CSPJ_C,
    LS_CS_DETAIL   TYPE ZST_BM_CSPJ_D,
    LT_SEL_DETAIL  TYPE TABLE OF ZST_BM_CSPJ_D,
    LT_CSPJ_C      TYPE TABLE OF  ZTB_BM_CSPJ_C,
    LT_SEL_ROWS    TYPE LVC_T_ROID,
    LT_SELECT_FUNC TYPE TABLE OF ZST_BM_CSPJ_C.
  FIELD-SYMBOLS:
    <LF_SEL_FUNC> TYPE ZST_BM_CSPJ_C.
  DATA:
     LS_SEL_ROW  TYPE LVC_S_ROID.
  CALL METHOD G_ALV_GRID->CHECK_CHANGED_DATA.
  CALL METHOD G_ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_ROW_NO = LT_SEL_ROWS.

*  IF LT_SEL_ROWS[] IS INITIAL.
*    MESSAGE 'Hãy chọn ít nhất  một dòng' TYPE 'S' DISPLAY LIKE 'E'.
*    RETURN.
*  ELSEIF LINES( LT_SEL_ROWS ) > 1.
*    MESSAGE 'Chỉ được chọn một dòng' TYPE 'S' DISPLAY LIKE 'E'.
*    RETURN.
*  ELSE.

    LOOP AT LT_SEL_ROWS INTO LS_SEL_ROW.
      READ TABLE GT_ALV INTO DATA(LS_ALV) INDEX LS_SEL_ROW-ROW_ID .
      CASE  LS_ALV-CSFSTS.
        WHEN '4'.
          MESSAGE 'Already applied' TYPE 'S'.
        WHEN '3'.
          MESSAGE 'No completed ' TYPE 'S' DISPLAY LIKE 'E'.
        WHEN OTHERS.
          CALL FUNCTION 'ZFM_ALV_ROWS_GET_SELECTED'
            EXPORTING
              I_ALV_GRID  = G_ALV_GRID
              IT_ALV_DATA = GT_ALV
            IMPORTING
              ET_SEL_DATA = LT_SELECT_FUNC.

          LOOP AT LT_SELECT_FUNC ASSIGNING <LF_SEL_FUNC>.
            LOOP AT GT_DETAIL ASSIGNING FIELD-SYMBOL(<LS_CS_DETAIL>)
              WHERE PROJID = <LF_SEL_FUNC>-PROJID
                AND MODLID = <LF_SEL_FUNC>-MODLID
                AND CSFUNC = <LF_SEL_FUNC>-CSFUNC
                AND CSFSTS <> GC_CSFSTS-SUCCESS.

              PERFORM 9999_APPLY_SINGLE_CS
                CHANGING <LF_SEL_FUNC>
                         <LS_CS_DETAIL>.

            ENDLOOP.
* &-----------------------------------------------------------------
* ThinhQT - Check status <> complete
* &-----------------------------------------------------------------
            IF SY-SUBRC IS NOT INITIAL.
              MESSAGE S003(ZMS_BM_CS) DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.
* &-----------------------------------------------------------------
            <LF_SEL_FUNC>-CSFSTS = <LS_CS_DETAIL>-CSFSTS.
            IF SY-SUBRC IS INITIAL.
              UPDATE ZTB_BM_CSPJ_C
                 SET CSFSTS   = <LF_SEL_FUNC>-CSFSTS
                     COMREC   = <LF_SEL_FUNC>-COMREC
                     CHUSR    = SY-UNAME
                     CHDAT    = SY-DATUM
                     CHTIM    = SY-UZEIT
               WHERE PROJID   = <LF_SEL_FUNC>-PROJID
                 AND MODLID   = <LF_SEL_FUNC>-MODLID
                 AND CSFUNC   = <LF_SEL_FUNC>-CSFUNC .

              CALL METHOD G_ALV_GRID->CHECK_CHANGED_DATA.
              CALL METHOD G_ALV_GRID->GET_SELECTED_ROWS
                IMPORTING
                  ET_ROW_NO = LT_SEL_ROWS.
              LOOP AT LT_SEL_ROWS INTO LS_SEL_ROW.
                READ TABLE GT_ALV ASSIGNING FIELD-SYMBOL(<GFS_ALV>) INDEX LS_SEL_ROW-ROW_ID.
                <GFS_ALV>-CSFSTS = <LF_SEL_FUNC>-CSFSTS.
                <GFS_ALV>-COMREC = <LF_SEL_FUNC>-COMREC.
                <GFS_ALV>-ICON = '@08@'.
              ENDLOOP.

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
            ENDIF.
            MESSAGE 'APPLY CS successful' TYPE 'S'.
          ENDLOOP.
      ENDCASE.
    ENDLOOP.
*  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form 9999_APPLY_SINGLE_CS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LPS_CS_DETAIL
*&---------------------------------------------------------------------*
FORM 9999_APPLY_SINGLE_CS
  CHANGING LPS_SEL_FUNC  TYPE ZST_BM_CSPJ_C
           LPS_CS_DETAIL TYPE ZST_BM_CSPJ_D.

  DATA:
    LR_DATA    TYPE REF TO DATA,
    LT_FCAT    TYPE LVC_T_FCAT,
    LW_XSTRING TYPE XSTRING,
    LW_TABNM   TYPE TABNAME,
    LW_VIEWNM  TYPE COMT_STRUCT_NAME,
    LT_RETURN  TYPE BAPIRET2_T.
  FIELD-SYMBOLS:
    <LFT_VIEW_NEW> TYPE STANDARD TABLE,
    <LFS_VIEW_NEW> TYPE ANY,
    <LFT_TABL_NEW> TYPE STANDARD TABLE,
    <LFS_TABL_NEW> TYPE ANY,
    <LFT_TABL_ORG> TYPE STANDARD TABLE,
    <LFS_TABL_ORG> TYPE ANY,
    <LFW_VALUE>    TYPE ANY.
  READ TABLE GT_CSTR_C INTO DATA(LS_CSTR_C)
    WITH KEY CSFUNC = LPS_CS_DETAIL-CSFUNC
             MODLID = LPS_CS_DETAIL-MODLID
             BINARY SEARCH.
  IF SY-SUBRC IS INITIAL.
    IF LS_CSTR_C-STTURE IS INITIAL.
      LW_VIEWNM = LPS_CS_DETAIL-VIEWNAME.
    ELSE.
*& ------------------------------------------------------------
*  Process for structure
*& ------------------------------------------------------------
      LW_VIEWNM = LS_CSTR_C-STTURE.
    ENDIF.
  ELSE.
    LW_VIEWNM = LPS_CS_DETAIL-VIEWNAME.
  ENDIF.

  CREATE DATA LR_DATA TYPE TABLE OF (LW_VIEWNM).
  ASSIGN LR_DATA->* TO <LFT_VIEW_NEW>.
  CREATE DATA LR_DATA TYPE (LW_VIEWNM).

  CALL TRANSFORMATION ID
    SOURCE XML LPS_CS_DETAIL-XMLSTR
    RESULT DATA = LR_DATA->*.
  ASSIGN LR_DATA->* TO <LFS_VIEW_NEW>.
  APPEND <LFS_VIEW_NEW> TO <LFT_VIEW_NEW>.

  IF LS_CSTR_C-STTURE IS INITIAL.
    TRY.
        SELECT SINGLE TABNAME, TABCLASS, CLIDEP,  ROOTTAB
          INTO @DATA(LS_VIEW_INFO)
          FROM DD02L AS T
*           INNER JOIN DD26S AS S
*            ON T~TABNAME  = S~VIEWNAME
           LEFT JOIN DD25L AS V
            ON T~TABNAME  = V~VIEWNAME
           AND T~AS4LOCAL = V~AS4LOCAL
           AND T~AS4VERS  = V~AS4VERS
         WHERE T~AS4LOCAL = 'A'
           AND T~TABNAME = @LPS_CS_DETAIL-VIEWNAME.
        .
        IF SY-SUBRC IS INITIAL.
          SELECT VIEWNAME, TABNAME
            INTO TABLE @DATA(LT_DD26S)
            FROM DD26S
            WHERE AS4LOCAL = 'A'
              AND VIEWNAME = @LPS_CS_DETAIL-VIEWNAME.
          .
          IF SY-SUBRC IS INITIAL.
            LOOP AT LT_DD26S INTO DATA(LS_DD26S).
*            SELECT SINGLE TABNAME, TABCLASS, CLIDEP,  ROOTTAB
*          INTO @DATA(LS_VIEW_INFO)
*          FROM DD02L AS  T LEFT JOIN DD25L AS V
*            ON T~TABNAME  = V~VIEWNAME
*           AND T~AS4LOCAL = V~AS4LOCAL
*           AND T~AS4VERS  = V~AS4VERS
*         WHERE T~AS4LOCAL = 'A'
*           AND T~TABNAME = @LPS_CS_DETAIL-VIEWNAME.
*            IF SY-SUBRC IS INITIAL.
*       If object is view, set root table for transport
              IF LS_VIEW_INFO-TABCLASS = 'VIEW'.
*              LW_TABNM  = LS_VIEW_INFO-ROOTTAB.
                LW_TABNM  = LS_DD26S-TABNAME.
                CREATE DATA LR_DATA TYPE TABLE OF (LW_TABNM).
                ASSIGN LR_DATA->* TO <LFT_TABL_NEW>.
                CREATE DATA LR_DATA TYPE TABLE OF (LW_TABNM).
                ASSIGN LR_DATA->* TO <LFT_TABL_ORG>.
                CREATE DATA LR_DATA TYPE (LW_TABNM).
                ASSIGN LR_DATA->* TO <LFS_TABL_NEW>.
                MOVE-CORRESPONDING <LFS_VIEW_NEW> TO <LFS_TABL_NEW>.
                MOVE-CORRESPONDING <LFT_VIEW_NEW> TO <LFT_TABL_NEW>.
                ASSIGN COMPONENT 'SPRAS' OF STRUCTURE <LFS_TABL_NEW>
                                             TO <LFW_VALUE>.
                IF SY-SUBRC IS INITIAL.
                  <LFW_VALUE> = SY-LANGU.
                ELSE.
                  ASSIGN COMPONENT 'LANGU' OF STRUCTURE <LFS_TABL_NEW>
                                            TO <LFW_VALUE>.
                  IF  SY-SUBRC IS INITIAL.
                    <LFW_VALUE> = SY-LANGU.
                  ENDIF.

                ENDIF.

                LOOP AT <LFT_TABL_NEW> ASSIGNING FIELD-SYMBOL(<LFS_NEW>).
                  ASSIGN COMPONENT 'SPRAS' OF STRUCTURE <LFS_NEW>
                                             TO <LFW_VALUE>.
                  IF SY-SUBRC IS INITIAL.
                    <LFW_VALUE> = SY-LANGU.
                  ELSE.
                    ASSIGN COMPONENT 'LANGU' OF STRUCTURE <LFS_NEW>
                                         TO <LFW_VALUE>.
                    IF  SY-SUBRC IS INITIAL.
                      <LFW_VALUE> = SY-LANGU.
                    ELSE.
                      ASSIGN COMPONENT 'SPRSL' OF STRUCTURE <LFS_NEW>
                                      TO <LFW_VALUE>.
                      IF  SY-SUBRC IS INITIAL.
                        <LFW_VALUE> = SY-LANGU.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDLOOP.

*       If object is table, set table for transport
              ELSEIF LS_VIEW_INFO-TABCLASS = 'TRANSP'.
*              LW_TABNM  = LS_VIEW_INFO-TABNAME.
                LW_TABNM  = LS_DD26S-TABNAME.
                CREATE DATA LR_DATA TYPE TABLE OF (LW_TABNM).
                ASSIGN LR_DATA->* TO <LFT_TABL_NEW>.
                CREATE DATA LR_DATA TYPE TABLE OF (LW_TABNM).
                ASSIGN LR_DATA->* TO <LFT_TABL_ORG>.
                CREATE DATA LR_DATA TYPE (LW_TABNM).
                ASSIGN LR_DATA->* TO <LFS_TABL_NEW>.
                <LFT_TABL_NEW> = <LFT_VIEW_NEW>.
                <LFS_TABL_NEW> = <LFS_VIEW_NEW>.
                ASSIGN COMPONENT 'SPRAS' OF STRUCTURE <LFS_TABL_NEW>
                                              TO <LFW_VALUE>.
                IF SY-SUBRC IS INITIAL.
                  <LFW_VALUE> = SY-LANGU.
                ELSE.
                  ASSIGN COMPONENT 'LANGU' OF STRUCTURE <LFS_TABL_NEW>
                                         TO <LFW_VALUE>.
                  IF  SY-SUBRC IS INITIAL.
                    <LFW_VALUE> = SY-LANGU.
                  ELSE.
                    ASSIGN COMPONENT 'SPRSL' OF STRUCTURE <LFS_TABL_NEW>
                                           TO <LFW_VALUE>.
                    IF  SY-SUBRC IS INITIAL.
                      <LFW_VALUE> = SY-LANGU.
                    ENDIF.
                  ENDIF.
                ENDIF.

                LOOP AT <LFT_TABL_NEW> ASSIGNING <LFS_NEW>.
                  ASSIGN COMPONENT 'SPRAS' OF STRUCTURE <LFS_NEW>
                                             TO <LFW_VALUE>.
                  IF SY-SUBRC IS INITIAL.
                    <LFW_VALUE> = SY-LANGU.
                  ELSE.
                    ASSIGN COMPONENT 'LANGU' OF STRUCTURE <LFS_NEW>
                                         TO <LFW_VALUE>.
                    IF  SY-SUBRC IS INITIAL.
                      <LFW_VALUE> = SY-LANGU.
                    ELSE.
                      ASSIGN COMPONENT 'SPRSL' OF STRUCTURE <LFS_NEW>
                                           TO <LFW_VALUE>.
                      IF  SY-SUBRC IS INITIAL.
                        <LFW_VALUE> = SY-LANGU.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDLOOP.
              ENDIF.
*            ENDIF.

              CALL FUNCTION 'ZFM_DATA_ORIGINAL_GET'
                EXPORTING
                  T_TABLE_CURRENT    = <LFT_TABL_NEW>
                  I_STRUCTURE        = LW_TABNM
                IMPORTING
                  T_TABLE_ORIGINAL   = <LFT_TABL_ORG>
                EXCEPTIONS
                  NO_STRUCTURE       = 1
                  CONFLICT_STRUCTURE = 2
                  NO_DATA            = 3
                  OTHERS             = 4.
              IF SY-SUBRC IS INITIAL.
                IF <LFT_TABL_ORG> IS ASSIGNED AND <LFT_TABL_ORG> IS NOT INITIAL.
                  READ TABLE <LFT_TABL_ORG> ASSIGNING <LFS_TABL_ORG> INDEX 1.
                  MOVE-CORRESPONDING <LFS_TABL_ORG> TO <LFS_TABL_NEW>.
                  MOVE-CORRESPONDING <LFS_VIEW_NEW> TO <LFS_TABL_NEW>.

                  UPDATE (LW_TABNM) FROM <LFS_TABL_NEW>.
                ELSE.
                  ASSIGN COMPONENT 'SPRAS' OF STRUCTURE <LFS_TABL_NEW>
                  TO <LFW_VALUE>.
                  IF SY-SUBRC IS INITIAL.
                    <LFW_VALUE> = SY-LANGU.
                  ELSE.
                    ASSIGN COMPONENT 'LANGU' OF STRUCTURE <LFS_TABL_NEW>
                                         TO <LFW_VALUE>.
                    IF  SY-SUBRC IS INITIAL.

                      <LFW_VALUE> = SY-LANGU.
                    ELSE.
                      ASSIGN COMPONENT 'SPRSL' OF STRUCTURE <LFS_TABL_NEW>
                                           TO <LFW_VALUE>.
                      IF  SY-SUBRC IS INITIAL.
                        <LFW_VALUE> = SY-LANGU.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                  INSERT (LW_TABNM) FROM <LFS_TABL_NEW>.
                ENDIF.
                IF SY-SUBRC IS INITIAL.
                  LPS_CS_DETAIL-CSFSTS = GC_CSFSTS-SUCCESS.
                ELSE.
                  LPS_CS_DETAIL-CSFSTS = GC_CSFSTS-ERROR.
                ENDIF.
              ENDIF.
            ENDLOOP.
            UPDATE ZTB_BM_CSPJ_D
               SET CSFSTS   = LPS_CS_DETAIL-CSFSTS
                   CHUSR    = SY-UNAME
                   CHDAT    = SY-DATUM
                   CHTIM    = SY-UZEIT
             WHERE PROJID   = LPS_CS_DETAIL-PROJID
               AND MODLID   = LPS_CS_DETAIL-MODLID
               AND CSFUNC   = LPS_CS_DETAIL-CSFUNC
               AND VIEWNAME = LPS_CS_DETAIL-VIEWNAME
               AND TABKEY   = LPS_CS_DETAIL-TABKEY.

            IF SY-SUBRC IS INITIAL
            AND LPS_CS_DETAIL-CSFSTS = GC_CSFSTS-SUCCESS.
              LPS_SEL_FUNC-COMREC = LPS_SEL_FUNC-COMREC + 1.
            ELSE.
              ROLLBACK WORK.
              MESSAGE S001(ZMS_COL_LIB) INTO LPS_CS_DETAIL-MESSAGE.
            ENDIF.
          ELSE.
            IF LS_VIEW_INFO-TABCLASS = 'VIEW'.
              LW_TABNM  = LS_VIEW_INFO-ROOTTAB.
              CREATE DATA LR_DATA TYPE TABLE OF (LW_TABNM).
              ASSIGN LR_DATA->* TO <LFT_TABL_NEW>.
              CREATE DATA LR_DATA TYPE TABLE OF (LW_TABNM).
              ASSIGN LR_DATA->* TO <LFT_TABL_ORG>.
              CREATE DATA LR_DATA TYPE (LW_TABNM).
              ASSIGN LR_DATA->* TO <LFS_TABL_NEW>.
              MOVE-CORRESPONDING <LFS_VIEW_NEW> TO <LFS_TABL_NEW>.
              MOVE-CORRESPONDING <LFT_VIEW_NEW> TO <LFT_TABL_NEW>.
*       If object is table, set table for transport
            ELSEIF LS_VIEW_INFO-TABCLASS = 'TRANSP'.
              LW_TABNM  = LS_VIEW_INFO-TABNAME.
              CREATE DATA LR_DATA TYPE TABLE OF (LW_TABNM).
              ASSIGN LR_DATA->* TO <LFT_TABL_NEW>.
              CREATE DATA LR_DATA TYPE TABLE OF (LW_TABNM).
              ASSIGN LR_DATA->* TO <LFT_TABL_ORG>.
              CREATE DATA LR_DATA TYPE (LW_TABNM).
              ASSIGN LR_DATA->* TO <LFS_TABL_NEW>.
              <LFT_TABL_NEW> = <LFT_VIEW_NEW>.
              <LFS_TABL_NEW> = <LFS_VIEW_NEW>.
            ENDIF.
*            ENDIF.

            CALL FUNCTION 'ZFM_DATA_ORIGINAL_GET'
              EXPORTING
                T_TABLE_CURRENT    = <LFT_TABL_NEW>
                I_STRUCTURE        = LW_TABNM
              IMPORTING
                T_TABLE_ORIGINAL   = <LFT_TABL_ORG>
              EXCEPTIONS
                NO_STRUCTURE       = 1
                CONFLICT_STRUCTURE = 2
                NO_DATA            = 3
                OTHERS             = 4.
            IF SY-SUBRC IS INITIAL.
              IF <LFT_TABL_ORG> IS ASSIGNED AND <LFT_TABL_ORG> IS NOT INITIAL.
                READ TABLE <LFT_TABL_ORG> ASSIGNING <LFS_TABL_ORG> INDEX 1.
                MOVE-CORRESPONDING <LFS_TABL_ORG> TO <LFS_TABL_NEW>.
                MOVE-CORRESPONDING <LFS_VIEW_NEW> TO <LFS_TABL_NEW>.
                UPDATE (LW_TABNM) FROM <LFS_TABL_NEW>.
              ELSE.
                INSERT (LW_TABNM) FROM <LFS_TABL_NEW>.
              ENDIF.
              IF SY-SUBRC IS INITIAL.
                LPS_CS_DETAIL-CSFSTS = GC_CSFSTS-SUCCESS.
              ELSE.
                LPS_CS_DETAIL-CSFSTS = GC_CSFSTS-ERROR.
              ENDIF.
            ENDIF.
            UPDATE ZTB_BM_CSPJ_D
               SET CSFSTS   = LPS_CS_DETAIL-CSFSTS
                   CHUSR    = SY-UNAME
                   CHDAT    = SY-DATUM
                   CHTIM    = SY-UZEIT
             WHERE PROJID   = LPS_CS_DETAIL-PROJID
               AND MODLID   = LPS_CS_DETAIL-MODLID
               AND CSFUNC   = LPS_CS_DETAIL-CSFUNC
               AND VIEWNAME = LPS_CS_DETAIL-VIEWNAME
               AND TABKEY   = LPS_CS_DETAIL-TABKEY.

            IF SY-SUBRC IS INITIAL
            AND LPS_CS_DETAIL-CSFSTS = GC_CSFSTS-SUCCESS.
              LPS_SEL_FUNC-COMREC = LPS_SEL_FUNC-COMREC + 1.
            ELSE.
              ROLLBACK WORK.
              MESSAGE S001(ZMS_COL_LIB) INTO LPS_CS_DETAIL-MESSAGE.
            ENDIF.
          ENDIF.
        ENDIF.

      CATCH CX_SY_OPEN_SQL_DB INTO DATA(EXC).
        CL_DEMO_OUTPUT=>DISPLAY( EXC->GET_TEXT( ) ).
    ENDTRY.
  ELSE.
    IF LS_CSTR_C-FUNCMD IS NOT INITIAL.
      CALL FUNCTION LS_CSTR_C-FUNCMD
        EXPORTING
          I_DATA    = <LFS_VIEW_NEW>
        IMPORTING
          ET_RETURN = LT_RETURN.

      LOOP AT LT_RETURN INTO DATA(LS_RETURN) WHERE TYPE = 'E'.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC IS INITIAL.
        LPS_CS_DETAIL-CSFSTS = GC_CSFSTS-ERROR.
      ELSE.
        LPS_CS_DETAIL-CSFSTS = GC_CSFSTS-SUCCESS.
      ENDIF.

      UPDATE ZTB_BM_CSPJ_D
           SET CSFSTS   = LPS_CS_DETAIL-CSFSTS
               CHUSR    = SY-UNAME
               CHDAT    = SY-DATUM
               CHTIM    = SY-UZEIT
         WHERE PROJID   = LPS_CS_DETAIL-PROJID
           AND MODLID   = LPS_CS_DETAIL-MODLID
           AND CSFUNC   = LPS_CS_DETAIL-CSFUNC
           AND VIEWNAME = LPS_CS_DETAIL-VIEWNAME
           AND TABKEY   = LPS_CS_DETAIL-TABKEY.

      IF SY-SUBRC IS INITIAL
      AND LPS_CS_DETAIL-CSFSTS = GC_CSFSTS-SUCCESS.
        LPS_SEL_FUNC-COMREC = LPS_SEL_FUNC-COMREC + 1.
      ELSE.
        ROLLBACK WORK.
        MESSAGE S001(ZMS_COL_LIB) INTO LPS_CS_DETAIL-MESSAGE.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
