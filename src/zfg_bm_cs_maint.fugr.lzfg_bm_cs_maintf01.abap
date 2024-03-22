*----------------------------------------------------------------------*
***INCLUDE LZFG_BM_CS_MAINTF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  0102_SET_KEYS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE 0102_SET_KEYS INPUT.
  CASE SY-UCOMM.
    WHEN 'FC_SETKEY'.
      PERFORM 0102_SET_KEYS.
    WHEN 'FC_RSKEY'.
      PERFORM 0102_RESET_KEYS.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.

*&---------------------------------------------------------------------*
*& Form 0102_SET_KEYS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0102_SET_KEYS .
  DATA:
    LS_CSTR_K   TYPE ZTB_BM_CSTR_K,
    LT_CSTR_K_O TYPE TABLE OF ZTB_BM_CSTR_K,
    LS_CS_MAP   TYPE ZST_BM_CS_MAP,
    LT_CS_MAP   TYPE ZTT_BM_CS_MAP,
    LW_ANSWER   TYPE C.

  LOOP AT EXTRACT.
    ZVI_BM_CSTR_C_EXTRACT = EXTRACT.
    CHECK ZVI_BM_CSTR_C_EXTRACT-MARK IS NOT INITIAL.
    MOVE-CORRESPONDING ZVI_BM_CSTR_C_EXTRACT TO ZVI_BM_CSTR_C.

    READ TABLE GT_CSTR_K TRANSPORTING NO FIELDS
      WITH KEY MODLID = ZVI_BM_CSTR_C-MODLID
               CSFUNC = ZVI_BM_CSTR_C-CSFUNC
               VIEWNAME = ZVI_BM_CSTR_C-VIEWNAME BINARY SEARCH.
    IF SY-SUBRC IS NOT INITIAL.
      SELECT *
        FROM ZTB_BM_CSTR_K
        INTO TABLE GT_CSTR_K
       WHERE MODLID = ZVI_BM_CSTR_C-MODLID
         AND CSFUNC = ZVI_BM_CSTR_C-CSFUNC.
      SORT GT_CSTR_K BY MODLID CSFUNC VIEWNAME.
    ENDIF.

    MOVE-CORRESPONDING ZVI_BM_CSTR_C TO LS_CS_MAP.
    APPEND LS_CS_MAP TO LT_CS_MAP.

    CALL FUNCTION 'ZFM_BM_CS_TCODE_TO_TABLE'
      CHANGING
        CT_CS_MAP = LT_CS_MAP.
    LOOP AT LT_CS_MAP ASSIGNING FIELD-SYMBOL(<LF_CS_MAP>).
      READ TABLE GT_CSTR_K TRANSPORTING NO FIELDS
        WITH KEY MODLID = <LF_CS_MAP>-MODLID
                 CSFUNC = <LF_CS_MAP>-CSFUNC
                 VIEWNAME = <LF_CS_MAP>-TABNAME.
      IF SY-SUBRC IS NOT INITIAL.
        LS_CSTR_K-MODLID = <LF_CS_MAP>-MODLID.
        LS_CSTR_K-CSFUNC = <LF_CS_MAP>-CSFUNC.
        LS_CSTR_K-VIEWNAME = <LF_CS_MAP>-TABNAME.
        IF <LF_CS_MAP>-CLIDEP IS INITIAL.
          LS_CSTR_K-TABKEY = '*'.
        ELSE.
          LS_CSTR_K-TABKEY = SY-MANDT && '*'.
        ENDIF.
        APPEND LS_CSTR_K TO GT_CSTR_K.
      ENDIF.
    ENDLOOP.

    LT_CSTR_K_O = GT_CSTR_K.
    CALL FUNCTION 'ZFM_ALV_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM      = SY-REPID
*       I_CALLBACK_PF_STATUS_SET          = ''
        I_CALLBACK_USER_COMMAND = '9999_KEY_USER_COMMAND'
        I_STRUCTURE_NAME        = 'ZTB_BM_CSTR_K'
*       IT_FIELDCAT             =
      TABLES
        T_OUTTAB                = GT_CSTR_K.

    IF LT_CSTR_K_O <> GT_CSTR_K.
      CALL FUNCTION 'ZFM_POPUP_CONFIRM_SAVE_CHANGED'
        EXPORTING
          I_DISPLAY_CANCEL_BUTTON = ' '
          I_SHOWICON              = ' '
        IMPORTING
          E_ANSWER                = LW_ANSWER.
      IF LW_ANSWER = '1'.

        CALL FUNCTION 'ZFM_DATA_UPDATE_TABLE'
         EXPORTING
           I_STRUCTURE                  = 'ZTB_BM_CSTR_K'
           T_TABLE_CHANGED              = GT_CSTR_K
           T_TABLE_ORIGINAL             = LT_CSTR_K_O
         EXCEPTIONS
           NO_STRUCTURE                 = 1
           CONFLICT_STRUCTURE           = 2
           NO_ORIGINAL_DATA             = 3
           CONFLICT_ORIGINAL_DATA       = 4
           NO_DATA                      = 5
           UPDATE_ERROR                 = 6
           INSERT_ERROR                 = 7
           DELETE_ERROR                 = 8
           DEL_FIELD_NOT_EXISTS         = 9
           OTHERS                       = 10
                  .
        IF SY-SUBRC <> 0.
          MESSAGE S011(ZMS_COL_LIB) DISPLAY LIKE 'E' WITH 'ZTB_BM_CSTR_K'.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: GT_CSTR_K.
*
*    EXTRACT = ZVI_BM_CSTR_C_EXTRACT.
*    MODIFY EXTRACT.
    EXIT.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form 0102_RESET_KEYS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM 0102_RESET_KEYS .
  DATA:
    LS_CSTR_K   TYPE ZTB_BM_CSTR_K,
    LT_CSTR_K_O TYPE TABLE OF ZTB_BM_CSTR_K,
    LS_CS_MAP   TYPE ZST_BM_CS_MAP,
    LT_CS_MAP   TYPE ZTT_BM_CS_MAP,
    LW_ANSWER   TYPE C.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Reset Keys'
      TEXT_QUESTION         = 'Do you want reset keys of selected functions to initial (all values)?'
      TEXT_BUTTON_1         = 'Yes'
      TEXT_BUTTON_2         = 'No'
      DISPLAY_CANCEL_BUTTON = SPACE
    IMPORTING
      ANSWER                = LW_ANSWER.           "Answer
  CHECK LW_ANSWER = '1'.

  LOOP AT EXTRACT.
    CLEAR: LT_CS_MAP, GT_CSTR_K.
    ZVI_BM_CSTR_C_EXTRACT = EXTRACT.

    CHECK ZVI_BM_CSTR_C_EXTRACT-MARK IS NOT INITIAL.
    MOVE-CORRESPONDING ZVI_BM_CSTR_C_EXTRACT TO ZVI_BM_CSTR_C.

    DELETE FROM ZTB_BM_CSTR_K
     WHERE MODLID = ZVI_BM_CSTR_C-MODLID
       AND CSFUNC = ZVI_BM_CSTR_C-CSFUNC.


    MOVE-CORRESPONDING ZVI_BM_CSTR_C TO LS_CS_MAP.
    APPEND LS_CS_MAP TO LT_CS_MAP.

    CALL FUNCTION 'ZFM_BM_CS_TCODE_TO_TABLE'
      CHANGING
        CT_CS_MAP = LT_CS_MAP.
    LOOP AT LT_CS_MAP ASSIGNING FIELD-SYMBOL(<LF_CS_MAP>).
      READ TABLE GT_CSTR_K TRANSPORTING NO FIELDS
        WITH KEY MODLID = <LF_CS_MAP>-MODLID
                 CSFUNC = <LF_CS_MAP>-CSFUNC
                 VIEWNAME = <LF_CS_MAP>-TABNAME.
      IF SY-SUBRC IS NOT INITIAL.
        LS_CSTR_K-MODLID = <LF_CS_MAP>-MODLID.
        LS_CSTR_K-CSFUNC = <LF_CS_MAP>-CSFUNC.
        LS_CSTR_K-VIEWNAME = <LF_CS_MAP>-TABNAME.
        IF <LF_CS_MAP>-CLIDEP IS INITIAL.
          LS_CSTR_K-TABKEY = '*'.
        ELSE.
          LS_CSTR_K-TABKEY = SY-MANDT && '*'.
        ENDIF.
        APPEND LS_CSTR_K TO GT_CSTR_K.
      ENDIF.
    ENDLOOP.
    INSERT ZTB_BM_CSTR_K FROM TABLE GT_CSTR_K.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  9999_KEY_USER_COMMAND
*&---------------------------------------------------------------------*
*       Show top HTML of ALV
*----------------------------------------------------------------------*
*      -->LW_UCOMM     User command
*      -->LS_SELFIELD  Selection Fields
*----------------------------------------------------------------------*
FORM 9999_KEY_USER_COMMAND
    USING LW_UCOMM     TYPE SY-UCOMM
          LS_SELFIELD TYPE SLIS_SELFIELD.
  DATA:
    LS_KEY            TYPE  E071K,
    LS_REQUEST_HEADER TYPE  TRWBO_REQUEST_HEADER,
    LS_CSTR_K         TYPE ZTB_BM_CSTR_K,
    LW_CURRENT        TYPE I,
    LW_INDEX          TYPE I,
    LW_NEWFLAG        TYPE XMARK.

  IF LW_UCOMM IS NOT INITIAL.
    READ TABLE GT_CSTR_K ASSIGNING FIELD-SYMBOL(<LF_CSTR_K>)
     INDEX LS_SELFIELD-TABINDEX.
    IF SY-SUBRC IS INITIAL.

      LS_KEY-TRKORR   = ''.
      LS_KEY-PGMID    = ''.
      LS_KEY-OBJECT   = ''.
      LS_KEY-OBJNAME  = <LF_CSTR_K>-VIEWNAME.
      LS_KEY-MASTERNAME = <LF_CSTR_K>-VIEWNAME.
      LS_KEY-VIEWNAME   = <LF_CSTR_K>-VIEWNAME.
      LS_KEY-AS4POS     = SY-TABIX.
      LS_KEY-TABKEY     = <LF_CSTR_K>-TABKEY.
      CALL FUNCTION 'TRKLE_POPUP_TO_EDIT_TABKEY'
        EXPORTING
          IV_EDIT_MODE      = 'X'
          IV_NO_CHECK       = 'X'
          IS_KEY            = LS_KEY
          IS_REQUEST_HEADER = LS_REQUEST_HEADER
        IMPORTING
          EV_NEW_TABKEY     = <LF_CSTR_K>-TABKEY
        EXCEPTIONS
          USER_CANCELED     = 1
          INVALID_INPUT     = 2
          OTHERS            = 3.
      LS_SELFIELD-REFRESH = 'X'.
      LW_INDEX = LS_SELFIELD-TABINDEX.

      WHILE SY-UCOMM = 'NEXT' OR SY-UCOMM = 'PREVIOUS'.
        CLEAR: LW_NEWFLAG.

        LW_CURRENT = LW_INDEX.
        IF SY-UCOMM = 'NEXT'.
          LW_INDEX = LW_INDEX + 1.
        ELSEIF SY-UCOMM = 'PREVIOUS'.
          LW_INDEX = LW_INDEX - 1.
        ENDIF.
        IF LW_INDEX = 0.
          LW_INDEX = 1.
        ENDIF.

        IF LW_INDEX BETWEEN 1 AND LINES( GT_CSTR_K ).
          READ TABLE GT_CSTR_K INTO LS_CSTR_K INDEX LW_INDEX.
          IF SY-SUBRC IS INITIAL.
            IF LS_CSTR_K-VIEWNAME <> <LF_CSTR_K>-VIEWNAME.
              LW_NEWFLAG = 'X'.
            ELSE.
              READ TABLE GT_CSTR_K ASSIGNING <LF_CSTR_K> INDEX LW_INDEX.
            ENDIF.
          ELSE.
            LW_NEWFLAG = 'X'.
          ENDIF.
        ELSE.
          LW_NEWFLAG = 'X'.
        ENDIF.

        IF LW_NEWFLAG IS NOT INITIAL.
          IF LW_INDEX < LW_CURRENT..
            LW_INDEX = LW_CURRENT + 1.
          ENDIF.
          LS_CSTR_K = <LF_CSTR_K>.
          CLEAR: LS_KEY-TABKEY, LS_CSTR_K-TABKEY.
          INSERT LS_CSTR_K INTO GT_CSTR_K INDEX LW_INDEX ASSIGNING <LF_CSTR_K>.
        ENDIF.

        LS_KEY-AS4POS     = LW_INDEX.
        LS_KEY-TABKEY     = <LF_CSTR_K>-TABKEY.
        CALL FUNCTION 'TRKLE_POPUP_TO_EDIT_TABKEY'
          EXPORTING
            IV_EDIT_MODE      = 'X'
            IV_NO_CHECK       = 'X'
            IS_KEY            = LS_KEY
            IS_REQUEST_HEADER = LS_REQUEST_HEADER
          IMPORTING
            EV_NEW_TABKEY     = <LF_CSTR_K>-TABKEY
          EXCEPTIONS
            USER_CANCELED     = 1
            INVALID_INPUT     = 2
            OTHERS            = 3.
        LS_SELFIELD-REFRESH = 'X'.
      ENDWHILE.
    ENDIF.

  ENDIF.
ENDFORM.                    "9999_KEY_USER_COMMAND
