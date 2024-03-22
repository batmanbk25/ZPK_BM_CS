FUNCTION ZFM_BM_CS_COLLECT_KEYS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_CSFUNC) TYPE  ZTT_BM_CSTR_C
*"  EXPORTING
*"     REFERENCE(ET_CS_MAP) TYPE  ZTT_BM_CS_MAP
*"     REFERENCE(ET_CSTR_K) TYPE  ZTT_BM_CSTR_K
*"----------------------------------------------------------------------
  DATA:
    LT_TSTCP  TYPE TABLE OF TSTCP,
    LT_CS_MAP TYPE ZTT_BM_CS_MAP,
    LS_CS_MAP TYPE ZST_BM_CS_MAP.

  MOVE-CORRESPONDING IT_CSFUNC TO LT_CS_MAP.

  CALL FUNCTION 'ZFM_BM_CS_TCODE_TO_TABLE'
    CHANGING
      CT_CS_MAP = LT_CS_MAP.

  IF ET_CSTR_K IS INITIAL
  AND IT_CSFUNC IS NOT INITIAL.
    SELECT *
      FROM ZTB_BM_CSTR_K
      INTO TABLE ET_CSTR_K
       FOR ALL ENTRIES IN IT_CSFUNC
     WHERE MODLID = IT_CSFUNC-MODLID
       AND CSFUNC = IT_CSFUNC-CSFUNC.
  ENDIF.
  SORT LT_CS_MAP BY MODLID CSFUNC.
  SORT ET_CSTR_K BY MODLID CSFUNC.

  LOOP AT IT_CSFUNC INTO DATA(LS_CSFUNC).
    LOOP AT LT_CS_MAP INTO LS_CS_MAP
      WHERE MODLID = LS_CSFUNC-MODLID
        AND CSFUNC = LS_CSFUNC-CSFUNC.
      READ TABLE ET_CSTR_K TRANSPORTING NO FIELDS BINARY SEARCH
        WITH KEY MODLID = LS_CS_MAP-MODLID
                 CSFUNC = LS_CS_MAP-CSFUNC
                 VIEWNAME = LS_CS_MAP-TABNAME.
      IF SY-SUBRC IS INITIAL.
        LOOP AT ET_CSTR_K INTO DATA(LS_CSTR_K) FROM SY-TABIX.
          IF LS_CSTR_K-MODLID <> LS_CS_MAP-MODLID
          OR LS_CSTR_K-CSFUNC <> LS_CS_MAP-CSFUNC
          OR LS_CSTR_K-VIEWNAME <> LS_CS_MAP-TABNAME.
            EXIT.
          ENDIF.

          LS_CS_MAP-TABKEY = LS_CSTR_K-TABKEY.
          APPEND LS_CS_MAP TO ET_CS_MAP.
        ENDLOOP.
      ELSE.
        LS_CS_MAP-MODLID = LS_CS_MAP-MODLID.
        LS_CS_MAP-CSFUNC = LS_CS_MAP-CSFUNC.
        LS_CS_MAP-TABNAME = LS_CS_MAP-TABNAME.
        IF LS_CS_MAP-CLIDEP IS INITIAL.
          LS_CS_MAP-TABKEY = '*'.
        ELSE.
          LS_CS_MAP-TABKEY = SY-MANDT && '*'.
        ENDIF.
        APPEND LS_CS_MAP TO ET_CS_MAP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFUNCTION.
