FUNCTION-POOL ZFG_BM_CS_SALES_ORG.          "MESSAGE-ID ..

* INCLUDE LZFG_BM_CS_SALES_ORGD...           " Local class definition
TABLES:
  TVKO,
  TVKOT.
TABLES:
  TVST,
  TVSTT.
TABLES:
  TVBUR,
  TVKBT.
TYPES:
  BEGIN OF GTY_TVKO,
    VKORG     TYPE TVKO-VKORG,
    VTEXT     TYPE TVKOT-VTEXT,
    SPRAS     TYPE TVKOT-SPRAS,
    WAERS     TYPE TVKO-WAERS,
    TXNAM_ADR TYPE TVKO-TXNAM_ADR,
    TXNAM_KOP TYPE TVKO-TXNAM_KOP,
    TXNAM_FUS TYPE TVKO-TXNAM_FUS,
    TXNAM_GRU TYPE TVKO-TXNAM_GRU,
    VKOAU     TYPE TVKO-VKOAU,
    KUNNR     TYPE TVKO-KUNNR,
    BOAVO     TYPE TVKO-BOAVO,
    VKOKL     TYPE TVKO-VKOKL,
    EKORG     TYPE TVKO-EKORG,
    EKGRP     TYPE TVKO-EKGRP,
    LIFNR     TYPE TVKO-LIFNR,
    WERKS     TYPE TVKO-WERKS,
    BSART     TYPE TVKO-BSART,
    BSTYP     TYPE TVKO-BSTYP,
    BWART     TYPE TVKO-BWART,
    LGORT     TYPE TVKO-LGORT,
    TXNAM_SDB TYPE TVKO-TXNAM_SDB,
    HIDE      TYPE TVKO-HIDE,
    ADRNR     TYPE TVKO-ADRNR,
  END OF GTY_TVKO.
TYPES:
  BEGIN OF GTY_TVST,
    VSTEL          TYPE TVST-VSTEL,
    VTEXT          TYPE TVSTT-VTEXT,
    FABKL          TYPE TVST-FABKL,
    ADRNR          TYPE TVST-ADRNR,
    VTRZT          TYPE TVST-VTRZT,
    ALAND          TYPE TVST-ALAND,
    AZONE          TYPE TVST-AZONE,
    TXNAM_ADR      TYPE TVST-TXNAM_ADR,
    TXNAM_KOP      TYPE TVST-TXNAM_KOP,
    TXNAM_FUS      TYPE TVST-TXNAM_FUS,
    TXNAM_GRU      TYPE TVST-TXNAM_GRU,
    KSCHL          TYPE TVST-KSCHL,
    SPRAS          TYPE TVSTT-SPRAS,
    ANZAL          TYPE TVST-ANZAL,
    VSZTP          TYPE TVST-VSZTP,
    NACHA          TYPE TVST-NACHA,
    LAZBS          TYPE TVST-LAZBS,
    RIZBS          TYPE TVST-RIZBS,
    LAZZT          TYPE TVST-LAZZT,
    RIZZT          TYPE TVST-RIZZT,
    KOQUI          TYPE TVST-KOQUI,
    KOMSU          TYPE TVST-KOMSU,
    IMESS          TYPE TVST-IMESS,
    TXNAM_SDB      TYPE TVST-TXNAM_SDB,
    LOADTG         TYPE TVST-LOADTG,
    LOADTN         TYPE TVST-LOADTN,
    PIPATG         TYPE TVST-PIPATG,
    PIPATN         TYPE TVST-PIPATN,
    TSTRID         TYPE TVST-TSTRID,
    AUTOMATIC_PICK TYPE TVST-AUTOMATIC_PICK,
  END OF GTY_TVST.
TYPES:
  BEGIN OF GTY_TVBUR,
    VKBUR TYPE  TVBUR-VKBUR,
    ERNAM TYPE TVBUR-ERNAM,
    BEZEI TYPE TVKBT-BEZEI,
    ADRNR TYPE TVBUR-ADRNR,
    HIDE  TYPE TVBUR-HIDE,
    SPRAS TYPE TVKBT-SPRAS,
  END OF GTY_TVBUR.
*TYPES:
*  BEGIN OF GTY_SD66,
*    KVEWE   TYPE T682I-KVEWE,
*    KAPPL   TYPE T682I-KAPPL,
*    KOZGF   TYPE T682I-KOZGF,
*    KOLNR   TYPE T682I-KOLNR,
*    KOTABNR TYPE T682I-KOTABNR,
*    KOBED   TYPE T682I-KOBED,
*    KZEXL   TYPE T682I-KZEXL,
*    KKOPF   TYPE T682I-KKOPF,
*    GZUGR   TYPE T682I-GZUGR,
*    ZAEHK   TYPE T682Z-ZAEHK,
*    ZIFNA   TYPE T682Z-ZIFNA,
*    QUSTR   TYPE T682Z-QUSTR,
*    QUFNA   TYPE T682Z-QUFNA,
*
*  END OF GTY_SD66.



DATA:
  GS_DATA      TYPE   ZST_BM_SALES,
  GS_DATASD09  TYPE   ZST_BM_SALES_SD09,
  GS_DATASD08  TYPE   ZST_BM_SALES_SD08,
  GS_DATASD10  TYPE   ZST_BM_SALES_SD10,
  GS_DATASD11  TYPE   ZST_BM_SALES_SD11,
  GS_DATASD12  TYPE   ZST_BM_SALES_SD12,
  GS_DATASD13  TYPE   ZST_BM_SALES_SD13,
  GT_SD13      TYPE   ZST_BM_SALES_SD13,
  GS_SD13      TYPE   ZST_BM_SALES_SD13,
  GS_DATASD16  TYPE   ZST_BM_SALES_SD16,
  GS_DATASD15  TYPE   ZST_BM_SALES_SD15,
  GT_SD15      TYPE   ZST_BM_SALES_SD15,
  GS_SD15      TYPE   ZST_BM_SALES_SD15,
  GS_DATASD17  TYPE   ZST_BM_SALES_SD17,
  GS_DATASD26  TYPE   ZST_BM_SALES_SD26,
  GT_SD26      TYPE   ZST_BM_SALES_SD26,
  GS_SD26      TYPE   ZST_BM_SALES_SD26,
  GS_DATASD28  TYPE   ZST_BM_SALES_SD28,
  GS_DATASD29  TYPE   ZST_BM_SALES_SD29,
  GS_DATASD31  TYPE   ZST_BM_SALES_SD31,
  GS_DATASD33  TYPE   ZST_BM_SALES_SD33,
  GS_DATASD35  TYPE   ZST_BM_SALES_SD35,
  GS_DATASD37  TYPE   ZST_BM_SALES_SD37,
  GS_DATASD39  TYPE   ZST_BM_SALES_SD39,
  GT_SD39      TYPE   ZST_BM_SALES_SD39,
  GS_SD39      TYPE   ZST_BM_SALES_SD39,
  GS_DATASD42  TYPE   ZST_BM_SALES_SD42,
  GS_DATASD44  TYPE   ZST_BM_SALES_SD44,
  GT_SD44      TYPE   ZST_BM_SALES_SD44,
  GS_SD44      TYPE   ZST_BM_SALES_SD44,
  GS_DATASD47  TYPE   ZST_BM_SALES_SD47,
  GS_DATASD49  TYPE   ZST_BM_SALES_SD49,
  GT_SD49      TYPE   ZST_BM_SALES_SD49,
  GS_SD49      TYPE   ZST_BM_SALES_SD49,
  GS_DATASD52  TYPE   ZST_BM_SALES_SD52,
  GS_DATASD52A TYPE   ZST_BM_SALES_SD52A,
  GT_SD52A     TYPE   ZST_BM_SALES_SD52A,
  GS_SD52A     TYPE   ZST_BM_SALES_SD52A,
  GS_DATASD55  TYPE   ZST_BM_SALES_SD55,
  GS_DATASD56  TYPE   ZST_BM_SALES_SD56,
  GS_DATASD59  TYPE   ZST_BM_SALES_SD59,
  GS_DATASD60  TYPE   ZST_BM_SALES_SD60,
  GS_DATASD61  TYPE   ZST_BM_SALES_SD61,
  GS_DATASD65  TYPE   ZST_BM_SALES_SD65,
  GS_DATASD66  TYPE   ZST_BM_SALES_SD66,
  GS_DATASD67  TYPE   ZST_BM_SALES_SD67,
  GS_DATASD68  TYPE   ZST_BM_SALES_SD68,
  GS_DATASD69  TYPE   ZST_BM_SALES_SD69,
  GS_DATASD70  TYPE   ZST_BM_SALES_SD70,
  GS_DATASD71  TYPE   ZST_BM_SALES_SD71,
  GS_DATASD74  TYPE   ZST_BM_SALES_SD74,
  GS_DATASD75  TYPE   ZST_BM_SALES_SD75,
  GS_DATASD78  TYPE   ZST_BM_SALES_SD78,
  GS_DATASD79  TYPE   ZST_BM_SALES_SD79,
  GS_DATASD85  TYPE   ZST_BM_SALES_SD85,
  GS_DATASD84  TYPE   ZST_BM_SALES_SD84,
  GS_DATASD98  TYPE   ZST_BM_SALES_SD98,
  GS_DATASD99  TYPE   ZST_BM_SALES_SD99,
  GS_DATASD100 TYPE   ZST_BM_SALES_SD100,
  GS_DATASD101 TYPE   ZST_BM_SALES_SD101,
  GS_DATASD102 TYPE   ZST_BM_SALES_SD102,
  GS_DATASD103 TYPE   ZST_BM_SALES_SD103,
  GS_DATASD105 TYPE   ZST_BM_SALES_SD105,
  GS_DATASD106 TYPE   ZST_BM_SALES_SD106,
  GS_DATASD107 TYPE   ZST_BM_SALES_SD107,
  GS_DATASD108 TYPE   ZST_BM_SALES_SD108,
  GS_DATASD125 TYPE   ZST_BM_SALES_SDD125,
  GS_DATASD126 TYPE   ZST_BM_SALES_SD126,
  GS_DATASD127 TYPE   ZST_BM_SALES_SD127,
  GS_DATASD146 TYPE   ZST_BM_SALES_SD146,
  GS_DATASD147 TYPE   ZST_BM_SALES_SD147,
  GS_DATASD154 TYPE   ZST_BM_SALES_SD154,
  GS_DATASD155 TYPE   ZST_BM_SALES_SD155,
  GS_DATASD156 TYPE   ZST_BM_SALES_SD156,
  GS_DATASD169 TYPE   ZST_BM_SALES_SD169,
  GS_DATASD175 TYPE   ZST_BM_SALES_SD175,
  GS_DATASD176 TYPE   ZST_BM_SALES_SD176,
  GS_DATASD192 TYPE   ZST_BM_SALES_SD192,
  GS_DATASD193 TYPE   ZST_BM_SALES_SD193,
  GS_DATASD195 TYPE   ZST_BM_SALES_SD195,
  GS_DATASD197 TYPE   ZST_BM_SALES_SD197,
  GS_DATASD202 TYPE   ZST_BM_SALES_SD202,
  GS_DATASD214 TYPE   ZST_BM_SALES_SD214,
  GS_DATASD215 TYPE   ZST_BM_SALES_SD215,
  GS_DATASD216 TYPE   ZST_BM_SALES_SD216,
  GS_DATASD217 TYPE   ZST_BM_SALES_SD217,
  GT_SD217 TYPE   ZST_BM_SALES_SD217,
  GS_SD217 TYPE   ZST_BM_SALES_SD217,
  GT_SD216 TYPE   ZST_BM_SALES_SD216,
  GS_SD216 TYPE   ZST_BM_SALES_SD216,
  GT_SD215 TYPE   ZST_BM_SALES_SD215,
  GS_SD215 TYPE   ZST_BM_SALES_SD215,
  GT_SD214 TYPE   ZST_BM_SALES_SD214,
  GS_SD214 TYPE   ZST_BM_SALES_SD214,
  Gt_SD202 TYPE   ZST_BM_SALES_SD202,
  GS_SD202 TYPE   ZST_BM_SALES_SD202,
  GT_SD197 TYPE   ZST_BM_SALES_SD197,
  GS_SD197 TYPE   ZST_BM_SALES_SD197,
  GT_SD195     TYPE   ZST_BM_SALES_SD195,
  GS_SD195     TYPE   ZST_BM_SALES_SD195,
  GT_SD193     TYPE   ZST_BM_SALES_SD193,
  GS_SD193     TYPE   ZST_BM_SALES_SD193,
  GT_SD192     TYPE   ZST_BM_SALES_SD192,
  GS_SD192     TYPE   ZST_BM_SALES_SD192,
  GT_SD176     TYPE   ZST_BM_SALES_SD176,
  GS_SD176     TYPE   ZST_BM_SALES_SD176,
  GT_SD175     TYPE   ZST_BM_SALES_SD175,
  GS_SD175     TYPE   ZST_BM_SALES_SD175,
  GT_SD169     TYPE   ZST_BM_SALES_SD169,
  GS_SD169     TYPE   ZST_BM_SALES_SD169,
  GT_SD156     TYPE   ZST_BM_SALES_SD156,
  GS_SD156     TYPE   ZST_BM_SALES_SD156,
  GT_SD155     TYPE   ZST_BM_SALES_SD155,
  GS_SD155     TYPE   ZST_BM_SALES_SD155,
  GT_SD154     TYPE   ZST_BM_SALES_SD154,
  GS_SD154     TYPE   ZST_BM_SALES_SD154,
  GT_SD147     TYPE   ZST_BM_SALES_SD147,
  GS_SD147     TYPE   ZST_BM_SALES_SD147,
  GT_SD146     TYPE   ZST_BM_SALES_SD146,
  GS_SD146     TYPE   ZST_BM_SALES_SD146,
  GT_SD127     TYPE   ZST_BM_SALES_SD127,
  GS_SD127     TYPE   ZST_BM_SALES_SD127,
  GT_SD126     TYPE   ZST_BM_SALES_SD126,
  GS_SD126     TYPE   ZST_BM_SALES_SD126,
  GT_SD125     TYPE   ZST_BM_SALES_SDD125,
  GS_SD125     TYPE   ZST_BM_SALES_SDD125,
  GT_SD108     TYPE   ZST_BM_SALES_SD108,
  GS_SD108     TYPE   ZST_BM_SALES_SD108,
  GT_SD107     TYPE   ZST_BM_SALES_SD107,
  GS_SD107     TYPE   ZST_BM_SALES_SD107,
  GT_SD106     TYPE   ZST_BM_SALES_SD106,
  GS_SD106     TYPE   ZST_BM_SALES_SD106,
  GT_SD105     TYPE   ZST_BM_SALES_SD105,
  GS_SD105     TYPE   ZST_BM_SALES_SD105,
  GT_SD103     TYPE   ZST_BM_SALES_SD103,
  GS_SD103     TYPE   ZST_BM_SALES_SD103,
  GT_SD102     TYPE   ZST_BM_SALES_SD102,
  GS_SD102     TYPE   ZST_BM_SALES_SD102,
  GT_SD101     TYPE   ZST_BM_SALES_SD101,
  GS_SD101     TYPE   ZST_BM_SALES_SD101,
  GT_SD100     TYPE   ZST_BM_SALES_SD100,
  GS_SD100     TYPE   ZST_BM_SALES_SD100,
  GT_SD99      TYPE   ZST_BM_SALES_SD99,
  GS_SD99      TYPE   ZST_BM_SALES_SD99,
  GT_SD98      TYPE   ZST_BM_SALES_SD98,
  GS_SD98      TYPE   ZST_BM_SALES_SD98,
  GT_SD84      TYPE   ZST_BM_SALES_SD84,
  GS_SD84      TYPE   ZST_BM_SALES_SD84,
  GT_SD85      TYPE   ZST_BM_SALES_SD85,
  GS_SD85      TYPE   ZST_BM_SALES_SD85,
  GT_SD79      TYPE   ZST_BM_SALES_SD79,
  GS_SD79      TYPE   ZST_BM_SALES_SD79,
  GT_SD78      TYPE   ZST_BM_SALES_SD78,
  GS_SD78      TYPE   ZST_BM_SALES_SD78,
  GT_SD75      TYPE   ZST_BM_SALES_SD75,
  GS_SD75      TYPE   ZST_BM_SALES_SD75,
  GT_SD74      TYPE   ZST_BM_SALES_SD74,
  GS_SD74      TYPE   ZST_BM_SALES_SD74,
  GT_SD71      TYPE   ZST_BM_SALES_SD71,
  GS_SD71      TYPE   ZST_BM_SALES_SD71,
  GT_SD70      TYPE   ZST_BM_SALES_SD70,
  GS_SD70      TYPE   ZST_BM_SALES_SD70,
  GT_SD69      TYPE   ZST_BM_SALES_SD69,
  GS_SD69      TYPE   ZST_BM_SALES_SD69,
  GT_SD68      TYPE   ZST_BM_SALES_SD68,
  GS_SD68      TYPE   ZST_BM_SALES_SD68,
  GT_SD67      TYPE   ZST_BM_SALES_SD67,
  GS_SD67      TYPE   ZST_BM_SALES_SD67,
  GT_SD66      TYPE   ZST_BM_SALES_SD66,
  GS_SD66      TYPE   ZST_BM_SALES_SD66,
  GT_SD65      TYPE   ZST_BM_SALES_SD65,
  GS_SD65      TYPE   ZST_BM_SALES_SD65,
  GT_SD61      TYPE   ZST_BM_SALES_SD61,
  GS_SD61      TYPE   ZST_BM_SALES_SD61,
  GT_SD60      TYPE   ZST_BM_SALES_SD60,
  GS_SD60      TYPE   ZST_BM_SALES_SD60,
  GT_SD59      TYPE   ZST_BM_SALES_SD59,
  GS_SD59      TYPE   ZST_BM_SALES_SD59,
  GT_SD56      TYPE   ZST_BM_SALES_SD56,
  GS_SD56      TYPE   ZST_BM_SALES_SD56,
  GT_SD55      TYPE   ZST_BM_SALES_SD55,
  GS_SD55      TYPE   ZST_BM_SALES_SD55,
  GT_SD52      TYPE   ZST_BM_SALES_SD52,
  GS_SD52      TYPE   ZST_BM_SALES_SD52,
  GT_SD47      TYPE   ZST_BM_SALES_SD47,
  GS_SD47      TYPE   ZST_BM_SALES_SD47,
  GT_SD42      TYPE   ZST_BM_SALES_SD42,
  GS_SD42      TYPE   ZST_BM_SALES_SD42,
  GT_SD37      TYPE   ZST_BM_SALES_SD37,
  GS_SD37      TYPE   ZST_BM_SALES_SD37,
  GT_SD35      TYPE   ZST_BM_SALES_SD35,
  GS_SD35      TYPE   ZST_BM_SALES_SD35,
  GT_SD33      TYPE   ZST_BM_SALES_SD33,
  GS_SD33      TYPE   ZST_BM_SALES_SD33,
  GT_SD31      TYPE   ZST_BM_SALES_SD31,
  GS_SD31      TYPE   ZST_BM_SALES_SD31,
  GT_SD29      TYPE   ZST_BM_SALES_SD29,
  GS_SD29      TYPE   ZST_BM_SALES_SD29,
  GT_SD28      TYPE   ZST_BM_SALES_SD28,
  GS_SD28      TYPE   ZST_BM_SALES_SD28,
  GT_SD17      TYPE   ZST_BM_SALES_SD17,
  GS_SD17      TYPE   ZST_BM_SALES_SD17,
  GT_SD16      TYPE   ZST_BM_SALES_SD16,
  GS_SD16      TYPE   ZST_BM_SALES_SD16,
  GT_SD12      TYPE   ZST_BM_SALES_SD12,
  GS_SD12      TYPE   ZST_BM_SALES_SD12,
  GT_SD11      TYPE   ZST_BM_SALES_SD11,
  GS_SD11      TYPE   ZST_BM_SALES_SD11,
  GT_SD10      TYPE   ZST_BM_SALES_SD10,
  GS_SD10      TYPE   ZST_BM_SALES_SD10,
  GT_SD08      TYPE   ZST_BM_SALES_SD08,
  GS_SD08      TYPE   ZST_BM_SALES_SD08,
  GT_TVKOV     TYPE   ZST_BM_SALES_SD09,
  GS_TVKOV     TYPE   ZST_BM_SALES_SD09,
  GT_RETURN    TYPE  TABLE OF BAPIRET2,
  GT_TVKO      TYPE TABLE OF GTY_TVKO,
  GS_TVKO      TYPE GTY_TVKO,
  G_MODE       TYPE C.
DATA:
  GS_DATA1 TYPE   ZST_BM_SALES_V_TVST,
  GS_DATA2 TYPE   ZST_BM_SALES_V_TVBUR,
  GT_TVST  TYPE TABLE OF GTY_TVST,
  GT_TVBUR TYPE TABLE OF GTY_TVBUR,
  GS_TVST  TYPE GTY_TVST,
  GS_TVBUR TYPE GTY_TVBUR.
DATA: GF_FLAG_ADDRESS_CHANGE TYPE C.
DATA: MAINT_STAT LIKE VIMSTATUS.
FIELD-SYMBOLS:
               <STATUS>          STRUCTURE VIMSTATUS DEFAULT MAINT_STAT.

DATA: ADTEL_WA TYPE SZADR_ADTEL_LINE.


DATA: BEGIN OF ADTEL OCCURS 0.
        INCLUDE STRUCTURE ADTEL.
DATA: END OF ADTEL.
DATA: BEGIN OF ADFAX OCCURS 0.
        INCLUDE STRUCTURE ADFAX.
DATA: END OF ADFAX.
DATA: BEGIN OF ADSMTP OCCURS 0.
        INCLUDE STRUCTURE ADSMTP.
DATA: END OF ADSMTP.
DATA: BEGIN OF E_TABLE OCCURS 0.
        INCLUDE STRUCTURE ADDR_ERROR.
DATA: END OF E_TABLE.
TYPES:
  BEGIN OF SZADR_ADTEL_LINE,
    DATE_FROM LIKE ADRC-DATE_FROM,
    ADTEL     LIKE ADTEL,
  END OF SZADR_ADTEL_LINE.
