**********************************************************************
* Show actual adoption of the HighRadius interface by users, based
* on POType, Reason, etc.
**********************************************************************
REPORT yfir1210 MESSAGE-ID yfi LINE-SIZE 256.
SET PF-STATUS 'SALV_TABLE'.

TYPES: BEGIN OF gty_output,
         bukrs TYPE bukrs_vf,
         vkorg TYPE vkorg,
         augru TYPE augru,
         kunnr TYPE kunag,
         auart TYPE auart,
         bsark TYPE bsark,
         vbeln TYPE vbeln,
         xblnr TYPE xblnr_v1,
         erdat TYPE erdat,
         ernam TYPE ernam,
         netwr TYPE netwr_ak,
         waerk TYPE waerk.
TYPES END OF gty_output.

INCLUDE zfii1210.
INCLUDE zfio1210.
INCLUDE zfip1210.

START-OF-SELECTION.
  lcl_data_processing=>get_instance( )->process_data( ).
