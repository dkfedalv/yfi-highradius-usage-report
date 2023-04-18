**********************************************************************
* Show actual adoption of the HighRadius interface by users, based
* on POType, Reason, etc.
**********************************************************************
REPORT yfir1210 MESSAGE-ID yfi LINE-SIZE 256.

TYPES: BEGIN OF gty_output,
         bukrs TYPE bukrs_vf,
         vkorg TYPE vkorg,
         bsark TYPE bsark,
         augru TYPE augru,
         kunnr TYPE kunag,
         auart TYPE auart,
         vbeln TYPE vbeln,
         xblnr TYPE xblnr_v1,
         erdat TYPE erdat,
         ernam TYPE ernam,
         netwr TYPE netwr_ak,
         waerk TYPE waerk,
         docs  TYPE int8.
TYPES END OF gty_output.
TYPES gtt_output TYPE SORTED TABLE OF gty_output
                 WITH NON-UNIQUE KEY primary_key
                 COMPONENTS bukrs vkorg bsark augru kunnr auart.

INCLUDE zfii1210.
INCLUDE zfio1210.
INCLUDE zfip1210.

START-OF-SELECTION.
  lcl_data_processing=>get_instance( )->process_data( ).
