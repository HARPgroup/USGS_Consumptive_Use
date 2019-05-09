<?php

$plugin = array(
  'label' => t('DMR Discharge Over Year in MGY'),
  'handler' =>  array(
    'class' => 'dHECHODMRAnnMGY',
    'file' => 'dh_epa_echo_dmr.class.php',
  ),
  'variable' =>  array(
    'varname' => 'ECHO NPDES DMR Discharge Over Year in MGY',
    'vardesc' => 'Annual sum from Discharge Monitoring Report for U.S. EPA National Pollutant Discharge Elimination System.',
    'vocabulary' => 'epa_echo',
    'varunits' => 'mgy',
    'varkey' => 'dmr_ann_mgy',
    'datatype' => 'value',
    'varcode' => 'dmr_ann_mgy',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHECHODMRAnnMGY',
    'options' => '',
    'varabbrev' => 'DMR MGY',
  ),
);

?>