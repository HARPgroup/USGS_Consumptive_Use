<?php

$plugin = array(
  'label' => t('DMR Discharge Over Year in MGD'),
  'handler' =>  array(
    'class' => 'dHVariablePluginDefault',
    'file' => 'dh.display.inc',
  ),
  'variable' =>  array(
    'varname' => 'ECHO NPDES DMR Discharge Over Year in MGD',
    'vardesc' => 'Annual sum from Discharge Monitoring Report for U.S. EPA National Pollutant Discharge Elimination System.',
    'vocabulary' => 'epa_echo',
    'varunits' => 'mgd',
    'varkey' => 'dmr_ann_mgd',
    'datatype' => 'value',
    'varcode' => 'dmr_ann_mgd',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHECHODMRAnnMGD',
    'options' => '',
    'varabbrev' => 'DMR MGD',
  ),
);

?>