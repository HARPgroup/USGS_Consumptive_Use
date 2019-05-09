<?php

$plugin = array(
  'label' => t('DMR Discharge Over Some Period in MGD'),
  'handler' =>  array(
    'class' => 'dHECHODMRPeriodMGD',
    'file' => 'dh_epa_echo_dmr.class.php',
  ),
  'variable' =>  array(
    'varname' => 'ECHO NPDES DMR Period Discharge',
    'vardesc' => 'Discharge Monitoring Report for U.S. EPA National Pollutant Discharge Elimination System.  Flexible time period.',
    'vocabulary' => 'epa_echo',
    'varunits' => 'mgd',
    'varkey' => 'dmr_period_mgd',
    'datatype' => 'value',
    'varcode' => 'dmr_value_nmbr',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHECHODMRPeriodMGD',
    'options' => '',
    'varabbrev' => 'DMR MGD',
  ),
);

?>