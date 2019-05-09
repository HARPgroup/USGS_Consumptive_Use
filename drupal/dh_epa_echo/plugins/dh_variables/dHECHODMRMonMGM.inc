<?php

$plugin = array(
  'label' => t('DMR Discharge Over Month in MGM'),
  'handler' =>  array(
    'class' => 'dHECHODMRMonMGM',
    'file' => 'dh_epa_echo_dmr.class.php',
  ),
  'variable' =>  array(
    'varname' => 'ECHO NPDES DMR Discharge Over Month in MGM',
    'vardesc' => 'Discharge Monitoring Report for U.S. EPA National Pollutant Discharge Elimination System.  Month in MG',
    'vocabulary' => 'epa_echo',
    'varunits' => 'mgm',
    'varkey' => 'dmr_mon_mgm',
    'datatype' => 'value',
    'varcode' => 'dmr_mon_mgm',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHECHODMRMonMGM',
    'options' => '',
    'varabbrev' => 'DMR MGM',
  ),
);

?>