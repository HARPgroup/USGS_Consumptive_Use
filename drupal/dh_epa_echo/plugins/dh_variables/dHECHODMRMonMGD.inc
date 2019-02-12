<?php

$plugin = array(
  'label' => t('DMR Discharge Over Month in MGD'),
  'handler' =>  array(
    'class' => 'dHECHODMRMonMGD',
    'file' => 'dh_epa_echo_dmr.class.php',
  ),
  'variable' =>  array(
    'varname' => 'ECHO NPDES DMR Discharge Over Month in MGD',
    'vardesc' => 'Discharge Monitoring Report for U.S. EPA National Pollutant Discharge Elimination System.  Month in MG.',
    'vocabulary' => 'epa_echo',
    'varunits' => 'mgd',
    'varkey' => 'dmr_mon_mgd',
    'datatype' => 'value',
    'varcode' => 'dmr_mon_mgd',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHECHODMRMonMGD',
    'options' => '',
    'varabbrev' => 'DMR MGD',
  ),
);

?>