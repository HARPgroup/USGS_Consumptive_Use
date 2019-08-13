<?php
// this is a pluginless plugin (only a variable def)
$plugin = array(
  'variable' =>  array(
    'varname' => 'dmr_wk_mgd', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'EPA ECHO Weekly Average Discharge',
    'vocabulary' => 'epa_echo',
    'varunits' => 'mgd',
    'varkey' => 'dmr_wk_mgd',
    'datatype' => 'Weekly Average',
    'varcode' => 'dmr_value_nmbr',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'week',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'dmr_wk_mgd',
  ),
);

?>