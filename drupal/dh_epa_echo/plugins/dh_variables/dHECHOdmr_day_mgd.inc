<?php
// this is a pluginless plugin (only a variable def)
$plugin = array(
  'variable' =>  array(
    'varname' => 'dmr_day_mgd', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'EPA ECHO Daily Average Discharge',
    'vocabulary' => 'epa_echo',
    'varunits' => 'mgd',
    'varkey' => 'dmr_day_mgd',
    'datatype' => 'Daily Average',
    'varcode' => 'dmr_value_nmbr',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'day',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'dmr_day_mgd',
  ),
);

?>