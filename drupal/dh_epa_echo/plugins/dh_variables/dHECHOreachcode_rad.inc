<?php
// this is a pluginless plugin (only a variable def)
$plugin = array(
  'variable' =>  array(
    'varname' => 'reachcode_rad', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'Reach code from the Reach Address Database (RAD) in which the facility may discharge. A reach code is nationally unique and permanent identifier for the waterbody, assigned by the US Geological Survey',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'reachcode_rad',
    'datatype' => 'code',
    'varcode' => 'RadReachcode',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'RadReachcode',
  ),
);

?>