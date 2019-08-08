<?php
// this is a pluginless plugin (only a variable def)

$plugin = array(
  'variable' =>  array(
    'varname' => 'NODI', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'Indicator that describes the reason why the data for an expected DMR value were not submitted by the permittee.',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'NODI',
    'datatype' => 'flag',
    'varcode' => 'NODICode',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'NODI',
  ),
);

?>