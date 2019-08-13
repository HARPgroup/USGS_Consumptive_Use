<?php
// this is a pluginless plugin (only a variable def)

$plugin = array(
'variable' =>  array(
    'varname' => 'internal_sum', // choose a unique, descriptive name here, can be the same as plugin label above
    'vardesc' => 'Outfall that monitors the cumulative waste streams within a facility before being discharged.',
    'vocabulary' => 'epa_echo',
    'varunits' => 'n/a',
    'varkey' => 'internal_sum',
    'datatype' => 'categories',
    'varcode' => 'vpdes_outfall_type',
    'isregular' => 0,
    'timestep' => 0,
    'timeunits' => 'n/a',
    'nodataval' => 0,
    'data_entry' => 'value,code',
    'plugin' => 'dHVariablePluginDefault',
    'options' => '',
    'varabbrev' => 'internal_sum',
  ),
);

?>